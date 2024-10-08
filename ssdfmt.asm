; Copyright Joshua Hudson 2022-24

; Next up: FAT32 part of rebuild

BITS 16
CPU 8086

ORG 0

_start:
	;At entry point we probably aren't where want to be
	;fix DS and ES as required; leave SS and SP be
	;stack is either below us or at the top of 64KB
	push	cs
	call	.pic
.pic	pop	ax
	sub	ax, .pic
	pop	bx
	mov	cl, 4
	shr	ax, cl	; We *will* be at the start of a paragraph
	add	bx, ax
	mov	ds, bx
	mov	es, bx
	add	bx, 1000h
	mov	[blockseg], bx
	cld
	mov	[saveddl], dl
	mov	[rebootmsg], byte 0

	; Enumerate disks
	mov	dl, 80h
nextdisk:
	mov	di, dx
	and	di, 7Fh
	mov	cl, 4
	shl	di, cl
	add	di, disktable
	push	dx	; DX gets trashed
	push	es
	push	di
	mov	di, dap	; Allow this region to get trashed
	stc
	mov	ah, 08h
	int	13h
	pop	di
	pop	es
	jnc	.more
	jmp	.nomore
.more	mov	ah, 0
	; Check if BIOS reserved an extra cylinder for park heads
	; We have an SSD: we do *not* park heads
	cmp	ch, 255
	jne	.chkadj
	push	cx
	and	cl, 0C0h
	cmp	cl, 0C0h
	pop	cx
	jz	.fadj		; Maxxed
.chkadj	push	ax
	push	bx
	push	dx
	push	cx
	push	bp
	mov	bp, sp
	push	es
	mov	es, [blockseg]
	inc	ch
	jnz	.adjinc
	add	cl, 040h
.adjinc	mov	ax, 0201h
	xor	bx, bx
	int	13h
	jc	.noadj		; Only error matters
	mov	[bp + 2], cx
.noadj	pop	es
	pop	bp
	pop	cx
	pop	dx
	pop	bx
	pop	ax
	; Save CHS disk info to table
.fadj	mov	[di], word 512		; CHS must be 512 bytes per sector
	mov	al, dh
	inc	ax
	mov	[di + 4], ax
	mov	ah, 0
	mov	al, cl
	and	al, 63
	mov	[di + 2], ax
	mov	al, ch
	mov	ah, cl
	mov	cl, 6
	shr	ah, cl
	inc	ax
	mov	[di + 6], ax
	mul	word [di + 2]
	mov	[di + 8], ax
	mov	[di + 10], dx
	;mov	ax, [di + 8]		; It's still in ax
	mul	word [di + 4]
	mov	[di + 8], ax
	mov	ax, [di + 10]
	mov	[di + 10], dx
	mul	word [di + 4]
	xor	cx, cx
	add	[di + 10], ax
	adc	dx, cx		;Just the carry
	mov	[di + 12], dx
	mov	[di + 14], cx	;Zero

	; Now check for LBA and larger size
	pop	dx
	push	dx
	call	diskhaslba
	jc	.nolba
	push	dx
	mov	bx, [si + 18h]
	mov	[di], bx
	mov	cl, 9
	shr	bx, cl
	mov	ax, [si + 10h]
	mul	bx
	mov	[di + 8], ax
	mov	[di + 10], dx
	mov	ax, [si + 12h]
	mul	bx
	add	[di + 10], ax
	adc	dx, 0
	mov	[di + 12], dx
	mov	ax, [si + 14h]
	mul	bx
	add	[di + 14], ax
	adc	dx, 0
	or	dx, dx
	jz	.novflo
	; Disk is too big; just say max size
	mov	ax, 0FFFFh
	mov	[di + 8], ax
	mov	[di + 10], ax
	mov	[di + 12], ax
	mov	[di + 14], ax
.novflo	pop	dx
.nolba	inc	dx
	jmp	nextdisk

.nomore	pop	dx
	and	dx, 7Fh
	mov	[numdisks], dx
	xor	ax, ax

	; Draw box around screen
	xor	ax, ax
	mov	cx, 1950h	; 80x25
	mov	bl, 3
	call	draw_box
	mov	cx, 14
	mov	dx, 0020h
	mov	si, s_name
	call	out_stringat
	mov	bh, 0
	mov	dx, 0305h
	mov	ah, 02h
	int	10h
	
	; Draw disks
mainscreen:
	mov	bl, 8
	mov	si, s_vsn
	mov	cx, 4
	mov	dx, 014Bh
	call	out_stringat
	mov	si, disktable
	xor	dx, dx
.nextdiskdraw:
	cmp	dx, [numdisks]
	jb	.nextdiskdraw2
	jmp	.lastdiskdrawn
.nextdiskdraw2:
	push	dx
	mov	bh, 0
	mov	dh, dl
	mov	dl, 6
	add	dh, 2
	mov	ah, 02h
	int	10h
	
	; Keystroke and disk number
	mov	bl, 7
	pop	ax
	push	ax
	mov	cx, 1
	add	al, '1'
	mov	ah, 09h
	int	10h
	inc	dl
	mov	ah, 02h
	int	10h
	mov	ax, 0929h
	int	10h
	add	dl, 3
	mov	ah, 02h
	int	10h

	; Now disk size in MB
	push	si
	xor	ax, ax
	mov	dx, (1024 * 1024) / 65536
	div	word [si]
	xchg	ax, bx
	std
	add	si, 14
	mov	di, bignum + 6
	xor	dx, dx
	mov	cx, 4
	xor	dx, dx
.divd1	lodsw
	div	bx
	stosw
	loop	.divd1
	mov	di, sizestr + 19
	mov	cx, 20
	mov	bx, 10
.divd2a	push	cx
	mov	cx, 4
	mov	si, bignum + 6
	xor	dx, dx
.divd2b mov	ax, [si]
	div	bx
	mov	[si], ax
	sub	si, 2
	loop	.divd2b
	pop	cx
	add	dl, '0'
	mov	al, dl
	stosb
	loop	.divd2a
	cld
	mov	si, sizestr
	mov	bx, 7
	mov	cx, 20
	call	out_string
	mov	si, s_mb
	mov	cx, 3
	call	out_stringat
	pop	si
	pop	dx
	add	si, 16
	inc	dx
	jmp	.nextdiskdraw
.lastdiskdrawn:
	mov	bh, 0
	mov	dh, dl
	mov	dl, 6
	add	dh, 2
	mov	ah, 02h
	int	10h
	mov	si, s_esc
	mov	cx, 9
	call	out_string

keyboarddisk:
	mov	ah, 0
	int	16h
	cmp	al, 27
	jne	.notxit
	jmp	exit
.notxit	cmp	al, '1'
	jb	keyboarddisk
	mov	dl, [numdisks]
	add	dl, '0'
	cmp	al, dl
	ja	keyboarddisk
	mov	dl, al
	add	dl, 80h - '1'

havedisk:
	mov	[disk], dl
	call	diskclearlast
	mov	dl, [disk]

	mov	es, [blockseg]
	xor	bx, bx
	mov	cx, 1
	mov	dh, 0
	mov	ax, 0201h
	int	13h
	jc	short .error
	cmp	[es:bx + 510], word 0AA55h
	jne	.novrchk
	cmp	[es:bx + 1BEh + 4], byte 0
	je	.ovrchk
	cmp	[es:bx + 1CEh + 4], byte 0
	je	.ovrchk
	cmp	[es:bx + 1DEh + 4], byte 0
	je	.ovrchk
	cmp	[es:bx + 1EEh + 4], byte 0
	jne	short .novrchk
.ovrchk:
	push	ds
	pop	es
	push	dx
	mov	si, s_data1
	mov	al, dl
	sub	al, 80h - '1'
	mov	[si + 5], al
	mov	bl, 9
	mov	ax, 0918h
	mov	cx, 041Eh
	call	draw_box
	mov	cx, 15
	mov	dx, 0A1Ah
	mov	si, s_data1
	call	out_stringat
	mov	cx, 16
	mov	dx, 0B1Ah
	mov	si, s_data2
	call	out_stringat
	mov	ah, 0
	int	16h
	pop	dx
	cmp	al, 'y'
	je	.novrchk	; Overwrite!
	jmp	mainscreenreturn
.error	push	ds
	pop	es
	jmp	diskerror

.novrchk:
	push	ds
	pop	es
	;prompt physical sector size
	mov	ax, 0718h
	mov	cx, 0B1Eh
	mov	bl, 11
	call	draw_box
	mov	cx, 15
	mov	dx, 0720h
	mov	si, s_sect
	call	out_stringat
	mov	cx, 25
	mov	dx, 081Ah
	mov	si, s_stxt1
	call	out_stringat
	mov	cx, 26
	mov	dx, 091Ah
	mov	si, s_stxt2
	call	out_stringat
	mov	cx, 26
	mov	dx, 0A1Ah
	mov	si, s_stxt3
	call	out_stringat
	mov	bl, 7
	call	getdiskstructure
	mov	di, [di]
	cmp	di, word 512
	je	.n512
	cmp	di, word 1024
	je	.n1024
	cmp	di, word 2048
	je	.n2048
	cmp	di, word 4096
	je	.n4096
	cmp	di, word 8192
	je	.n8192
	cmp	di, word 16384
	je	.n16384
	mov	ah, 0FEh
	jmp	diskerror	; int13h is bugged!
.n512	mov	cx, 12
	mov	dx, 0B1Bh
	mov	si, s_512
	call	out_stringat
.n1024	mov	cx, 13
	mov	dx, 0C1Bh
	mov	si, s_1024
	call	out_stringat
.n2048	mov	cx, 13
	mov	dx, 0D1Bh
	mov	si, s_2048
	call	out_stringat
.n4096	mov	cx, 13
	mov	dx, 0E1Bh
	mov	si, s_4096
	call	out_stringat
.n8192	mov	cx, 13
	mov	dx, 0F1Bh
	mov	si, s_8192
	call	out_stringat
.n16384	mov	cx, 14
	mov	dx, 101Bh
	mov	si, s_16384
	call	out_stringat
.koops	mov	ah, 0
	int	16h
	mov	bx, 512
	cmp	al, '5'
	je	.ksize
	mov	bx, 1024
	cmp	al, '1'
	je	.ksize
	mov	bx, 2048
	cmp	al, '2'
	je	.ksize
	mov	bx, 4096
	cmp	al, '4'
	je	.ksize
	mov	bx, 8192
	cmp	al, '8'
	je	.ksize
	mov	bx, 16384
	cmp	al, '6'
	je	.ksize
	cmp	al, 27
	jne	.koops
.kesc	jmp	mainscreen
.ksize	mov	[minclustsize], bx
	shr	bh, 1
	mov	bl, bh
	mov	bh, 0
	mov	[minclustsizem], bx
	mov	[diskebr], byte 0
	
	mov	ax, 0718h
	mov	cx, 0B1Eh
	mov	bl, 11
	call	draw_box
	mov	cx, 14
	mov	dx, 0720h
	mov	si, s_min
	call	out_stringat
	mov	cx, 24
	mov	dx, 081Ah
	mov	si, s_min1
	call	out_stringat
	mov	cx, 24
	mov	dx, 091Ah
	mov	si, s_min2
	call	out_stringat
	mov	cx, 13
	mov	dx, 0B1Bh
	mov	si, s_dos2
	call	out_stringat
	mov	cx, 13
	mov	dx, 0C1Bh
	mov	si, s_dos3
	call	out_stringat
	mov	cx, 14
	mov	dx, 0D1Bh
	mov	si, s_dos33
	call	out_stringat
	mov	cx, 14
	mov	dx, 0E1Bh
	mov	si, s_dos4
	call	out_stringat
	mov	cx, 14
	mov	dx, 0F1Bh
	mov	si, s_952
	call	out_stringat
	mov	cx, 22
	mov	dx, 101Bh
	mov	si, s_flba
	call	out_stringat
.koops2	mov	ah, 0
	int	16h
	cmp	al, 27
	je	.kesc
	cmp	al, '1'
	jb	.koops2
	cmp	al, '6'
	ja	.koops2
	mov	[oslevel], al
	call	getdiskstructure
	cmp	al, '6'
	jne	.nflba
	jmp	mkfat32.lba
.nflba	mov	[disklba], byte 0	; FAT32 can turn it back on, FAT16 can't
	cmp	al, '5'
	jne	.nf32
	jmp	mkfat32
.nf32	cmp	al, '1'
	jne	mkfat16
	;jmp	mkfat12

	; If we got here we either have a really small disk or an OS version that crimps us to 64k sectors anyway.
	; Fine. Make a small disk.
mkfat12	mov	bp, 0A000h
	mov	ax, [di + 2]
	mul	word [di + 4]
	mul	word [di + 6]
	mov	bx, [di + 2]
	mov	cx, [di + 4]
	mov	si, [di + 6]
	or	dx, dx		; MS-DOS before 3.31 can't access disk sectors past 32 MB due to 16 bit math in disk geometry
	jz	.n64k
	xor	ax, ax
	mov	dx, 1
	jmp	mksmallcommon
.n64k	cmp	ah, 0
	jne	mksmallcommon
	mov	bx, 7		; Below 256 sectors isn't worth bothering with
	mov	si, s_small	; Below 224 sectors we would miscalculate on some inputs
	mov	cx, 17		; The difference isn't worth handling; below 1MB SSD is nuts
	mov	dx, 161Fh
	call	out_stringat
	jmp	mainscreenreturn
mksmallcommon:
	xor	bx, bx
	mov	[diskebroffset], bx
	mov	[diskebroffset + 2], bx
	mov	[diskebr], byte 2
	mov	[usabledisksize], ax
	mov	[usabledisksize + 2], dx
	call	getleadingsectors
	mov	ds:[bp + 8], bx
	mov	ds:[bp + 10], cx
	call	calcsizes12
	jmp	mkall

	; Got here trying to make a FAT16; that is, anything below DOS 7.1
mkfat16	mov	bl, al
	mov	ax, [di + 2]	; Use CHS size in case whole isn't LBA accessible
	mul	word [di + 4]
	mul	word [di + 6]	; Size is in DX:AX
	or	dx, dx
	jnz	.i64k
	cmp	ah, 0
	je	mkfat12		; Protect builder algorithm against ridiculously small inputs
.i64k	cmp	bl, '4'
	jae	.oksiz
	cmp	dx, 10h		;According to some documentation, DOS 3.31 can't handle disk sizes > 512MB so we truncate.
	jbe	.n512m
	xor	ax, ax
	mov	dx, 10h
.n512m	cmp	bl, '2'
	jne	.oksiz
	or	dx, dx		; MS-DOS before 3.31 can't access disk sectors past 32 MB due to 16 bit math in disk geometry
	jz	.oksiz
	xor	ax, ax
	mov	dx, 1
	mov	ax, 0FFFFh
.oksiz	mov	bp, 0A000h
	mov	[diskebr], byte 0
	cmp	bl, '2'		; MS-DOS this old doesn't know about EBR
	jbe	mksmallcommon
.next	call	calcebralign
	call	calcsizes16
	or	[diskebr], byte 1
	test	[diskebr], byte 2
	jz	.next
	jmp	mkall

; New mkfat32 will live here

mkall:
	mov	ax, [minclustsizem]
	xor	dx, dx
	mov	si, 0A000h
.scan	add	ax, [si + 16]
	adc	dx, [si + 18]
	add	ax, [si + 16]
	adc	dx, [si + 18]
	add	ax, [si + 6]
	adc	dx, 0
	add	si, 32
	cmp	[si], byte 0
	jne	.scan
	mov	bp, si
	mov	cx, 01701h
	mov	bx, 04F07h
	mov	si, pgbar
	call	initprogressbar
	mov	[diskebr], byte 3
.go	call	applyentry
	jc	.error
	and	[diskebr], byte 0FDh
	cmp	bp, 0A000h
	jne	.go
	jmp	afterfat
.error	jmp	diskerror

;Calculate offset to next EBR entry
;On the *first call*, DX:AX is the size of the disk in 512 byte sectors
calcebralign:
	cmp	[oslevel], byte '5'
	jae	.calcebralign32
	test	[diskebr], byte 1
	jz	.isprimary
	mov	bx, [diskpriorebr]
	mov	cx, [diskpriorebr + 2]
	mov	[diskebroffset], bx
	mov	[diskebroffset + 2], cx
	add	bx, [minclustsizem]
	adc	cx, 0
	jmp	.calcmaxfat16
.isprimary:
	xor	bx, bx
	mov	[diskebroffset], bx
	mov	[diskebroffset + 2], bx
	mov	[usabledisksize], ax
	mov	[usabledisksize + 2], dx
	mov	[usabledisksize2], ax
	mov	[usabledisksize2 + 2], dx
	call	getleadingsectors
.calcmaxfat16:
	add	bx, 80h	; FFF6h * 20h + 100h * 2h (see 40h as loop terminator in fatsize)
	add	cx, 20h
	push	bx
	push	cx
	sub	bx, [usabledisksize2]
	sbb	cx, [usabledisksize2 + 2]
	jnc	.whole				; Doesn't fit: must be last
	not	cx
	neg	bx
	adc	cx, 0
	jnz	.ebr
	cmp	bx, 2048
	jb	.whole		; That partition is too small to bother with
.ebr	pop	cx
	pop	bx
	; We have the stopping point in CX:BX; compute alignment
	call	cylinderaligncxbx
	mov	[diskpriorebr], bx
	mov	[diskpriorebr + 2], cx
	sub	[usabledisksize2], bx
	sbb	[usabledisksize2 + 2], cx
	sub	bx, [diskebroffset]		; DOES THIS MAKE SENSE ??
	sbb	cx, [diskebroffset + 2]
	mov	[usabledisksize], bx
	mov	[usabledisksize + 2], cx
	jmp	.finish
	ret
.whole	pop	cx
	pop	bx
	or	[diskebr], byte 2
.finish	call	getleadingsectors
	mov	ds:[bp + 8], bx
	mov	ds:[bp + 10], cx
	ret
.calcebralign32:			; Just use whole disk (TODO: memory constraint)
	jmp	.whole

cylinderaligncxbx:
	mov	ax, [di + 4]
	mul	word [di + 2]	; Align to this many sectors
	mov	si, ax		; EBR must start at the the start of a cylinder
	xor	dx, dx
	mov	ax, cx
	div	si
	mov	ax, bx
	div	si
	sub	bx, dx		; Subtract off the partial cylinder
	sbb	cx, 0
.scan	xor	dx, dx		; Now scan backwards for an aligned cylinder
	mov	ax, cx
	div	word [minclustsizem]
	mov	ax, bx
	div	word [minclustsizem]
	or	dx, dx
	jz	.found
	sub	bx, si
	sbb	cx, 0
	jmp	.scan
.found	ret

; Calculate size of FAT partition, this entry point means FAT32 is permissible
; Input: BP = output area; DI = disktable
calcsizes32:
	call	calcsizesprep
	jz	calcsizes16	; Clearly impossible to fit a FAT32 here; this protects the calc routine from underflow
	mov	bx, [minclustsizem]
	call	fatsize32
	or	si, si
	jnz	.ok32
	cmp	cx, 0FFF6h
	jbe	calcsizes16	; Doesn't fit
.ok32	mov	ds:[bp], byte 32
	push	bx		; Yup calcsizesstuff takes an argument on the stack
	push	ax
	mov	ax, bx		; Reserved sectors = 3 clusters
	add	bx, ax		; Could change this to 3 physical sectors (2kb minimum)
	add	bx, ax
	pop	ax
	jmp	calcsizesstuff
calcsizes16:			; FAT32 not permissible, FAT16 is
	call	calcsizesprep
	call	reservedsectorsadj
	or	dx, dx
	jz	.ok16p
	test	ah, 80h		; According to the documentation this must be a FAT12 or some DOS is unhappy
	jz	calcsizes12
.ok16p	mov	bx, [minclustsizem]
	mov	cx, 0FFF4h		; Maximum number of clusters is FFF4
	mov	si, 100h
	call	fatsize
	cmp	ax, 0FF5h
	jb	calcsizes12	; MS operating systems uses a < here; FF5 cannot occur
	mov	ds:[bp], byte 16
	jmp	calcsizesstuffnot32
calcsizes12:			; FAT16 not permissible either; codebase assumes there's no way FAT12 can't be made.
	call	calcsizesprep
	call	reservedsectorsadj
	mov	bx, [minclustsizem]
	mov	cx, 0FF4h	; Maximum number of clusters is FF4
	mov	si, 155h
	call	fatsize
	mov	ds:[bp], word 010Ch
calcsizesstuffnot32:
	xchg	ax, cx
	xchg	ax, bx
	xor	dx, dx
	xor	si, si
	push	bx
	mov	bx, 1
	cmp	[oslevel], byte '5'
	jb	calcsizesstuff
	mov	bx, [minclustsizem]
calcsizesstuff:
	xchg	di, bp
	mov	[di + 2], bx
	add	bx, 32		; Number of sectors for root directory
	mov	[di + 6], bx
	pop	bx
	mov	[di + 4], bx	; logical sectors per cluster
	mov	[di + 20], cx	; number of clusters
	mov	[di + 22], si
	mov	[di + 16], ax
	mov	[di + 18], dx
	sub	cx, 2		; Number of clusters in DX:AX goes to number of sectors in AX:CX
	sbb	si, 0
	mov	ax, cx
	mul	bx
	mov	cx, ax
	xchg	ax, dx
	mul	bx
	add	ax, dx
	add	cx, [di + 20]
	adc	ax, [di + 22]
	add	cx, [di + 20]
	adc	ax, [di + 22]
	mov	[di + 12], cx
	mov	[di + 14], ax
	mov	bx, [diskebroffset]
	mov	cx, [diskebroffset + 2]
	mov	[di + 24], bx
	mov	[di + 26], cx
	cmp	[di], byte 32	; Get partition type code
	jne	.f1216
	test	[disklba], byte 1
	jne	.chs32
	mov	[di + 1], byte 0Ch
	jmp	.final
.chs32	mov	[di + 1], byte 0Bh
	jmp	.final
.f1216	test	[disklba], byte 1
	jz	.chs
	mov	[di + 1], byte 0Eh
	jmp	.final
.chs	mov	ax, [di + 14]	; Big fat is defiend as FAT that doesn't fit within the first 32MB of disk
	or	ax, ax
	jnz	.big
	mov	ax, [di + 12]
	add	ax, [di + 8]
	jc	.big
	cmp	[di], byte 12
	jne	.f16s
	mov	[di + 1], byte 1
	jmp	.final
.f16s	mov	[di + 1], byte 4
.big	mov	[di + 1], byte 6
.final	add	di, 32
	mov	[di], byte 0
	xchg	di, bp
	ret
calcsizesprep:
	mov	ax, [usabledisksize]
	mov	dx, [usabledisksize + 2]
	call	getleadingsectors
	mov	ds:[bp + 12], ax
	mov	ds:[bp + 14], dx
	sub	ax, bx
	sbb	dx, cx
	ret
reservedsectorsadj:
	mov	bx, 1
	cmp	[oslevel], byte '5'
	jb	.one
	mov	bl, [minclustsizem]
.one	add	ax, bx
	adc	dx, 0
	ret

applyentry:
	push	es
	mov	es, [blockseg]
	sub	bp, 32
	cmp	bp, 0A000h
	jne	.ebr
	and	[diskebr], byte 0FEh
.ebr	mov	si, bp
	mov	ax, [si + 24]
	mov	dx, [si + 26]
	mov	[diskebroffset], ax
	mov	[diskebroffset + 2], dx
	mov	al, [si]
	cmp	al, 12
	je	.mkfat12fs
	cmp	al, 16
	je	.mkfat16fs
	cmp	al, 32
	je	.mkfat32fs
	mov	ah, 7Fh
	stc
.error	pop	es
	ret
.mkfat12fs:
	mov	dx, 0FFh
	jmp	.mkfat1216common
.mkfat16fs:
	mov	dx, 0FFFFh
.mkfat1216common:
	mov	ax, 0FFF8h
	mov	bx, [si + 16]
	mov	cx, [si + 8]
	mov	si, [si + 10]
	add	cx, ds:[bp + 2]
	adc	si, 0
	call	writeemptyfat
	jc	.error
	mov	si, bp
	push	word [si + 10]
	push	word [si + 8]
	push	word [si + 20]
	push	word [si + 16]
	push	word [si + 4]
	push	word [si + 2]
	call	gensuperblock16
	mov	si, bp
	cmp	[si], byte 12
	jne	.nf2l
	mov	[es:3Ah], byte '2'
.nf2l	xor	bx, bx
	mov	cx, [si + 8]
	mov	si, [si + 10]
	mov	ax, 0301h
	cmp	[oslevel], byte '5'
	jb	.osm2
	mov	al, [minclustsizem]
.osm2	xor	bx, bx
	mov	dl, [disk]
	call	lineardiskop
	jc	.error
	call	progressal_base
	mov	ax, [minclustsizem]	; Zero rest of physical cluster if FAT superblock
	dec	ax			; is located at the end of the physical cluster
	jz	.nbfill
	and	ax, cx
	jz	.nbfill
	sub	cx, ax
	sbb	si, 0
	mov	bx, 204h
	mov	ah, 3
	call	lineardiskop
.nbfill	mov	bl, [ds:bp + 1]
	call	patchsuperblockmbr
	mov	cx, [ds:bp + 24]
	mov	si, [ds:bp + 26]
	mov	al, [minclustsizem]
	test	[diskebr], byte 1
	jz	.mbr
	mov	al, [ds:bp + 8]			; It's a 32 bit subtract but we *know* the difference is between 1 and 16
	sub	al, [ds:bp + 24]		; No point computing the rest at all
	jnz	.mbr				; There's a distance
	inc	al				; EBR directly overlaps boot sector; should not normally happen
.mbr	mov	ah, 3
	xor	bx, bx
	call	lineardiskop
	jc	.error
	call	progressal_base
	mov	[rebootmsg], byte 1
	test	[diskebr], byte 1		; CF = 0
	jnz	.ebrnc
	call	writebootcheck
.ebrnc	pop	es
	ret
.mkfat32fs:
	ret					; $$$ TODO $$$$


mkfat32:
	cmp	[di], word 512
	jne	.chs
	mov	ax, [di + 2]
	mul	word [di + 4]
	mul	word [di + 6]
	cmp	ax, [di + 8]
	jne	.lba
	cmp	dx, [di + 10]
	jne	.lba
	xor	ax, ax
	cmp	ax, [di + 12]
	jne	.lba
	cmp	ax, [di + 14]
	je	.chs
.lba	mov	[disklba], byte 1
	jmp	.mkfat32
.chs	mov	[disklba], byte 0
.mkfat32:
	test	[disklba], byte 1
	je	.lbasz
	mov	ax, [di + 2]
	mul	word [di + 4]
	mul	word [di + 6]
	jmp	.hsz
.lbasz	cmp	[di + 12], word 0
	jne	.glbasz
	cmp	[di + 14], word 0
	jne	.glbasz
	mov	ax, [di + 8]
	mov	dx, [di + 10]
	jmp	.hsz
.mkf16	jmp	mkfat16
.glbasz	mov	ax, 0FFFFh	; MBR can't handle larger
	mov	dx, ax
.hsz	call	getleadingsectors
	sub	ax, bx
	sbb	dx, cx
	mov	bx, [minclustsizem]
	call	fatsize32
	or	si, si
	jnz	.afat32
	cmp	cx, 0FFF5h	; MS OSes use a < here
	jb	.mkf16		; Too small, make a FAT16

	;Get size for progress bar
.afat32	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	mov	si, pgbar
	add	ax, ax
	adc	dx, dx
	mov	cx, 1 + 32 + 4
	add	cx, bx
	add	cx, bx
	add	cx, bx
	add	ax, cx
	adc	dx, 0
	mov	cx, 01701h
	mov	bx, 04F07h
	call	initprogressbar
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	;Write filesystem to disk
	push	si
	push	cx
	push	bx
	push	dx
	push	ax
	mov	cx, bx	; Reserve slots for backup boot sectors and fs info sector
	add	cx, bx
	add	cx, bx
	call	getleadingsectors
	push	bx
	mov	si, cx
	xchg	bx, cx
	add	cx, bx
	adc	si, 0
	call	writeemptyfat32
	jc	.error6
	pop	bp
	pop	ax
	pop	dx
	pop	bx
	pop	cx
	pop	si
	mov	es, [blockseg]
	call	gensuperblock32
	push	bx
	mov	al, bl
	mov	ah, 3
	xor	si, si
	mov	cx, bp		; number of leading sectors
	xor	bx, bx
	mov	dl, [disk]
	call	lineardiskop
	jc	.error1
	call	progressal_base
	pop	bx
	add	cx, bx
	push	bx
	mov	al, bl
	mov	ah, 3
	xor	bx, bx
	call	lineardiskop
	jc	.error1
	call	progressal_base
	pop	bx
	add	cx, bx
	mov	al, bl
	mov	ah, 3
	mov	bx, [es:30h]
	mov	bh, bl
	mov	bl, 0
	shl	bh, 1
	call	lineardiskop
	jc	.error
	call	progressal_base
	mov	bl, 0Bh
	call	patchsuperblockmbr
	xor	bx, bx
	xor	cx, cx
	inc	cx
	mov	dh, 0
	mov	dl, [disk]
	mov	ah, 03h
	mov	al, [minclustsizem]
	int	13h
	jc	short	.error
	mov	al, 1
	call	progressal_base
	mov	[rebootmsg], byte 1
	call	writebootcheck
	jc	short	.error
	mov	al, 4
	call	progressal_base
	push	ds
	pop	es
	jmp	afterfat

.error6	sub	sp, 2
.error5	sub	sp, 8
.error1	sub	sp, 2
.error	push	ds
	pop	es
	jmp	diskerror

fatsize:
	; Expects number of sectors in partition in DX:AX, starting cluster size in BX, max clusters in CX, entries per sector in SI
	; Returns AX = number of clusters, BX = number of sectors per FAT, CX = number of sectors per cluster
	; Preserves DI, BP
	push	bp
	mov	bp, sp
	sub	sp, 28
	sub	ax, 33		; Subtract off 33 sectors that are neither FAT nor DATA
	sbb	dx, 0
	push	di
	xor	di, di
	mov	[bp - 2], ax	; number of logical sectors low
	mov	[bp - 4], dx	; number of logical sectors high
	mov	[bp - 6], bx	; number of logical sectors per physical sector
	mov	[bp - 8], si	; number of entries per logical sector
	mov	ax, si
	mul	bx		; Cannot overflow: biggest is 40h * 155h = 9440h
	mov	[bp - 10], ax	; number of entries per physical sector
	mov	dx, di
	mov	ax, [bp - 4]
	div	bx
	mov	[bp - 14], ax	; number of physical sectors high
	mov	ax, [bp - 2]
	div	bx
	mov	[bp - 12], ax	; number of physical sectors low
	mov	[bp - 16], cx	; max number of clusters
	mov	[bp - 18], di	; best number of clusters
	mov	[bp - 20], bx	; best number of logical sectors per cluster
	mov	[bp - 22], di	; best number of logical data sectors low
	mov	[bp - 24], di	; best number of physical data sectors high
	mov	[bp - 26], di	; best number of sectors per FAT
	mov	cx, 40h
	mov	[bp - 28], cx	; loop terminator
	pop	di

.trysectors:
	push	bx
	mov	ax, bx		; Factor = logical sectors per cluster / physical sectors per cluster
	div	byte [bp - 6]
	mov	cx, ax
	mov	bx, [bp - 10]
	mov	ax, [bp - 4]
	xor	dx, dx		; fatsizexcomp needs number of physical sectors not logical
	div	word [bp - 6]
	push	ax
	mov	ax, [bp - 2]
	div	word [bp - 6]
	or	dx, dx
	pop	dx
	jz	.xactc
	sub	ax, 1
	sbb	dx, 0
.xactc	call	fatsizexcomp
	pop	bx

	or	dx, dx
	jnz	.vd		; Way too large
	cmp	ax, [bp - 16]
	ja	.vd
	mov	[bp - 28], bx	; Once we don't have to clamp number of clusters, don't try bigger sizes
	jmp	.nvd		; this keeps wasted slack space to a reasonable amount
.vd	mov	ax, [bp - 16]
	;We changed number of clusters, compute logical sectors per fat
	push	ax
	xor	dx, dx
	div	word [bp - 10]
	jz	.exact
	inc	ax
.exact	xchg	ax, cx
	pop	ax
.nvd	mov	si, ax		; High half of sectors per FAT must be 0 by now
	mul	bx
	cmp	dx, [bp - 24]
	ja	.better
	cmp	ax, [bp - 22]
	jbe	.worse
.better	mov	[bp - 18], si	; save current best
	mov	[bp - 20], bx
	mov	[bp - 22], ax
	mov	[bp - 24], dx
	mov	ax, cx
	mul	word [bp - 6]
	mov	[bp - 26], ax
.worse	shl	bx, 1
	cmp	bx, [bp - 28]
	jb	short	.trysectors	; Biggest = 16k clusters

	mov	ax, [bp - 18]
	mov	bx, [bp - 26]
	mov	cx, [bp - 20]

	mov	sp, bp
	pop	bp
	ret

;Input: Number of secters in partition in DX:AX, sectors per cluster in BX
;Output: Number of sectors in FAT in DX:AX, number of clusters in SI:CX, actual sectors per cluster in BX
;Preserves DI, BP
fatsize32:
	push	bp

	test	dx, 8000h	; 1TB FAT32 needs 8kb clusters
	jz	.szc2
	cmp	bx, 16
	jae	.szok
	mov	bx, 16

.szc2	cmp	bx, 8	; Don't make FAT32 with less than 4KB clusters.
	jae	.szok
	mov	bx, 8
.szok:
	;Convert 512 byte sectors to clusters
	xchg	ax, bp
	xchg	ax, dx
	xor	dx, dx
	div	bx
	xchg	ax, bp
	div	bx
	or	dx, dx
	mov	dx, bp
	jz	.nodummy
	sub	ax, 1
	sbb	dx, 0
.nodummy:
	
	; Boot sector region is 3 clusters in size
	sub	ax, 3
	sbb	dx, 0

	; Always fits because bx is <= 32
	mov	bp, bx
	mov	cl, 7
	shl	bx, cl		; Number of clusters addressed per sector
	mov	cx, 1
	call	fatsizexcomp
	xchg	ax, cx
	xchg	dx, si
	mov	bx, bp

	; Fat size is in physical sectors, convert to logical
	push	di
	mov	di, dx
	mul	bx
	push	ax
	xchg	ax, dx
	mul	bx
	xchg	ax, dx
	add	dx, di
	pop	ax
	pop	di

	pop	bp
	ret

;FAT Size computation
;DX:AX = number of sectors (ficticious sectors and reserved sectors not included)
;BX = entries per sector
;CX = sectors per cluster
;Returns number of clusters in DX:AX, number of sectors per FAT in SI:CX
;Preserves BX, DI, BP
fatsizexcomp:
	push	bp
	mov	bp, sp
	sub	sp, 16
	;[bp - 2] = low half of numerator or low half of clusters
	;[bp - 4] = high half of numerator or high half of clusters
	;[bp - 6] = numerator overflow
	;[bp - 8] = low half of sectors in filesystem
	;[bp - 10] = high half of sectors in filesystem
	;[bp - 12] = low half of number of sectors in FAT
	;[bp - 14] = high half of number of sectors in FAT
	;[bp - 16] = sectors per cluster

	mov	[bp - 16], cx
	;Add two bogus clusters worth of sectors so the calculation is easier
	shl	cx, 1
	add	ax, cx
	adc	dx, 0

	mov	[bp - 8], ax
	mov	[bp - 10], dx

	;The calculation is clusters = (sectors + 2 * sectors per cluster) * entries per sector / (2 + sectors per cluster * entries per sector)
	;but it calculates up to entries per sector too many clusters so we have to check it below.

	mov	[bp - 4], dx
	mul	bx
	mov	[bp - 2], ax
	mov	ax, [bp - 4]
	mov	[bp - 4], dx
	mul	bx
	add	[bp - 4], ax
	adc	dx, 0
	mov	[bp - 6], dx
	mov	ax, [bp - 16]
	mul	bx		; Cannot overflow, max = 80h * 155h
	inc	ax
	inc	ax
	mov	cx, ax
	mov	dx, [bp - 6]
	mov	ax, [bp - 4]
	div	cx		; fatsize32 prevents overflow here
	mov	[bp - 4], ax
	mov	ax, [bp - 2]
	div	cx
	mov	[bp - 2], ax

	;So, the problem is, this is too large by up to number of entires (BX)
	;We have to check this and recompute down

	xor	dx, dx
	mov	ax, [bp - 4]
	div	bx
	push	ax
	mov	ax, [bp - 2]
	div	bx
	or	dx, dx
	pop	dx
	jz	.nup
	add	ax, 1
	adc	dx, 0
.nup	mov	[bp - 12], ax
	mov	[bp - 14], dx

	; Now figure out how many sectors used
	mov	cx, [bp - 16]
	mov	ax, [bp - 2]
	mul	cx
	mov	[bp - 6], dx
	push	ax
	mov	ax, [bp - 4]
	mul	cx
	add	ax, [bp - 6]
	xchg	ax, dx
	pop	ax
	add	ax, [bp - 12]
	adc	dx, [bp - 14]
	add	ax, [bp - 12]
	adc	dx, [bp - 14]

	; Check for overflow
	sub	ax, [bp - 8]
	sbb	dx, [bp - 10]
	jnc	.ncr
	not	ax
	add	ax, 1
	not	dx
	adc	dx, 0
	div	cx		; Cannot overflow
	or	dx, dx
	jz	.noc
	inc	ax
	xor	dx, dx
	;AX contains overflow in clusters, DX 0
.noc	sub	[bp - 2], ax
	sbb	[bp - 4], dx
	
.ncr	mov	ax, [bp - 2]
	mov	dx, [bp - 4]
	mov	cx, [bp - 12]
	mov	si, [bp - 14]

	sub	ax, 2	; Take off the bogus clusters we added at start
	sbb	dx, 0	; Cluster numbers 0 and 1 are unusable; first cluster is #2

	mov	sp, bp
	pop	bp
	ret

; Arguments on stack
;	es:[0] - output location
;	[sp] = ret
;	[sp + 2] = [bp + 4] - number of reserved sectors
;	[sp + 4] = [bp + 6] - number of sectors per cluster
;	[sp + 6] = [bp + 8] - number of sectors per fat
;	[sp + 8] = [bp + 10] - number of clusters
;	[sp + 10] = [bp + 12] - number of sectors before start of fs (hidden sectors)
;	Cleans up the stack itself
;	Preserves DI, BP
gensuperblock16:
	push	bp
	mov	bp, sp
	push	di
	sub	sp, 4

	xor	si, si
	mov	ax, [bp + 6]
	mul	word [bp + 10]
	add	ax, [bp + 8]
	adc	dx, si
	add	ax, [bp + 8]
	adc	dx, si
	add	ax, [bp + 4]
	adc	dx, si
	mov	[bp - 4], dx	; [BP - 4] - total sectors high
	mov	[bp - 6], ax	; [BP - 6] - total sectors low
	
	xor	di, di
	mov	ax, 03CEBh	; JMP to 0x3E
	stosw
	mov	al, 90h
	stosb
	mov	cx, 4
	mov	si, oemlbl
	rep	movsw
	mov	ax, 512		; Bytes per sector
	stosw
	mov	ax, [bp + 6]	; Sectors per cluster
	stosb
	mov	ax, [bp + 4]	; number of reserved sectors
	stosw
	mov	al, 2		; Number of fats
	stosb
	mov	ax, 512		; Number of root dir entries
	stosw
	mov	ax, [bp - 6]	; Number of sectors (two byte version)
	mov	bx, [bp - 4]
	or	bx, bx
	jz	.b2
	xor	ax, ax
.b2	stosw
	mov	al, 0F8h	; Media Descriptor: Hard Disk
	stosb
	mov	ax, [bp + 8]	; Number of sectors per FAT
	stosw
	mov	si, [bp - 2]	; Number of sectors per cluster
	mov	ax, [si + 2]
	stosw
	mov	ax, [si + 4]	; Number of heads
	stosw
	mov	ax, [bp + 12]	; Number of sectors before start of partition
	stosw
	mov	ax, [bp + 14]
	stosw
	mov	ax, [bp - 6]	; Total number of sectors (four byte version)
	stosw
	mov	ax, [bp - 4]
	stosw
	mov	ax, 80h		; Drive number if bootable (always 80 for hard disks)
	stosw			; and also flags; should be set to 0 on format
	mov	al, 29h		; EBPB recognition flag
	stosb
	call	volumeserial
	mov	si, vollbl	; Volume label slot
	mov	cx, 11
	rep	movsb
	mov	si, fatnm	; Fat name slot
	mov	cx, 4
	rep	movsw
	call	gennobootmsg
	mov	ax, '44'
	stosw
	mov	ax, '49'
	stosw

	add	sp, 4
	pop	di
	pop	bp
	ret	12

;DX:AX = number of sectors in FAT
;SI:CX = number of clusters
;BX = number of sectors per cluster, ES:0 = output area
;preserves BP, DI, BX
gensuperblock32:
	push	di
	push	bp
	mov	bp, sp
	sub	sp, 10
	mov	[bp - 2], dx
	mov	[bp - 4], ax
	mov	[bp - 6], si
	mov	[bp - 8], cx

	xor	di, di
	mov	ax, 058EBh	; JMP to 0x5A
	stosw
	mov	al, 90h
	stosb
	mov	cx, 4
	mov	si, oemlbl
	rep	movsw
	mov	ax, 512		; Bytes per sector
	stosw
	mov	ax, bx		; Sectors per cluster (1 byte)
	stosb
	add	ax, bx
	add	ax, bx
	stosw			; Reserved logical sectors
	mov	al, 2		; Number of fats
	stosb
	xor	ax, ax		; No sectors allocated to root directory
	stosw
	stosw			; Old 2 byte location of logical sectors
	mov	al, 0F8h	; Media descriptor byte F8
	stosb
	xor	ax, ax		; Old logical sectors per file allocation table
	stosw
	mov	si, [bp + 2]	; Number of sectors per cluster
	mov	ax, [si + 2]
	stosw
	mov	ax, [si + 4]	; Number of heads
	stosw
	push	bx		; Number of sectors before start of partition
	call	getleadingsectors
	mov	ax, bx
	pop	bx
	stosw
	xchg	ax, cx
	stosw
	mov	ax, [bp - 8]	; Number of sectors in filesystem
	mul	bx
	add	ax, [bp - 4]
	adc	dx, 0
	add	ax, [bp - 4]
	adc	dx, 0
	add	ax, [es:0Eh]
	adc	dx, 0
	stosw
	mov	[bp - 10], dx
	mov	ax, [bp - 6]
	mul	bx
	add	ax, [bp - 2]
	add	ax, [bp - 2]
	add	ax, [bp - 10]	; Cannot overflow
	stosw
	mov	ax, [bp - 4]	; Number of sectors per FAT
	stosw
	mov	ax, [bp - 2]
	stosw
	xor	ax, ax		; Mirroring control (standard)
	stosw
	stosw			; FAT32 version (0.0)
	mov	ax, 2		; Cluster number of root directory
	stosw
	xor	ax, ax
	stosw
	mov	ax, bx		; FS Information sector location
	add	ax, bx
	stosw
	sub	ax, bx		; Backup boot sector location
	stosw
	xor	ax, ax		; 12 bytes reserved
	mov	cx, 6
	rep	stosw
	mov	ax, 80h		; Boot physical drive and reserved field
	stosw
	mov	al, 29h		; Volume serial and label slot valid
	stosb
	push	bx		; Preserve sectors per cluster across this call
	call	volumeserial
	pop	bx
	mov	si, vollbl	; Volume label slot
	mov	cx, 11
	rep	movsb
	mov	si, fat32nm	; Fat name slot
	mov	cx, 4
	rep	movsw
	call	gennobootmsg
	mov	ax, '39'
	stosw
	mov	ax, '85'
	stosw

	;Now generate info sector
	mov	di, [es:30h]
	mov	cl, 9
	shl	di, cl
	mov	ax, 5252h	; FS Info Magic
	stosw
	mov	ax, 4161h
	stosw
	xor	ax, ax		; Unused space
	mov	cx, 240
	rep	stosw
	mov	ax, 7272h	; FS Info Magic 2
	stosw
	mov	ax, 6141h
	stosw
	xor	dx, dx
	mov	ax, 32		; Number of free clusters (total - root)
	div	bx
	xchg	ax, si
	mov	ax, [bp - 8]
	sub	ax, si
	stosw
	mov	ax, [bp - 6]
	sbb	ax, 0
	stosw
	xchg	ax, si		; Last allocated cluster = 2 + number of clusters in root dir - 1
	inc	ax
	stosw
	xor	ax, ax
	stosw
	xor	ax, ax		; Unused space and first two bytes of signature
	mov	cx, 7
	rep	stosw
	mov	ax, 0AA55h	; Last two bytes of signature
	stosw

	mov	sp, bp
	pop	bp
	pop	di
	ret

; Generates volume serial number and writes it to ES:DI
; Trashes AX, BX, CX, DX
volumeserial:
	stc
	mov	ah, 4
	int	1Ah
	jc	.noday
.day	mov	al, dl
	call	unpackbcd
	mov	[dap], al		;Day
	mov	al, dh
	call	unpackbcd
	mov	[dap + 1], al		;Month
	mov	al, cl
	call	unpackbcd
	mov	ah, 0
	mov	[dap + 2], ax		;Year (low)
	mov	ah, ch
	call	unpackbcd
	mov	ah, 100
	mul	ah
	add	[dap + 2], ax		;Year (includes century)
	mov	ah, 0
	int	1Ah
	xchg	ax, dx
	mov	dx, cx
	mov	bl, al
	mov	bh, al
	mov	cx, 65520
	div	cx
	mov	[dap + 3], al		; Hour
	xchg	ax, dx
	xor	dx, dx
	mov	cx, 1092
	div	cx
	mov	[dap + 4], al		; Minute
	xchg	ax, dx
	mov	cx, 10
	mul	cx
	mov	cx, 182			; Divide by 18.2
	div	cx
	mov	[dap + 5], al		; Second
	xchg	ax, dx
	mov	cl, 100
	mul	cl
	mov	cl, 182
	div	cl
	mov	[dap + 6], al		; Hundredths
	mov	ah, [dap + 1]
	mov	al, [dap]
	mov	dh, [dap + 5]
	mov	dl, [dap + 6]
	add	ax, dx
	stosw
	mov	ah, [dap + 3]
	mov	al, [dap + 4]
	mov	dx, [dap + 2]
	add	ax, dx
	stosw
	ret
.noday	xor	cx, cx
	xor	dx, dx
	jmp	.day

unpackbcd:
	push	cx
	mov	ch, al
	mov	al, 4
	shr	al, cl
	mov	cl, 10
	mul	cl
	add	al, ch
	pop	cx
	ret

gennobootmsg:
	mov	si, nobootmsg	; Not bootable msg
	mov	cx, (nobootmsg.end - nobootmsg) / 2
	rep	movsw
	mov	di, 190h
	mov	si, s_sysc
	test	[diskebr], byte 1
	jz	.ntebr
	mov	si, s_nbebr
.ntebr	mov	cx, 21
	rep	movsw

	mov	di, 1FDh	; Old disk identity
	mov	al, 80h
	stosb
	mov	ax, 0AA55h	; Bootable/FAT magic
	stosw
	ret

;Convert superblock to MBR; works with 12/16/32
;BL = parititon type
;Preserves BP, DI, DX
patchsuperblockmbr:
	push	bp
	push	es
	push	dx
	mov	es, [blockseg]
	test	[diskebr], byte 1
	jz	.pri
	jmp	.ebr
.pri	push	di
	xor	cx, cx
	mov	ax, [es:1Ch]
	mov	bp, ax
	; FIXUP FAT BPB for its new location
	mov	[es:1Ch], cx
	add	[es:0Eh], ax
	mov	[es:13h], cx
	add	[es:20h], ax
	adc	[es:22h], cx
	cmp	[es:16h], cx
	jne	.n32
	add	[es:30h], ax
	mov	ax, [minclustsizem]
	add	ax, 3
	mov	[es:32h], ax
.n32	mov	ax, [es:0]		; Some BIOSes check for xor ax, ax as first MBR instruction
	mov	[es:0], word 0C031h
	sub	ah, 2
	mov	[es:2], ax
	mov	al, ah
	mov	ah, 0
	mov	di, ax
	add	di, 4
	mov	si, procmbreloc		; Relocate MBR routine
	mov	cx, (procmbreloc.end - procmbreloc) / 2
	rep	movsw
	xor	ax, ax			; Zero the Win95 disk identification area so it can use it
	mov	di, 0DAh
	mov	cx, 3
	rep	stosw
	mov	si, procmbr		; MBR boot routine
	mov	cx, (procmbr.end - procmbr) / 2
	rep	movsw
	mov	si, s_rderr		; MBR boot messages
	mov	di, 0198h
	mov	cx, 13
	rep	movsw
	xor	ax, ax			; Zero the rest of the stuff
	mov	cx, 35
	rep	stosw
	pop	di	; Get disk table back
	xor	cx, cx			; Generate MBR
	mov	[es:1BEh], byte 80h
	xor	ax, ax
	
	xor	si, si
	mov	cx, bp
	call	lineartochs
	mov	[es:1BEh + 1], dh
	mov	[es:1BEh + 2], cx
	mov	[es:1BEh + 4], bl
	mov	[es:1BEh + 8], bp	; First linear sector of partition
	mov	ax, [es:20h]
	mov	dx, [es:22h]
	mov	[es:1BEh + 12], ax	; Number of sectors in partition
	mov	[es:1BEh + 14], dx
	dec	bp			; Get linear address of last sector
	add	ax, bp
	adc	dx, 0
	sub	ax, bp
	call	lineartochsoroverflow	; Get CHS address of last sector
	mov	[es:1BEh + 5], dh
	mov	[es:1BEh + 6], cx
	xor	cx, cx		; We wrote last sector above
	stc			; but we want number of sectors
	adc	[es:1BEh + 12], cx
	adc	[es:1BEh + 14], cx
	; Write MBR pointer to boot check
	xor	si, si
	mov	cx, [minclustsizem]
	cmp	[disklba], byte 0
	je	.bcchs
	mov	[es:1CEh + 4], byte 0B5h
	jmp	.bccx
.bcchs	mov	[es:1CEh + 4], byte 0B0h
.bccx	mov	[es:1CEh + 8], cx
	mov	[es:1CEh + 10], si
	call	lineartochs
	mov	dl, 0
	mov	[es:1CEh + 0], dx
	mov	[es:1CEh + 2], cx
	xor	si, si
	mov	cx, [minclustsizem]
	add	cx, 4
	mov	[es:1CEh + 12], cx
	mov	[es:1CEh + 14], si
	dec	cx
	call	lineartochs
	mov	[es:1CEh + 5], dh
	mov	[es:1CEh + 6], cx
	mov	[es:1FDh], byte 0
	jmp	.final

.ebr	mov	ax, [es:1Ch]	; Save distance to partition start
	mov	dx, [es:1Eh]
	push	ax
	push	dx
	mov	cx, [es:20h]	; Save length of partition in sectors
	push	cx
	mov	cx, [es:22h]
	push	cx
	cmp	ax, [diskebroffset]
	jne	.ebrb
	cmp	dx, [diskebroffset]
	jne	.ebrb
	jmp	.ebrgen
.ebrb	push	di
	push	ax
	xor	di, di
	mov	cx, 512
	xor	ax, ax
	rep	stosw
	xor	di, di
	call	gennobootmsg
	pop	ax
.ebrgen pop	di			; Get disk table back
	mov	[es:1FDh], byte 0
	mov	[es:1BEh + 4], bl
	pop	dx
	pop	ax
	mov	[es:1BEh + 12], ax
	mov	[es:1BEh + 14], dx
	pop	dx
	pop	ax
	sub	ax, [diskebroffset]
	sbb	dx, [diskebroffset + 2]
	mov	[es:1BEh + 8], ax
	mov	[es:1BEh + 10], dx
	call	lineartochsoroverflow
	mov	[es:1BEh + 1], dh
	mov	[es:1BEh + 2], cx
	mov	ax, [es:1BEh + 8]
	mov	dx, [es:1BEh + 10]
	add	ax, [es:1BEh + 12]
	adc	dx, [es:1BEh + 14]
	add	ax, 1
	adc	dx, 0
	call	lineartochsoroverflow
	mov	[es:1BEh + 5], dh
	mov	[es:1BEh + 6], cx
.final	test	[diskebr], byte 2
	jnz	.nxnebr
	mov	ax, [diskpriorebr]	; We iterate backwards so this is the *next* entry
	mov	dx, [diskpriorebr + 2]
	sub	ax, [diskebroffset]
	sbb	dx, [diskebroffset + 2]
	mov	[es:1EEh + 8], ax
	mov	[es:1EEh + 10], dx
	call	lineartochsoroverflow
	mov	[es:1EEh + 1], dh
	mov	[es:1EEh + 2], cx
	mov	ax, [di + 8]
	mov	dx, [di + 10]
	mov	cx, [di + 12]
	or	cx, [di + 14]
	jz	.ismbrsized
	mov	ax, 0FFFFh
	mov	dx, 0FFFFh
.ismbrsized:
	sub	ax, [diskebroffset]
	sbb	dx, [diskebroffset + 2]
	push	ax
	push	dx
	call	lineartochsoroverflow
	mov	dl, 05h
	test	[disklba], byte 1
	jz	.ebrchs
	mov	dl, 0Fh
.ebrchs	mov	[es:1EEh + 4], dx
	mov	[es:1EEh + 6], cx
	pop	dx
	pop	ax
	sub	ax, [diskpriorebr]
	sbb	dx, [diskpriorebr + 2]
	mov	[es:1EEh + 12], ax
	mov	[es:1EEh + 14], dx

.nxnebr	mov	cx, [minclustsize]
	sub	cx, 200h
	jz	.noxfil
	push	di
	xor	ax, ax
	mov	di, 0200h	; Zeros in the rest of the physical sector
	rep	stosw
	pop	di
.noxfil	pop	dx
	pop	es
	pop	bp
	ret

	; DX:AX = linear address; DI = disk table entry
	; Outputs CX, DH = CHS address, preserves BX, SI, DI, BP
	; Special routine for computing MBR offsets because these are CHS when possible, linear when not
lineartochsoroverflow:
	push	ax
	xchg	ax, dx
	xor	dx, dx
	div	word [di + 2]
	xchg	ax, cx
	pop	ax
	div	word [di + 2]
	xchg	cx, dx
	inc	cl		; Sectors
	cmp	dx, [di + 4]
	jae	.overflow
	div	word [di + 4]
	cmp	ax, 1023
	ja	.overflow
	mov	dh, dl
	xchg	ah, al
	ror	al, 1
	ror	al, 1
	or	cx, ax
	ret
.overflow:
	mov	cx, 0FFFFh
	mov	dh, 0FFh
	ret

;Arguments: DX:AX bottom of FAT marker
;BX: number of sectors
;SI:CX address of first sector
;DI: disktable
writeemptyfat:
	push	es
	push	di
	push	cx
	push	ax
	mov	es, [blockseg]
	xor	ax, ax
	mov	di, ax
	mov	cx, 4100h
	rep	stosw
	pop	ax
	pop	cx
	pop	di
	push	bp
	mov	bp, bx
	push	bp
	push	ax
	push	dx
	call	.writeafat
	pop	dx
	pop	ax
	pop	bp
	jc	.oops
	call	.writeafat
	jc	.oops
	;We now have a pointer to root directory in SI:CX
	;clear it too
	mov	bx, 512
.clrr	mov	ax, 0320h
	call	lineardiskop
	jc	.oops
	call	progressal_base
.oops	pop	bp
	pop	es
	ret
.writeafat:
	xor	bx, bx
	mov	[es:bx], ax
	mov	[es:bx + 2], dx
	mov	dl, [disk]
.writeafatloop:
	mov	al, [minclustsizem]
	mov	ah, 3
	call	lineardiskop
	jc	.out
	mov	bx, 512
	mov	ah, 0
	call	progressal_base
	add	cx, ax
	adc	si, 0
	sub	bp, ax
	ja	.writeafatloop
	;carry flag is clear already because sub bp, ax
.out	ret

;Arguments:
;	SI:CX = offset to start of FAT
;	BX = sectors per cluster
;	DX:AX = number of sectors per fat
;Preserves DI, BP
writeemptyfat32:
	push	bp
	push	es
	mov	es, [blockseg]
	xchg	ax, bp
	push	cx
	push	di
	xor	ax, ax
	xor	di, di
	mov	cx, 4200h
	rep	stosw
	xor	di, di
	mov	ax, 0FFF8h	; Reserved entries in FAT
	stosw
	mov	ax, 00FFFh
	stosw
	mov	ax, 0FFFFh
	stosw
	mov	ax, 00FFFh
	stosw
	mov	cx, 32		; Allocate the root directory
	push	si
	mov	si, 2
.next	cmp	cx, bx
	jbe	.last
	inc	si
	mov	ax, si
	stosw
	xor	ax, ax
	stosw
	sub	cx, bx
	jmp	.next
.last	mov	ax, 0FFF8h
	stosw
	mov	ax, 00FFFh
	stosw
	pop	si
	pop	di
	pop	cx
	call	.writeafat
	jc	.out
	call	.writeafat
	jc	.out
	mov	ax, 0320h	; Zero the root directory
	mov	bx, 0200h
	mov	dl, [disk]
	call	lineardiskop
	jc	.out
	call	progressal_base
.out	pop	es
	pop	bp
	ret
.writeafat:
	push	dx
	push	bp
	xor	bx, bx
.loop	mov	al, 32
	or	dx, dx
	jnz	.big
	cmp	bp, 32
	jae	.big
	mov	ax, bp
.big	mov	ah, 3
	push	dx
	mov	dl, [disk]
	call	lineardiskop
	pop	dx
	jc	.out2
	mov	ah, 0
	sub	bp, ax
	sbb	dx, 0
	add	cx, ax
	adc	si, 0
	call	progressal_base
	mov	bh, 2
	or	bp, bp
	jnz	.loop
	or	dx, dx
	jnz	.loop
	;CF is guaranateed clear here
.out2	pop	bp
	pop	dx
	ret

;Trashes AX,BX,CX,DH,SI
writebootcheck:
	push	di
	push	ds
	push	es
	pop	ds
	xor	si, si
	mov	di, 0600h
	mov	cx, 0100h
	rep	movsw
	pop	ds
	mov	si, bootcheckprefix
	xor	di, di
	mov	cx, 5
	rep	movsw
	mov	al, 7Fh
	mov	ah, [minclustsizem]
	stosw
	xor	ax, ax
	mov	cx, 18
	rep	stosw
	mov	si, bootcheck
	mov	cx, (bootcheck_end - bootcheck) / 2
	rep	movsw
	xor	bx, bx
	mov	ax, 0304h
	mov	cx, [es:7CEh + 2]
	mov	dh, [es:7CEh + 1]
	int	13h
	pop	di
	ret

getleadingsectors:
	test	[diskebr], byte 1
	jnz	.ebr
.first	xor	cx, cx
	mov	bx, [minclustsize]
	cmp	bx, 1024
	jbe	.six
	mov	bl, bh	; We need twice the number of 2048 byte units so divide by 256
	mov	bh, 0
	jmp	.off
.six	mov	bx, 6
.off	cmp	[oslevel], byte '5'	; FAT32
	jae	.ret
	;MS-DOS < 5.0 can't handle reserved sectors before the FAT. Adjust partition start to last
	;location in SSD sector to avoid the problem. Rather than make this conditional, we just always do it.
	add	bx, [minclustsizem]
	dec	bx
.ret	ret
.ebr	mov	bx, [diskebroffset]	; Starting from EBR
	mov	cx, [diskebroffset + 2]
	jmp	.off

;Check if disk referenced by DL has LBA support
;Clobbers SI, AX
diskhaslba:
	mov	si, dap
	mov	[si], word 1Eh
	mov	[si + 18h], word 0
	mov	ah, 48h
	int	13h			; LBA disabled for the moment
	jc	.nolba
	cmp	[si + 18h], word 0
	je	.nolbac			; BIOS clearly doesn't have the call
	clc
	ret
.nolbac	stc
.nolba	ret

getdiskstructure:
	mov	dx, 0
	mov	dl, [disk]
	sub	dl, 80h
	mov	cl, 4
	shl	dl, cl
	add	dx, disktable
	mov	di, dx
	ret

;Performs disk op
;SI:CX = disk address
;ES:BX = memory address
;DS:DI = disk structure
;AL = number of sectors
;AH = disk op (02 = read; 03 = write)
;DL = disk
;Clobbers AH,DH
lineardiskop:
	cmp	[disklba], byte 0
	je	.chs
	add	ah, 40h
	push	si
	push	di
	mov	di, si
	mov	si, dap
	push	bp
	xor	bp, bp
	mov	[si], word 10h
	mov	[si + 2], bp
	mov	[si + 2], al
	mov	[si + 4], bx
	mov	[si + 6], es
	mov	[si + 8], cx
	mov	[si + 10], di
	mov	[si + 12], bp
	mov	[si + 14], bp
	mov	al, 0		; On write, AL = verify flag
	pop	bp
	int	13h
	mov	al, [si + 2]
	pop	di
	pop	si
	ret
.chs	jmp	linearchsdiskop

progressal_base:
	push	si
	mov	si, pgbar
	mov	[si], al
	mov	[si + 1], byte 0
	call	progressbar
	pop	si
	ret

out_string:
	push	cx
	mov	ah, 03h
	int	10h
	pop	cx
out_stringat:
	mov	ah, 02h
	int	10h
	lodsb
	mov	ah, 09h
	push	cx
	mov	cx, 1
	int	10h
	pop	cx
	inc	dl
	loop	out_stringat
	mov	ah, 02h
	int	10h
	ret

draw_box:
	push	ax
	push	cx
	push	bp
	mov	bp, sp
	mov	bh, 0
	mov	dx, [bp + 4]
	mov	ah, 02h
	int	10h
	mov	cx, 1
	mov	ax, 09C9h
	int	10h
	inc	dl
	mov	ah, 2
	int	10h
	mov	ch, 0
	mov	cl, [bp + 2]
	sub	cl, 2
	mov	ax, 09CDh
	int	10h
	add	dl, cl
	mov	ah, 02h
	int	10h
	mov	cx, 1
	mov	ax, 09BBh
	int	10h
	mov	ch, 0
	mov	cl, [bp + 3]
	sub	cl, 2
	inc	dh
.bgline	push	cx
	mov	dl, [bp + 4]
	mov	ah, 02h
	int	10h
	mov	cx, 1
	mov	ax, 09BAh
	int	10h
	inc	dl
	mov	ah, 02h
	int	10h
	mov	cl, [bp + 2]
	sub	cl, 2
	mov	ax, 0920h
	int	10h
	add	dl, cl
	mov	ah, 02h
	int	10h
	mov	cx, 1
	mov	ax, 09BAh
	int	10h
	pop	cx
	inc	dh
	loop	.bgline
	mov	dl, [bp + 4]
	mov	ah, 02h
	int	10h
	mov	cx, 1
	mov	ax, 09C8h
	int	10h
	inc	dl
	mov	ah, 02h
	int	10h
	mov	ch, 0
	mov	cl, [bp + 2]
	sub	cl, 2
	mov	ax, 09CDh
	int	10h
	add	dl, cl
	mov	ah, 02h
	int	10h
	mov	cx, 1
	mov	ax, 09BCh
	int	10h
	pop	bp
	pop	cx
	pop	ax
	ret

diskclearlast:
	mov	bx, 7
	mov	cx, 78
	mov	dx, 1601h
	mov	ah, 02h
	int	10h
	mov	ax, 0920h
	int	10h
	mov	dx, 1701h
	mov	ah, 02h
	int	10h
	mov	ax, 0920h
	int	10h
	ret

diskerror:
	;Erase screen except for progress bar
	push	ax
	and	ah, 15
	call	xdigit
	mov	[s_derr + 11], ah
	pop	ax
	mov	cl, 4
	shr	ah, cl
	call	xdigit
	mov	[s_derr + 10], ah
	mov	bx, 7
	mov	si, s_derr
	mov	cx, 12
	mov	dx, 1622h
	call	out_stringat
	jmp	mainscreenreturn

afterfat:
	cmp	[disk], byte 80h
	jbe	.msr
	mov	es, [blockseg]
	xor	bx, bx
	mov	dx, 80h
	mov	cx, 1
	mov	ax, 0201h
	int	13h
	jc	.msr
	mov	di, 1BEh
	mov	cx, 4
.scan	mov	al, es:[di + 4]
	cmp	al, byte 0B0h
	je	.found
	cmp	al, byte 0B5h
	je	.found
	add	di, 16
	loop	.scan
.msr	jmp	mainscreenreturnes
.found	mov	bl, 9
	mov	ax, 091Ah
	mov	cx, 041Ah
	call	draw_box
	mov	cx, 24
	mov	dx, 0A1Bh
	mov	si, s_exd1
	call	out_stringat
	mov	cx, 24
	mov	dx, 0B1Bh
	mov	si, s_exd2
	call	out_stringat
	mov	ah, 0
	int	16h
	cmp	al, 'y'
	je	.y
	cmp	al, 'Y'
	jne	.msr
.y	mov	bx, 0200h
	mov	ah, 2
	call	.afdiskio
	jc	.msr2
	lea	si, [bx + 10]
	mov	dl, [disk]
	mov	dh, [minclustsizem]
	mov	cx, 16
.loop2	cmp	es:[si], dl
	je	.msr2
	cmp	es:[si], byte 0
	je	.slot
	add	si, 2
	loop	.loop2
.msr2	jmp	mainscreenreturnes	;Nope! (Emergency safeguard)
.slot	mov	es:[si], dx
	mov	es:[si + 2], byte 0
	mov	ah, 3
	call	.afdiskio
	jmp	mainscreenreturnes
.afdiskio:
	mov	al, 1
	cmp	[di + 4], byte 0B5h
	je	.lba
	mov	dh, es:[di + 1]
	mov	cx, es:[di + 2]
	mov	dl, 80h
	int	13h
	ret
.lba	add	ah, 40h
	xor	cx, cx
	mov	si, dap
	mov	[si], word 10h
	mov	[si + 2], cx
	mov	[si + 2], al
	mov	[si + 4], bx
	mov	[si + 6], es
	mov	[si + 12], cx
	mov	[si + 14], cx
	mov	cx, [di + 8]
	mov	[si + 8], cx
	mov	cx, [di + 10]
	mov	[si + 10], cx
	int	13h
	ret
mainscreenreturnes:
	push	ds
	pop	es
mainscreenreturn:
	mov	bx, 7
	mov	cx, 78
	mov	dx, 0101h
.erase	mov	ah, 02h
	int	10h
	mov	ax, 0920h
	int	10h
	inc	dh
	cmp	dh, 22
	jb	.erase
	jmp	mainscreen

;Exit - waay down here to keep from being overwritten by bios reboot routine
exit:
%if 0
	;Clear screen on way out
	mov	bx, 7
	mov	cx, 80
	mov	dx, 0000h
.erase	mov	ah, 02h
	int	10h
	mov	ax, 0920h
	int	10h
	inc	dh
	cmp	dh, 25
	jb	.erase
%endif
	xor	dx, dx
	mov	es, dx
	mov	ax, [es:86h]
	mov	bx, [es:84h]
	or	ax, ax
	jnz	.dos
	or	bx, bx
	jnz	.dos
.bios	xor	ax, ax
	mov	dl, [saveddl]
	mov	dh, 0
	mov	cx, 1
	mov	es, ax
	mov	ds, ax
	mov	bx, 7C00h
	cli
	mov	ss, ax
	mov	sp, bx
	sti
	mov	ax, 0201h
	int	13h
	jc	.again
.entry	jmp	07C0h:0h
.again	mov	ax, 0201h
	int	13h
	jnc	.entry
.hlt	hlt
	jmp	short	.hlt
.dos	mov	es, ax
	cmp	[es:bx], byte 0CFh
	je	.bios
	cmp	[rebootmsg], byte 0
	je	.nmsg
	mov	ah, 9
	mov	dx, s_rebootmsg
	int	21h
.nmsg	mov	ax, 4C00h
	int	21h

align	4, db 0

oemlbl	db	'IBM  2.0'	; OEM Label in FAT is so broken that only IBM  2.0 works everywhere!
vollbl	db	'NO NAME     '
fatnm	db	'FAT16   '
fat32nm	db	'FAT32   '

align 4, db 0

procmbreloc:
	mov	di, di		; Two byte NOP
				; Hitpoint for mov dl, 80h if required because of buggy BIOS
	cld
	cli
	xor	ax, ax
	mov	ss, ax
	mov	sp, 7C00h
	sti
	mov	ds, ax
	mov	es, ax
	mov	si, 7CE0h
	mov	di, 06E0h
	mov	cx, 90h
	rep	movsw
	mov	bp, 06E0h + procmbr.normal - procmbr
	test	dl, 80h		; Check if DL is sane
	jnz	.keep
	mov	dl, 80h
.keep	jmp	0:06E0h
	align	2, db 0
procmbreloc.end:

procmbr:
	mov	al, 3
	mov	si, 07BEh
.srch1	mov	ah, [si + 4]
	cmp	ah, byte 0B0h
	je	.found
	cmp	ah, byte 0B5h
	je	.found
	add	si, 16
	cmp	si, 07FEh
	jb	.srch1
.normal	mov	al, 1
	mov	si, 07BEh
.srch2	test	[si], byte 80h	; If somebody deletes our boot partiton, become a normal MBR loader
	jnz	.found
	add	si, 16
	cmp	si, 07FEh
	jb	.srch2
	int	18h		; Try next boot path
	mov	si, 0798h + 14
	mov	cx, 12
	jmp	.msg
.found	mov	bx, 7C00h
	mov	dh, [si + 1]
	mov	cx, [si + 2]
	mov	ah, [si + 4]
	cmp	ah, byte 0B5h
	je	.lba
	cmp	ah, byte 0Ch
	je	.lba
	cmp	ah, byte 0Eh
	je	.lba
	cmp	dh, 0FFh
	jne	.chs
	cmp	cx, 0FFFFh
	jne	.chs
.lba	mov	di, 0602h
	mov	ah, 0
	mov	[di], word 10h
	mov	[di + 2], ax
	mov	[di + 4], bx
	mov	[di + 6], ds
	mov	cx, [si + 8]
	mov	[di + 8], cx
	mov	cx, [si + 10]
	mov	[di + 10], cx
	xor	cx, cx
	mov	[di + 12], cx
	mov	[di + 14], cx
	push	si
	mov	si, di
	mov	ah, 42h
	int	13h
	pop	si
	jmp	.chki
.chs	mov	ah, 02h
	int	13h
.chki	jc	.error
	jmp	7C0h:0
.error	mov	si, 0798h
	mov	cx, 14
.msg	mov	bx, 7
.msglp	lodsb
	mov	ah, 0Eh
	int	10h
	loop	.msglp
	mov	ah, 0
	int	16h
	cli
	jmp	0F000h:0FFF0h	; Reboot
align	2, db 0
procmbr.end:
;Normally I don't bother to check these but procmbr is tight
%if (procmbr.end - procmbr) > (0798h - 06E0h)
%error procmbr is too long
%endif

nobootmsg:
	mov	si, 7D90h
	mov	bx, 7
	mov	cx, 42
.msg	lodsb
	mov	ah, 0Eh
	int	10h
	loop	.msg
	mov	ah, 0
	int	16h
	cli
	jmp	0F000h:0FFF0h	; Reboot
align	2, db 0
nobootmsg.end:

bootcheckprefix:
	db	0EBh, 2Eh, 'SSD->H'
	db	3 ;1 byte nm sectors to backup MBR
	db	0 ;1 byte reserved
	;n byte pairs disk id and number of logical sectors per physical sector
	;1 byte 0

bootcheck:
	;Address = 7C30h
	;Entry SP = 7C00h
	;Work table = 7C08h
	;BSS = 0602h
	;End of code + data = 8200h
	;DS=ES=SS=0; CS may not be
	;BP = boot return jump
bootcheckbase	equ	7C30h - bootcheck
bc_saveddl	equ	0602h
bc_walktbl	equ	0604h
bc_tablow	equ	0606h
bc_tabhigh	equ	0608h
bc_offsetlow	equ	060Ah
bc_offsethigh	equ	060Ch
bc_activedisk	equ	060Eh
bc_sectsize	equ	060Fh
bc_dap		equ	0610h
bc_prg		equ	0620h
bc_diskstr	equ	0630h
bc_disksect	equ	0632h
bc_diskhead	equ	0634h
bc_diskcyl	equ	0636h
bc_disklen	equ	0638h	; Stores CHS length
bc_uselba	equ	0640h
	mov	ax, 840h
	mov	es, ax		; ES = work segment
	mov	[bc_saveddl], dl
	mov	si, 7C0Ah
.loop	lodsw
	cmp	al, 0
	jne	.cont
.break	mov	dl, [bc_saveddl]
	push	ds
	pop	es		; Set ES back to 0 to not confuse reentry point
	xor	ax, ax
	push	ax
	push	bp
	retf			; jmp 0:bp
.cont	mov	[bc_uselba], byte 0
	cmp	al, 07Fh
	jne	.fixed
	mov	al, [bc_saveddl]
.fixed	push	si
	xchg	ax, dx
	xor	ax, ax
	mov	[bc_tablow], ax
	mov	[bc_tabhigh], ax
	mov	[bc_activedisk], dx
	push	dx
	mov	si, bootcheckbase + bcs_checkdisk
	sub	dl, 80h - '1'
	mov	[si + 14], dl
	pop	dx
	call	bootcheckstr
	; Get disk geometry
	xor	di, di
	push	es
	mov	es, di
	mov	ah, 08h
	stc
	int	13h
	pop	es
	jc	short .e2v
	mov	dl, dh
	mov	dh, 0
	inc	dx
	mov	[bc_diskhead], dx
	mov	bl, cl
	and	bl, 63
	mov	bh, 0
	mov	[bc_disksect], bx
	rol	cl, 1
	rol	cl, 1
	xchg	ch, cl
	and	ch, 3
	inc	cx
	mov	[bc_diskcyl], cx
	mov	ax, [bc_disksect]
	mul	word [bc_diskhead]	; Cannot overflow
	mul	cx
	mov	[bc_disklen], ax
	mov	[bc_disklen + 2], dx
	;Count how many
	call	bootcheckcount
.e2v	jc	short .error
	;Initialize progress bar
	mov	bh, 0
	mov	ah, 3
	int	10h
	mov	si, bc_prg
	mov	ch, dh
	mov	cl, 0
	mov	bx, 5007h
	mov	ax, [bc_tablow]
	mov	dx, [bc_tabhigh]
	call	initprogressbar
	;Check the disk
	call	bootcheckdisk
	pushf
	push	ax
	mov	si, bootcheckbase + bcs_newline
	call	bootcheckstr
	pop	ax
	popf
	jc	.error
.loop2	pop	si
	jmp	.loop
.error	mov	si, bootcheckbase + bcs_error
	push	ax
	and	ah, 15
	call	xdigit
	mov	[si + 11], ah
	pop	ax
	mov	cl, 4
	shr	ah, cl
	call	xdigit
	mov	[si + 10], ah
	call	bootcheckstr
	jmp	short	.loop2

bootcheckcount:
	mov	ax, .descend
	call	bootcheckfixcodeptr
	call	bootcheckdescend
	ret
.descend:
	xor	bx, bx
	xor	si, si
	xor	cx, cx
	mov	ax, 0201h
	call	bootcheckdiskoprel
	jc	.error
	call	bootcheckisfatandlen
	add	[bc_tablow], ax
	adc	[bc_tabhigh], dx
	clc
.error	ret

bootcheckisfatandlen:
	xor	dx, dx
	xor	ax, ax
	cmp	es:[0], byte 0EBh
	je	.short
	cmp	es:[0], byte 0E9h
	jne	.skip
.short	cmp	es:[015h], byte 0F8h
	jne	.skip
	cmp	es:[10h], byte 2
	jb	.skip
	cmp	es:[1FEh], word 0AA55h
	jne	.skip
	mov	ax, es:[16h]
	or	ax, ax
	jz	.try32		; If we run out of space squeeze one byte here
	ret
.try32	mov	ax, es:[24h]
	mov	dx, es:[26h]
	push	ax
	or	ax, dx
	pop	ax
	ret
.skip	xor	ax, ax	; Clear ZF
	ret

bootcheckdisk:
	mov	ax, .descend
	call	bootcheckfixcodeptr
	call	bootcheckdescend
	ret
.skip	clc
.error1	ret
.descend:
	xor	bx, bx
	xor	si, si
	xor	cx, cx
	mov	ax, 0201h
	call	bootcheckdiskoprel
	jc	.error1
	;Blanked FAT32 boot sector skipped in the count step above
	call	bootcheckisfatandlen
	jz	.skip

	mov	di, es:[0Eh]	; Number of reserved sectors
	;number of sectors per FAT is in DX:AX
	mov	bp, sp		; Caller saves bp for us
	sub	sp, 24
	xor	si, si
	mov	[bp - 2], si	; Current position low
	mov	[bp - 4], si	; Current position high
	mov	[bp - 6], ax	; Total low
	mov	[bp - 8], dx	; Total high
	mov	[bp - 10], ax	; Remainder low
	mov	[bp - 12], dx	; Remainder high
	mov	[bp - 14], di	; adjust
.loop	xor	bx, bx
	mov	[bp - 16], bx	; status flags clear
				; [bp - 15] = number of sectors
				; [bp - 18] = fat chunk address1
				; [bp - 22] = fat chunk address2

	;Read in chunks and check
	test	[bp - 12], word 0
	jne	.cst
	mov	ax, [bp - 10]
	cmp	ah, 0
	jne	.cst
	cmp	al, [bc_sectsize]
	jbe	.chk
.cst	mov	al, [bc_sectsize]
.chk	mov	[bp - 15], al
	mov	ah, 2
	mov	cx, [bp - 2]
	mov	si, [bp - 4]
	add	cx, [bp - 14]
	adc	si, 0
	call	bootcheckdiskoprel
	jc	.err2v
	mov	[bp - 18], cx
	mov	[bp - 20], si
	mov	bx, 8000h		; Could shrink if it ever mattered
	mov	al, [bp - 15]
	mov	ah, 2
	add	cx, [bp - 6]
	adc	si, [bp - 8]
	call	bootcheckdiskop
	jc	.err2v
	mov	[bp - 22], cx
	mov	[bp - 24], si
	mov	ch, 0
	mov	cl, [bp - 15]
	xor	bx, bx
.loop2	mov	di, bx
	mov	si, bx
	call	.isrep
	jz	.rep1
	add	di, 8000h
	call	.isrep
	jnz	.loop2e
	call	.fix
	or	[bp - 16], byte 2
	jmp	.loop2e
.err2v	jmp	.error2
.rep1	add	di, 8000h
	call	.isrep
	jz	.bothrep
	xchg	si, di
	call	.fix
	or	[bp - 16], byte 1
	;jmp	.loop2e
.bothrep:	; Both rep-digits; what do we do? Defer to the main check tool.
		; Both 0000 and FFFF are valid and they're the most likely cases.
		; Also there's no way to end up with two different reps by torn writes
.loop2e	inc	bh
	inc	bh
	loop	.loop2

	;Write back any fixes
	test	[bp - 16], byte 1
	jz	.nf1
	mov	cx, [bp - 18]
	mov	si, [bp - 20]
	xor	bx, bx
	mov	ah, 3
	mov	al, [bp - 15]
	call	bootcheckdiskop
.nf1	test	[bp - 16], byte 2
	jz	.nf2
	mov	cx, [bp - 22]
	mov	si, [bp - 24]
	mov	bx, 8000h
	mov	ah, 3
	mov	al, [bp - 15]
	call	bootcheckdiskop
	jc	.error2
.nf2	xor	dx, dx
	mov	ah, 0
	mov	al, [bp - 15]
	mov	[bc_prg], ax
	mov	[bc_prg + 2], dx
	mov	si, bc_prg
	call	progressbar
	add	[bp - 2], ax
	adc	[bp - 4], dx
	sub	[bp - 10], ax
	sbb	[bp - 12], dx
	mov	ax, [bp - 12]
	or	dx, [bp - 10]
	jz	.done
	jmp	.loop
.done	clc
.error2	mov	sp, bp
	ret

.isrep	push	di
	push	cx
	mov	ax, es:[di]
	inc	di		; If we run out of space, squeeze two bytes here
	inc	di
	mov	cx, 255
	repe	scasw
.retw	pop	cx
	pop	di
	ret

.fix	push	cx
	push	ds
	push	es
	pop	ds
	mov	cx, 256
	rep	movsw
	pop	ds
	pop	cx
	ret

;bootcheckdescend: AX = callback ptr; clobbers all registers except BP, and segment registers
;Callback is made with ES=pointer to first free buffer, BP = pointer to callback method
;bc_offsetlow/high are filled so that disk access accesses start at base of partition
;Callback need preserve only DS and SS (not even BP or ES).
bootcheckdescend:
	xor	bx, bx
	mov	[bc_offsetlow], bx
	mov	[bc_offsethigh], bx
	push	bp
	mov	bp, ax
	call	.reentr
	pop	bp
	ret
.reentr	xor	cx, cx
	xor	si, si
	xor	bx, bx
	mov	ax, 0201h
	call	bootcheckdiskoprel
	jc	.error
	mov	si, 01BEh
.loop	mov	al, es:[si + 4]
	mov	bl, [bc_uselba]
	push	bx
	cmp	al, 1
	je	.fat
	cmp	al, 4
	je	.fat
	cmp	al, 6
	je	.fat
	cmp	al, 0Bh
	je	.fat
	cmp	al, 0Ch
	je	.lbafat
	cmp	al, 0Eh
	je	.lbafat
	cmp	al, 5
	je	.mbr
	cmp	al, 0Fh
	je	.lbambr
.nxt	pop	bx
	mov	[bc_uselba], bl
	add	si, 10h
	cmp	si, 01FEh
	jb	.loop
.error	ret		; CF is cleared by cmp above
.lbambr	or	[bc_uselba], byte 1
.mbr	mov	ax, bootcheckdescend.reentr
	call	bootcheckfixcodeptr
	xchg	ax, cx
	jmp	.act
.lbafat or	[bc_uselba], byte 1
.fat	mov	cx, bp
.act	push	si
	mov	ax, [bc_offsetlow]
	mov	dx, [bc_offsethigh]
	push	ax
	push	dx
	add	ax, es:[si + 8]
	adc	dx, es:[si + 10]
	mov	[bc_offsetlow], ax
	mov	[bc_offsethigh], dx
	push	es
	mov	ax, es
	add	ax, 20h
	mov	es, ax
	push	bp
	call	cx
	pop	bp
	pop	es
	pop	dx
	pop	cx	; Can't use AX it must survive on error path
	mov	[bc_offsetlow], cx
	mov	[bc_offsethigh], dx
	pop	si
	jnc	.nxt
	pop	bx
	jmp	.error

bootcheckfixcodeptr:		; This routine is not a compile time constant from its input
	;AX = code pointer in base	trashes BX
	call	.worker
.adj	sub	ax, .adj	; Subtracts where assembler thinks .adj is
	ret
.worker	mov	bx, sp
	add	ax, ss:[bx]	; Adds where .adj actually is
	ret

%if 0
bootcheckfat_dbg:
	push	ax
	push	bx
	push	cx
	push	si
	mov	si, bootcheckbase + bcs_fat
	call	bootcheckstr
	pop	si
	pop	cx
	pop	bx
	pop	ax
	ret
%endif

bootcheckstr:
	mov	bx, 7
.more	lodsb
	or	al, al
	jz	.ret
	mov	ah, 0Eh
	int	10h
	jmp	.more
.ret	ret

bootcheckdiskoprel:
	;SI:CX remembers absolute address after call; clobbers DI, AH, DH
	add	cx, [bc_offsetlow]
	adc	si, [bc_offsethigh]
	jc	.no
	call	bootcheckdiskop
	ret
.no	mov	ah, 40h	; We can't handle disks > 2TB, but neither can MBR!
	ret		; This catches a partition that starts within range but ends out of range

bootcheckdiskop:
	;SI:CX = disk address
	;ES:BX = memory address
	;AL = number of sectors
	;AH = disk op (02 = read; 03 = write)
	;Clobbers DI, AH, DX
	mov	dl, [bc_activedisk]
	test	[bc_uselba], byte 1
	jnz	.lba
	cmp	si, [bc_disklen + 2]
	ja	.lba
	jb	.chs
	cmp	cx, [bc_disklen]
	jb	.chs
.lba	mov	di, si
	push	si
	push	bp
	mov	si, bc_dap
	add	ah, 40h
	xor	bp, bp
	mov	[si], word 10h
	mov	[si + 2], bp
	mov	[si + 2], al
	mov	[si + 4], bx
	mov	[si + 6], es
	mov	[si + 8], cx
	mov	[si + 10], di
	mov	[si + 12], bp
	mov	[si + 14], bp
	mov	al, 0			; AL on write = verify flag
	pop	bp
	int	13h
	mov	al, [si + 2]
	pop	si
	ret
.chs	mov	di, bc_diskstr
	;enter linearchsdiskop
	
;Can only be used if the address is actually in range
;There's an implicit assumption here about CHS addressability
;SI:CX = disk address
;ES:BX = memory address
;DS:DI = disk structure
;AL = number of sectors
;AH = disk op (02 = read; 03 = write)
;DL = disk
;Clobbers AX,DH
linearchsdiskop:
	push	si
	push	cx
	call	lineartochs
	int	13h
	pop	cx
	pop	si
	ret

;Can only be used if the address is actually in range
;SI:CX = disk address
;DS:DI = disk structure
;Clobbers SI:CX
lineartochs:
	push	ax
	push	dx
	push	cx
	xchg	ax, si
	xor	dx, dx
	div	word [di + 2]
	xchg	ax, si
	pop	ax
	div	word [di + 2]
	mov	cx, dx		; CX = sector; SI:AX = head * cylinder
	inc	cx
	mov	dx, si
	div	word [di + 4]
	xchg	ax, si
	xchg	ax, dx		; CX = sector, SI = cylinder, AX = head
	pop	dx
	mov	dh, al
	xchg	ax, si
	ror	ah, 1
	ror	ah, 1
	or	cl, ah
	mov	ch, al
	pop	ax
	ret

;Convrt nybble in AH to hex digit
xdigit:	cmp	ah, 9
	jbe	.x1
	add	ah, 7
.x1	add	ah, 48
	ret

; DS:SI = descriptor
; BL = color
; DX:AX = length
; CH = row, CL = starting column
; BH = ending column
initprogressbar:
	mov	[si + 12], ch
	mov	[si + 13], cl
	mov	[si + 14], bh
	mov	[si + 15], cl
	mov	[si + 8], ax
	mov	[si + 10], dx
	xor	dx, dx
	mov	[si + 4], dx
	mov	[si + 6], dx
	mov	[si + 2], dx
	mov	[si], dx
	mov	ah, 02h
	mov	bh, 0
	mov	dx, cx
	int	10h
	mov	ax, 0920h
	mov	bh, [si + 14]
	sub	bh, cl
	mov	cl, bh
	mov	ch, 0
	mov	bh, 0
	int	10h
	ret
	
; DS:SI = progress bar descriptor
;	+ 0 = advance by
;	+ 4 = current steps
;	+ 8 = total steps
;	+ 12 = line
;	+ 13 = left border
;	+ 14 = right border
;	+ 15 = current position
progressbar:
	push	ax
	push	bx
	push	cx
	push	dx
	push	di

	mov	ax, [si + 4]
	mov	dx, [si + 6]
	add	ax, [si]
	adc	dx, [si + 2]
	mov	[si + 4], ax
	mov	[si + 6], dx

	;Multiply by size
	mov	ch, 0
	mov	cl, [si + 14]
	sub	cl, [si + 13]
	mov	bx, dx
	mul	cx
	xchg	ax, bx
	mov	di, dx
	mul	cx	; Cannot overflow: input is only 24 bit
	add	ax, di	; (DX:)AX:BX
	xchg	dx, ax
	xchg	ax, bx
	shl	ax, 1	; And double so we can collect rounding
	rcl	dx, 1

	mov	bx, [si + 8]
	mov	di, [si + 10]

	;Divide 32 bits by 32 (24) bits result in 16 bits
	push	si
	push	bp
	xor	cx, cx
	xor	bp, bp
.dinc	inc	cx
	shl	bx, 1
	rcl	di, 1
	jnc	.dinc
.div	rcr	di, 1
	rcr	bx, 1
	cmp	dx, di
	jb	.dno
	ja	.dyes
	cmp	ax, bx
	jb	.dno
.dyes	sub	ax, bx
	sbb	dx, di
	mov	si, 1
	shl	si, cl	; Cannot overflow
	shr	si, 1
	or	bp, si
.dno	dec	cx
	clc
	jnz	.div
	xchg	ax, bp
	pop	bp
	pop	si

	push	ax	; Save round up/down for later
	shr	al, 1
	add	al, [si + 13]
	mov	dl, al
	mov	dh, [si + 12]
	mov	bh, 0
	cmp	dl, [si + 15]
	jbe	.noadv
	push	dx
	mov	dl, [si + 15]
	mov	[si + 15], al
	mov	ah, 2h
	int	10h
	mov	bh, 0
	mov	cl, [si + 15]
	sub	cl, dl
	mov	ch, 0
	mov	ax, 0ADBh
	int	10h
	pop	dx
.noadv	pop	ax
	test	al, 1
	jz	.ret
	mov	ah, 2h
	int	10h
	mov	ax, 0ADDh
	mov	cx, 1
	int	10h
.ret	pop	di
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret

align	2, db 0CCh

bcs_checkdisk	db	'Checking disk   for torn writes to '
bcs_fat		db	'FAT'
bcs_newline	db	13, 10, 0
bcs_error	db	'SSD Error   ', 13, 10, 0

align	2, db 0

bootcheck_end:
; It's starting to get tight; much more feature enhancement and it won't fit.
%if (bootcheck_end - bootcheck) > (512 * 3 - 48)
%error bootcheck is too long: correct generator to use variable sized bootcheck routine after all
%endif

; MS-DOS version restrictions
; MS-DOS 2.0 (FAT12 / 1 partition only 0x01; max cluster size can make this not work )
; MS-DOS 3.0 (FAT16 < 32MB / 1 partition only 0x04; ditto )
; MS-DOS 3.31 (FAT16 up to 512MB / extended partitions 0x06 0x05)
; MS-DOS 4.01 (FAT16 up to 2GB)
; Windows 95 OSR2 / PC DOS 7.1 (FAT32)

; Stringtable

s_name	db	0B5h, ' SSD Format ', 0C6h
s_vsn	db	'1.8a'		; should start with v but this is unreleased checkpoint
s_disk	db	'format disk '
s_mb	db	' MB'
s_small	db	'Disk is too small'
s_data1	db	'Disk X has data'
s_data2	db	'Press Y to erase'
s_exd1	db	'     Is fixed disk?     '
s_exd2	db	'Press Y to check on boot'
s_esc	db	'ESC) exit'
s_sect	db	0B5h, ' Sector Size ', 0C6h
s_stxt1	db	'Must be the physical size'
s_stxt2	db	'of the underlying hardware'
s_stxt3	db	'not BIOS or VM sector size'
s_512	db	'5) 512 bytes'
s_1024	db	'1) 1024 bytes'
s_2048	db	'2) 2048 bytes'
s_4096	db	'4) 4096 bytes'
s_8192	db	'8) 8192 bytes'
s_16384	db	'6) 16384 bytes'
s_min	db	0B5h, ' Minimum OS ', 0C6h
s_min1	db	'Reduce capacity to allow'
s_min2	db	'which OS to access disk?'
s_dos2	db	'1) MS-DOS 2.0'
s_dos3	db	'2) MS-DOS 3.0'
s_dos33	db	'3) MS-DOS 3.31'
s_dos4	db	'4) MS-DOS 4.01'
s_952	db	'5) Win 95 OSR2'
s_flba	db	'6) Force LBA (95 OSR2)'
s_derr	db	'SSD Error   '
s_sysc	db	'Run SYS C: first. Press any key to reboot.'
s_nbebr	db	'Impossible to boot an EBR by normal means.'
s_rderr	db	'SDD Read Error'
s_nptr	db	'No Boot Part'
s_rebootmsg	db	'Reboot DOS to reread partition table', 13, 10, '$'

align	4, db 0

;DEBUG
%if 0
debug_dumpregs:
	push	sp
	push	bp
	push	di
	push	si
	push	dx
	push	cx
	push	bx
	push	ax
	push	es
	push	ds
	pop	es
	pushf
	cld
	
	mov	di, .dumpstr + 3
	mov	si, sp
	add	si, 4
	mov	bx, .hex

	mov	cx, 8
.outer	push	ds
	push	ss
	pop	ds
	lodsw
	pop	ds
	push	cx
	mov	cl, 12
.nybbles:
	push	ax
	shr	ax, cl
	and	al, 15
	xlatb
	stosb
	pop	ax
	sub	cl, 4
	jnc	.nybbles
	add	di, 4
	pop	cx
	loop	.outer
	mov	dx, 1705h
	mov	bx, 7
	mov	cx, 63
	mov	si, .dumpstr
	call	out_stringat

	popf
	pop	es
	pop	ax
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	di
	pop	bp
	add	sp, 2

	ret

.hex		db 	'0123456789ABCDEF'
.dumpstr	db	'AX=xxxx BX=xxxx CX=xxxx DX=xxxx SI=xxxx DI=xxxx BP=xxxx SP=xxxx'
align	4, db 0
%endif

;END DEBUG

bss		equ	08000h
numdisks	equ	bss
disk		equ	bss + 2
saveddl		equ	bss + 3
sizestr		equ	bss + 4		; 20 bytes
minclustsizem	equ	bss + 24	; single byte size of min clust size as a multiple of 512
blockseg	equ	bss + 26	; 2 bytes
minclustsize	equ	bss + 28
oslevel		equ	bss + 30
rebootmsg	equ	bss + 31
dap		equ	bss + 32	; Size = 32 bytes
bignum		equ	bss + 64	; Size = 8 bytes
pgbar		equ	bss + 64	; Size = 16 bytes; overlaps bignum
disklba		equ	bss + 80
diskebr		equ	bss + 81
diskebralign	equ	bss + 82	; unused, retained for alginment
diskebroffset	equ	bss + 84	; Size = 4 bytes
diskpriorebr	equ	bss + 88	; Size = 4 bytes
usabledisksize	equ	bss + 92	; Size = 4 bytes, amount usable for this paritition
usabledisksize2	equ	bss + 96	; Size = 4 bytes, amount usable after this partition

disktable	equ	09000h
;[disktable] = bytes per sector
;[disktable + 2] = sectors
;[disktable + 4] = heads
;[disktable + 6] = cylinders
;[disktable + 8] = number of 512 byte sectors

buildtable	equ	0A000h
;[buildtable] = 0, 12, 16, or 32
;[buildtable + 1] = partiton type
;[buildtable + 2] = number of reserved sectors
;[buildtable + 4] = number of sectors per cluster
;[buildtable + 6] = number of extra sectors to write (not including MBR/EBR write but including superblock)
;[buildtable + 8] = offset of start of filesystem
;[buildtable + 12] = number of sectors allocated to filesystem
;[buildtable + 16] = number of sectors per fat
;[buildtable + 20] = number of clusters
;[buildtable + 24] = offset of start of EBR table (0 = MBR)
