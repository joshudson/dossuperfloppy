; SSD Host
; For resurrected ancient machines that can no longer depend on working floppy drives

; API Surface:
; int12h -> hooked to reserve our RAM
; int19h -> unhook and chain int19h
; int13h dl = 0: our disk
; int13h dl > 0 and < 80h = chain with dl-=1
; int13h ah=0 ; chain
; int13h ah=42h ds:[si] = 12h ds:[si + 2] = 0; ds:[si + 10h] = AABBh
;	This API reports we exist and our status
;	Sets ds:[si + 10h] = ABABh; ds:[si + 2] = number of sectors (including bootstrap), ds:[si + 4] = starting sector, dl = underlying
; int13h ah=43h ds:[si] = 12h ds:[si + 2] = 0; ds:[si + 10h] = AABBh
;	ah=0 Relocate here (buffer parameter is used; sector size is of buffer is 1 sector)
;	ah=7F set readwrite   ah=FF set readonly

; Copyright (C) Joshua Hudson 2026

CPU 8086
ORG 0

_start:
	xor	ax, ax
	mov	ds, ax
	mov	es, ax
	cld
	cli
	mov	ss, ax
	mov	sp, 7C00h
	sti
	mov	si, 7C40h
	mov	di, 640h
	mov	cx, 0E0h
	rep	movsw
	jmp	0:650h
	times	40h - ($ - _start)	db 0
	db	80h, 0, 1, 0, 11h, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0
	mov	si, 640h
	mov	bx, 7C00h
	mov	cx, 2
	mov	dx, 80h
	mov	ax, 0201h
	int	13h
	jnc	.go
	int	13h
	jc	.e
.go	jmp	bx
.e	mov	si, 600h + .error - _start
.e2	lodsb
	cmp	al, 0
	je	.x
	mov	bx, 7
	mov	ah, 0Eh
	int	10h
	jmp	.e2
.x	nop
	jmp	.x
.error	db "Pseudo MBR can't read SSD", 0
	times 510 - ($ - _start)	db 0
	db	55h, 0AAh
_loader:
	jmp	near	loader
ldr_type	db	0
ldr_sectors	dw	18
ldr_heads	dw	2
ldr_cyls	dw	1
ldr_unused	db	0
ldr_bps		dw	512
ldr_spc		db	0
ldr_reserved	db	5
ldr_fats	db	0
ldr_rootdir	dw	0
ldr_totalsector	dw	36
ldr_descriptor	db	0
ldr_spf		dw	0
ldr_host_spc	dw	0
ldr_host_heads	dw	0
ldr_hidden	dd	1
ldr_logicbig	dd	0
ldr_drive	db	0
ldr_signature	db	28h	; Don't want volume label here
ldr_serial	dd	0

loader:
	; We're allowed to assume the MBR is actually sane:
	; SI points to MBR row entry and resides within 600h-800h; DL is the actual disk we are on
	cld
	cli
	xor	bx, bx
	mov	ss, bx
	mov	sp, 7BFEh
	sti
	int	12h
%ifdef FREEDOS
	cmp	ax, 159
%else
	cmp	ax, 48
%endif
	jae	.ram
.ramerr	mov	si, 7C00h + .rmsg - _loader
	jmp	.error
.ram	mov	cx, 80h
	mov	ds, cx
	mov	es, bx
	mov	[ramkb], ax
	mov	[disk], dl
	mov	ax, [es:si + 8]
	mov	cx, [es:si + 10]
	mov	[offset], ax
	mov	[offset + 2], cx
	xor	si, si			; Check if drive has 4xh extensions
	mov	[si], word 1Eh
	mov	[si + 18h], si
	mov	ah, 48h
	int	13h
	jc	.no
	cmp	[si + 18h], word 512
	jne	.no
	mov	[ldr_haslba], byte 1
	jmp	.ldr2
.no	mov	[ldr_haslba], byte 0
	xor	di, di
	mov	ah, 08h
	int	13h
	jc	.derr
	mov	dl, dh
	mov	dh, 0
	inc	dx
	mov	[host_heads], dx
	and	cx, 63
	mov	[host_sectors], cx
.ldr2	mov	bx, 7E00h	; Get second loader sector
	mov	ax, 0201h
	mov	cx, [offset]
	mov	dx, [offset + 2]
	add	cx, 1
	adc	dx, 0
	call	.disk
	jc	.derr
	mov	ax, [es:bx + 510]
	mov	[es:bx - 2], ax	; Patchpoint: overwrite 55AA
	jmp	.menu
.derr	mov	si, 7C00h + .dmsg - _loader
.error	call	.tty
.x	nop
	jmp	.x

.tty	push	ds
	xor	bx, bx
	mov	ds, bx
.e2	lodsb
	cmp	al, 0
	je	.e3
	mov	bx, 7
	mov	ah, 0Eh
	int	10h
	jmp	.e2
.e3	pop	ds
	ret

.disk	test	[ldr_haslba], byte 1
	jz	.chs
	
.lba	push	si
	mov	si, dap
	mov	[si], word 10h
	mov	[si + 2], al
	mov	[si + 3], byte 0
	mov	[si + 4], bx
	mov	[si + 6], es
	mov	[si + 8], cx
	mov	[si + 10], dx
	mov	[si + 12], word 0
	mov	[si + 14], word 0
	or	ah, 40h
	mov	[alspill], al
	mov	al, 0
	mov	dl, [disk]
	int	13h
	mov	al, [alspill]
	pop	si
.lbaend	ret

.chs	push	ax
	xor	ax, ax
	xchg	ax, dx
	cmp	dx, [host_sectors]
	jae	.o
	div	word [host_sectors]
	push	ax
	xchg	ax, cx
	div	word [host_sectors]
	mov	cx, dx
	inc	cx
	pop	dx
	cmp	dx, [host_heads]
	jae	.o
	div	word [host_heads]
	cmp	ah, 3
	ja	.o
	mov	ch, al
	ror	ah, 1
	ror	ah, 1
	or	cl, ah
	mov	dh, dl
	mov	dl, [disk]
	pop	ax
	int	13h
	ret
.o	pop	ax
	mov	ah, 4
	stc
.chsend	ret
%ifdef FREEDOS
.rmsg	db	"Insufficient RAM (need 159K)", 0
%else
.rmsg	db	"Insufficient RAM (need 48K)", 0
%endif
.dmsg	db	"Error reading SSD", 0
.menu	mov	si, 7C00h + .menutext - _loader
	; End of code that must exist in the first sector
	call	.tty
	mov	[es:7C01h], word (.derr - _loader - 3)
	mov	[es:7BFEh], word 13CDh	; int 13h
.key	mov	ah, 0
	int	16h
	cmp	al, '1'
	je	.normal
	cmp	al, '2'
	je	.emergency
	jmp	.key
.normal	mov	si, 7BEh
	mov	dl, [disk]
	mov	bp, ds
	xor	bx, bx
	mov	ds, bx
.nloop	mov	dh, [si + 1]
	mov	cx, [si + 2]
	mov	ah, [si + 4]
	cmp	ah, 0
	je	.nnext
	cmp	ah, 0B0h
	je	.nnext
	cmp	ah, 0B5h
	je	.nnext
	cmp	ah, 0
	je	.nnext
	cmp	ah, 5
	je	.nnext
	cmp	ah, 0Fh
	je	.nnext
	cmp	ah, 011h
	je	.nnext	; Us (HIDDEN FAT-12)
	cmp	ah, 0Ch
	je	.lban
	cmp	ah, 0Eh
	je	.lban
	cmp	dh, 0FEh
	jb	.chsn
	cmp	cx, 0FFFFh
	jne	.chsn
.lban	mov	cx, [si + 8]
	mov	bx, [si + 10]
	mov	si, 0600h
	mov	[si], word 10
	mov	[si + 2], word 1
	mov	[si + 4], word 7C00h
	mov	[si + 6], es
	mov	[si + 8], cx
	mov	[si + 10], bx
	mov	[si + 12], es
	mov	[si + 14], es
	mov	ax, 4200h
	jmp	.ngo
.chsn	mov	bx, 7C00h
	mov	ax, 0201h
.ngo	jmp	0:7BFEh
.nnext	add	si, 16
	cmp	si, 07FEh
	jb	.nloop
	mov	ds, bp
.emergency:
	mov	ax, [ramkb]
	dec	ax
	mov	[ramkb], ax
	mov	cl, 6
	shl	ax, cl
	sub	ax, (512 * 3) / 16
	mov	es, ax
	mov	cx, [offset]
	mov	dx, [offset + 2]
	add	cx, 2
	adc	dx, 0
	mov	bx, 512 * 3
	mov	ax, 0202h
	call	.disk
	jc	.derr
	
	mov	bp, ds
	xor	bx, bx
	mov	ds, bx
	; Tie in interrupt handlers
	mov	si, 12h
	mov	di, int12h_vector
	call	.tie
	mov	si, 13h
	mov	di, int13h_vector
	call	.tie
	mov	si, 19h
	mov	di, int19h_vector
	call	.tie
	mov	si, 1Eh
	mov	di, int1Eh_vector
	call	.tie
	mov	ds, bp
	; Copy disk routine up
	test	[ldr_haslba], byte 1
	jz	.uchs
	mov	si, .lba - _loader + 7C00h
	mov	cx, (.lbaend - .lba + 2) / 2
	jmp	.ucpy
.uchs	mov	si, .chs - _loader + 7C00h
	mov	cx, (.chsend - .chs + 2) / 2
.ucpy	mov	ds, bx
	mov	di, diskroutine
	rep	movsw
	mov	ds, bp
	; Copy parameters up
	mov	ax, [ramkb]
	mov	[es:ramkb], ax
	mov	ax, [offset]
	mov	dx, [offset + 2]
	mov	cl, [disk]
	mov	[es:offset], ax
	mov	[es:offset + 2], dx
	mov	[es:disk], cl
	mov	ax, [es:host_sectors]
	mov	dx, [es:host_heads]
	mov	ds, bx
	mov	ax, [7C04h]
	mov	[es:floppy_sectors], ax
	mov	ax, [7C06h]
	mov	[es:floppy_heads], ax
	mov	ax, [7C13h]
	sub	ax, 4
	mov	[es:floppy_size], ax
	xor	dx, dx
	div	word [es:floppy_heads]
	div	word [es:floppy_sectors]
	mov	[es:floppy_cyls], ax
	mov	ah, [7C03h]
	mov	[es:floppy_type], ah
	and	ah, 7Fh
	mov	al, [es:floppy_sectors]
	mov	cl, [es:floppy_cyls]
	dec	cl
	mov	[es:int1Eh_entry.sectors], al
	mov	[es:int1Eh_entry.max_track], cl
	mov	[es:int1Eh_entry.drive_type], ah
	mov	ds, bx
	mov	es, bx
	mov	bx, 7C00h
	mov	ax, 0201h
	mov	cx, 1
	xor	dx, dx
	jmp	0:7BFEh

.tie	shl	si, 1
	shl	si, 1
	mov	ax, [es:di]
	mov	dx, es
	xchg	ax, [si]
	xchg	dx, [si + 2]
	mov	[es:di], ax
	mov	[es:di + 2], dx
	ret
.copyright	db	"Copyright (C) Joshau Hudson 2026"
	
.menutext	db	13, 10
		db	"1) Boot Normal", 13, 10
		db	"2) Boot Emergency", 13, 10
		db	0
	times 1024 - ($ - _loader) db 0

		db "->H", 0
int1Eh_entry:
.specify1	db	0AFh
.specify2	db	02h
.shutoff	db	37
.bps_code	db	2
.sectors	db	18
.interblock	db	1Bh
.data		db	0FFh
.gap		db	6Ch
.fill		db	0F6h
.settle		db	15
.startup	db	8
.max_track	db	79
.data_rate	db	3
.drive_type	db	0

int12h_entry:
	mov	ax, [cs:ramkb]
	nop
	nop
	nop
	iret
paramschsv:
	jmp	int13h_entry.paramschs
paramslbav:
	jmp	int13h_entry.paramslba
int13h_entry:
	cmp	ah, 0
	je	.chain
	cmp	dl, 0
	je	.ours
	test	dl, 80h
	jnz	.chain
	dec	dl
.chain	jmp	far [cs:int13h_vector]
.ours	push	ds
	push	dx
	push	cx
	push	bp
	mov	bp, sp
	push	cs
	pop	ds
	cmp	ah, 1
	jne	.nrs
	mov	ah, [diskfail]
	and	[bp + 12], word 0FFFEh
	jmp	.iret2
.nrs	cmp	ah, 3
	jbe	.readwritechs
	cmp	ah, 8
	je	short paramschsv
	cmp	ah, 41h
	jb	short .no
	je	short .extlba
	cmp	ah, 43h
	jbe	short .readwritelba
	cmp	ah, 48h
	je	short paramslbav
.no	mov	ah, 1
	jmp	.stc_iret

.keepc_iret:
	jc	.stc_iret
.clc_iret:
	and	[bp + 12], word 0FFFEh
	jmp	.iret
.stc_iret:
	or	[bp + 12], word 0001h
.iret	mov	[diskfail], ah
.iret2	pop	bp
	pop	cx
	pop	dx
	pop	ds
	iret

.readwritechs:
	call	.writecheck
	jnz	short .readonly
	call	.floppy_geometry
	jc	.badgeo
	call	.diskop
	jmp	.keepc_iret

.readwritelba:
	push	es
	mov	es, [bp + 6]
	cmp	[es:si], byte 12h
	jne	.readwritelbaop
	jmp	.lbacustom

.extlba:
	mov	ah, 1
	mov	[bp + 2], word 1
	mov	bx, 0AA55h
	jmp	.clc_iret

.readwritelbaop:
	call	.writecheck
	jnz	short .readonlyes
	cmp	[es:si], byte 10h
	jne	.lbainval
	cmp	[es:si + 3], byte 0
	jne	.lbabads
	mov	cx, [es:si + 8]
	mov	dx, [es:si + 10]
	or	dx, [es:si + 10]
	or	dx, [es:si + 12]
	or	dx, [es:si + 14]
	jnz	.badgeoes
	cmp	cx, [floppy_size]
	jae	.badgeoes
	push	bx
	mov	al, [es:si + 2]
	and	ah, 3
	les	bx, [es:si + 4]
	call	.diskop
	pop	bx
	pop	es
	jmp	.keepc_iret

.readonlyes:
	pop	es
.readonly:
	mov	ah, 3
	jmp	.stc_iret

.lbainval:
	mov	ah, 1
	pop	es
	jmp	.stc_iret

.badgeoes:
	pop	es
.badgeo:
	mov	ah, 4
	jmp	.stc_iret

.lbabads:
	mov	ah, 0Dh
	pop	es
	jmp	.stc_iret

.paramschs:
	mov	cl, [floppy_sectors]
	mov	ch, [floppy_cyls]
	dec	ch
	mov	dh, [floppy_heads]
	dec	dh
	mov	[bp + 2], cx
	mov	[bp + 4], dx
	mov	bl, [floppy_type]
	and	bl, 7Fh
	xor	di, di
	mov	ds, di
	les	di, [1Eh * 4]
	jmp	.clc_iret

.paramslba:
	push	es
	mov	cx, [bp + 6]
	mov	es, cx
	cmp	[es:si], word 1Eh
	jne	.lbainval
	xor	dx, dx
	mov	[es:si + 2], dx
	mov	cx, [floppy_cyls]
	mov	[es:si + 4], cx
	mov	[es:si + 6], dx
	mov	cx, [floppy_heads]
	mov	[es:si + 8], cx
	mov	[es:si + 10], dx
	mov	cx, [floppy_sectors]
	mov	[es:si + 12], cx
	mov	[es:si + 14], dx
	mov	cx, [floppy_size]
	mov	[es:si + 16], cx
	mov	[es:si + 18], dx
	mov	[es:si + 20], dx
	mov	[es:si + 22], dx
	mov	[es:si + 24], word 512
	mov	[es:si + 26], dx
	mov	[es:si + 28], dx
	pop	es
	jmp	.clc_iret

.floppy_geometry:
	push	ax
	mov	al, ch
	mul	byte [floppy_heads]
	add	al, dh
	adc	ah, 0
	mul	word [floppy_sectors]
	mov	ch, 0
	dec	cx
	js	.fbadgeo
	add	cx, ax
	pop	ax
	ret
.fbadgeo:
	pop	ax
	stc
	ret

.lbacustom:
	cmp	[es:si + 16], word 0AABBh
	jne	short .lbainvalv
	cmp	[es:si + 2], word 0
	jne	short .lbainvalv
	cmp	ah, 43h
	je	.lbawritedecode
	mov	ah, [disk]		; Status report
	mov	[bp + 4], ah
	mov	cx, [floppy_size]
	add	cx, 4
	mov	[es:si + 2], cx
	mov	cx, [offset]
	mov	dx, [offset + 2]
	mov	[es:si + 8], cx
	mov	[es:si + 10], dx
	xor	dx, dx
	mov	[es:si + 12], word dx
	mov	[es:si + 14], word dx
	mov	[es:si + 16], word 0ABABh
	mov	ah, 0
	mov	al, [floppy_type]
	pop	es
	jmp	.clc_iret
.lbawritedecode:
	cmp	al, 7Fh
	je	.lbasetrw
	cmp	al, 0FFh
	je	.lbasetrw
	cmp	al, 0
	je	.lbamove
.lbainvalv:
	jmp	.lbainval
.lbasetrw:
	push	ax
	push	bx
	les	bx, [es:si + 4]
	mov	cx, [offset]
	mov	dx, [offset + 2]
	mov	ax, 0201h
	call	diskroutine
	jc	.lbasetrwexit
	mov	cl, [bp - 4]
	and	cl, 80h
	mov	al, [es:bx + 3]
	and	al, 7Fh
	or	al, cl
	mov	[es:bx + 3], al
	mov	cx, [offset]
	mov	dx, [offset + 2]
	mov	ax, 0301h
	call	diskroutine
	jc	.lbasetrwexit
	mov	ch, [es:bx + 3]
	mov	[floppy_type], ch
.lbasetrwexit:
	mov	[bp - 3], ah
	pop	bx
	pop	ax
	pop	es
	jmp	.keepc_iret
.lbamove:
	push	si		; Relocate us to sectors starting at [es:si + 8]
	push	bx
	push	bp
	mov	bp, sp
	sub	sp, 12
%define offsetdelta	bp - 12
%define offsetsrc	bp - 8
%define offsetdest	bp - 4
	mov	cx, [es:si + 8]
	mov	dx, [es:si + 10]
	mov	[offsetdest], cx
	mov	[offsetdest + 2], dx
	mov	cx, [offset]
	mov	dx, [offset + 2]
	mov	[offsetsrc], cx
	mov	[offsetsrc + 2], dx
	les	bx, [es:si + 4]
	mov	si, [floppy_sectors]
	add	si, 4
	sti			; Interrupts are on for the copy loop
	cmp	dx, [offsetdest + 2]
	ja	.lbamoveup
	jb	.lbamovedown
	cmp	cx, [offsetdest]
	ja	.lbamoveup
	jb	.lbamovedown
	mov	ah, 0
	clc
	jmp	.lbamoveexit	; Nothing to do
.lbamoveup:
	mov	[offsetdelta], word 1
	mov	[offsetdelta + 2], word 0
	jmp	.lbamovego
.lbamovedown:
	mov	dx, -1
	mov	[offsetdelta], dx
	mov	[offsetdelta + 2], dx
	inc	dx
	dec	si
	add	[offsetsrc], si
	adc	[offsetsrc + 2], dx
	add	[offsetdest], si
	adc	[offsetdest + 2], dx
	inc	si
.lbamovego:
	mov	cx, [offsetsrc]
	mov	dx, [offsetsrc + 2]
	mov	ax, 0201h
	call	diskroutine
	jc	.lbamoveexit
	mov	cx, [offsetdest]
	mov	dx, [offsetdest + 2]
	call	diskroutine
	jc	.lbamoveexit
	mov	cx, [offsetdelta]
	mov	dx, [offsetdelta + 2]
	add	[offsetsrc], cx
	adc	[offsetsrc + 2], dx
	add	[offsetdest], cx
	adc	[offsetdest + 2], dx
	dec	si
	jnz	.lbamovego
	mov	ah, 0
	clc
.lbamoveexit:
	cli
%undef	offsetdelta
%undef	offsetsrc
%undef	offsetdest
	mov	sp, bp
	pop	bp
	pop	bx
	pop	si
	jc	.lbamoveexit2
	mov	cx, [es:si + 8]
	mov	dx, [es:si + 10]
	mov	[offset], cx
	mov	[offset + 2], dx
.lbamoveexit2:
	pop	es
	jmp	.keepc_iret

.diskop:
	test	dx, dx
	jnz	.diskbadgeo
	cmp	cx, [floppy_size]
	jae	.diskbadgeo
	add	cx, [offset]
	adc	dx, [offset + 2]
	jc	.diskbadgeo
	add	cx, 4
	adc	dx, 0
	jc	.diskbadgeo
	jmp	diskroutine

.diskbadgeo:
	mov	ah, 4
	stc
	ret

.writecheck:
	test	ah, 1
	jz	.wcok
	test	[floppy_type], byte 80h
.wcok	ret	

int19h_entry:
	xor	ax, ax
	mov	es, ax
	mov	ax, cs
	mov	ds, ax
	mov	ax, [int12h_vector]
	mov	dx, [int12h_vector + 2]
	mov	es:[12h * 4], ax
	mov	es:[12h * 4 + 2], dx
	mov	ax, [int13h_vector]
	mov	dx, [int13h_vector + 2]
	mov	es:[13h * 4], ax
	mov	es:[13h * 4 + 2], dx
	mov	ax, [int19h_vector]
	mov	dx, [int19h_vector + 2]
	mov	es:[19h * 4], ax
	mov	es:[19h * 4 + 2], dx
	mov	ax, [int1Eh_vector]
	mov	dx, [int1Eh_vector + 2]
	mov	es:[1Eh * 4], ax
	mov	es:[1Eh * 4 + 2], dx
	int	19h
	iret	; That's not good

	align 2, db 0

int12h_vector:
	dw	int12h_entry
	dw	0
int13h_vector:
	dw	int13h_entry
	dw	0
int19h_vector:
	dw	int19h_entry
	dw	0
int1Eh_vector:
	dw	int1Eh_entry
	dw	0

dap		dw	16, 0, 0, 0, 0, 0, 0, 0
ramkb		dw	640
floppy_size	dw	2880
floppy_cyls	dw	80
floppy_heads	dw	2
floppy_sectors	dw	18
host_heads	dw	255
host_sectors	dw	63
offset		dd	0
disk		db	0
diskfail	db	0
floppy_type	db	0
alspill		db	0
ldr_haslba:	; Only used by loader
diskroutine:
	times (loader.chsend - loader.chs + 1)	db 0CCh
	times (1024 - ($ - (int1Eh_entry - 4)))	db 0
