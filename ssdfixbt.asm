BITS 16

CPU 8086

ORG 0100h

f_have_dig	equ 01h
f_force_mbr	equ 02h
f_force_boot	equ 04h
f_rebuild_info	equ 08h
f_force_part	equ 10h
f_update	equ 20h
f_lba		equ 80h

f2_loopbreak	equ 01h
f2_dirty	equ 02h
f2_fat32	equ 04h
f2_foundmbr	equ 08h
f2_foundfree	equ 10h

_start:
	cld
	mov	si, 80h
	mov	bl, [si]
	mov	bh, 0
	inc	bx
	mov	[bx + si], byte 0	; If overflow, takes out first byte of _start, which doesn't matter
	xor	ax, ax
	mov	cx, 10
.nop	inc	si
	mov	bl, [si]
	cmp	bl, 0
	je	parsedargs
	cmp	bl, '/'
	je	.nop
	cmp	bl, ' '
	je	.nop
	cmp	bl, '0'
	jb	.ndig
	cmp	bl, '9'
	ja	.ndig
	mul	cl
	sub	bl, '0'
	add	al, bl
	or	ch, f_have_dig
	jmp	.nop
.ndig	and	bl, 0DFh		; Uppercase BL
	cmp	bl, 'M'
	jne	.nm
	or	ch, f_force_mbr
	jmp	.nop
.nm	cmp	bl, 'B'
	jne	.nb
	or	ch, f_force_boot
	jmp	.nop
.nb	cmp	bl, 'I'
	jne	.ni
	or	ch, f_rebuild_info
	jmp	.nop
.ni	cmp	bl, 'P'
	jne	.np
	or	ch, f_force_part
	jmp	.nop
.np	cmp	bl, 'U'
	jne	.usg
	or	ch, f_update
	jmp	.nop
.usg	mov	dx, usage
	jmp	error_out
senseerror:
	jmp	ssderror
parsedargs:
	test	ch, f_have_dig
	jnz	.h
	mov	al, 1
.h	add	al, 7Fh
	mov	ah, 0
	mov	[disk], ax
	mov	[flags], ch
	mov	dx, ax
	mov	ah, 08h
	push	es
	xor	di, di
	mov	es, di
	int	13h
	pop	es
	jc	senseerror
	mov	dl, dh
	mov	dh, 0
	inc	dx
	mov	[heads], dx
	and	cx, 63
	mov	[sectors], cx

checkmbr:
	mov	[flags2], byte 0
	mov	dx, [disk]
	mov	cx, 1
	mov	bx, buf1
	mov	ax, 0201h
	int	13h
	jc	senseerror
	test	[flags], byte f_force_part
	jnz	.bkpart1
	cmp	[buf1 + 510], word 0AA55h	; Magic bytes gone=whole thing assumed wiped out
	jne	.bkpart1
.cpart1	mov	bp, chkpart1_compare
	call	chkpart
	jnz	.bkpart2
	mov	ax, [si + 8]
	mov	cx, [si + 10]
	mov	[infooff], ax
	mov	[infooff + 2], cx
	or	[flags2], byte f2_foundmbr
	mov	bp, chkpart2_compare
	call	chkpart
	jnz	.bkpart2
	mov	ax, [si + 8]
	mov	cx, [si + 10]
	mov	[superblockoff], ax
	mov	[superblockoff + 2], cx
	and	[flags2], byte ~f2_loopbreak
	cmp	[si + 4], byte 0Bh
	jb	.nfat32
	cmp	[si + 4], byte 0Ch
	ja	.nfat32
	or	[flags2], byte f2_fat32
.nfat32	cmp	[si + 4], byte 0Ch
	jb	.cmbr
	cmp	[si + 4], byte 0Eh
	ja	.cmbr
	or	[flags], byte f_lba
.cmbr	test	[flags], byte f_force_mbr
	jnz	.bkmbr
	cmp	[buf1], byte 0EBh
	je	.cboot
	cmp	[buf1 + 2], byte 0EBh
	jne	.bkmbr
.cboot	test	[flags2], byte f2_dirty
	jz	.clean
	call	writembr
.clean	jmp	updatembr

.bkpart1:	; Unconditionally restore partition table
	call	.bkpartloopbreak
	and	[flags], byte ~f_force_part
	call	locatembr
	mov	si, buf1 + 512 + 1B6h
	mov	di, buf1 + 1B6h
	mov	cx, 25h
	rep	movsw
	jmp	.cpart1

.bkpart2:	; Slot in partition; identifying function in bp
	call	.bkpartloopbreak
	call	locatembr
	push	bp
	mov	bp, chkpartempty_compare
	call	chkpart
	pop	bp
	jz	.bkpf
	mov	dx, noslot
.bkpfn	jmp	error_out
.bkpf	mov	di, si
	mov	si, buf1 + 512 + 1BEh
	call	chkpartanywhere
	jz	.bkpf2
	mov	dx, nobp
	jmp	.bkpfn
.bkpf2	mov	cx, 8
	rep	movsw
	;We fixed something so we made progress
	and	[flags2], byte ~f2_loopbreak
	jmp	.cpart1

.bkmbr:
	test	[flags2], byte f2_loopbreak
	jnz	.bkmbrloop
	or	[flags2], byte f2_loopbreak | f2_dirty
	and	[flags], byte ~f_force_mbr
	call	locatembr
	mov	si, buf1 + 512
	mov	di, buf1
	mov	bx, di
	mov	cx, 0DBh
	rep	movsw
	mov	dx, [disk]
	mov	cx, 1
	jmp	.cmbr
.bkmbrloop:
	mov	dx, nobm
	jmp	error_out

.bkpartloopbreak:
	test	[flags2], byte f2_loopbreak
	jz	.bknlb
	mov	dx, nobp
	jmp	error_out
.bknlb	or	[flags2], byte f2_loopbreak | f2_dirty
	ret

updatenombr:
	mov	dx, nobb
	jmp	error_out

updatembr:
	test	[flags], byte f_update
	jz	checkboot
	call	writebackupmbr

checkboot:
	mov	cx, [superblockoff]
	mov	si, [superblockoff + 2]
	mov	bx, buf2
	mov	ax, 0208h
	call	lineardiskop
	test	[flags], byte f_force_boot
	jnz	.bkboot
	call	isfat
	jnz	.bkboot
.cinfo	call	isnotfat32
	jnz	.nupd
	;Check that INFO sector corresponds to MBR location of INFO sector
	mov	ax, cx
	mov	cx, [bx + 30h]
	or	cx, cx
	je	.ifrg
	cmp	cx, [bx + 0Eh]
	jae	.ifro
	add	cx, ax
	adc	si, 0
	jz	.ifrg
.ifro	mov	cx, 0FFFFh
.ifrg	cmp	[buf1 + 30h], cx
	je	.ifrgd
	call	writembr
.ifrgd	test	[flags], byte f_update
	jz	.ifrnbk
	call	writebackupmbr
.ifrnbk	call	maybeclearxinfo
.chkupd	test	[flags], byte f_update
	jz	.nupd
	mov	ax, [bx + 32h]
	or	ax, ax
	jz	.nupd			; No backup sector
	cmp	ax, [bx + 0Eh]
	jae	.nupd			; No backup sector (out of range)
	add	cx, ax
	adc	si, 0
	mov	ax, 0308h
	call	lineardiskop
.nupd	test	[flags], byte f_rebuild_info
	jz	.exit
	call	rebuildinfo
.exit	mov	ax, 4C00h
	int	21h

.bkboot:
	mov	bx, buf2
	test	[flags2], byte f2_fat32
	jz	.bkbootemergency
	mov	cx, [superblockoff]
	mov	si, [superblockoff + 2]
	add	cx, 8
	adc	si, 0
	mov	ax, 0208h
	call	lineardiskop
	call	isfat
	jz	.bkrecover		; Found it
	add	cx, 8
	adc	si, 0
	mov	ax, 0208h
	call	lineardiskop
	call	isfat
	jnz	.bkbootemergency
.bkrecover:
	mov	cx, [superblockoff]
	mov	si, [superblockoff + 2]
	mov	ax, 0301h
	call	lineardiskop
	jmp	.cinfo
.bkbootemergency:		; We have a copy of the BPB sitting in buf1 but no bootloader
	mov	si, buf1
	mov	di, bx
	cmp	[si + 15h], byte 0F8h
	jne	.eee
	cmp	[si], byte 0EBh
	jne	.e2
	mov	cl, [si + 1]
	add	cl, 2
	jmp	.ex
.e2	cmp	[si + 2], byte 0EBh
	jne	.eee
	mov	cl, [si + 3]
	add	cl, 4
.ex	mov	ch, 0
	mov	al, cl
	sub	al, 2
	rep	movsb
	mov	[bx], byte 0EBh
	mov	[bx + 1], al
	mov	[bx + 2], byte 90h
	mov	[bx + 3], byte 'I'
	mov	si, fat16noboot
	mov	cx, (fat16noboot_end - fat16noboot) / 2
	rep	movsw
	mov	cx, buf2 + 1FCh
	sub	cx, di
	shr	cx, 1
	xor	ax, ax
	rep	stosw
	mov	ax, 8000h
	stosw
	mov	ax, 0AA55h
	stosw
	mov	bx, buf2
	call	isfat
	jnz	.eee
	mov	dx, sysc
	mov	ah, 9
	int	21h
	mov	[bx + 512], word 0	; Ensure that maybeclearxinfo doesn't trash anything
	jmp	.bkrecover
.eee	mov	dx, nobbs
	jmp	error_out

isfat:	; Checks if bx starts with a hard disk FAT boot sector
	cmp	[bx], byte 0EBh		; Checking for the JMP
	je	.short
	cmp	[bx], byte 0E9h
	jne	.no
	cmp	[bx + 2], byte 0
	jne	.no
.short	cmp	[bx + 0Bh], byte 0	; Needs to be a multipe of 512 therefore low byte must be 0
	jne	.no
	cmp	[bx + 15h], byte 0F8h	; We always set F8 = hard disk (or anything else for which BPB must be used)
	jne	.no
	cmp	[bx + 0Eh], word 0	; Must be > 0 or it's not a FAT image
	je	.no
	cmp	[bx + 10h], byte 2	; SSD tools can't handle not two FATs
	jne	.no
	test	[bx + 11h], byte 15	; # root directory entries must be a multipe of 16
	jnz	.no
	cmp	[bx + 1FEh], word 0AA55h
	jne	.no
.no	ret

isnotfat32:
	cmp	[bx + 13h], word 0
	;Carry flag is always clear by that subtraction
	ret

chkpart:	; Find partition with boot check routine
	mov	si, buf1 + 01BEh
chkpartanywhere:
	mov	cx, 4
.loop	call	bp
	je	.f
	add	si, 16
	loop	.loop
.f	clc	; Decision ladders depend on CF remains clear exiting this function
	ret

chkpart1_compare:
	mov	al, [si + 4]
	cmp	al, byte 0B0h
	jz	.out
	cmp	al, byte 0B5h
.out	ret

chkpart2_compare:
	mov	al, [si + 4]
	cmp	al, byte 01h
	jz	.out
	cmp	al, byte 04h
	jz	.out
	cmp	al, byte 06h
	jz	.out
	cmp	al, byte 0Bh
	jz	.out
	cmp	al, byte 0Ch
	jz	.out
	cmp	al, byte 0Eh
.out	ret

chkpartempty_compare:
	cmp	[si + 4], byte 0
	ret

locatembr:
	test	[flags2], byte f2_foundmbr
	jnz	.useinf
	xor	si, si
	mov	cx, 1
.ltx	mov	bx, buf1 + 512
	mov	ax, 0201h
	call	lineardiskop
	call	isbootpart
	jz	.gotcha
	shl	cl, 1
	cmp	cl, 64
	jb	.ltx
.no	mov	dx, nobb
	jmp	error_out
.gotcha	call	locatembradj.is
	jmp	.is
.useinf	call	locatembrnoread.mybuf
	jnz	.no
.is	mov	ax, 0201h
	call	lineardiskop
	ret

locatembrnoread:
	mov	bx, buf1 + 512
.mybuf	mov	cx, [infooff]	; Note that these are always filled by now
	mov	si, [infooff + 2]
	mov	ax, 0201h
	call	lineardiskop
	call	locatembradj
	ret

locatembradj:
	call	isbootpart
	jnz	.no
.is	pushf
	mov	al, [bx + 8]
	mov	ah, 0
	add	cx, ax
	adc	si, 0
	popf
.no	ret

isbootpart:
	cmp	[bx], byte 0EBh
	jne	.no
	cmp	[bx + 2], word 'SS'
	jne	.no
	cmp	[bx + 4], word 'D-'
	jne	.no
	cmp	[bx + 6], word '>H'
.no	ret

;Remove bogus info sector because it's somewhere else
maybeclearxinfo:
	mov	bx, buf2
	cmp	[bx + 30h], word 1
	je	.nxinfo
	lea	di, [bx + 512]
	cmp	[di], word 5252h
	jne	.nxinfo
	cmp	[di + 2], word 4161h
	jne	.nxinfo
	mov	ax, '39'
	stosw
	mov	ax, '85'
	stosw
	xor	ax, ax
	mov	cx, 254
	rep	stosw
	mov	cx, 1
	add	cx, [superblockoff]
	adc	si, [superblockoff + 2]
	mov	ax, 0301h
	add	bx, 512
	call	lineardiskop
	sub	bx, 512
	cmp	[bx + 32h], word 0
	je	.nxinfo
	mov	ax, [bx + 0Eh]
	or	ax, ax
	jz	.nxinfo
	cmp	[bx + 32h], word ax
	jae	.nxinfo
	add	cx, [bx + 32h]
	adc	si, 0
	mov	ax, 0301h
	add	bx, 512
	call	lineardiskop
.nxinfo	ret

writembr:
	mov	bx, buf1
	mov	cx, 1
	mov	dx, [disk]
	mov	ax, 0301h
	int	13h
	ret

writebackupmbr:
	call	locatembrnoread
	jc	.out
	jnz	.out
	mov	bx, buf1
	mov	ax, 0301h
	call	lineardiskop
.out	ret

rebuildinfo:
	xor	bp, bp		; BP is zero register in this function
	mov	bx, buf2
	call	isnotfat32
	jnz	.exitz
	mov	cx, [bx + 030h]
	or	cx, cx
	jz	.exitz
	cmp	cx, [bx + 0Eh]
	jae	.exitz
	xor	si, si
	add	cx, [superblockoff]
	add	si, [superblockoff + 2]
	mov	[infooff], cx
	mov	[infooff + 2], si
	mov	ax, [bx + 24h]
	mov	dx, [bx + 26h]
	mov	[fatsects], ax
	mov	[fatsects + 2], dx
	mov	ax, [bx + 20h]		; Turn sector count back into cluster count
	mov	dx, [bx + 22h]
	sub	ax, [bx + 0Eh]
	sbb	dx, bp
	sub	ax, [fatsects]
	sbb	dx, [fatsects + 2]
	sub	ax, [fatsects]
	sbb	dx, [fatsects + 2]
	mov	cl, [bx + 0Dh]
	jmp	short	.ndecl
.exitz	ret
.ndec	shr	dx, 1
	rcr	ax, 1
.ndecl	shr	cl, 1
	jnz	short	.ndec
	add	ax, 2
	adc	dx, bp
	mov	[firstfree], ax
	mov	[firstfree + 2], dx
	mov	[remaining], ax
	mov	[remaining + 2], dx
	mov	[free], bp
	mov	[free + 2], bp
	mov	cx, [superblockoff]
	mov	si, [superblockoff + 2]
	add	cx, [bx + 0Eh]
	adc	si, bp
	mov	[firstfatsec], cx
	mov	[firstfatsec + 2], si
.fxl	mov	ax, 32
	cmp	[fatsects + 2], bp
	jne	short	.fxl32
	cmp	[fatsects], ax
	jae	.fxl32
	mov	al, [fatsects]
.fxl32	sub	[fatsects], ax
	sbb	[fatsects + 2], bp
	mov	ah, 02h
	mov	bx, buf1
	call	lineardiskop
	push	ax
	push	cx		; Accessed on stack by .firstfree below
	push	si
	mov	cl, 7
	shl	ax, cl
	cmp	[remaining + 2], bp
	jne	short	.rm32
	cmp	[remaining], ax
	jae	short	.rm32
	mov	ax, [remaining]
.rm32	push	ax		; skipped over unsused by .firstfree below
	xchg	ax, cx
	mov	dx, 1
	mov	si, bx
.el	lodsw
	xchg	ax, bx
	lodsw
	or	ax, bx
	jnz	short	.ell
	add	[free], dx
	adc	[free + 2], bp
	test	[flags2], byte f2_foundfree
	jz	short	.firstfree
.ell	loop	.el
	pop	bx
	pop	si
	pop	cx
	pop	ax
	add	cx, ax
	adc	si, bp
	sub	[remaining], bx
	jz	short	.fxlm
	sbb	[remaining + 2], bp
.fxlv	jmp	.fxl
.fxlm	cmp	[remaining + 2], bp	; If subtraction was exactly zero, we couldn't have carried, so just compare
	jne	short	.fxlv
	mov	bx, buf1
	mov	di, bx
	mov	ax, 5252h
	stosw
	mov	ax, 4161h
	stosw
	mov	cx, 240
	xor	ax, ax
	rep	stosw
	mov	ax, 7272h
	stosw
	mov	ax, 6141h
	stosw
	mov	ax, [free]
	stosw
	mov	ax, [free + 2]
	stosw
	mov	ax, [firstfree]
	sub	ax, 1
	stosw
	mov	ax, [firstfree + 2]
	sbb	ax, bp
	stosw
	mov	cx, 7
	xor	ax, ax
	rep	stosw
	mov	ax, 0AA55h
	stosw
	mov	cx, [infooff]
	mov	si, [infooff + 2]
	mov	ax, 0301h
	call	lineardiskop
	ret
.firstfree:
	;Nasty looking algorithm to get current cluster
	lea	di, [si - buf1 - 4]
	shr	di, 1
	shr	di, 1
	or	[flags2], byte f2_foundfree
	mov	bx, sp
	push	cx
	mov	dx, [bx + 2]	; High half of FAT address
	mov	ax, [bx + 4]	; Low half of FAT address
	sub	ax, [firstfatsec]
	sbb	dx, [firstfatsec + 2]
	mov	cx, 7
.ffl	shl	ax, 1
	rcl	dx, 1
	loop	.ffl
	pop	cx
	add	ax, di
	adc	dx, bp
	mov	[firstfree], ax
	mov	[firstfree + 2], dx
	mov	dx, 1		; Reset DX back to its constant
	jmp	.ell

;Input: AH=2 or 3, AL=count, BX=buffer SI:CX = disk offset (SI is almost always zero)
;Preserves AL, BX, SI, CX, DI, BP
lineardiskop:
	test	[flags], byte f_lba
	jnz	.lba
	push	si
	push	cx
	xchg	ax, si
	xor	dx, dx
	div	word [sectors]
	xchg	ax, cx
	div	word [sectors]
	xchg	dx, cx
	inc	cx
	div	word [heads]
	ror	ah, 1
	ror	ah, 1
	xchg	ah, al
	or	cx, ax
	mov	dh, dl
	mov	dl, [disk]
	xchg	ax, si
	int	13h
	pop	cx
	pop	si
	jmp	.epilog
.lba	push	di
	mov	di, dap
	mov	[di], word 10h
	mov	[di + 2], al
	mov	[di + 3], byte 0
	mov	[di + 4], bx
	mov	[di + 6], es
	mov	[di + 8], cx
	mov	[di + 10], si
	mov	[di + 12], word 0
	mov	[di + 14], word 0
	xchg	si, di
	mov	dl, [disk]
	or	ah, 40h
	int	13h
	xchg	si, di
	pop	di
.epilog	jc	ssderror
	ret

hexdigit:
	cmp	al, 10
	jb	.small
	add	al, 'A' - '0' - 10
.small	add	al, '0'
	ret
ssderror:
	mov	si, error
	mov	al, ah
	push	ax
	mov	cl, 4
	shr	al, cl
	call	hexdigit
	mov	[si + 10], al
	pop	ax
	and	al, 15
	call	hexdigit
	mov	[si + 11], al
	mov	dx, si
error_out:
	mov	ah, 9
	int	21h
	mov	ax, 4C01h
	int	21h

align 2, db 0CCh
fat16noboot:
	call	.msg
db	'Run SYS C: next. Press any key to reboot.', 0
.msg	pop	si
	push	cs
	pop	ds
	cld
.loop	mov	bx, 7
	lodsb
	cmp	al, 0
	je	.elp
	mov	ah, 0Eh
	int	10h
	jmp	.loop
.elp	mov	ah, 0
	int	16h
	jmp	0F000h:0FFF0h
align 2, db 0CCh
fat16noboot_end:

usage	db	'Usage: SSDFIXBT [/M] [/P] [/B] [/I] [#]', 13, 10
	db	'#     1 based hard disk number, default 1', 13, 10
	db	'/M    Unconditionally restore MBR from backup', 13, 10
	db	'/P    Unconditionally restore partition table from backup', 13, 10
	db	'/B    Unconditionally restore boot sector from backup', 13, 10
	db	'      Unless the disk is FAT32, it will not boot until SYS C: is ran', 13, 10
	db      '/I    Unconditionally regenerate FS Info sector', 13, 10
	db	'/U    Update backup sectors (after repairing if needed)', 13, 10, '$'
noslot	db	'No free slots found when restoring deleted partition', 13, 10, '$'
nobb	db	'Boot check partition not found', 13, 10, '$'
nobm	db	'Backup MBR is unusable', 13, 10, '$'
nobp	db	'Backup partition table is unusable', 13, 10, '$'
nobbs	db	'Backup boot sector is unusable', 13, 10, '$'
sysc	db	'Need to run SYS C: to finish restoring boot sector', 13, 10, '$'
error	db	'SSD Error   ', 13, 10, '$'

align 2, db '$'
eof:
flags		equ	eof
flags2		equ	eof + 1
disk		equ	eof + 2
heads		equ	disk + 2
sectors		equ	heads + 2
superblockoff	equ	sectors + 2
infooff		equ	superblockoff + 4
firstfree	equ	infooff + 4
free		equ	firstfree + 4
total		equ	free + 4
remaining	equ	total + 4
firstfatsec	equ	remaining + 4
fatsects	equ	firstfatsec + 4
dap		equ	fatsects + 4
buf1		equ	dap + 16
buf2		equ	buf1 + 1024
