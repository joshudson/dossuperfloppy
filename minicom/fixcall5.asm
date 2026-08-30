; FIXCALL5: correct CALL5 entry point on modern hardware

CPU 8086

ORG 0100h

_start:
	xor	bp, bp
	mov	si, 81h
.loop0	lodsb
	cmp	al, 9
	je	.loop0
	cmp	al, ' '
	je	.loop0
	cmp	al, '/'
	jne	.n
	lodsb
	cmp	al, 'A'
	jne	.n
	lodsb
	cmp	al, 'H'
	jne	.n
	inc	bp
.n	push	ds
	xor	ax, ax
	mov	ds, ax
	dec	ax
	mov	es, ax
	mov	si, 0C0h
	mov	di, 0D0h
	mov	cx, 5
	repe	cmpsb
	pop	ds
	jne	checkpatchcall5
	mov	dx, msg_na20
_exit:
	mov	ah, 9
	int	21h
	int	20h

checkpatchcall5:
	mov	dx, msg_already
	cmp	[es:0D0h], word 0C0EAh
	jne	needpatchcall5
	cmp	[es:0D2h], word 0
	jne	needpatchcall5
	cmp	[es:0D4h], byte 0
	je	_exit

needpatchcall5:
	mov	ax, 4300h
	int	2Fh
	cmp	al, 80h
	jne	patchcall5
	test	bp, bp
	jnz	.ah
	mov	dx, msg_ah
	jmp	_exit

.ah	mov	ax, 4310h
	int	2Fh
	mov	[76h], bx
	mov	[78h], es
	mov	dx, 0FFFFh
	mov	ah, 1
	call	far [76h]
	cmp	ax, 1
	je	patchcall5
	cmp	bl, 91h
	je	.ahu
	cmp	bl, 90h
	je	.nohma
	mov	dx, msg_chma
	jmp	_exit

.ahu	mov	dx, msg_hmaused
	jmp	_exit
	
.nohma	mov	dx, msg_nohma
	jmp	_exit

patchcall5:
	cli
	xor	ax, ax
	mov	es, ax
	; It says we cannot point interrupt vectors into the HMA
	; but we can, because A20 line doesn't exist anymore.
	mov	bx, 18h
	mov	dx, 0FFFFh
	xchg	[es:2Fh * 4], bx
	xchg	[es:2Fh * 4 + 2], dx
	test	dx, dx
	jnz	.pok
	test	bx, bx
	jnz	.pok
	; interrupt not previously installed, make sure the far jmp goes to an iret
	mov	dx, 0FFFFh
	mov	bx, 14h + icode_iret - icode
.pok	dec	ax
	mov	es, ax
	mov	[es:10h], bx
	mov	[es:12h], dx
	mov	si, icode
	mov	di, 14h
	mov	cx, (icode_end - icode + 1) / 2
	rep	movsw
	sti
	mov	[es:0D0h], word 0C0EAh
	mov	[es:0D2h], word 0
	mov	[es:0D4h], word 0CC00h
	mov	dx, msg_installed
	mov	cl, 9
	call	near 5
	jmp	near 0

	; Suballocation driver: the rest of HMA is free
	align	2, db 0CCh
icode:
	dw	0D6h
	dw	10000h - 0D6h
	cmp	ax, 4A02h
	je	.two
	cmp	ax, 4A01h
	je	.one
	jmp	far [cs:10h]
.one	mov	bx, [cs:16h]
	iret
.two	cmp	[cs:16h], bx
	jb	.no
	mov	di, 0FFFFh
	mov	es, di
	mov	di, [cs:14h]
	add	[cs:14h], bx
	sub	[cs:16h], bx
	iret
.no	mov	di, 0FFFFh
	mov	es, di
icode_iret:
	iret
	align	2, db 0CCh
icode_end:

msg_na20	db	"A20 gate disabled: patch not needed", 13, 10, '$'
msg_ah		db	"XMS driver detected, pass /AH to allocate HMA", 13, 10, '$'
msg_hmaused	db	"HMA in use", 13, 10, '$'
msg_chma	db	"Can't allocate HMA", 13, 10, '$'
msg_nohma	db	"HMA does not exist", 13, 10, '$'
msg_already	db	"CALL 5 patch already installed", 13, 10, '$'
msg_installed	db	"CALL 5 patch installed", 13, 10, '$'
