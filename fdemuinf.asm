CPU 8086
ORG 100h

_start:
	cmp	sp, _end + 15 + 1536
	jae	.ram
	mov	dx, ram
	mov	bl, 3
	jmp	outstr
.ram	mov	bx, (_end + 15 - _start + 100h) / 16
	mov	ax, ds
	add	bx, ax
	lea	cx, [bx + 20h]
	mov	ax, bx
	and	ax, 0F000h
	mov	dx, cx
	and	dx, 0F000h
	cmp	ax, dx
	jne	.keep
	cmp	bp, sp
	mov	bx, cx	; Buffer address
.keep	mov	si, 81h
.next	lodsb
	cmp	al, ' '
	je	.next
	cmp	al, 9
	je	.next
	dec	si
	mov	di, rw1
	call	strxcmp
	je	readwrite
	mov	di, rw2
	call	strxcmp
	je	readwrite
	mov	di, ro1
	call	strxcmp
	je	readonly
	mov	di, ro2
	call	strxcmp
	je	readonly
	jmp	ask
readonly:
	mov	al, 0FFh
	jmp	rorw
readwrite:
	mov	al, 7Fh
rorw:
	xor	dx, dx
	mov	si, dap
	mov	[si], word 12h
	mov	[si + 2], dx
	mov	[si + 4], dx
	mov	[si + 6], bx
	mov	[si + 10h], word 0AABBh
	mov	ah, 43h
	int	13h
	jc	.error
	ret
.error	mov	dx, error
	cmp	ah, 1
	jne	.error2
.noemu	mov	dx, noemu
.error2	jmp	outstr

ask	mov	dx, copy
	mov	ah, 9
	int	21h
	mov	si, dap
	xor	dx, dx
	mov	[si], word 12h
	mov	[si + 2], dx
	mov	[si + 10h], word 0AABBh
	mov	ax, 4200h
	int	13h
	jc	rorw.noemu
	cmp	[si + 10h], word 0ABABh
	jc	rorw.noemu
	mov	dh, al
	mov	[86h], dx
	mov	cx, [si + 2]
	mov	ax, [si + 8]
	mov	dx, [si + 10]
	mov	[80h], cx
	mov	[82h], ax
	mov	[84h], dx
	mov	[si], word 1Eh
	mov	ah, 48h
	mov	dl, 0
	int	13h	; Can't fail now
	mov	es, bx
	xor	di, di
	mov	si, emul1
	mov	cx, emut1 - emul1
	rep	movsb
	mov	al, [87h]
	and	al, 7Fh
	cmp	al, 1
	je	.ask1
	cmp	al, 2
	je	.ask2
	cmp	al, 3
	je	.ask3
	cmp	al, 4
	je	.ask4
	cmp	al, 5
	je	.ask5
	mov	si, emutq
	mov	cx, 7
	jmp	.askx
.ask1	mov	si, emut1
	mov	cx, emut2 - emut1
	jmp	.askx
.ask2	mov	si, emut2
	mov	cx, emut3 - emut2
	jmp	.askx
.ask3	mov	si, emut3
	mov	cx, emut4 - emut3
	jmp	.askx
.ask4	mov	si, emut4
	mov	cx, emut5 - emut4
	jmp	.askx
.ask5	mov	si, emut5
	mov	cx, emutq - emut5
.askx	rep	movsb
	mov	si, emul2
	mov	cx, emul3 - emul2
	rep	movsb
	mov	al, [86h]
	mov	ah, 0
	xor	dx, dx
	call	number
	mov	si, emul3
	mov	cx, emul4 - emul3
	rep	movsb
	mov	ax, [82h]
	mov	dx, [84h]
	call	number
	mov	si, emul4
	mov	cx, emuf2 - emul4
	rep	movsb
	xor	dx, dx
	mov	ax, [dap + 24]
	call	number
	mov	si, emuf2
	mov	cx, emuf3 - emuf2
	rep	movsb
	mov	ax, [dap + 12]
	mov	dx, [dap + 14]
	call	number
	mov	si, emuf3
	mov	cx, emuf4 - emuf3
	rep	movsb
	mov	ax, [dap + 8]
	mov	dx, [dap + 10]
	test	dx, dx
	jnz	.nsides
	cmp	ax, 1
	je	.side1
	cmp	ax, 2
	je	.side2
.nsides	call	number
	mov	si, siden
	mov	cx, ero - siden
	jmp	.sidep
.side1	mov	si, side1
	mov	cx, side2 - side1
	jmp	.sidep
.side2	mov	si, side2
	mov	cx, siden - side2
.sidep	rep	movsb
	mov	ax, [dap + 4]
	mov	dx, [dap + 6]
	call	 number
	mov	si, emuf4
	mov	cx, emuf5 - emuf4
	rep	movsb
	mov	ax, [dap + 16]
	mov	dx, [dap + 18]
	push	ax
	push	dx
	call	number
	pop	dx
	pop	ax
	mov	si, emuf5
	mov	cx, emuf6 - emuf5
	rep	movsb
	shr	dx, 1
	rcr	ax, 1
	call	number
	mov	si, emuf6
	mov	cx, 4
	rep	movsb
	test	[87h], byte 80h
	jz	.nro
	mov	si, ero
	mov	cx, ero_end - ero
	rep	movsb
.nro	mov	al, '$'
	stosb
	mov	bl, 0
	push	es
	pop	ds
	xor	dx, dx
outstr	mov	ah, 9
	int	21h
	mov	ax, 4Ch
	mov	al, bl
	int	21h
	ret		; DOS1
number:	xor	cx, cx
	mov	bx, 10	; DX:AX = before
.loop1	xchg	ax, si	; DX:SI = before
	xchg	ax, dx	; AX:SI = before
	xor	dx, dx
	div	bx
	xchg	ax, si	; SI:?? = after
	div	bx	; SI:AX = after, DX = remainder
	add	dx, '0'
	push	dx
	inc	cx
	mov	dx, si
	test	ax, ax
	jnz	.loop1
	test	dx, dx
	jnz	.loop1
.loop2	pop	ax
	stosb
	loop	.loop2
	ret
strxcmp:
	push	si
.loop	lodsb
	and	al, 0DFh
	cmp	al, [di]
	jne	.ret
	inc	di
	cmp	al, 13
	jne	.loop
.ret	pop	si
	ret

copy	db	"SSD Floppy Emulator Query - Copyright (C) Joshua Hudson 2026", 13, 10, '$'
ram	db	"Insufficient RAM", 13, 10, '$'
noemu	db	"Not SSD Emulated floppy", 13, 10, '$'
error	db	"SSD Error", 13, 10, '$'
ro1	db	15, "READONLY", 13
rw1	db	15, "READWRITE", 13
ro2	db	15, "RO", 13
rw2	db	15, "RW", 13
emul1	db	"Emulating a "
emut1	db	"5", 0ACh, " double density"
emut2	db	"5", 0ACh, " high density"
emut3	db	"3", 0ABh, " double density"
emut4	db	"3", 0ABh, " high density"
emut5	db	"3", 0ABh, " extra density"
emutq	db	"strange"
emul2	db	" drive on host drive "
emul3	db	13, 10, "at offset "
emul4	db	" sectors.", 13, 10
emuf1	db	"Disk has "
emuf2	db	" bytes per sector, "
emuf3	db	" sectors, "
emuf4	db	" tracks,", 13, 10, "for a total of "
emuf5	db	" sectors, or "
emuf6	db	"KB", 13, 10
side1	db	"one side, ",
side2	db	"two sides, ",
siden	db	" sides, ",
ero	db	"Disk is read only.", 13, 10
ero_end:


	align 2, db 0
dap:
_end	equ	dap + 20h
