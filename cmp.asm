ORG	0x100

file1	equ	0x0102
file2	equ	0x0104
block1	equ	0x0400
block2	equ	0x0600

_start:
	mov	bh, 0
	mov	si, 0x80
	mov	bl, [si]
	inc	si
	mov	[si + bx], bh
	mov	[si + bx + 1], bh
	call	getarg
	mov	[file1], bx
	call	getarg
	mov	[file2], bx
	mov	dx, [file1]
	call	openfile
	mov	[file1], ax
	mov	dx, [file2]
	call	openfile
	mov	[file2], ax
.loop	mov	bx, [file1]
	mov	dx, block1
	call	readfile
	mov	di, ax
	mov	bx, [file2]
	mov	dx, block2
	call	readfile
	cmp	ax, di
	jne	.diff
	or	ax, ax
	jz	.same
	mov	si, block1
	mov	di, block2
	mov	cx, ax
	repe	cmpsb
	je	.loop
.diff	mov	ax, 0x4C01
	int	0x21
.same	mov	ax, 0x4C00
	int	0x21

openfile:
	mov	ax, 0x3D00
	int	0x21
	jc	error
	ret

readfile:
	push	dx
	mov	cx, 512
.read	mov	ah, 0x3F
	int	0x21
	jc	error
	or	ax, ax
	jz	.eof
	add	dx, ax
	sub	cx, ax
	jnz	.read
.eof	mov	ax, dx
	pop	dx
	sub	ax, dx
	ret

error:
	mov	bx, ax
	shl	bx, 1
	mov	dx, [errorcodes + bx]
	jmp	short	msgexit

getarg:
	mov	bx, si
	lodsb
	or	al, al
	jz	.nope
	cmp	al, ' '
	je	getarg
.scan	lodsb
	or	al, al
	jz	.yup
	cmp	al, ' '
	jne	.scan
.yup:	;mov	dx, bx
	;mov	al, '$'
	;mov	[si - 1], al
	;mov	ah, 0x09
	;int	0x21

	mov	al, 0
	mov	[si - 1], al
	ret
.nope	mov	dx, usagemsg
msgexit:
	mov	ah, 0x9
	int	0x21
	mov	ax, 0x4C03
	int	0x21

align 2

%include 'errormsg.asm'

usagemsg	db	'Usage: cmp file1 file2', 13, 10, 'Returns 0 if files are equal', 13, 10, '$'

