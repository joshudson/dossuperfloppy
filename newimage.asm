BITS 16

ORG	0x100

_start:
	clc
	mov	di, zeroes
	mov	cx, 16128
	xor	ax, ax
	rep	stosw
	mov	si, 0x81
.loop1	lodsb
	cmp	al, ' '
	je	.loop1
	cmp	al, 0x0D
	je	.usage
	mov	dx, si
	dec	dx
	mov	[_start], dx
.loop2	lodsb
	cmp	al, 0x0D
	je	.usage
	cmp	al, ' '
	jne	.loop2
	mov	[si - 1], byte 0
	mov	cx, 10
.loop3	lodsb
	cmp	al, ' '
	je	.loop3
.loop4	cmp	al, '0'
	jb	.usage
	cmp	al, '9'
	ja	.usage
	mov	ah, 0
	sub	al, '0'
	mov	bx, ax
	call	.mx2b
	lodsb
	cmp	al, ' '
	je	.usage
	cmp	al, 0x0D
	je	main
	cmp	al, 'K'
	je	.k
	cmp	al, 'k'
	je	.k
	cmp	al, 'M'
	je	.m
	cmp	al, 'm'
	je	.m
	cmp	al, 'G'
	je	.g
	cmp	al, 'g'
	je	.g
	jmp	.loop4

.overflow:
	mov	dx, overflow
	jmp	.ue
.usage:
	mov	dx, usage
.ue	mov	ah, 9
	int	0x21
	mov	ax, 0x4C03
	int	0x21

.k	mov	cx, 2
	jmp	.mx
.m	mov	cx, 2048
	jmp	.mx
.g	mov	cx, 2048
	call	.mx2
	mov	cx, 1024
	jmp	.mx
.mx2	xor	bx, bx
.mx2b	mov	ax, [space]
	mul	cx
	add	ax, bx
	adc	dx, 0
	mov	bx, dx
	mov	[space], ax
	mov	ax, [space + 2]
	mul	cx
	add	ax, bx
	adc	dx, 0
	mov	[space + 2], ax
	or	dx, dx
	jnz	.overflow
	ret
.mx	call	.mx2
main:	;space = the size of the image in 512 byte sectors; I need to divide by 1008 for cylinders
	mov	cx, 1008
	mov	dx, [space + 2]
	cmp	dx, cx
	jae	_start.overflow
	mov	ax, [space]
	div	cx
	or	dx, dx
	jz	.noinc
	inc	ax
	jz	_start.overflow
.noinc	mov	bp, ax	; BP = number of cylinders in image
	mov	di, number + 5
	mov	cx, 10
.itoa	xor	dx, dx
	div	cx
	add	dl, '0'
	dec	di
	mov	[di], dl
	or	ax, ax
	jnz	.itoa

mainloop:
	mov	dx, [_start]
	mov	cx, 0
	mov	ah, 0x3C
	int	0x21
	jc	.doserror
	or	bp, bp
	jz	.nodata
	mov	dx, zeroes
	mov	bx, ax
	mov	cx, 32256
.loop1	mov	si, 16
.loop2	mov	ah, 0x40
	int	0x21
	jc	.cdoserror
	dec	si
	jnz	.loop2
	push	bx
	push	cx
	push	dx
	mov	dx, dot
	mov	cx, 1
	mov	bx, 2
	mov	ah, 0x40
	int	0x21
	pop	dx
	pop	cx
	pop	bx
	dec	bp
	jnz	.loop1
	mov	ah, 0x3E
	int	0x21
	jc	.doserror	; Our most graciuos host has different rules
	mov	dx, eol
	mov	cx, 2
	mov	bx, 2
	mov	ah, 0x40
	int	0x21
.nodata:

	mov	ah, 0x9
	mov	dx, newimg1
	int	0x21
	call	outname
	mov	ah, 0x9
	mov	dx, newimg2
	int	0x21
	mov	ah, 0x9
	mov	dx, di
	int	0x21
	mov	ah, 0x9
	mov	dx, newimg3
	int	0x21
	call	outname
	mov	ah, 0x9
	mov	dx, newimg4
	int	0x21
	mov	ax, 0x4C00
	int	0x21
.cdoserror:
	push	ax
	mov	ah, 0x3E
	int	0x21
	pop	ax
.doserror:
	mov	bx, ax
	shl	bx, 1
	mov	dx, [bx + errorcodes]
	mov	ah, 0x9
	int	0x21
	mov	ax, 0x4C01
	int	0x21

outname:
	mov	dx, [_start]
	mov	si, dx
	xor	cx, cx
.loop	inc	cx
	lodsb
	or	al, al
	jnz	.loop
	dec	cx
	mov	bx, cx
	mov	si, dx
	mov	[bx + si], byte '$'
	mov	ah, 0x9
	int	0x21
	ret

align	2, db 0xCC
%include 'errormsg.asm'
align	2, db 0

space	dd	0
overflow	db	'Arithmatic overflow computing image geometry, 13, 10, $'
usage		db	'Usage: dosbox NEWIMAGE'
dot		db	'.'
		db	'COM C.IMG size[K/M/G]'
eol		db	13, 10, '$'
newimg1	db	'imgmount 2 $'
newimg2	db	' -t hdd -size 512,63,16,$'
newimg3	db	' -fs none', 13, 10, 'boot $'
newimg4	db	' -l C', 13, 10, '$'
number	db	'     $'
align	4, db 0
zeroes:
