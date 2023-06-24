BITS 16
CPU 8086
ORG	0x100

_start:
	mov	di, 0x80
	;The program name isn't here
	mov	si, 0x81
	mov	dl, 0
	mov	cl, 10
.loop	lodsb
	cmp	al, 0x0D
	jz	main
	cmp	al, '0'
	jb	.nd
	cmp	al, '9'
	ja	.nd
	xchg	ax, dx
	mul	cl
	sub	dl, '0'
	add	al, dl
	xchg	ax, dx
	jmp	.loop
.nd	cmp	al, 'F'
	je	.f
	cmp	al, 'f'
	je	.f
	cmp	al, '?'
	je	.usage
	jmp	.loop
.f	xor	di, di
	jmp	.loop
.usage	mov	dx, usage
	mov	ah, 9
	int	0x21
	mov	ax, 0x4C03
	int	0x21
main:	add	dx, di
	xor	cx, cx
	mov	dh, 0
	push	es
	xor	di, di
	mov	es, di
	mov	ah, 0x8
	int	0x13
	pop	es
	jc	.error
	push	cx
	push	dx
	and	cx, 63
	mov	dx, ms
	call	line
	pop	dx
	mov	cl, dh
	mov	ch, 0
	inc	cx
	mov	dx, mh
	call	line
	pop	cx
	xchg	cl, ch
	shr	ch, 1
	shr	ch, 1
	shr	ch, 1
	shr	ch, 1
	shr	ch, 1
	shr	ch, 1
	mov	dx, mc
	inc	cx
	call	line
	mov	ax, 0x4C00
	int	0x21
.error	mov	dx, me
	mov	ah, 9
	int	0x21
	mov	ax, 0x4C01
	int	0x21
line	mov	ah, 0x9
	int	0x21
	mov	ax, 10
	mov	di, eol
	xchg	ax, cx
.loop	xor	dx, dx
	div	cx
	add	dl, '0'
	dec	di
	mov	[di], dl
	or	ax, ax
	jnz	.loop
	mov	dx, di
	mov	ah, 0x9
	int	0x21
	ret

usage	db	'Usage: GEOMETRY [/F] [disknumber]', 13, 10, '$'
ms	db	'Sectors: $'
mh	db	'Heads: $'
mc	db	'Cylinders: $'
me	db	'Disk error'
eol	db	13, 10, '$'
