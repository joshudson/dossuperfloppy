CPU 8086

BITS 16

ORG 7C00h

_start:
	xor	ax, ax
	mov	di, di		; Patch point
	cld
	cli
	mov	ss, ax
	mov	sp, 7B00h
	sti
	mov	ds, ax
	mov	es, ax
	mov	di, ax
	int	13h
	jc	.no
	mov	al, cl
	and	al, 63
	mov	ah, 0
	call	numberout
	mov	si, s_sects
	call	stringout
	mov	al, dl
	mov	ah, 0
	inc	ax
	call	numberout
	mov	si, s_heads
	call	stringout
	mov	ax, cx
	rol	al, 1
	rol	al, 1
	and	al, 0C0h
	xchg	ah, al
	call	numberout
	mov	si, s_cyls
	call	stringout
	jmp	.hlt

.no	mov	si, no
	call	stringout
.hlt	hlt
	jmp	.hlt

numberout:
	push	cx
	push	dx
	mov	cx, 10
	mov	si, 7BFFh
	mov	[si], byte 0
.loop	xor	dx, dx
	div	cx
	add	dl, '0'
	dec	si
	mov	[si], dl
	or	ax, ax
	jnz	.loop
	pop	dx
	pop	cx

stringout:
	mov	bx, 7
	lodsb
.loop	mov	ah, 0Eh
	int	10h
	lodsb
	or	al, al
	jnz	.loop
.out	ret

s_heads	db	' heads', 13, 10, 0
s_sects	db	' sectors', 13, 10, 0
s_cyls	db	' cylinders', 13, 10, 0
no	db	'Error!', 13, 10, 0
