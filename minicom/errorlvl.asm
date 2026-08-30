;Reports errorlevel of last command
;Only really useful in DOSBOX
BITS 16

ORG 0x100

	mov	ah, 0x4D
	int	0x21
	mov	si, m0
	xor	dx, dx
	mov	cx, 10
.loop	div	cx
	add	dl, '0'
	dec	si
	mov	[si], dl
	or	ax, ax
	jnz	.loop
	mov	dx, si
	mov	ah, 0x09
	int	0x21
	int	0x20
pad	db 0, 0, 0, 0, 0	
m0	db 13, 10, '$'
