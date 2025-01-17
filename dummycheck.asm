; Quick check of INT25h on hard disk images for breakage
; Diagnostic routine for debugging SSDSCAN; has no other real purpose

BITS 16
ORG	0100h

_start:
	mov	al, 2
	mov	dx, 0
	mov	cx, 1
	mov	bx, 512
	int	25h
	jc	.error
	mov	ax, 4C00h
	int	21h
.error	mov	cx, 0404h
.loop	rol	ax, cl
	push	ax
	and	al, 15
	add	al, '0'
	cmp	al, '9'
	jbe	.nadj
	add	al, 7
.nadj	mov	dl, al
	mov	ah, 2
	int	21h
	pop	ax
	dec	ch
	jnz	.loop
	mov	dl, 13
	mov	ah, 2
	int	21h
	mov	dl, 10
	mov	ah, 2
	int	21h
	mov	ax, 4C01h
	int	21h
