BITS 16

CPU 8086

ORG 0100h

buf equ 400h

_start:
	mov	si, 81h
	mov	cl, [si - 1]
	mov	ch, 0
	or	cx, cx
	jz	.usg
	xor	di, di
	mov	bl, 10
	mov	ah, 0
	mov	dl, 0
.loop	lodsb
	cmp	al, '0'
	jb	.ndig
	cmp	al, '9'
	ja	.ndig
	sub	al, '0'
	xchg	ax, di
	mul	bl
	add	ax, di
	xchg	ax, di
	or	dl, 1
	jmp	.eloop
.q	or	dl, 2
	jmp	.eloop
.usg	mov	dx, usage
	mov	ah, 9
	int	21h
	mov	ax, 4C01h
	int	21h
.ndig	cmp	al, 'q'
	je	.q
	cmp	al, 'Q'
	je	.q
	cmp	al, '/'
	je	.eloop
	cmp	al, ' '
	je	.eloop
	cmp	al, 9
	jne	.usg
.eloop	loop	.loop
	test	dl, 1
	jz	.usg
	xchg	dx, di
	mov	bx, buf
	mov	cx, 1
	mov	dh, 0
	or	dl, 80h
	mov	ax, 0201h
	int	13h
	jnc	readok
	test	di, 2
	jnz	.x4
	mov	dx, error
	mov	ah, 9
	int	21h
.x4	mov	ax, 4C04h
	int	21h
readok	mov	si, buf + 1BEh
	mov	cx, 4
.scan	mov	al, [si + 4]
	cmp	al, byte 01h
	je	.chk
	cmp	al, byte 04h
	je	.chk
	cmp	al, byte 06h
	je	.chk
	cmp	al, byte 0Bh
	je	.chk
	cmp	al, byte 0Ch
	je	.chk
	cmp	al, byte 0Eh
	je	.chk
.snxt	add	si, 16
	loop	.scan
	test	di, 4
	jz	.non
	cmp	[buf + 0Bh], byte 0
	jne	.std
	cmp	[buf + 15h], byte 0F8h
	jne	.std
	cmp	[buf + 0Eh], word 0
	je	.std
	test	[buf + 11h], byte 15
	jnz	.std
	test	di, 2
	jnz	.x1
	mov	dx, shadow
	mov	ah, 9
	int	21h
.x1	mov	ax, 4C01h
	int	21h
.std	test	di, 2
	jnz	.x2
	mov	dx, standard
	mov	ah, 9
	int	21h
.x2	mov	ax, 4C02h
	int	21h
.non	test	di, 2
	jnz	.x3
	mov	dx, nonfat
	mov	ah, 9
	int	21h
.x3	mov	ax, 4C03h
	int	21h
.chk	or	di, 4
	cmp	[si + 1], byte 0
	jne	.snxt
	cmp	[si + 2], word 1
	jne	.snxt
	test	di, 2
	jnz	.x0
	mov	dx, superfloppy
	mov	ah, 9
	int	21h
.x0	mov	ax, 4C00h
	int	21h

usage		db	'Usage: ISSF # [/Q]', 13, 10
		db	'Error level 0=Superfloppy, 1=shadow superfloppy, 2=standard, 3=nonfat, 4=error', 13, 10, '$'
superfloppy	db	'Superfloppy formatted disk', 13, 10, '$'
shadow		db	'Shadow superfloppy formatted disk', 13, 10, '$'
standard	db	'Standard formatted disk', 13, 10, '$'
nonfat		db	'No primary FAT filesystem', 13, 10, '$'
error		db	'Error reading disk', 13, 10, '$'
