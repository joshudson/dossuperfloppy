BITS 16

CPU 8086

ORG 100h

_start	mov	dx, stconv
	mov	ah, 9
	int	21h
	int	12h
	xor	dx, dx
	call	atoix

	xor	cx, cx
	xor	dx, dx
	mov	ax, 0E801h
	int	15h
	jc	.ne801
	cmp	ah, 86h
	je	.ne801
	cmp	ah, 80h
	je	.ne801
	or	cx, cx
	jnz	.uax
	mov	ax, cx
	mov	bx, dx
.uax	push	bx
	push	ax
	mov	ah, 9
	mov	dx, h1conv
	int	21h
	pop	ax
	xor	dx, dx
	call	atoix
	pop	bx
	mov	ah, 9
	mov	dx, h2conv
	int	21h
	xchg	ax, bx
	mov	bx, 64
	mul	bx
	call	atoix
	jmp	.nhigh

.ne801	mov	ax, 0DA88h
	int	15h
	jc	.nda88
	or	cl, cl
	jz	.nda88
	or	dx, dx
	jz	.nda88
	push	dx
	mov	dx, h1conv
	mov	ah, 9
	int	21h
	pop	dx
	mov	ch, 0
	xchg	ax, dx
	mov	dx, cx
	call	atoix
	jmp	.nhigh

	; int 15h, ah = 8ah
.nda88	mov	ah, 8Ah
	int	15h
	jc	.n8a
	or	dx, dx
	jnz	.ram8a
	or	ax, ax
	jz	.n8a
.ram8a	push	dx
	push	ax
	mov	dx, h1conv
	mov	ah, 9
	int	21h
	pop	ax
	pop	dx
	call	atoix
	jmp	.nhigh
	
	; Last resort: int 15h, ah=88h
.n8a	clc
	mov	ah, 88h
	int	15h
	jc	.nhigh
	push	ax
	mov	dx, h1conv
	mov	ah, 9
	int	21h
	pop	ax
	xor	dx, dx
	call	atoix
.nhigh:	; Try to generate memory map if possible
	pushf
	xor	ax, ax
	push	ax
	popf
	pushf
	pop	ax
	popf
	and	ah, 0F0h
	cmp	ah, 0F0h
	jne	mmap
	ret
mmap:	; We have a 386+ therefore we can call the memory map routine
CPU	386
	mov	edi, mmapbuf
	xor	ebx, ebx
	mov	edx, 0534D4150h
	mov	eax, 0E820h
	mov	ecx, 20
	int	15h
	jc	.nope		; DOSBOX says nope
	mov	edx, 0534D4150h
	cmp	eax, edx
	jne	.nope
	jmp	.mid
.loop	mov	eax, 0E820h
	mov	edx, 0534D4150h
	mov	[di + 20], dword 1
	mov	ecx, 20
	int	15h
	jc	.nope
.mid	cmp	cl, 20
	jb	.skip
	push	ebx
	push	edi
	mov	esi, edi
	mov	edi, mmentry + 6
	mov	eax, [si + 4]
	call	unhex32
	inc	di
	mov	eax, [si]
	call	unhex32
	inc	di
	mov	eax, [si + 12]
	call	unhex32
	inc	di
	mov	eax, [si + 8]
	call	unhex32
	inc	di
	mov	ax, [si + 16]
	mov	si, type0
	cmp	ax, 2
	ja	.tov
	mov	bx, ax
	shl	bx, 1
	add	ax, bx
	add	si, ax
.tov	lodsw
	stosw
	lodsb
	stosb
	mov	dx, mmentry
	mov	ah, 9
	int	21h
	pop	edi
	pop	ebx
.skip	or	ebx, ebx
	jnz	.loop
.nope	ret

unhex32	push	cx
	mov	bx, hexdig
	mov	cx, 4
.next	rol	eax, 8
	push	eax
	push	ax
	shr	al, 4
	call	.nybble
	pop	ax
	call	.nybble
	pop	eax
	loop	.next
	pop	cx
	ret
.nybble	and	al, 15
	xlatb
	stosb
	ret

CPU	8086

atoix	std
	mov	di, xconv + 9
	mov	cx, 10
	mov	bx, 10
.div	xchg	ax, si
	xchg	ax, dx
	xor	dx, dx
	div	bx
	xchg	ax, si
	div	bx
	xchg	ax, dx
	add	al, '0'
	stosb
	xor	ax, ax
	xchg	ax, dx
	mov	dx, si
	loop	.div
	cld
	inc	di
	mov	dx, di
	mov	si, di
	mov	cx, xconv + 9
	sub	cx, si
.scan	lodsb
	cmp	al, '0'
	jne	.put
	mov	al, ' '
	stosb
	loop	.scan
.put	mov	ah, 9
	int	21h
	ret

stconv	db	'Conventional Memory: $'
h1conv	db	'High Memory        : $'
h2conv	db	'Higher Memory      : $'
xconv	db	'          K', 13, 10, '$'
mmentry	db	'MMAP: ________-________ ________-________ ___', 13, 10, '$'
type0	db	'???'
type1	db	'ARM'
type2	db	'ARR'
hexdig	db	'0123456789ABCDEF'

mmapbuf:
