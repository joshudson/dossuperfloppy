; Copyright (C) Joshua Hudson 2023
;
; Creative Commons Attribution-ShareAlike 4.0 International Public License
;
;	Divide 48 bit integer by 48 bit integer; arguments on stack, writes result to stack; DON'T divide by 0
;	clobbers everything but BP
;		push	dividendlow
;		push	dividendmid
;		push	dividendhigh
;		push	divisorlow
;		push	divisormid
;		push	divisorhigh
;		call div48
;		pop	quotenthigh
;		pop	quotentmid
;		pop	quotentlow
;		pop	remainderhigh
;		pop	remaindermid
;		pop	remainderlow
div48:
	push	bp
	mov	bp, sp
	xor	di, di		; current bit
	mov	dx, [bp + 4]	; divisor high
	mov	bx, [bp + 6]	; divisor mid
	mov	ax, [bp + 8]	; divisor low
	mov	[bp + 4], di	; output high
	mov	[bp + 6], di	; output mid
	mov	[bp + 8], di	; output low

	; Shift upwards until overflow and count how many times
.up	shl	ax, 1
	rcl	bx, 1
	rcl	dx, 1
	inc	di
	jnc	.up

.div	rcr	dx, 1
	rcr	bx, 1
	rcr	ax, 1
	cmp	dx, [bp + 10]
	ja	.no
	jb	.yes
	cmp	bx, [bp + 12]
	ja	.no
	jb	.yes
	cmp	ax, [bp + 14]
	ja	.no
.yes	sub	[bp + 10], dx
	sbb	[bp + 12], bx
	sbb	[bp + 14], ax
	mov	si, 1
	mov	cx, di
	dec	cx
	cmp	cl, 16
	jb	.stash1
	cmp	cl, 32
	jb	.stash2
.stash3	sub	cl, 32
	shl	si, cl
	or	[bp + 4], si
	jmp	.no
.stash2	sub	cl, 16
	shl	si, cl
	or	[bp + 6], si
	jmp	.no
.stash1	shl	si, cl
	or	[bp + 8], si
.no	dec	di		; Continue until we've divided by 48 bits
	clc
	jnz	.div
	pop	bp
	ret
