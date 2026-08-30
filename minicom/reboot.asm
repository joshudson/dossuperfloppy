BITS 16

CPU 8086

ORG 0100h

_start:
	; Flush disk cache
	mov	ah, 0Dh
	int	21h
	; Wait for flush disk to finish
	xor	ax, ax
	mov	ds, ax
.send	mov	ax, 4F53h	; Send DEL
	mov	[472h], byte 0Ch ; with modifiers Ctrl+Alt
	stc
	int	15h
	jc	.endwait
	mov	ah, 0
	int	1Ah
	mov	si, cx
	mov	di, dx
.wait	mov	ah, 0
	int	1Ah
	cmp	si, cx
	jne	.send
	cmp	di, dx
	jne	.send
	jmp	.wait
.endwait:
	
	; Check for 286
	push	sp
	pop	ax
	cmp	ax, sp
	jne	realmode
CPU 286
	smsw	ax
	test	ax, 1
	je	protmode

CPU 8086
realmode:
	cli
	jmp	0F000h:0FFF0h
CPU 286

protmode:
	; There's no way out of protected mode on 286; use keyboard reset
	; Using this on 386 as well unless I can prove it doesn't really work
	; Clear keyboard controller state
	jmp	short $+2
.r2	in	al, 64h
	test	al, 2
	jnz	protmode
	
	; Reset via keyboard
	cli
	mov	al, 0FEh
	out	64h, al
	; Wait for reset to finish
.hlt	hlt
	jmp	short .hlt
