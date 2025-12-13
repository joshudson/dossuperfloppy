CPU 386

BITS 32

%include "syscalls32.inc"

ORG 1000h
	db	127, "ELF", 1, 1, 1, 3
	times	8 db	 0
	dw	2	; ET_EXEC
	dw	3	; 80386
	dd	1
	dd	_start
	dd	_peoff - 1000h
	dd	0
	dd	0
	dw	0
	dw	20h
_peoff:			; Overlap trick: works because there's 1 phent and it loads
	dd	1
	dd	0
	dd	1000h
	dd	0
	dd	_end - 1000h
	dd	_end - 1000h
	dd	5
	dd	4096
_start:
	xor	ebp, ebp
	pop	eax
	cmp	eax, 2
	jb	.go
	mov	ebx, [esp + 4]
	cmp	[ebx], word "-?"
	jne	.go
	inc	ebp
.go	sub	esp, 512 + 8
outerloop:
	xor	ebx, ebx
	mov	ecx, esp
	mov	edx, 512
.rloop	mov	eax, __NR_read
	int	80h
	cmp	eax, 0
	jl	error
	jz	.eof
	add	ecx, eax
	sub	edx, eax
	jnz	.rloop
	mov	esi, esp
	call	sparcify_check
	jc	.llseek
	mov	edx, esp
	call	write1
	and	ebp, ~2
	jmp	outerloop
.llseek	lea	esi, [esp + 512]
	call	llseek
	jmp	outerloop
.eof	test	ebp, 2
	jnz	.trunc2
	mov	esi, esp
	cmp	esi, ecx
	je	.exit
	call	sparcify_check
	jc	.trunc
	mov	edx, esp
	call	write1
	jmp	.exit
.trunc	lea	esi, [esp  + 512]
	call	llseek
.trunc2	mov	ecx, [esp + 512]
	mov	edx, [esp + 516]
	mov	ebx, 1
	mov	eax, __NR_ftruncate64
	int	80h
	cmp	eax, 0
	jl	error
.exit	xor	ebx, ebx
	mov	eax, __NR_exit
	int	80h

sparcify_check:
.loop	lodsb
	cmp	al, 0
	je	.z
	test	ebp, 1
	jz	.n
	cmp	al, '?'
	jne	.n
.z	cmp	esi, ecx
	jne	.loop
	stc
	ret
.n	clc
	ret

llseek	mov	edi, 1
	neg	edx
	add	edx, 512
	xor	ecx, ecx
	mov	ebx, 1
	mov	eax, __NR_llseek
	int	80h
	cmp	eax, 0
	jl	error
	or	ebp, 2
	ret

write1	xor	ebx, ebx
	inc	ebx
	xchg	ecx, edx
	sub	edx, ecx
.loop	mov	eax, __NR_write
	int	80h
	cmp	eax, 0
	jle	error
	add	ecx, eax
	sub	edx, eax
	jnz	.loop
	ret
error:	cmp	ebx, 2
	je	.exit
	mov	ebx, 2
	mov	ecx, .msg
	mov	edx, 6
	call	write1.loop
.exit	mov	ebx, 1
	mov	eax, __NR_exit
	int	80h
.msg	db	"Error", 10
_end:
