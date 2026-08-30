CPU 8086

ORG 0100h

_start:
	mov	ax, 0FFFFh
	mov	cl, 80h
	shr	ax, cl
	jnz	.c186
	mov	si, s_8086
	jmp	hcpu
CPU 186
.c186	push	sp
	pop	ax
	cmp	ax, sp
	je	.c286
	mov	si, s_80186
	jmp	hcpu
CPU 286
.c286	sub	sp, 6
	mov	bp, sp
	sgdt	[bp]
	mov	al, [bp + 5]
	add	sp, 6
	cmp	al, 0FFh
	jne	.c386
	mov	si, s_80286
	jmp	hcpu
CPU 386
.c38616	mov	si, s_80386_16
	jmp	hcpu
.c386	xor	ebx, ebx
	mov	bl, 81h
	mov	eax, 0417A000h
	mul	ebx
	cmp	edx, 2
	jne	.c38616
	cmp	eax, 0FE7A000h
	jne	.c38616

	pushfd
	pop	eax
	mov	ecx, eax
	xor	eax, 240000h
	push	eax
	popfd
	pushfd
	pop	eax
	push	ecx
	popfd
	mov	si, s_80386
	cmp	eax, ecx
	je	hcpu

CPU 486
	and	eax, 200000h
	and	ecx, 200000h
	cmp	eax, ecx
	jne	.ccpuid
.c486	mov	si, s_80486
	jmp	hcpu

.ccpuid	xor	eax, eax
CPU 586		; NASM is wrong this can run on some 486 CPUs
	cpuid
CPU 486
	test	eax, eax
	jz	.c486
	mov	eax, 1
CPU 586
	cpuid
CPU 486
	mov	di, ax
	shr	di, 8
	shl	di, 1
	and	di, 15
	mov	si, [di + cpubase]
	; Could do other stuff with eax, ebx, ecx, edx but nobody cares
CPU 8086
hcpu	sub	sp, 2
	mov	bp, sp
	mov	[bp], word 0
	fninit
	fnstcw	[bp]
	mov	di, nfpu
	nop
	pop	ax
	cmp	ah, 3
	jne	.nfpu
	mov	di, fpu
	fld	dword [dnum]
	fdiv	dword [ddom]
	fmul	dword [ddom]
	fcomp	dword [dchk]
	sub	sp, 2
	fstsw	[bp]
	pop	ax
	sahf
	jnz	.nfpu
	mov	di, fpudiv
.nfpu	mov	dx, si
	mov	ah, 9
	int	21h
	mov	dx, di
	mov	ah, 9
	int	21h
	ret
	align 4, ret

dnum	dd	4195835.0
ddom	dd	3145727.0
dchk	dd	256.0
cpubase	dw	s_zero, s_8086, s_80286, s_80386,
	dw	s_80486, s_80586, s_80686, s_80686	; After Pentium II, we got random large values sometimes
	dw	s_80686, s_80686, s_80686, s_80686
	dw	s_80686, s_80686, s_80686, s_80686
s_80386_16	db	"386 CPU, for 16 bit code only"
s_zero	db	"Weird 486 CPU$"
s_8086	db	"8086 CPU$"
s_80186	db	"186 CPU$"
s_80286	db	"286 CPU$"
s_80386	db	"386 CPU$"
s_80486	db	"486 CPU$"
s_80586	db	"Pentium CPU$"
s_80686	db	"Pentium II+ CPU$"
fpudiv	db	", FPU present with FDIV bug", 13, 10, '$'
fpu	db	", FPU present", 13, 10, '$'
nfpu	db	", FPU absent", 13, 10, '$'
