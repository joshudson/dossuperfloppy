ORG 100h

CPU 8086

_start:
	mov	si, 81h
.arg	lodsb
	cmp	al, ' '
	je	.arg
	cmp	al, 9
	je	.arg
	and	al, 0CFh
	cmp	al, 'A'
	jb	usage
	cmp	al, 'Z'
	jbe	run
usage:
	mov	dx, umsg
	mov	ah, 9
	int	21h
	mov	ax, 4C03h
	int	21h
run:	sub	al, 'A'
	mov	[disk], al
	mov	[accessbpb], word 0
	push	ds
	pop	es
	mov	ax, 440Dh
	mov	bl, [disk]
	mov	bh, 0
	mov	cx, 0867h
	mov	dx, [accessbpb]
	int	21h
	jc	.error1
	or	[accessbpb + 1], byte 1
	mov	ax, 440dh
	mov	cl, 47h
	int	21h
	jc	.error2
	xor	ax, ax
.loop	inc	ax
	push	ax
	call	diskread
	pop	ax
	jc	.nope
	call	dot
	mov	cl, [buffer]
	cmp	cl, 0E9h
	je	.jmp
	cmp	cl, 0EBh
	je	.jmp
	jmp	.nope
.error1	mov	dx, ioctl1
	jmp	errorx
.error2 mov	dx, ioctl2
	jmp	errorx
.jmp	mov	cl, [buffer + 0Bh]
	cmp	cl, 0		; Sectors per cluster must be a power of 2
	je	.nope
	mov	bl, cl
	dec	bl
	test	bl, cl
	jnz	.nope
	cmp	[buffer + 0Eh], ax
	jbe	.nope		; Not enough reserved sectors
	cmp	[buffer + 15h], byte 0E0h
	jnb	.nope		; Media Descriptor is nonsensical (at least for HD)
	jmp	restorebpb
.nope	cmp	ax, 96		; Worst case backup BPB location is 97
	jbe	.loop
	mov	dx, nobackupbpb
	mov	ah, 9
	int	21h
	mov	ax, 4C01h
	int	21h
restorebpb:
	xor	ax, ax
	call	diskwrite
	jc	.badwrite
	mov	ax, 4C00h
	int	21h
.badwrite:
	mov	dx, writeerror
errorx:
	mov	ah, 9
	int	21h
	mov	ax, 4C02h
	int	21h

dot:	push	ax
	mov	dl, 2Eh
	mov	ah, 2
	int	21h
	pop	ax
	ret

diskread:
	call	mkpacket
	int	25h
	mov	sp, [cs:saved_sp]	; Fix for one bug in one version of DR-DOS
diskreadwritepost:
	push	cs			; Repair destroyed segment registers
	pop	ds
	push	cs
	pop	es
	jc	error_rtn
	popf
	clc
	ret

error_rtn:
	popf
	stc
	ret

diskwrite:
	call	mkpacket
	int	26h
	mov	sp, [cs:saved_sp]	; Fix for one bug in one version of DR-DOS
	jmp	diskreadwritepost

mkpacket:
	mov	[saved_sp], sp		; Yes that's right.
	mov	[bptr], word buffer
	mov	[bptr + 2], ds
	mov	[sector], ax
	mov	[sector + 2], word 0
	mov	[count], word 1
	mov	al, [disk]
	mov	cx, 0FFFFh
	mov	bx, sector
	ret
	

umsg:	db	"Unconditionally restores BPB from backup BPB", 13, 10
	db	"Usage: RBPB C:", 13, 10, "$"
nobackupbpb	db	"No backup BPB found or all reads failed", 13, 10, "$"
writeerror	db	"Error writing restored BPB", 13, 10, "$"
ioctl1	db	"IOCTL 0867h failed", 13, 10, "$"
ioctl2	db	"IOCTL 0847h failed", 13, 10, "$"
ALIGN 16, db 0
_end:

section .bss

saved_sp	resw	1
disk		resb	1
		resb	1
sector		resw	2
count		resw	1
bptr		resw	2
accessbpb	resw	1
buffer		resw	512	; Must be last; size isn't actually known
				; while 512 is the largest reasonable,
				; stranger things have happened
