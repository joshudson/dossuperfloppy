;; Copyright (C) Joshua Hudson 2021
;; Licensed Under CC-BY-SA 4.0
;; The actual patches themselves have unknown attribution
;; and I judge them too short to copyright.

BITS 16

ORG	0x100

filepath	equ	_start + 2	; Uninitialized data (_start is taken with a possible 0)
filedate	equ	_start + 4
filetime	equ	_start + 6
left		equ	_start + 8
lefth		equ	_start + 10

_start:
	mov	si, 0x0080
	mov	bl, [si]
	mov	bh, 0
	inc	si
	mov	[bx + si], bh
.scan	lodsb
	or	al, al
	jz	short	.nope
	cmp	al, ' '
	jne	short	.scan
	jmp	short	main
.nope	mov	dx, usage
	jmp	error

	db	0xCC			; Align
fileattr	dw	0		; Initialized data (cheapeat direct)
patch		dw	0
length		dw	0
lengthh		dw	0

main:
	mov	[filepath], si
	mov	dx, si
	mov	ax, 0x4300
	call	syscallbase
	mov	[fileattr], cx
	xor	cx, cx
	mov	ax, 0x4301
	call	syscallbase
	mov	ax, 0x3D02
	call	syscallbase
	xchg	ax, bx
	mov	ax, 0x5700
	int	0x21
	mov	[filedate], dx
	mov	[filetime], cx
	
	; Just read the whole file into memory above us
	xor	si, si
	mov	cx, 512
	push	cs
	pop	di
	add	di, 0x1000
	xor	dx, dx

.read	push	ds
	push	di
	pop	ds
	mov	ah, 0x3F
	int	0x21
	pop	ds
	jc	short	.tec
	add	[length], ax
	adc	[lengthh], si
	call	advhuge
	or	ax, ax
	jnz	short	.read

	push	cs
	pop	di
	add	di, 0x1000
	xor	dx, dx
	call	patchloop

	cmp	[patch], byte 4
	jne	short	.nowrite
	push	cx
	xor	cx, cx
	mov	ax, 0x4200
	int	0x21	; Cannot fail
	pop	cx
	xor	si, si

.write	cmp	[lengthh], si
	jne	short	.wna
	cmp	[length], cx
	ja	short	.wna
	mov	cx, [length]
.wna	push	ds
	push	di
	pop	ds
	mov	ah, 0x40
	int	0x21
	pop	ds
.tec	jc	errorcode_c
	sub	[length], ax
	sbb	[lengthh], si
	call	advhuge
	cmp	[length], si
	jne	short	.write
	cmp	[lengthh], si
	jne	short	.write

	mov	ax, 0x5701
	mov	dx, [filedate]
	mov	cx, [filetime]
	int	0x21
.nowrite:
	mov	ah, 0x3E
	int	0x21
	call	resetattr
	mov	bx, [patch]
	mov	dx, [bx + patchtable]
	mov	ah, 0x09
	int	0x21
	mov	ax, 0x4C01
	cmp	[patch], byte 0
	je	short	.xnw
	mov	al, 0
.xnw	int	0x21

syscallbase:
	int	0x21
	jc	short	errorcode
	ret

errorcode_c:
	push	ax
	mov	ah, 0x3E
	int	0x21
	pop	ax
errorcode:
	mov	bl, al
	mov	bh, 0
	shl	bx, 1
	mov	dx, [bx + errorcodes]
error:
	mov	ah, 0x09
	int	0x21
	call	resetattr
	mov	ax, 0x4C01
	int	0x21

patchloop:
	push	di
	push	bx
	push	cx
	push	dx
	push	es

	mov	ax, [length]
	mov	bx, [lengthh]
	mov	[left], ax
	mov	[lefth], bx
	xchg	dx, di
	; DX:DI is the huge pointer into kernel

.nextbyte:
	push	dx
	pop	es
	mov	bx, patches
.ploop	mov	ch, 0
	mov	cl, [bx]
	or	cl, 0
	jz	short	.lastpatch
	xor	ax, ax
	cmp	[lefth], ax
	jne	short	.ncx
	cmp	[left], cx
	jb	short	.nextpatch
.ncx	mov	si, bx
	inc	si
	push	cx
	push	di
	repe	cmpsb
	pop	di
	pop	cx
	jne	short	.nomatch

	mov	si, bx
	inc	si
	add	si, cx
	push	di
	rep	movsb
	pop	di
	mov	[patch], byte 4
	jmp	short	.lastpatch

.nomatch:
	cmp	[patch], byte 0
	jne	short	.nextpatch
	mov	si, bx
	inc	si
	add	si, cx
	push	cx
	push	di
	repe	cmpsb
	pop	di
	pop	cx
	jne	short	.nextpatch
	mov	[patch], byte 2
	jmp	short	.lastpatch

.nextpatch:
	inc	bx
	add	bx, cx
	add	bx, cx
	jmp	.ploop

.lastpatch:
	xchg	dx, di
	mov	ax, 1
	call	advhuge
	xchg	dx, di
.ncc	sub	[left], word 1
	jnc	short	.nextbyte
	sub	[lefth], word 1
	jnc	short	.nextbyte

	pop	es
	pop	dx
	pop	cx
	pop	bx
	pop	di
	ret

advhuge:
	add	dx, ax
	push	cx
	push	ax
	mov	ax, dx
	mov	cl, 4
	shr	ax, cl
	add	di, ax
	pop	ax
	pop	cx
	and	dx, 0x000F
	ret

resetattr:
	mov	cx, [fileattr]
	or	cx, cx
	je	.exit
	mov	ax, 0x4301
	mov	dx, [filepath]
	int	0x21
.exit	ret

usage	db	'Usage: PATCHDOS.COM A:\IO.SYS', 13, 10, '$'

%ifdef DEBUG
checkpointdxdi:
	push	ax
	push	dx
	push	di
	push	si
	mov	si, .s + 2
	call	.gen
	shl	dx, 4
	call	.gen
	shl	dx, 4
	call	.gen
	shl	dx, 4
	call	.gen
	mov	dx, di
	call	.gen
	shl	dx, 4
	call	.gen
	shl	dx, 4
	call	.gen
	shl	dx, 4
	call	.gen
	mov	dx, .s
	mov	ah, 9
	int	0x21
	pop	si
	pop	di
	pop	dx
	pop	ax
	ret
.gen	push	dx
	shr	dx, 12
	add	dl, '0'
	cmp	dl, '9'
	jbe	.nh
	add	dl, 'A' - '0' - 10
.nh	mov	[si], dl
	inc	si
	pop	dx
	ret
.s	db	13, 10, 0, 0, 0, 0, 0, 0, 0, 0, '$'
%endif

patchtable:
	dw	.notpatched
	dw	.matched
	dw	.patched
.notpatched	db	'Change not found', 13, 10, '$'
.matched	db	'Already patched', 13, 10, '$'
.patched	db	'Patch applied', 13, 10, '$'

%include "errormsg.asm"

patches:
	;MS-DOS 4.0-6.0
	db	16
	db	0x2D, 0x01, 0x00, 0x83, 0xDA, 0x00, 0x26, 0x03, 0x47, 0x08, 0x26, 0x13, 0x57, 0x0A, 0x73, 0x05
	db	0x26, 0x03, 0x47, 0x08, 0x26, 0x13, 0x57, 0x0A, 0x73, 0x0B, 0x09, 0xD0, 0x74, 0x07, 0x90, 0x90
	;MS-DOS 7.0-8.0 (need to uncompress IO.SYS on 8.0 though)
	db	14
	db	0x83, 0xE8, 0x01, 0x83, 0xDA, 0x00, 0x03, 0x47, 0x08, 0x13, 0x57, 0x0A, 0x73, 0x06
	db	0x03, 0x47, 0x08, 0x13, 0x57, 0x0A, 0x73, 0x0C, 0x09, 0xD0, 0x74, 0x08, 0x90, 0x90
	db	0
