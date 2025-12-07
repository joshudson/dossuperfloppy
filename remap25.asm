CPU 8086

ORG 0100h

param_env	equ	82
param_args	equ	84
param_fcb1	equ	88
param_fcb2	equ	92

offset		equ	60
limit		equ	64
vector1		equ	68
vector2		equ	72
logdisk		equ	76
physdisk	equ	77
lbadapsize	equ	78
lbacount	equ	80
lbaptr		equ	84
lbasector	equ	88
ahaction	equ	5	; Pretty sure nobody cares if I trash my own CALL 5 gate
heads		equ	78
sectors		equ	80
programstart	equ	96

_start:
	jmp	main

usage:
	mov	dx, .message
	mov	ah, 9
	int	21h
	mov	ax, 4CFFh
	int	21h
.message:
	db	"REMAP25: remap access to a drive letter to a partition", 13, 10
	db	"Only raw disk access to the drive letter should be used.", 13, 10
	db	"Copyright (C) Joshua Hudson 2025", 13, 10
	db	"Usage: REMAP25 [/C|/L] disk", 13, 10
	db	"Usage: REMAP25 [/C|/L] disk partition Z: A:\SSDSCAN.COM Z: options", 13, 10, '$'

error	db	"EXEC failed", 13, 10, '$'
diskerr	db	"Disk Error", 13, 10, '$'
nopart	db	"No such partition", 13, 10, '$'
stack	db	"Stack overflow", 13, 10, '$'
fcbbase	db	0, "           "

readnumber:
	mov	bx, 0A00h
.l	cmp	al, '0'
	jb	.done
	cmp	al, '9'
	ja	.done
	xchg	al, bl
	mul	bh
	xchg	al, bl
	sub	al, '0'
	add	bl, al
	lodsb
	jmp	.l
.done	ret

list:
	mov	di, buffer + 60
	call	loadlist
	mov	[tablptr], di
	mov	bx, buffer + 60
	xor	bp, bp		; Build string
.loop	cmp	[bx], byte 0FFh
	je	.next
	mov	di, buffer
	xor	cx, cx
	push	cx
	push	cx
	push	bp
	mov	cl, 2
	call	.number
	mov	al, ' '
	stosb
	mov	al, [bx]
	add	al, '0'
	cmp	al, '9'
	jle	.xl
	add	al, 'A' - '0' - 10
.xl	stosb
	mov	al, ' '
	stosb
	xor	cx, cx
	push	cx
	push	word [bx + 4]
	push	word [bx + 2]
	mov	cl, 10
	call	.number
	mov	al, ' '
	stosb
	push	word [bx + 10]
	push	word [bx + 8]
	push	word [bx + 6]
	mov	cx, 15
	call	.number
	mov	al, 13
	stosb
	mov	al, 10
	stosb
	mov	al, '$'
	stosb
	mov	dx, buffer
	mov	ah, 9
	int	21h
.next	inc	bp
	add	bx, 12
	cmp	bx, [tablptr]
	jb	.loop
	mov	ax, 4C00h
	int	21h
.number	push	bp
	mov	bp, sp
	add	di, cx
	push	di
	mov	si, 10
	std
	dec	di
.digits	xor	dx, dx
	mov	ax, [bp + 8]
	div	si
	mov	[bp + 8], ax
	mov	ax, [bp + 6]
	div	si
	mov	[bp + 6], ax
	mov	ax, [bp + 4]
	div	si
	mov	[bp + 4], ax
	mov	al, dl
	add	al, '0'
	stosb
	mov	ax, [bp + 4]
	or	ax, [bp + 6]
	or	ax, [bp + 8]
	jz	.spcpad
	loop	.digits
.nr	cld
	pop	di
	pop	bp
	ret	6
.spcpad	dec	cx
	mov	al, ' '
	rep	stosb
	jmp	.nr

	;Keep list function above main
	;This chews up space so that programbuilder doesn't overwrite itself
loadlist:
	cmp	[type], byte 'C'
	je	.chs
	mov	dl, [physdisk]
	mov	si, di
	mov	[si], word 1Eh
	mov	ah, 48h
	int	13h
	jc	.nolba
	mov	[type], byte 'L'
	mov	ax, [si + 10h]
	mov	dx, [si + 12h]
	mov	cx, [si + 14h]
	mov	bx, [si + 16h]
	or	bx, bx
	jnz	.stupidhuge
	mov	[di + 6], ax
	mov	[di + 8], dx
	mov	[di + 10], cx
	jmp	.norm
.stupidhuge:
	mov	ax, 0FFFFh
	mov	[di + 6], ax
	mov	[di + 8], ax
	mov	[di + 10], ax
.norm	jmp	.param
.nolba	cmp	[type], byte 'L'
	jne	.chsm
.err	mov	dx, diskerr
	mov	ah, 9
	int	21h
	mov	ax, 4CFFh
	int	21h
.chsm	mov	[type], byte 'C'
.chs	mov	ah, 08h
	mov	dl, [physdisk]
	push	es
	push	di
	xor	di, di
	mov	es, di
	int	13h
	pop	di
	pop	es
	jc	.err
	mov	dl, dh
	mov	dh, 0
	inc	dx
	mov	[heads], dx
	mov	al, cl
	and	ax, 63
	mov	[sectors], ax
	xchg	cl, ch
	rol	ch, 1
	rol	ch, 1
	and	ch, 3
	inc	cx
	mul	dx
	mov	bx, dx
	mul	cx
	mov	[di + 6], ax
	mov	[di + 8], dx
	xchg	ax, bx
	mul	cx
	add	[di + 8], ax
	adc	dx, 0
	mov	[di + 10], dx
.param	mov	[di], byte 0
	mov	[di + 2], word 0
	mov	[di + 4], word 0
	add	di, 12

	mov	bx, di
	xor	ax, ax
	xor	dx, dx
	mov	cx, 1
	push	di
	call	.diskaccess
	pop	di
	jc	.err
	mov	bx, di
	push	bp
	mov	bp, sp
	sub	sp, 64
	lea	si, [di + 446]
	mov	di, sp
	mov	cx, 32
	rep	movsw
	mov	di, bx
	mov	si, sp
	xor	ax, ax
	xor	dx, dx
	mov	ch, 0
	call	.slot
	add	si, 16
	mov	ch, 0
	call	.slot
	add	si, 16
	mov	ch, 0
	call	.slot
	add	si, 16
	mov	ch, 0
	call	.slot
	mov	si, sp
	mov	ch, 1
	call	.slot
	add	si, 16
	mov	ch, 1
	call	.slot
	add	si, 16
	mov	ch, 1
	call	.slot
	add	si, 16
	mov	ch, 1
	call	.slot
	mov	sp, bp
	pop	bp
	ret

.slot	cmp	[si + 12], word 0
	jne	.nzl
	cmp	[si + 14], word 0
	je	.noslot
.nzl	mov	cl, [si]
	cmp	cl, 0
	je	.slot0
	cmp	cl, 80h
	jb	.noslot
.slot0	mov	cl, [si + 4]
	cmp	ch, 1
	je	.ebrx
	cmp	cl, 1
	je	.fat
	cmp	cl, 4
	je	.fat
	cmp	cl, 6
	je	.fat
	cmp	cl, 0Bh
	je	.fat
	cmp	cl, 0Ch
	je	.fat
	cmp	cl, 0Eh
	je	.fat
	cmp	ch, 0
	je	.eslot
.ebrx	cmp	cl, 5
	je	.ebr
	cmp	cl, 0Fh
	je	.ebr
	ret
.noslot	cmp	ch, 0
	jne	.ret
.eslot	mov	[di], byte 0FFh
	add	di, 12
.ret	ret
.ebr	push	si
	push	ax
	push	dx
	add	ax, [si + 8]
	adc	dx, [si + 10]
	push	ax
	push	dx
	mov	bx, di
	push	di
	add	di, 32768 + 130
	cmp	di, sp
	jae	.stack
	call	.diskaccess
	jc	.err
	pop	bx
	pop	dx
	pop	ax
	lea	si, [bx + 446]
	push	bp
	mov	bp, sp
	sub	sp, 64
	mov	di, sp
	mov	cx, 32
	rep	movsw
	mov	di, bx
	mov	si, sp
	mov	ch, 2
	call	.slot
	add	si, 16
	mov	ch, 2
	call	.slot
	add	si, 16
	mov	ch, 2
	call	.slot
	add	si, 16
	mov	ch, 2
	call	.slot
	mov	sp, bp
	pop	bp
	pop	dx
	pop	ax
	pop	si
	ret
.fat	push	ax
	push	dx
	add	ax, [si + 8]
	adc	dx, [si + 10]
	mov	[di], cl
	mov	[di + 2], ax
	mov	[di + 4], dx
	mov	ax, [si + 12]
	mov	dx, [si + 14]
	mov	[di + 6], ax
	mov	[di + 8], dx
	mov	[di + 10], word 0
	pop	dx
	pop	ax
	add	di, 12
	ret
.stack	mov	dx, stack
	mov	ah, 9
	int	21h
	mov	ax, 4CFFh
	int	21h

.diskaccess:
	mov	[ahaction], byte 2h
	cmp	[type], byte 'L'
	je	.lbaaccess
	jmp	chsdiskaccess
.lbaaccess:
	xor	di, di
	jmp	lbadiskaccess

main:
	; Dump our own ENV and get parent's ENV
	mov	ax, [16h]
	cmp	ax, 40
	jbe	.npe
	mov	bx, ds
	cmp	ax, bx
	je	.npe
	mov	es, ax
	mov	ax, [es:2Ch]
	cmp	ax, [2Ch]
	je	.npe
	test	ax, ax
	jz	.npe
	cmp	ax, 0FFFFh
	je	.npe
	xchg	ax, [2Ch]
	mov	es, ax
	mov	ah, 49h
	int	21h
.npe	mov	si, 81h
	mov	[si + 128], byte 13	; In case cmd line is completely full
	push	ds
	pop	es
.p1	lodsb
	cmp	al, ' '
	je	.p1
	cmp	al, '/'
	jne	.nopt
	lodsb
	cmp	al, 'L'
	je	.olba
	cmp	al, 'l'
	je	.olba
	cmp	al, 'c'
	je	.ochs
	cmp	al, 'C'
	je	.ochs
.uv	jmp	usage
.olba	mov	[type], byte 'L'
	jmp	.p1
.ochs	mov	[type], byte 'C'
	jmp	.p1
.nopt	cmp	al, '1'
	jb	.uv
	cmp	al, '9'
	ja	.uv
	call	readnumber
	add	bl, 7Fh
	mov	[physdisk], byte bl
	cmp	al, 13
	jne	.nlist
.list	jmp	list
.nlist	cmp	al, ' '
	jne	.uv
.as1	lodsb
	cmp	al, ' '
	je	.as1
	cmp	al, 13
	je	.list
	cmp	al, '0'
	jb	.uv
	cmp	al, '9'
	ja	.uv
	call	readnumber
	mov	[partno], bl
	cmp	al, ' '
	jne	.uv
.as2	lodsb
	cmp	al, ' '
	je	.as2
	and	al, ~('a' - 'A')
	sub	al, 'A'
	jc	.uv
	cmp	al, 26
	ja	.uv
	mov	[logdisk], al
	lodsb
	cmp	al, ':'
	jne	.uv
	lodsb
	cmp	al, ' '
	jne	.uv
.as3	lodsb
	cmp	al, ' '
	je	.as3
	mov	di, buffer
	mov	[progptr], di
.npc	stosb
	lodsb
	cmp	al, ' '
	je	.args
	cmp	al, 13
	jne	.npc
.noargs	mov	al, 0
	stosb
	mov	[argptr], di
	mov	al, 13
	stosb
	jmp	haveprogram
.args	dec	si
	mov	al, 0
	stosb
	mov	[argptr], di
	mov	ah, 13
	call	stpcpy
haveprogram:
	mov	[tablptr], di
	call	loadlist
	mov	ax, di
	sub	ax, [tablptr]
	mov	cx, 12
	xor	dx, dx
	div	cx
	mov	bl, [partno]
	mov	bh, 0
	xchg	ax, bx
	cmp	ax, bx
	jbe	havepart
.hole	mov	dx, nopart
	mov	ah, 9
	int	21h
	mov	ax, 4CFFh
	int	21h

havepart:
	mul	cx
	mov	bx, ax
	add	bx, [tablptr]
	cmp	[bx], byte 0FFh
	je	haveprogram.hole
	mov	ax, [bx + 2]
	mov	dx, [bx + 4]
	mov	[offset], ax
	mov	[offset + 2], dx
	mov	ax, 0FFFFh
	mov	dx, 0FFFFh
	cmp	[bx + 12], word 0
	jne	.huge
	mov	ax, [bx + 8]
	mov	dx, [bx + 10]
	sub	ax, 1
	sbb	dx, 0
.huge	mov	[limit], ax
	mov	[limit + 2], dx
programbuilder:
	mov	di, programstart
	mov	si, error
	mov	cx, fcbbase - error
	rep	movsb
	mov	ax, di
	mov	dx, cs
	mov	ds, cx
	cli
	xchg	ax, [25h * 4]
	xchg	dx, [25h * 4 + 2]
	push	cs
	pop	ds
	mov	[vector1], ax
	mov	[vector1 + 2], dx
	mov	si, int25entry
	mov	cx, int26entry - int25entry
	rep	movsb
	mov	ax, di
	mov	dx, cs
	mov	ds, cx
	xchg	ax, [26h * 4]
	xchg	dx, [26h * 4 + 2]
	sti
	push	cs
	pop	ds
	mov	[vector2], ax
	mov	[vector2 + 2], dx
	mov	si, int26entry
	mov	cx, chsdiskaccess - int26entry
	rep	movsb
	cmp	[type], byte 'L'
	jne	.chs
	mov	si, lbadiskaccess
	mov	cx, lbadiskaccess.end - lbadiskaccess
	jmp	.acc
.chs	mov	si, chsdiskaccess
	mov	cx, chsdiskaccess.end - chsdiskaccess
.acc	rep	movsb
	mov	si, errorfinal
	mov	cx, errorfinal.end - errorfinal
	rep	movsb
	mov	bx, di		; where program tail will be writen to
	mov	si, programtail
	mov	cx, _end - programtail
	rep	movsb
	mov	[param_fcb1], di
	mov	[param_fcb1 + 2], ds
	mov	[param_fcb2], di
	mov	[param_fcb2 + 2], ds
	mov	si, fcbbase
	mov	cx, 6
	rep	movsw
	push	di
	mov	si, [progptr]
	mov	ah, 0
	call	stpcpy
	pop	word [progptr]
	mov	bp, di
	inc	di
	mov	si, [argptr]
	mov	ah, 13
	call	stpcpy
	lea	cx, [di - 2]	; args is number of bytes followed by bytes followed by 0D
	sub	cx, bp
	mov	[bp], cl
	mov	[param_args], bp
	mov	[param_args + 2], ds
	add	di, 8Fh
	and	di, 0FFF0h
	mov	bp, di		; New top of stack
	mov	[bx + programtail.hit + 1 - programtail], bp
	mov	di, bx
	mov	bx, bp
	mov	cl, 4
	shr	bx, cl
	mov	dx, [progptr]
	mov	si, param_env
	mov	ax, [2Ch]
	mov	[si], ax
	mov	ah, 4Ah
	jmp	di

stpcpy:
	lodsb
	stosb
	cmp	ah, al
	jne	stpcpy
	ret

; INT 25 and 26 are allowed to trash everything but SS, SP and return with retf
int25entry:
	cmp	al, [cs:logdisk]
	je	.i25e
	jmp	far [cs:vector1]
.i25e	mov	[cs:ahaction], byte 02h
	jmp	int2526common
int26entry:
	cmp	al, [cs:logdisk]
	je	.i26e
	jmp	far [cs:vector2]
.i26e	mov	[cs:ahaction], byte 03h
	jmp	int2526common
int2526common:
	cmp	cx, 0FFFFh
	je	.big
	mov	ax, dx
	xor	dx, dx
	push	ds
	pop	es
	jmp	.com
.big	mov	ax, [bx]
	mov	dx, [bx + 2]
	mov	cx, [bx + 4]
	les	bx, [bx + 6]
.com	push	cs
	pop	ds
	cmp	dx, [limit]
	jb	.lg
	ja	.lb
	cmp	ax, [limit]
	jbe	.lg
.lb	mov	ax, 0408h
	stc
	retf
.cb	mov	ax, 0BB03h
	stc
	retf
.lg	cmp	cx, 255
	ja	.cb
	add	ax, [offset]
	adc	dx, [offset + 2]

chsdiskaccess:
	mov	si, cx
	mov	di, ax
	xchg	ax, dx
	xor	dx, dx
	div	word [sectors]
	xchg	ax, di
	div	word [sectors]
	inc	dx
	mov	cx, dx	; Sector is in place
	mov	dx, di
	div	word [heads]
	mov	dh, dl	; Head is in place
	xchg	ah, al
	ror	al, 1
	ror	al, 1
	or	cx, ax	; Cylinder is in place
	mov	ax, si
	mov	dl, [physdisk]
	mov	ah, [ahaction]
	int	13h
.end	ret		; Used by us

lbadiskaccess:
	mov	di, 0		; Yup; handle 2TB part at end of 2TB MBR,
	adc	di, 0		; blowing completely past the limit
	mov	si, lbadapsize
	mov	[si], word 10h
	mov	[si + 2], cx
	mov	[si + 4], bx
	mov	[si + 6], es
	mov	[si + 8], ax
	mov	[si + 10], dx
	mov	[si + 12], di
	mov	[si + 14], word 0
	mov	dl, [physdisk]
	mov	al, 0
	mov	ah, [ahaction]
	or	ah, 40h
	int	13h
.end	ret

errorfinal:
	mov	al, 0
	jnc	.succ
	mov	al, 0Ch		; Turns out we don't care what the AL error code is
.succ	retf
.end:

programtail:
	mov	sp, bp
	int	21h		; AH=4Ah, shrink memory, BX=SP>>4= new size
	mov	ax, 4B00h	; DS:DX must point to program
	mov	bx, si		; ES:SI must point to parameter block
	int	21h
.earlyc	cli
	mov	bx, cs
	mov	ds, bx
	mov	ss, bx
.hit	mov	sp, 0
	mov	cx, 0		; Need to save carry flag
	mov	es, cx		; Unhook from DOS
	mov	cx, [vector1]
	mov	dx, [vector1 + 2]
	mov	[es:25h * 4], cx
	mov	[es:25h * 4 + 2], dx
	mov	cx, [vector2]
	mov	dx, [vector2 + 2]
	mov	[es:26h * 4], cx
	mov	[es:26h * 4 + 2], dx
	sti
	jnc	.exit
	push	ax
	mov	ah, 9
	mov	dx, programstart
	int	21h
	pop	ax
	jmp	.exit2
.exit	mov	ah, 4Dh
	int	21h
.exit2	mov	ah, 4Ch
	int	21h
_end:

partno	equ	_end
type	equ	partno + 1
progptr	equ	type + 1
argptr	equ	progptr + 2
tablptr	equ	argptr + 2
buffer	equ	tablptr + 2
