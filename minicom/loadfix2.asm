; loadfix2: align to next 64k boundary

CPU 8086

ORG 0100h

env		equ	2Ch	; Anybody inspecting RAM finds a usable ENV pointer
scratch		equ	51h
envend		equ	52h
scratch2	equ	54h

_start:
	cld
	mov	sp, 100h + _end - _start + ((_bssend - _bss + 15) & 0FFF0h) + 128
	mov	di, sp
	
	push	ds
	mov	ax, [env]
	cmp	ax, 0
	je	.noenv
	cmp	ax, 0FFFFh
	je	.noenv
	mov	ds, ax
	xor	si, si
.cpenvl	cmp	[si], word "CM"
	jne	.cpenv
	cmp	[si + 2], word "DL"
	jne	.cpenv
	cmp	[si + 4], word "IN"
	jne	.cpenv
	cmp	[si + 6], word "E="
	jne	.cpenv
.skpenv	lodsb
	cmp	al, 0
	jne	.skpenv
	jmp	.cpenvl
.cpenv	lodsb
	cmp	al, 0
	je	.denv
.cpenvx	stosb
	lodsb
	cmp	al, 0
	jne	.cpenvx
	stosb
	jmp	.cpenvl
.noenv	mov	al, 0
	stosb
	jmp	.denv2
.denv	stosb
	push	es
	push	ds		; Free original environment
	pop	es
	mov	ah, 49h
	int	21h
	pop	es
.denv2	pop	ds
	mov	[envend], di
	mov	ax, ds
	add	ax, (100h + _end - _start + ((_bssend - _bss + 15) & 0FFF0h) + 128) / 16
	mov	[env], ax
	mov	[arg_envseg], ax
	mov	[arg_tailoff], word 80h
	mov	[arg_tailseg], ds
	mov	[arg_fcb1off], word 5Ch
	mov	[arg_fcb1seg], ds
	mov	[arg_fcb2off], word 6Ch
	mov	[arg_fcb2seg], ds

	; parse command
	mov	di, 5Ch
	mov	ax, "??"
	xor	ax, ax
	mov	cx, 10h
	rep	stosw
	mov	[5Ch], byte 0
	mov	[6Ch], byte 0
	mov	si, 81h
	mov	bh, 0
	mov	bl, [80h]
	mov	[bx + si], byte 0
.loop0	lodsb
	cmp	al, 9
	je	.loop0
	cmp	al, ' '
	je	.loop0
	cmp	al, 0
	je	.nocmd
	dec	si
	mov	di, exbuffer
.loop1	lodsb
	stosb
	cmp	al, 9
	je	.arg
	cmp	al, ' '
	je	.arg
	cmp	al, 0
	jne	.loop1
	mov	[80h], byte 0
	mov	[81h], byte 13
	jmp	.nofcb
.nocmd	mov	dx, nocmd
	jmp	_exit
.arg	dec	si
	mov	[di - 1], byte 0
	mov	di, 81h
.loop2	lodsb
	stosb
	cmp	al, 0
	jne	.loop2
	mov	[di - 1], byte 13
	mov	cx, di
	sub	cx, 82h
	mov	[80h], byte cl
	mov	si, 81h
.loop3	lodsb		; Parse FCBs if any
	cmp	al, 13
	je	.nofcb
	cmp	al, ' '
	je	.loop3
	cmp	al, 9
	je	.loop3
	dec	si
	mov	di, 5Ch
	mov	ax, 2900h
	int	21h
	jc	.nofcb
.loop4	lodsb
	cmp	al, 13
	je	.nofcb
	cmp	al, ' '
	je	.loop4
	cmp	al, 9
	je	.loop4
	dec	si
	mov	di, 6Ch
	mov	ax, 2900h
	int	21h
.nofcb	mov	ah, 0
	mov	si, exbuffer
.loop5	lodsb
	cmp	al, '/'
	je	.d
	cmp	al, '\'
	je	.d
	cmp	al, ':'
	je	.d
	cmp	al, '.'
	je	.s
	cmp	al, 0
	je	.prg2
	jmp	.loop5
.d	mov	ah, 2
	jmp	.loop5
.s	or	ah, 1
	jmp	.loop5
.prg2	mov	[scratch], ah	; SI = end of progran name
	call	probe_extension
	jnc	.go
	test	[scratch], byte 2
	jnz	.nogo
	mov	di, sp		; Find PATH=
.loop6	cmp	[di], byte 0
	jz	.nogo
	cmp	[di], word "PA"
	jne	.loop7
	cmp	[di + 2], word "TH"
	jne	.loop7
	cmp	[di + 4], byte "="
	je	.path
.loop7	inc	di
	cmp	[di], byte 0
	jne	.loop7
	inc	di
	jmp	.loop6
.path	add	di, 5
.loop8	mov	dl, '.'
	mov	ah, 2
	int	21h
	cmp	[di], byte 0
	je	.nogo
	call	injectpath
	call	probe_extension
	jnc	.go
	call	removepath
	jmp	.loop8
.nogo	mov	dx, nocmd2
	jmp	_exit

.go	mov	ax, cs
	mov	bx, [envend]
	add	bx, 15
	mov	cl, 4
	shr	bx, cl
	add	bx, ax
	or	bx, 0FFFh
	sub	bx, ax
	mov	ah, 4Ah
	int	21h
	jnc	.go2
	mov	dx, nomem
	jmp	_exit
.go2	mov	dx, exbuffer
	mov	bx, arg_envseg
	mov	ax, 4B00h
	int	21h
	cli
	mov	ax, cs
	mov	ss, ax
	mov	sp, 100h + _end - _start + ((_bssend - _bss + 15) & 0FFF0h) + 128
	sti
	jc	.execc
	mov	ah, 4Dh
	int	21h
	jmp	_exit2
.execc	mov	dx, eerr
_exit	mov	ah, 9
	int	21h
	mov	al, 255
_exit2	mov	ah, 4Ch
	int	21h

injectpath:
	push	di
	xor	dx, dx		; how many bytes to copy from PATH
	mov	ah, 0
.loop1	mov	al, [di]
	cmp	al, byte 0
	je	.lf
	inc	di
	cmp	al, byte ';'
	je	.lf
	mov	ah, al
	inc	dx
	jmp	.loop1
.lf	cmp	ah, byte '\'	; Need separator?
	je	.ns1
	cmp	ah, byte '/'
	je	.ns1
	inc	dx
.ns1	mov	bx, si
	sub	bx, exbuffer	; length of file name
	mov	[scratch2], di
	std
	mov	di, exbuffer
	add	di, dx
	add	di, bx
	mov	cx, bx
	dec	si
	dec	di
	rep	movsb
	cld
	inc	di
	mov	bp, di		; Preserved start of file name
	mov	cx, dx
	add	di, bx
	pop	si
	push	di		; Preserved end of file name
	mov	di, exbuffer
	rep	movsb
	mov	[di - 1], byte '\'
	pop	si		; Preserved end of file name
	mov	di, [scratch2]
	ret

removepath:
	push	di
	mov	cx, si
	sub	cx, bp
	mov	di, exbuffer
	mov	si, bp
	rep	movsb
	mov	si, di
	pop	di
	ret

probe_extension:
	test	[scratch], byte 1
	jnz	.probe
	mov	[si - 1], byte '.'
	mov	[si], word "CO"
	mov	[si + 2], word "M"
	call	.probe
	jnc	.ret
	mov	[si], word "EX"
	mov	[si + 2], word "E"
.probe	mov	dx, exbuffer
	mov	ax, 4300h
	int	21h
	jc	.ret
	test	cl, byte 10h
	jz	.ret
	stc
.ret	ret

nocmd	db	"Program not given", 13, 10, '$'
nocmd2	db	"Program not found", 13, 10, '$'
nomem	db	"Out of RAM", 13, 10, '$'
eerr	db	"Exec error", 13, 10, '$'

align 16, db 0

_end:

section .bss

_bss:
arg_envseg	resb	2
arg_tailoff	resb	2
arg_tailseg	resb	2
arg_fcb1off	resb	2
arg_fcb1seg	resb	2
arg_fcb2off	resb	2
arg_fcb2seg	resb	2
exbuffer	resb	66

_bssend:
