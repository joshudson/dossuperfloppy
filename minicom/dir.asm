; DIR: get directory listing

CPU 8086
ORG 0100h

dtfmt		equ	78h
thsep		equ	7Ah
datesep		equ	7Ch
timesep		equ	7Eh

_start:
	cld
	mov	[dta], byte 0		; In case COUNTRY.SYS not loaded
	mov	[dta + 7h], word ','
	mov	[dta + 0Bh], word '/'
	mov	[dta + 0Dh], word ':'
	mov	[dta + 0Fh], byte 0
	mov	dx, dta
	mov	ax, 3800h
	int	21h
	mov	al, [dta]
	mov	dx, [dta + 7h]
	mov	ah, [dta + 11h]
	mov	bx, [dta + 0Bh]
	mov	cx, [dta + 0Dh]
	mov	[dtfmt], ax
	mov	[datesep], bx
	mov	[timesep], cx
	mov	[thsep], dx
	
	mov	si, 81h
	mov	bl, [80h]
	mov	bh, 0
	mov	[si + bx], byte 13
	mov	bx, wildcard
	mov	dx, 10h
.aloop	lodsb
	cmp	al, ' '
	je	.aloop
	cmp	al, 9
	je	.aloop
	cmp	al, '/'
	je	.opts
	cmp	al, 13
	je	go
	cmp	bx, wildcard
	jne	.usage
	lea	bx, [si - 1]
.ascan	lodsb
	cmp	al, 9
	je	.xscan
	cmp	al, ' '
	je	.xscan
	cmp	al, 13
	jne	.ascan
	mov	[si - 1], byte 0
	jmp	go
.xscan	mov	[si - 1], byte 0
	jmp	.aloop
.opts	lodsb
	cmp	al, 9
	je	.aloop
	cmp	al, ' '
	je	.aloop
	cmp	al, 13
	je	go
	cmp	al, 'P'
	je	.p
	cmp	al, 'p'
	je	.p
	cmp	al, 'A'
	je	.a
	cmp	al, 'a'
	je	.a
	cmp	al, 'W'
	je	.w
	cmp	al, 'w'
	je	.w
.usage	mov	dx, usage
	mov	cx, usage_len
	call	writeblockerr
	mov	ax, 4C02h
	int	21h
.p	or	dh, 1
	jmp	.opts
.a	mov	dl, 13h
	jmp	.opts
.w	or	dh, 2
	jmp	.opts
go	mov	si, dx
	mov	dx, dta
	mov	ah, 1ah
	int	21h
	mov	cx, si
	and	si, 700h
	mov	ch, 0
	mov	dx, bx
	mov	ah, 4Eh
.loop	int	21h
	jnc	.found
	test	si, 200h
	jz	.nn2
	push	ax
	mov	ax, si
	mov	ah, 0
	mov	bl, 5
	div	bl
	cmp	ah, 0
	je	.nn1
	call	writeblocknl
.nn1	pop	ax
.nn2	cmp	al, 12h		; 12h = no more files
	je	.last
	cmp	al, 2
	je	.nfound
	cmp	al, 3
	je	.pnfnd
.error	mov	dx, error
	mov	cx, error_len
.emsg	call	writeblockerr
	mov	ax, 4C02h
	int	21h
.nfound	mov	dx, fnf
	mov	cx, fnf_len
.xnfnd	call	writeblockerr
	mov	ax, 4C01h
	int	21h
.pnfnd	mov	dx, pnf
	mov	cx, pnf_len
	jmp	.xnfnd
.last	mov	ax, 4C00h
	int	21h
.found	mov	di, builder
	test	si, 200h
	push	si
	mov	si, dta + 1Eh
	jz	.long
	test	[dta + 15h], byte 10h
	jz	.fw
	mov	al, '['
	stosb
.fw	call	namecopy
	cmp	[si], byte 0
	je	.fwdz
	mov	al, '.'
	stosb
	call	extcopy
.fwdz	test	[dta + 15h], byte 10h
	jz	.fw2
	mov	al, ']'
	stosb
.fw2	mov	cx, builder + 15
	sub	cx, di
	mov	al, ' '
	rep	stosb
	jmp	.next
.long	call	namecopy
	mov	cx, builder + 9
	sub	cx, di
	mov	al, ' '
	rep	stosb
	call	extcopy
	mov	cx, builder + 13
	sub	cx, di
	mov	al, ' '
	rep	stosb
	test	[dta + 15h], byte 10h
	jz	.len
	mov	si, dir
	mov	cx, dir_len
	rep	movsb
	mov	cx, 14 - dir_len
	mov	al, ' '
	rep	stosb
	jmp	.dt
.len	mov	bx, 10
	xor	cx, cx
.lloop	cmp	cl, 3
	je	.lloopc
	cmp	cl, 7
	je	.lloopc
	cmp	cl, 11
	jne	.lloopn
.lloopc	mov	ax, [thsep]
	push	ax
	inc	cx
.lloopn	xor	dx, dx
	mov	ax, [dta + 1Ch]
	div	bx
	mov	[dta + 1Ch], ax
	mov	ax, [dta + 1Ah]
	div	bx
	mov	[dta + 1Ah], ax
	add	dl, '0'
	push	dx
	inc	cx
	or	ax, [dta + 1Ch]
	jnz	.lloop
	mov	dx, ' '
.lloop2	push	dx
	inc	cx
	cmp	cx, 14
	jb	.lloop2
.lloop3	pop	ax
	call	fmtout
	loop	.lloop3
.dt	mov	al, ' '
	stosb
	cmp	[dtfmt], byte 1
	je	.dmy
	ja	.ymd
	call	dtamonth
	call	dtsep
	call	dtaday
	call	dtsep
	call	dtayear
	jmp	.tm
.dmy	call	dtaday
	call	dtsep
	call	dtamonth
	call	dtsep
	call	dtayear
	jmp	.tm
.ymd	call	dtayear
	call	dtsep
	call	dtamonth
	call	dtsep
	call	dtaday
.tm	mov	al, ' '
	stosb

	mov	ax, [dta + 16h]
	mov	cl, 11
	shr	ax, cl
	push	ax
	cmp	[dtfmt + 1], byte 0
	jne	.mt
	cmp	al, 0
	jne	.ml
	mov	al, 12
.ml	cmp	al, 12
	jbe	.mt
	sub	al, 12
.mt	call	number2
	mov	ax, [timesep]
	call	fmtout
	mov	ax, [dta + 16h]
	mov	cl, 5
	shr	ax, cl
	and	ax, 63
	call	number2
	mov	ax, [timesep]
	call	fmtout
	mov	ax, [dta + 16h]
	and	ax, 31
	shl	ax, 1
	call	number2
	pop	ax
	cmp	[dtfmt + 1], byte 0
	jne	.mt2
	cmp	ax, 13
	mov	ax, "am"
	jb	.am
	mov	al, 'p'
.am	stosw
.mt2	mov	ax, 0A0Dh
	stosw
.next	pop	si
	inc	si
	mov	dx, builder
	mov	cx, di
	sub	cx, dx
	mov	bx, 1
	call	writeblock

	mov	ax, si
	test	si, 200h
	jz	.xlong
	mov	ah, 0
	mov	cl, 5
	div	cl
	cmp	ah, 0
	jne	.nnp
	push	ax
	call	writeblocknl
	pop	ax
.xlong	cmp	al, 24
	jb	.nnp
	test	si, 100h
	jz	.nnpf
	mov	dx, more
	mov	cx, more_len
	call	writeblockerr
	int	21h
	mov	ah, 8
	int	21h
	cmp	ah, 3
	je	.cc
.nnpf	and	si, 0F00h
.nnp	mov	ah, 4Fh
	jmp	.loop
.cc	mov	ax, 4C00h
	int	21h

namecopy:
	cmp	[si], byte '.'
	je	.dd
.loop	lodsb
	cmp	al, '.'
	je	.ext
	cmp	al, 0
	je	.end
	stosb
	jmp	.loop
.end	dec	si
.ext	ret
.dd	lodsb
	stosb
	cmp	al, 0
	jne	.dd
	dec	di
	dec	si
	ret

extcopy:
	lodsb
	stosb
	cmp	al, 0
	jne	extcopy
	dec	di
	ret

dtayear:
	mov	ax, [dta + 18h]
	mov	cl, 9
	shr	ax, cl
	add	ax, 1980
	jmp	number4
dtamonth:
	mov	ax, [dta + 18h]
	mov	cl, 5
	shr	ax, cl
	and	ax, 15
	jmp	number2
dtaday:
	mov	ax, [dta + 18h]
	and	ax, 31
	jmp	number2

number4:
	mov	bx, 100
	xor	dx, dx
	div	bx
	call	number2
	xchg	ax, dx

number2:
	mov	bl, 10
	div	bl
	add	ax, word "00"
	stosb
	mov	al, ah
	stosb
	ret

dtsep:
	mov	ax, [datesep]
fmtout:
	stosb
	cmp	ah, 0
	jz	.ret
	mov	al, ah
	stosb
.ret	ret

writeblocknl:
	mov	dx, newline
	mov	cx, 2
	mov	bx, 1
	jmp	writeblock
writeblockerr:
	mov	bx, 2
writeblock:
	mov	ah, 40h
	int	21h
	jc	.ret
	add	dx, ax
	sub	cx, ax
	ja	writeblock
.ret	ret

wildcard	db	"*.*", 0
dir		db	" <DIR>"
dir_len		equ	$ - dir
usage		db	"DIR [path] [/A] [/W] [/P]", 13, 10
usage_len	equ	$ - usage
error		db	"Error", 13, 10
error_len	equ	$ - error
more		db	"press any key for more", 13, 10
more_len	equ	$ - error
fnf		db	"File not found", 13, 10
fnf_len		equ	$ - fnf
pnf		db	"Path not found"
newline		db	13, 10
pnf_len		equ	$ - pnf
dta:
builder		equ	dta + 43
