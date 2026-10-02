CPU 8086

ORG 0100h

infile		equ	72h
outfile		equ	74h
inhandle	equ	76h
outhandle	equ	78h
attrs		equ	7Ah
date		equ	7Ch
time		equ	7Eh

_start:
	mov	bl, [80h]
	mov	bh, 0
	mov	si, 81h
	mov	[si + bx], byte 13	; Stopper
	cld
.spc1	lodsb
	cmp	al, 13
	je	.usage
	cmp	al, 8
	je	.spc1
	cmp	al, 32
	je	.spc1
	dec	si
	mov	[infile], si
.arg1	lodsb
	cmp	al, 13
	je	.usage
	cmp	al, 8
	je	.arg1e
	cmp	al, 32
	jne	.arg1
.arg1e	mov	[si - 1], byte 0
.spc2	lodsb
	cmp	al, 8
	je	.spc2
	cmp	al, 32
	je	.spc2
	dec	si
	mov	[outfile], si
.arg2	lodsb
	cmp	al, 13
	je	.arg2g
	cmp	al, 8
	je	.arg2e
	cmp	al, 32
	jne	.arg2
.arg2e	mov	[si - 1], byte 0
.spc3	lodsb
	cmp	al, 13
	je	.go
	cmp	al, 8
	je	.spc3
	cmp	al, 32
	je	.spc3
.usage	mov	cx, usagelen
	mov	dx, usage
	jmp	errortxtout
.arg2g	mov	[si - 1], byte 0
.go	mov	bx, 32768
	lea	ax, [bx + _end + 128]
	cmp	sp, bx
	jae	open
	shr	bx, 1
	cmp	bx, 512
	ja	.go
	mov	cx, ramlen
	mov	dx, ram
errortxtout:
	mov	bx, 2
	call	textout
	mov	ax, 4C01h
	int	21h

textout:
	mov	ah, 40h
	int	21h
	jc	.out
	add	dx, ax
	sub	cx, ax
	ja	textout
.out	ret

errorbase:
	xchg	ax, bp
	mov	ah, 59h
	int	21h
	jnc	.h59
	xchg	ax, bp
.h59	push	ax
	mov	si, dx
.loop	lodsb
	cmp	al, byte 0
	jne	.loop
	mov	cx, si
	dec	cx
	sub	cx, dx
	mov	bx, 2
	call	textout
	mov	dx, colonspace
	mov	cx, 2
	call	textout
	pop	ax
	mov	si, errortable - 4
.loop2	add	si, 4
	mov	ah, [si]
	cmp	ah, al
	jne	.code
	cmp	ah, 0
	jne	.loop2
.code	mov	cl, [si + 1]
	mov	ch, 0
	mov	dx, [si + 2]
	call	textout
	mov	ax, 4C01h
	int	21h

errorin:
	mov	dx, [infile]
	jmp	errorbase

errorout:
	mov	dx, [outfile]
errorbasev:
	jmp	errorbase

open:
	mov	di, bx
	mov	dx, [infile]
	mov	ax, 3D00h
	int	21h
	jc	errorbasev
	mov	[inhandle], ax
	mov	ax, 4300h
	int	21h
	jc	errorbasev
	mov	[attrs], cl
	mov	bx, [inhandle]
	mov	ax, 5700h
	int	21h
	jc	errorin
	mov	[date], dx
	mov	[time], cx
	mov	dx, [outfile]
	mov	cl, [attrs]
	mov	ch, 0
	mov	ah, 3Ch
	int	21h
	jc	errorbasev
	mov	[outhandle], ax
.loop	mov	bx, [inhandle]
	mov	dx, _end
	mov	cx, di
	mov	ah, 3Fh
	int	21h
	jc	errorin
	test	ax, ax
	jz	.end
	mov	bx, [outhandle]
	xchg	ax, cx
	mov	dx, _end
.loop2	mov	ah, 40h
	int	21h
	jc	errorout
	add	dx, ax
	sub	cx, ax
	jnz	.loop2
	jmp	.loop
.end	mov	dx, [date]
	mov	cx, [time]
	mov	bx, [outhandle]
	mov	ax, 5701h
	int	21h
	jc	errorout
	mov	ah, 3Eh
	int	21h
	jc	errorout
	mov	ax, 4C00h
	int	21h
	align 4, db 0CCh
errortable:
	db	2, fnflen
	dw	fnf
	db	3, pnflen
	dw	pnf
	db	4, nohandles
	dw	nohandles
	db	5, accesslen
	dw	access
	db	8, ramlen
	dw	ram
	db	0Fh, idrivelen
	dw	idrive
	db	13h, wprotectlen
	dw	wprotect
	db	15h, dnrlen
	dw	dnr
	db	17h, crclen
	dw	crc
	db	19h, seeklen
	dw	seek
	db	1Ah, umedialen
	dw	umedia
	db	1Bh, nsectorlen
	dw	nsector
	db	1Dh, wfaultlen
	dw	wfault
	db	1Eh, rfaultlen
	dw	rfault
	db	1Fh, gfailurelen
	dw	gfailure
	db	22h, ichangelen
	dw	ichange
	db	41h, accesslen
	dw	access
	db	53h, faillen
	dw	fail
	db	0, errorlen
	dw	error
fnf		db	"File not found", 13, 10
fnflen		equ	$ - fnf
pnf		db	"Path not found", 13, 10
pnflen		equ	$ - pnf
nohandles	db	"No more handles", 13, 10
nohandleslen	equ $ - nohandles
ram		db	"Out of memory", 13, 10
ramlen		equ $ - ram
idrive		db	"Invalid drive", 13, 10
idrivelen	equ $ - idrive
wprotect	db	"Write protected media", 13, 10
wprotectlen	equ $ - wprotect
dnr		db	"Drive not ready", 13, 10
dnrlen		equ $ - dnr
crc		db	"CRC error", 13, 10
crclen		equ $ - crc
seek		db	"Seek error", 13, 10
seeklen		equ $ - seek
umedia		db	"Uknown media type", 13, 10
umedialen	equ $ - umedia
nsector		db	"Sector not found", 13, 10
nsectorlen	equ $ - nsector
wfault		db	"Write fault", 13, 10
wfaultlen	equ $ - wfault
rfault		db	"Read fault", 13, 10
rfaultlen	equ $ - rfault
gfailure	db	"General failure", 13, 10
gfailurelen	equ $ - gfailure
ichange		db	"Invalid disk change", 13, 10
ichangelen	equ $ - ichange
access		db	"Access denied", 13, 10
accesslen	equ $ - access
fail		db	"Fail on INT 24", 13, 10
faillen		equ $ - fail
error		db	"Error", 13, 10
errorlen	equ $ - error
usage		db	"ACOPY.COM primordial file copy utility, with attributes", 13, 10
usage2		db	"Usage: ACOPY source destination", 13, 10
usagelen	equ $ - usage
colonspace	equ	usage2 + 5
_end:
