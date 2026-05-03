CPU 8086

ORG 0100h

_start:
	mov	si, 81h
	mov	di, 80h
	mov	[si + 128], byte 13
	call	consumespace
	cmp	al, 13
	je	usage
	cmp	[si], word "/?"
	je	usage
	mov	dx, di
	call	readarg
	cmp	al, 13
	je	usage
	call	consumespace1
	cmp	al, 13
	je	usage
	mov	cx, di
	call	readarg
	mov	di, cx
	mov	ah, 56h
	int	21h
	jc	error
	ret
usage:
	mov	dx, usage_msg
	mov	ah, 9
	int	21h
	mov	ax, 4C03h
	int	21h
error:
	xchg	ax, bp
	xor	bx, bx
	mov	ah, 59h
	int	21h
	jnc	.dec
	xchg	ax, bp
.dec	mov	si, errortbl - 4
.esn	add	si, 4
	mov	cx, [si]
	cmp	cx, ax
	je	.emm
	test	cx, cx
	jne	.esn
.emm	mov	dx, [si + 2]
	mov	ah, 9
	int	21h
	mov	ax, 4C01h
	int	21h
	
consumespace:
	lodsb
consumespace1:
	cmp	al, ' '
	je	consumespace
	cmp	al, 9
	je	consumespace
	dec	si
	ret

readarg:
	lodsb
	cmp	al, '"'
	je	.quot
	cmp	al, ' '
	je	.end
	cmp	al, 9
	je	.end
	cmp	al, 13
	je	.end
	stosb
	jmp	readarg
.end	mov	[di], byte 0
	inc	di
	ret
.quot	lodsb
	cmp	al, '"'
	je	readarg
	cmp	al, 13
	je	.end
	stosb
	jmp	.quot

align	4, db 0
errortbl	dw	1, .func
		dw	2, .fnf
		dw	3, .pnf
		dw	5, .access
		dw	8, .mem
		dw	15, .idrive
		dw	16, .curdir
		dw	17, .notsame
		dw	19, .wprotect
		dw	23, .crc
		dw	25, .seek
		dw	26, .umedia
		dw	27, .sector
		dw	29, .write
		dw	30, .read
		dw	31, .general
		dw	32, .share
		dw	65, .access
		dw	80, .already
		dw	82, .dent
		dw	84, .fail
		dw	0, .u

.func		db	"Invalid function", 13, 10, '$'
.fnf		db	"File not found", 13, 10, '$'
.pnf		db	"Path not found", 13, 10, '$'
.access		db	"Access denied", 13, 10, '$'
.mem		db	"Insufficient memory", 13, 10, '$'
.idrive		db	"Invalid drive", 13, 10, '$'
.curdir		db	"Attempt to rename current directory", 13, 10, '$'
.notsame	db	"Not same device", 13, 10, '$'
.wprotect	db	"Attempted to write on write-protected disk", 13, 10, '$'
.crc		db	"CRC error", 13, 10, '$'
.seek		db	"Seek error", 13, 10, '$'
.umedia		db	"Unknown media type", 13, 10, '$'
.sector		db	"Sector not found", 13, 10, '$'
.write		db	"Write fault", 13, 10, '$'
.read		db	"Read fault", 13, 10, '$'
.general	db	"General failure", 13, 10, '$'
.share		db	"Sharing violation", 13, 10, '$'
.already	db	"File already exists", 13, 10, '$'
.dent		db	"Cannot make directory entry", 13, 10, '$'
.fail		db	"Fail on INT 24h", 13, 10, '$'
.u		db	"Error", 13, 10, '$'

usage_msg	db	"RENAME1.COM: Copyright (C) Joshua Hudson 2026", 13, 10
		db	"This program is used to handle duplicate file names or file names w/ spaces.", 13, 10
		db	'Usage: RENAME1 ["]sourcepath["] ["]destpath["]', 13, 10, '$'
