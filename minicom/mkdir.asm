CPU 8086

ORG 0100h

_start:
	mov	si, 81h
	mov	bl, [80h]
	mov	bh, 0
	mov	[si + bx], byte 0
.loop	lodsb
	cmp	al, 9
	je	.loop
	cmp	al, 32
	je	.loop
	cmp	al, 0
	je	.u
	dec	si
	mov	dx, si
	mov	ah, 39h
	int	21h
	jc	.e
	mov	ax, 4C00h
	int	21h
.u	mov	dx, usage
	mov	ah, 9
	int	21h
	mov	ax, 4C01h
	int	21h
.e	xchg	ax, bp
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
	ret

align	4, db 0
errortbl	dw	1, .func
		dw	3, .pnf
		dw	5, .access
		dw	8, .mem
		dw	15, .idrive
		dw	19, .wprotect
		dw	23, .seek
		dw	25, .seek
		dw	26, .umedia
		dw	27, .sector
		dw	29, .write
		dw	30, .read
		dw	31, .general
		dw	80, .already
		dw	82, .dent
		dw	84, .fail
		dw	0, .u
.func		db	"Invalid function", 13, 10, '$'
.pnf		db	"Path not found", 13, 10, '$'
.access		db	"Access denied", 13, 10, '$'
.mem		db	"Insufficient memory", 13, 10, '$'
.idrive		db	"Invalid drive", 13, 10, '$'
.wprotect	db	"Attempted tow rite on write-protected disk", 13, 10, '$'
.crc		db	"CRC error", 13, 10, '$'
.seek		db	"Seek error", 13, 10, '$'
.umedia		db	"Unknown media type", 13, 10, '$'
.sector		db	"Sector not found", 13, 10, '$'
.write		db	"Write fault", 13, 10, '$'
.read		db	"Read fault", 13, 10, '$'
.general	db	"General failure", 13, 10, '$'
.already	db	"File already exists", 13, 10, '$'
.dent		db	"Cannot make directory entry", 13, 10, '$'
.fail		db	"Fail on INT 24h", 13, 10, '$'
.u		db	"Error", 13, 10, '$'
usage		db	"Usage: MKDIR directory", 13, 10, '$'
