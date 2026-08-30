; Minicom: reduced command.com for tiny memory usage

CPU 8086

ORG 0100h

env		equ	2Ch	; Anybody inspecting RAM finds a usable ENV pointer
savedsp		equ	82h
savedseglen	equ	84h
envlen		equ	86h
datetimefmt	equ	88h
datesep		equ	8Ah
timesep		equ	8Ch
decsep		equ	8Eh
axptr		equ	90h
prglen		equ	94h
scratch		equ	96h
breaks		equ	97h
div0		equ	98h
iinstr		equ	9Ch
arg_envseg	equ	0A0h
arg_tailoff	equ	0A2h
arg_tailseg	equ	0A4h
arg_fcb1off	equ	0A6h
arg_fcb1seg	equ	0A8h
arg_fcb2off	equ	0AAh
arg_fcb2seg	equ	0ACh
savedsegbase	equ	0AEh
scratch2	equ	0B0h

_start:
	cld
	mov	sp, 100h + _end - _start + ((_bssend - _bss + 15) & 0FFF0h) + 128
	mov	[savedsp], sp
	mov	ax, sp
	mov	cl, 4
	shr	ax, cl
	mov	cx, cs
	add	ax, cx
	mov	es, ax
	xor	di, di
	xor	si, si
	push	ds
	mov	ax, [env]
	cmp	ax, 0
	je	.noenv
	cmp	ax, 0FFFFh
	je	.noenv
	mov	ds, ax
.cpenvl	push	di
	push	es
	push	cs
	pop	es
	mov	di, cmdline_name
	call	matchenv
	pop	es
	pop	di
	jnz	.cpenv
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
	mov	[env], es
	mov	[arg_envseg], es
	push	ds
	pop	es
	mov	[envlen], di
	mov	bx, di
	add	bx, sp		; Won't exceed 64k; nobody's passing ENV that big
	add	bx, 15
	mov	cl, 4
	shr	bx, cl
	mov	ah, 4Ah
	int	21h

	mov	[80h], word 0D00h
	mov	[kbufsize], byte 253
	mov	[arg_tailoff], word kbufread
	mov	[arg_tailseg], ds
	mov	[arg_fcb1off], word 5Ch
	mov	[arg_fcb1seg], ds
	mov	[arg_fcb2off], word 6Ch
	mov	[arg_fcb2seg], ds

	; Add DOS and CPU handlers
	push	es
	xor	ax, ax
	mov	es, ax
	mov	ax, [es:0]
	mov	dx, [es:2]
	mov	bx, [es:24]
	mov	cx, [es:26]
	mov	[div0], ax
	mov	[div0 + 2], dx
	mov	[iinstr], bx
	mov	[iinstr + 2], cx
	call	hookcpu
	mov	[es:8Ch], word ctrlchandler
	mov	[es:8Eh], cs
	mov	[es:90h], word criticalhandler
	mov	[es:92h], cs
	pop	es

	; Load failure data in case COUNTRY.SYS not loaded
	mov	[exbuffer], byte 0
	mov	[exbuffer + 09h], word '.'
	mov	[exbuffer + 0Bh], word '/'
	mov	[exbuffer + 0Dh], word ':'
	mov	[exbuffer + 011h], byte 1
	mov	dx, exbuffer
	mov	ax, 3800h
	int	21h
	mov	al, [exbuffer]		; 0 = mmddyyyy 1 = ddmmyyyy 2 = yyyymmdd
	mov	ah, [exbuffer + 11h]	; 0 = 12 hour 1 = 24 hour
	mov	bx, [exbuffer + 0Bh]	; date separator
	mov	cx, [exbuffer + 0Dh]	; time separator
	mov	dx, [exbuffer + 09h]	; decimal separator (07h = thousands but we don't use)
	mov	[datetimefmt], ax
	mov	[datesep], bx
	mov	[timesep], cx
	mov	[decsep], dx

	mov	di, exbuffer
	call	datecore
	mov	al, ' '
	stosb
	call	timecore
	mov	ax, 0A0Dh
	stosw
	mov	cx, di
	call	outtxt

	cli
	; Free initializer memory
	push	ds
	mov	ax, ds
	dec	ax
	mov	ds, ax
	mov	ch, [0]
	mov	bx, [1]
	mov	dx, [3]
	mov	[0], byte 'M'
	mov	[3], word 10h
	sub	dx, 11h
	add	ax, 11h
	mov	es, ax
	mov	[es:0], byte 'M'
	mov	[es:1], word 0
	mov	[es:3], word (freeinitializerfinal - _start) / 16 - 1
	add	ax, (freeinitializerfinal - _start) / 16
	sub	dx, (freeinitializerfinal - _start) / 16
	mov	es, ax		; Now points to  freeinitializerfinal
	mov	[es:0], ch
	mov	[es:1], bx
	mov	[es:3], dx
	inc	ax
	mov	[savedsegbase + 10h], ax
	mov	[savedseglen + 10h], dx
	mov	si, 8
	mov	di, 8
	movsw
	movsw
	movsw
	movsw
	pop	ds
	push	ds
	pop	es
	jmp	freeinitializerfinal.hole
cmdline_name	db	"CMDLINE="
	align	16, db 0
freeinitializerfinal:
.mark	db	'M'
.owner	dw	0
.len	dw	0
.hole	sti
	jmp	short commandloop
.name	dw	"MINICOM "
commandloop:
	cld
	mov	[breaks], byte 0
	mov	ah, 19h
	int	21h
	mov	dl, al
	add	al, 'A'
	mov	[exbuffer], al
	mov	[exbuffer + 1], word ':\'
	inc	dl
	mov	si, exbuffer + 3
	mov	ah, 47h
	int	21h
.cwds	lodsb
	cmp	al, 0
	jne	.cwds
	mov	[si - 1], byte '>'
	mov	cx, si
	call	outtxt

	call	getbuffer	; Get command
	jc	commandloop	; This is me hoping

	; parse command
	mov	di, 5Ch
	mov	ax, "??"
	xor	ax, ax
	mov	cx, 10h
	rep	stosw
	mov	[5Ch], byte 0
	mov	[6Ch], byte 0
	mov	si, kbuffer
	mov	di, exbuffer
.loop0	lodsb
	cmp	al, ' '
	je	.loop0
	cmp	al, 9
	je	.loop0
	cmp	al, 13
	je	.nocmd
	cmp	al, 10
	je	.nocmd
	dec	si
.loop1	lodsb
	stosb
	cmp	al, 0
	je	.arg1
	cmp	al, ' '
	je	.arg1
	cmp	al, 9
	jne	.loop1
.arg1	mov	[di - 1], byte 0
	dec	si
	mov	[axptr], si

	mov	si, exbuffer
	mov	di, set_name
	call	strccmp
	jne	.nset
	jmp	set_builtin
.nocmd	jmp	commandloop		; displaced jmp vector
.nset	mov	di, cd_name
	call	strccmp
	jne	.ncd
	jmp	cd_builtin
.ncd	mov	di, loadfix_name
	call	strccmp
	jne	.nlfix
	jmp	loadfix_builtin
.nlfix	mov	di, date_name
	call	strccmp
	jne	.ndate
	jmp	date_builtin
.ndate	mov	di, time_name
	call	strccmp
	jne	.ntime
	jmp	time_builtin
.ntime	mov	di, exit_name
	call	strccmp
	jne	.nexit
	jmp	exit_builtin
.nexit	cmp	[si + 1], word ":"
	jne	program_run
	cmp	[si + 2], byte 0
	jne	program_run
	mov	dl, [si]
	and	dl, 0DFh
	sub	dl, 'A'
	jc	.idrive
	cmp	dl, 26
	jae	.idrive
	mov	ah, 0Eh
	int	21h
	jmp	.gbak
.idrive	mov	ax, 15
	stc
.gbak	jmp	chkdoserror_return

program_run:
	mov	ah, 0
	mov	si, exbuffer
.loop0	lodsb
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
	jmp	.loop0
.d	mov	ah, 2
	jmp	.loop0
.s	or	ah, 1
	jmp	.loop0
.prg2	mov	[scratch], ah
	call	probe_extension
	jnc	.go
	test	[scratch], byte 2
	jnz	.nogo
	push	si
	mov	si, path_name
	call	findenv
	pop	si
	add	di, 5		; PATH=
	test	cx, cx
	jz	.nogo
.loop1	cmp	[es:di], byte 0
	je	.nogo
	call	injectpath
	call	probe_extension
	jnc	.go
	call	removepath
	jmp	.loop1
.nogo	mov	dx, nocmd
	mov	ah, 9
	int	21h
	clc
	jmp	commandloopreturn

.go	push	ds
	pop	es
	mov	si, [axptr]
	push	si
	cmp	[si], byte 0
	je	.nofcb
.loop2	lodsb		; Parse FCBs if any
	cmp	al, 0
	je	.nofcb
	cmp	al, ' '
	je	.loop2
	cmp	al, 9
	je	.loop2
	dec	si
	mov	di, 5Ch
	mov	ax, 2900h
	int	21h
	jc	.nofcb
.loop3	lodsb
	cmp	al, 0
	je	.nofcb
	cmp	al, ' '
	je	.loop3
	cmp	al, 9
	je	.loop3
	dec	si
	mov	di, 6Ch
	mov	ax, 2900h
	int	21h
.nofcb	pop	si
	mov	di, kbuffer
	xor	cx, cx
.loop4	lodsb
	stosb
	inc	cx
	cmp	al, 0
	jne	.loop4
	dec	cx
	mov	[kbufread], cl
	mov	dx, exbuffer
	mov	bx, arg_envseg
	mov	ax, 4B00h
	int	21h
commandloopreturn:
	cli
	mov	bx, cs
	mov	ds, bx
	mov	es, bx
	mov	ss, bx
	mov	sp, [savedsp]
	sti
	pushf
	mov	bx, [savedseglen]
	call	setseglen
	popf
chkdoserror_return:
	jnc	.noerr
	cmp	[breaks], byte 0
	jne	.noerr		; Abort from load program
	xchg	ax, bp
	mov	ah, 59h
	int	21h
	jnc	.dec
	xchg	ax, bp
.dec	mov	si, errortable - 4
.esn	add	si, 4
	mov	cx, [si]
	cmp	cx, ax
	je	.err
	test	cx, cx
	jnz	.esn
.err	mov	dx, [si + 2]
	mov	ah, 9
	int	21h
.noerr	jmp	commandloop

injectpath:
	push	di
	xor	dx, dx		; how many bytes to copy from PATH
	mov	ah, 0
.loop1	mov	al, [es:di]
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
	push	es
	push	ds
	pop	es
	dec	si
	dec	di
	rep	movsb
	pop	es
	cld
	inc	di
	mov	bp, di		; Preserved start of file name
	mov	cx, dx
	add	di, bx
	pop	si
	push	di		; Preserved end of file name
	mov	di, exbuffer
	push	ds		; Swap DS,ES
	push	es
	pop	ds
	pop	es
	rep	movsb
	push	ds		; Swap back
	push	es
	pop	ds
	pop	es
	mov	[di - 1], byte '\'
	pop	si		; Preserved end of file name
	mov	di, [scratch2]
	ret

removepath:
	push	es
	push	ds
	pop	es
	push	di
	mov	cx, si
	sub	cx, bp
	mov	di, exbuffer
	mov	si, bp
	rep	movsb
	mov	si, di
	pop	di
	pop	es
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

getbuffer:
	mov	dx, kbufsize
	mov	ah, 0Ah
	int	21h
	pushf
	mov	dx, newline
	mov	ah, 9
	int	21h
	popf
	jc	.ret
	mov	bl, [kbufread]
	mov	bh, 0
.loop	test	bx, bx
	jz	.nloop
	dec	bx
	cmp	[kbuffer + bx], byte 13
	je	.loop
	cmp	[kbuffer + bx], byte 10
	je	.loop
	inc	bx
.nloop	mov	[kbuffer + bx], byte 0
	clc
.ret	ret

outtxt:
	mov	dx, exbuffer
	sub	cx, dx
outtxtptr:
	mov	bx, 1
.loop	mov	ah, 40h
	int	21h
	jc	.oops
	add	dx, ax
	sub	cx, ax
	jnz	.loop
.oops	ret

strccmp:
	push	si
.loop	lodsb
	and	al, 0DFh
	cmp	al, [di]
	jne	.ret
	inc	di
	cmp	al, 0
	jne	.loop
.ret	pop	si
	ret

hookcpu:
	mov	[es:0], word div0handler
	mov	[es:2], cs
	mov	[es:24], word invalidinsthandler
	mov	[es:26], cs
	ret

	align	2, db 0
errortable:
	dw	1, .func
	dw	2, .fnf
	dw	3, .pnf
	dw	5, .access
	dw	7, .mcb
	dw	8, .mem
	dw	10, .ienv
	dw	11, .dnf
	dw	13, .idata
	dw	15, .idrive
	dw	19, .write
	dw	21, .dnr
	dw	23, .crc
	dw	24, .len
	dw	25, .seek
	dw	27, .snf
	dw	28, .oop
	dw	29, .wfault
	dw	30, .rfault
	dw	31, .gen
	dw	32, .share
	dw	33, .lock
	dw	34, .idisk
	dw	36, .share2
	dw	83, .fail
	dw	0, .u
.critical:
	dw	.write
	dw	.idrive
	dw	.dnr
	dw	.func
	dw	.crc
	dw	.len
	dw	.seek
	dw	.dnf
	dw	.snf
	dw	.oop
	dw	.wfault
	dw	.rfault
	dw	.gen
	dw	.u
	dw	.u
	dw	.idisk
.func	db	"Invalid function", 13, 0, '$'
.fnf	db	"File not found", 13, 10, '$'
.pnf	db	"Path not found", 13, 10, '$'
.access	db	"Access denied", 13, 10, '$'
.mcb	db	"Memory control blocks destroyed", 13, 10, '$'
.mem	db	"Out of memory", 13, 10, '$'
.ienv	db	"Invalid environment", 13, 10, '$'
.dnf	db	"Invalid format", 13, 10, '$'
.idata	db	"Invalid data", 13, 10, '$'
.idrive	db	"Invalid drive", 13, 10, '$'
.write	db	"Write protected media", 13, 10, '$'
.dnr	db	"Drive not ready", 13, 10, '$'
.crc	db	"CRC Error", 13, 10, '$'
.len	db	"Incorrect length", 13, 10, '$'
.seek	db	"Seek error", 13, 10, '$'
.snf	db	"Sector not found", 13, 10, '$'
.oop	db	"Out of paper", 13, 10, '$'
.wfault	db	"Write fault", 13, 10, '$'
.rfault	db	"Read fault", 13, 10, '$'
.gen	db	"General failure", 13, 10, '$'
.share	db	"Sharing violation", 13, 10, '$'
.lock	db	"File is locked", 13, 10, '$'
.idisk	db	"Inappropriate disk change", 13, 10, '$'
.share2	db	"Sharing buffer overflow", 13, 10, '$'
.fail	db	"Fail on INT24", 10, 10, '$'
.u	db	"Error", 13, 10, '$'

path_name	db	"PATH="
invdrive	db	"Invalid drive", 13, 10, "$"
nocmd		db	"Bad command or file name", 13, 10, "$"
divzero		db	"Division by 0", 13, 10, "$"
noinst		db	"Invalid instruction "
ctrlc		db	"^C"
newline		db	13, 10, "$"
arif		db	"a)bort, r)etry, i)gnore, f)ail:$"
arf		db	"a)bort, r)etry, f)ail:$"

criticalhandler:
	push	ds
	push	dx
	push	bx
	push	bp
	push	cs
	pop	ds
	mov	[breaks], ah
	mov	bx, di
	and	bx, 15
	mov	dx, [bx + errortable.critical]
	mov	ah, 9
	int	21h
	mov	dx, arif
	test	ah, byte 32
	jz	.noi
	mov	dx, arf
.noi	mov	ah, 9
	int	21h
	mov	ah, 51h	; Put current PSP segment into BX
	int	21h
.key	mov	ah, 1
	int	21h
	cmp	al, 'a'
	je	.a
	cmp	al, 'r'
	je	.r
	cmp	al, 'i'
	je	.i
	cmp	al, 'f'
	jne	.key
.a	mov	dx, cs
	cmp	dx, bx	; Go back to command loop
	je	.f
	mov	al, 2
	jmp	.x
.r	mov	al, 1
	jmp	.x
.i	mov	al, 0
	jmp	.x
.f	mov	al, 3
.x	mov	ah, [breaks]
	pop	bp
	pop	bx
	pop	dx
	pop	ds
	iret

ctrlchandler:
	mov	dx, ctrlc
	jmp	commonhandler
div0handler:
	mov	dx, divzero
	jmp	commonhandler
invalidinsthandler:
	push	cs
	pop	ds
	push	cs
	pop	es
	mov	si, noinst
	mov	di, exbuffer
	mov	cx, ctrlc - noinst
	rep	movsb
	mov	bp, sp
	mov	ax, [bp + 8]
	call	hex4
	mov	ax, [bp + 10]
	mov	al, ':'
	stosb
	call	hex4
	mov	al, ' '
	stosb
	mov	bx, [bp + 6]
	mov	ds, [bp + 8]
	mov	ah, [ds:bx]
	push	cs
	pop	ds
	call	hex2
	mov	si, newline
	stosw
	stosb
	mov	dx, exbuffer
commonhandler:
	push	cs
	pop	ds
	mov	ah, 9
	int	21h
	mov	ah, 51h
	int	21h
	mov	ax, cs
	cmp	ax, bx
	je	.ret
	mov	ax, 4C03h
	int	21h
.ret	mov	bp, sp
	mov	[bp + 6], word commandloopreturn
	mov	[bp + 8], cs
	and	[bp + 10], byte 0FEh	; Clear carry flag
	iret
hex4:	mov	ch, 4
	jmp	hexany
hex2:	mov	ch, 2
hexany:	mov	cl, 4
.loop	rol	ax, cl
	push	ax
	and	al, 15
	add	al, '0'
	cmp	al, '9'
	jbe	.nine
	add	al, 'A' - '0' - 10
.nine	stosb
	pop	ax
	dec	ch
	jnz	.loop
	ret

; Builtins:
; SET
; CD
; LOADFIX
; DATE
; TIME
; EXIT

set_name	db	"SET", 0
set_builtin:
	mov	si, [axptr]
.pfl	lodsb
	cmp	al, ' '
	je	.pfl
	cmp	al, 9
	je	.pfl
	cmp	al, 0
	je	.ns
	dec	si
	mov	[axptr], si
.pfx	lodsb
	cmp	al, 0
	je	.ns
	cmp	al, '='
	jne	.pfx
	mov	ah, [si]
	mov	[scratch], ah
	jmp	set_envvalue
.sfx	lodsb
	cmp	al, 0
	jne	.sfx
	jmp	set_envvalue
.ns	xor	di, di
	push	es
	mov	es, [env]
.scan	mov	dx, di
	mov	cx, -1
	mov	al, 0
	repne	scasb
	mov	cx, di
	sub	cx, dx
	dec	cx
	push	ds
	push	es
	pop	ds
	call	outtxtptr
	pop	ds
	mov	dx, newline
	mov	ah, 9
	int	21h
	cmp	[es:di], byte 0
	jne	.scan
	pop	es
	jmp	commandloop

set_envvalue:
	mov	si, [axptr]
	call	findenv
	test	cx, cx
	jz	.nrepl
	mov	dx, [envlen]
	push	ds
	push	es
	pop	ds
	push	si
	push	cx
	mov	si, di
	add	si, cx
	mov	cx, dx
	sub	cx, si
	rep	movsb
	pop	cx
	pop	si
	pop	ds
	sub	dx, cx
	mov	[envlen], dx
	call	setenvlen
.nrepl	cmp	[scratch], byte 0
	je	.nset
	xor	dx, dx
	push	si
.lloop	lodsb
	inc	dx
	cmp	al, 0
	jnz	.lloop
	pop	si
	push	dx
	add	dx, [envlen]
	call	setenvlen
	jc	.nope
	pop	dx
	mov	di, [envlen]
	dec	di
	mov	cx, dx
	rep	movsb
	mov	al, 0
	stosb
	mov	[envlen], di
.nset	jmp	commandloopreturn
.nope	mov	dx, errortable.mem
	mov	ah, 9
	int	21h
	jmp	.nset	; Surprise! Doesn't leave anything on stack after all

setenvlen:
	add	dx, 15
	mov	cl, 4
	shr	dx, cl
	mov	bx, ((_end - commandloop) + (_bssend - _bss) + 128) / 16
	add	bx, dx
	mov	[savedseglen], bx
setseglen:
	push	es
	mov	es, [savedsegbase]
	mov	ah, 4Ah
	int	21h
	pop	es
	ret

findenv:
	mov	es, [env]
	xor	di, di
	xor	cx, cx
.loop0	call	matchenv
	jz	.found
.loop1	inc	di
	cmp	[es:di], byte 0
	jne	.loop1
.eoa	inc	di
	cmp	[es:di], byte 0
	jne	.loop0
	ret
.found	push	di
	inc	cx
.loop2	inc	cx
	inc	di
	cmp	[es:di], byte 0
	jne	.loop2
	pop	di
	ret

matchenv:
	push	si
	push	di
.loop	cmpsb
	jne	.ret
	cmp	[si - 1], byte '='
	jne	.loop
.ret	pop	di
	pop	si
	ret

cd_name		db	"CD", 0
cd_builtin:
	mov	si, [axptr]
.pfl	lodsb
	cmp	al, ' '
	je	.pfl
	cmp	al, 9
	je	.pfl
	cmp	al, 0
	je	.ns
	lea	dx, [si - 1]
	mov	ah, 3Bh
	int	21h
	jmp	chkdoserror_return
.ns	jmp	commandloop		; May decide this does something later

loadfix_name	db	"LOADFIX", 0
loadfix_builtin:
	mov	si, [axptr]
	mov	di, exbuffer
.loop1	lodsb
	cmp	al, 0
	je	short .oops
	cmp	al, ' '
	je	.loop1
	cmp	al, 9
	je	.loop1
.loop2	stosb
	lodsb
	cmp	al, 0
	je	.mark0
	cmp	al, ' '
	je	.mark
	cmp	al, 9
	jne	.loop2
.mark	mov	al, 0
.mark0	stosb
	dec	si
	mov	[axptr], si

	mov	dx, cs
	add	dx, (commandloop - _start + 100h) / 16
	add	dx, [freeinitializerfinal.len]		; dx = seg address of end
	mov	ax, dx
	and	ax, 0FFFh
	mov	bx, 0FFFh
	sub	bx, ax
	add	bx, [freeinitializerfinal.len]
	call	setseglen
	jmp	program_run
.oops	jmp	commandloop

date_name	db	"DATE", 0
date_builtin:
	mov	si, cdate
	mov	di, exbuffer
	mov	cx, cdate_end - cdate
	rep	movsb
	call	datecore
	mov	si, edate
	mov	cx, edate_end - edate
	rep	movsb
	cmp	[datetimefmt], byte 1
	ja	.ymd1
	je	.dmy1
.mdy1	call	.m1
	call	datesep1
	call	.d1
	call	datesep1
	call	.y1
	jmp	.pp1
.dmy1	call	.d1
	call	datesep1
	call	.m1
	call	datesep1
	call	.y1
	jmp	.pp1
.ymd1	call	.y1
	call	datesep1
	call	.m1
	call	datesep1
	call	.d1
.pp1	mov	ax, "):"
	stosw
	mov	cx, di
	call	outtxt
	call	getbuffer
	jc	.no
	mov	si, kbuffer
	cmp	[datetimefmt], byte 1
	ja	.ymd2
	je	.dmy2
.mdy2	call	number
	jc	.no
	cmp	ah, 0
	jnz	.no
	mov	dh, al
	call	datesep2
	jc	.no
	call	number
	jc	.no
	cmp	ah, 0
	jnz	.no
	mov	dl, al
	call	datesep2
	jc	.no
	call	number
	jc	.no
	mov	cx, ax
	jmp	.yes
.no	jmp	commandloop
.dmy2	call	number
	jc	.no
	cmp	ah, 0
	jnz	.no
	mov	dl, al
	call	datesep2
	jc	.no
	call	number
	jc	.no
	cmp	ah, 0
	jnz	.no
	mov	dh, al
	call	datesep2
	jc	.no
	call	number
	jc	.no
	mov	cx, ax
	jmp	.yes
.ymd2	call	number
	jc	.no
	mov	cx, ax
	call	datesep2
	jc	.no
	call	number
	jc	.no
	cmp	ah, 0
	jnz	.no
	mov	dh, al
	call	datesep2
	jc	.no
	call	number
	jc	.no
	cmp	ah, 0
	jnz	.no
	mov	dl, al
.yes	cmp	cx, 100
	jae	.nx2
	add	cx, 2000
.nx2	mov	ah, 2Bh
	int	21h
	jmp	chkdoserror_return

.y1	mov	si, ystr
	mov	cx, 3
	rep	movsw
	ret
.m1	mov	ax, "mm"
	stosw
	ret
.d1	mov	ax, "dd"
	stosw
	ret

datesep2:
	mov	bx, [datesep]
xyzsep2:
	lodsb
	cmp	al, bl
	jne	.no
	cmp	bh, byte 0
	je	.yes
	cmp	[si], bh
	jne	.yes
	inc	si
.yes	clc
	ret
.no	stc
	ret

datesep1:
	mov	ax, [datesep]
xyzsep1:
	cmp	ah, 0
	jne	.sd
	stosb
	ret
.sd	stosw
	ret

number:
	xor	ax, ax
	mov	bl, 10
	cmp	[si], byte '0'
	jb	.no
	cmp	[si], byte '9'
	ja	.no
.loop	mov	bh, [si]
	sub	bh, '0'
	jc	.end
	cmp	bh, 9
	ja	.end
	; Reaches its maximum value in the year 2559; if DOS isn't long dead by then something's up.
	; All extant DOS can't handle years > 2107 due to the filesystem date format, which is exposed
	; to the API
	mul	bl
	add	al, bh	; We're out of registers
	adc	ah, 0
	inc	si
	jmp	.loop
.end	clc
	ret
.no	stc
	ret

datecore:
	mov	ah, 2Ah
	int	21h
	mov	ah, 0
	mov	si, weekdays
	shl	ax, 1
	shl	ax, 1
	add	si, ax
	movsw
	movsw
	cmp	[datetimefmt], byte 1
	ja	.ymd
	je	.dmy
.mdy	call	.m
	call	datesep1
	call	.d
	call	datesep1
	call	.y
	ret
.dmy	call	.d
	call	datesep1
	call	.m
	call	datesep1
	call	.y
	ret
.ymd	call	.y
	call	datesep1
	call	.m
	call	datesep1
	call	.d
	ret
.m	mov	al, dh
	jmp	num2
.d	mov	al, dl
	jmp	num2
.y	xchg	ax, cx
	mov	cx, 4
	mov	bx, 10
	add	di, 3
	std
.yl	xor	dx, dx
	div	bx
	xchg	ax, dx
	add	al, '0'
	stosb
	xchg	ax, dx
	loop	.yl
	add	di, 5
	cld
	ret

num2	mov	bl, 10
	mov	ah, 0
	div	bl
	add	ax, "00"
	stosb
	xchg	al, ah
	stosb
	ret

weekdays	db	"Sun Mon Tue Wed Thu Fri Sat "


cdate	db	"Current date is "
cdate_end:
edate	db	13, 10, "Enter new date ("
edate_end:
ystr	db	"[cc]yy"
	
; time returns lowercase am/pm
; Forget it I'm not dealing with am/pm ; too much work too little reward
time_name	db	"TIME", 0
time_builtin:
	mov	si, ctime
	mov	di, exbuffer
	mov	cx, ctime_end - ctime
	rep	movsb
	call	timecore
	mov	si, etime
	mov	cx, etime_end - etime
	rep	movsb
	call	.timesep1
	mov	ax, "mm"
	stosw
	mov	al, '['
	stosb
	call	.timesep1
	mov	ax, "ss"
	stosw
	mov	ax, "])"
	stosw
	mov	ax, ":$"
	stosw
	mov	dx, exbuffer
	mov	ah, 9
	int	21h
	call	getbuffer
	jc	.no
	mov	si, kbuffer
	call	number
	jc	.no
	cmp	ah, 0
	jne	.no
	mov	ch, al
	mov	bx, [timesep]
	call	xyzsep2
	jc	.no
	call	number
	cmp	ah, 0
	jne	.no
	mov	cl, al
	xor	dx, dx
	cmp	[si], byte 0
	je	.yes
	mov	bx, [timesep]
	call	xyzsep2
	jc	.no
	call	number
	cmp	ah, 0
	jne	.no
	mov	dh, al
.yes	mov	ah, 2Dh
	int	21h
	jmp	chkdoserror_return
.no	jmp	commandloop

.timesep1:
	mov	ax, [timesep]
.timesep2:
	cmp	ah, 0
	jne	.dbcs
	stosb
	jmp	.dcc
.dbcs	stosw
.dcc	ret

timecore:
	mov	ah, 2Ch
	int	21h
	mov	al, ch
	call	num2
	mov	ax, [timesep]
	call	xyzsep1
	mov	al, cl
	call	num2
	mov	ax, [timesep]
	call	xyzsep1
	mov	al, dh
	call	num2
	mov	ax, [decsep]
	call	xyzsep1
	mov	al, dl
	call	num2
	ret

ctime	db	"Current time is "
ctime_end:
etime	db	13, 10, "Enter new time (hh"
etime_end:

exit_name	db	"EXIT", 0
exit_builtin:
	xor	ax, ax
	mov	es, ax
	mov	ax, [div0]
	mov	dx, [div0 + 2]
	mov	[es:0], ax
	mov	[es:2], dx
	mov	ax, [iinstr]
	mov	dx, [iinstr + 2]
	mov	[es:24], ax
	mov	[es:26], dx
	mov	ax, 4C00h
	int	21h
	;If we are the system shell this can actually fall through.
	call	hookcpu
	clc
	jmp	commandloopreturn

; Needed external commands for a minimal working set
; ACOPY
; DIR
; MKDIR
; RENAME1
; RM

align 16, db 0

_end:

section .bss

_bss:
kbufsize		resb	1
kbufread		resb	1
kbuffer			resb	254
exbuffer		resb	128

_bssend:
