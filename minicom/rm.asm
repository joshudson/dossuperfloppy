CPU 8086

ORG 100h

;%define DEBUG

_start:
	mov	bl, [80h]
	cmp	bl, 126
	jb	.ncmd
	mov	ax, [2Ch]
	test	ax, ax
	jz	.ncmd
	cmp	ax, 0FFFFh
	je	.ncmd
	mov	es, ax			; Try CMDLINE variable in case PSP version is truncated
	xor	bx, bx
.chke	cmp	[es:bx], byte 0
	je	.ncmd2
	cmp	[es:bx], word "CM"
	jne	.nexte
	cmp	[es:bx + 2], word "DL"
	jne	.nexte
	cmp	[es:bx + 4], word "IN"
	jne	.nexte
	cmp	[es:bx + 6], word "E="
	jne	.nexte
	add	bx, 7
.ecmd	inc	bx		; Skip until arguments
	mov	al, [es:bx]
	cmp	al, 0
	je	.hcmd
	cmp	al, byte 32
	je	.hcmd
	cmp	al, byte 9
	je	.hcmd
	jmp	.ecmd
.nexte	inc	bx
	cmp	[es:bx], byte 0
	jne	.nexte
	inc	bx
	jmp	.chke
.ncmd2	push	ds
	pop	es
.ncmd	mov	bh, 0
	mov	bl, [128]
	mov	[bx + 129], byte 0
	mov	bx, 129
	; Command line is now in ES:BX
.hcmd	dec	bx
.hcmd2	inc	bx
	mov	al, [es:bx]
	cmp	al, 32
	je	.hcmd2
	cmp	al, 9
	je	.hcmd2
	cmp	[es:bx], word "/?"	; Pure kludge, but prevents accident
	je	.help			; If you get this you know what mistake you made (forgot to pass --)
					; It's unlikely to be script generated because most stuff uses \
					; for directory separater even though / and \ both work.
	cmp	[es:bx], word "-h"
	je	.help
	mov	ch, 0			; Generate flags into ch
	push	bx
	xor	bx, bx
	mov	ax, 4400h
	int	21h
	pop	bx
	jc	.noni			; stdin is closed or something like that
	test	dx, word 8000h
	jz	.defi			; well if you're going to pass a file that's fine w/ me
	test	dl, byte 4h
	jnz	.noni
	jmp	.defi
.help	mov	dx, oopscmd
	mov	cx, helplen
	mov	bx, 1
	call	outtxt
	mov	ax, 4C00h
	int	21h
.oops	mov	dx, oopscmd
	mov	cx, oopslen
	mov	bx, 2
	call	outtxt
	mov	ax, 4C01h
	int	21h
.noni	mov	ch, 80h
.defi	mov	al, [es:bx]
	cmp	al, byte '-'
	jne	.arg
.nxtc	inc	bx
	mov	al, [es:bx]
	cmp	al, 32
	je	.ospace
	cmp	al, 9
	je	.ospace
	cmp	al, '-'
	je	.nopts
	cmp	al, 'r'
	jne	.nr
	or	ch, flag_recursive
	jmp	.nxtc
.nr	cmp	al, 'i'
	jne	.ni
	or	ch, flag_interactive
	jmp	.nxtc
.ni	cmp	al, 'd'
	jne	.nd
	or	ch, flag_emptydir
	jmp	.nxtc
.nd	cmp	al, 'f'
	jne	.oops
	or	ch, flag_force
	jmp	.nxtc
.ospace	inc	bx
	mov	al, [es:bx]
	cmp	al, 32
	je	.ospace
	cmp	al, 9
	je	.ospace
	jmp	.defi
.nopts	inc	bx
	jmp	.pstarg
.arg	cmp	al, 0
	je	.oops
.arg2	mov	di, bx
.args	inc	di
	mov	al, [es:di]
	cmp	al, 0
	je	.argse2
	cmp	al, 32
	je	.argse
	cmp	al, 9
	jne	.args
.argse	mov	[es:di], byte 0
	inc	di
.argse2	push	di
	call	haveone
	pop	bx
.pstarg	mov	al, [es:bx]
	cmp	al, 0
	je	.exit
	cmp	al, 9
	je	.pstasp
	cmp	al, 32
	jne	.arg2
.pstasp	inc	bx
	jmp	.pstarg
.exit	mov	al, 0
	test	ch, flag_errorhasoccurred
	jz	.exit0
	mov	al, 1
.exit0	mov	ah, 4Ch
	int	21h

	; entry loop; copy ES:BX to DS:activepath; preserves ES
haveone:
	and	ch, byte ~(flag_pwildcard | flag_prccall)
	push	ds
	push	es
	pop	ds
	pop	es
	mov	si, bx
	mov	di, activepath
.copy	lodsb
	stosb
	cmp	al, '?'
	je	.cwild
	cmp	al, '*'
	jne	.nwild
.cwild	or	ch, byte flag_pwildcard
.nwild	cmp	al, 0
	jne	.copy
	push	ds
	push	es
	pop	ds
	pop	es

	dec	di	; Points to trailing NUL
	mov	ax, [activepath]
	cmp	ax, word '/'
	je	.top
	cmp	ax, word '\'
	je	.top
	cmp	ax, word ':'
	je	.top
	mov	ax, [activepath + 1]
	cmp	ax, word '/'
	je	.top
	cmp	ax, word '\'
	je	.top
	cmp	ax, word ':'
	jne	.path
.top	push	cx
	mov	dx, notroot
	mov	cx, notrootlen
.tcgen	call	txterror
	pop	cx
	or	ch, flag_errorhasoccurred
	ret
.current:
	push	cx
	mov	dx, notcurrent
	mov	cx, notcurrentlen
	jmp	.tcgen
.path	cmp	[di - 2], word ".."	; Depends on last character of program being ?
	je	.current
	cmp	[di - 2], word "\."
	je	.current
	cmp	[di - 2], word "/."
	je	.current
	cmp	[di - 2], word ":."
	je	.current
	cmp	[activepath], word "."
	je	.current
	test	ch, flag_pwildcard
	jz	removethis
.backup	cmp	di, activepath
	je	removewild
	cmp	[di - 1], byte '/'
	je	removewild
	cmp	[di - 1], byte '\'
	je	removewild
	dec	di
	jmp	.backup
removewild:
	lea	ax, [di + 256]
	cmp	ax, sp
	jbe	.sok
	or	ch, flag_errorhasoccurred
	push	cx
	mov	dx, stackoverflow
	mov	cx, stackoverflowlen
	call	txterror
	pop	cx
	ret
.sok	push	bp
	sub	sp, 2Ch
	mov	bp, sp
	mov	[bp + 2Bh], byte 0	; Nothing found yet
	mov	dx, bp
	mov	ah, 1Ah
	int	21h
	push	cx
	xor	bx, bx
	test	ch, flag_emptydir|flag_recursive
	jz	.flat
	mov	bl, 16
.flat	test	ch, flag_prccall
	mov	cx, bx
	jnz	.big
	or	cl, 3h
.big	mov	dx, activepath
	mov	ah, 4Eh
	int	21h
	pop	cx
	mov	cl, 0
	jc	.nope
.next	lea	si, [bp + 1Eh]
	cmp	[si], byte '.'
	je	.skip		; . or .. entry
	mov	[bp + 2Bh], byte 1	; Found something
	push	di
.copy	lodsb
	stosb
	cmp	al, 0
	jne	.copy
	dec	di
	mov	cl, [bp + 15h]
	call	removethis.eentry
	pop	di
.skip	mov	dx, bp
	mov	ah, 1Ah
	int	21h
	mov	ah, 4Fh
	int	21h
	jnc	.next
	mov	cl, 2
	cmp	[bp + 2Bh], byte 0
	je	.nope		; Wildcard expanded to only . or .. and -d or -r given
	mov	cl, 1
	mov	[di], byte 0	; Errors from 4Fh are from the expansion of
	cmp	di, activepath	; the directory, not the wildcard
	jne	.nope
	mov	[di], word "."
.nope	add	sp, 2Ch
	pop	bp
	push	cx
	xchg	ax, si
	mov	ah, 59h
	int	21h
	jnc	.x
	xchg	ax, si
.x	pop	cx
	test	cx, ((flag_force | flag_prccall) << 8) | 1
	jz	.nlx
	cmp	al, 2
	je	.last
	cmp	al, 3
	je	.last
	cmp	al, 18
	je	.last
.nlx	or	ch, flag_errorhasoccurred
	push	cx
	call	perror
	pop	cx
.last	ret

removethis:
	mov	dx, activepath
	mov	ax, 4300h
	mov	bh, ch
	int	21h
	mov	ch, bh		; Call trashes ch?
	jc	.no
.eentry	test	cl, byte 16
	jz	.ndir
	test	ch, byte flag_emptydir | flag_recursive
	jnz	.dir
	push	cx
	mov	cx, 11		; says "directory"
	mov	dx, notroot + notrootlen - 11
	call	txterror
	pop	cx
	or	ch, flag_errorhasoccurred
	ret

.dir	test	ch, byte flag_noninteractive | flag_force
	jnz	.rmd
	test	ch, byte flag_interactive
	jz	.rmd
	push	cx
	mov	cx, removedirlen
	mov	dx, removedir
	call	prompt
	pop	cx
	jc	.ret1

.rmd	test	ch, byte flag_recursive
	jz	.rmdir
	; The trick is that find first file remembers attributes, so the clear
	; at get next argument is the only flag clear required.
	or	ch, byte flag_prccall
	push	di
	mov	[di], word "\*"
	mov	[di + 2], word ".*"
	mov	[di + 4], byte 0
	add	di, 4
	call	removewild
	pop	di
	mov	[di], byte 0
.rmdir:
%ifdef DEBUG
	call	outdebug
%else
	mov	dx, activepath
	mov	ah, 3Ah
	int	21h
%endif
	jc	.no
.ret1	ret

.ndir	test	cl, byte 2
	jz	.nsys
	test	ch, flag_noninteractive | flag_force
	jnz	.rma
	push	cx
	mov	dx, removesystem
	mov	cx, removesystemlen
	call	prompt
	pop	cx
	jc	.ret1
	jmp	.rma		; Executive decision: we prompted for system
				; don't need to prompt again for ro

.nsys	test	cl, byte 1
	jz	.nro
	test	ch, flag_noninteractive | flag_force
	jnz	.rma
	push	cx
	mov	dx, removewrite
	mov	cx, removewritelen
	call	prompt
	pop	cx
	jc	.ret1
	jmp	.rma

.nro	test	ch, byte flag_noninteractive | flag_force
	jz	.rm
	test	ch, byte flag_interactive
	jz	.rm
	push	cx
	mov	dx, removesystem
	mov	cx, 6
	call	prompt
	pop	cx
	jc	.ret1
	jmp	.rm

.rma	push	cx
	xor	cx, cx
	mov	ax, 4301h
	int	21h
	pop	cx
.rm:
%ifdef	DEBUG
	call	outdebug
%else
	mov	dx, activepath
	mov	ah, 41h
	int	21h
%endif
	jnc	.ret
.no	test	ch, flag_force
	jz	.no2
	cmp	ax, 2
	je	.ret
	cmp	ax, 3
	je	.ret
.no2	push	cx
	call	perror
	pop	cx
	or	ch, byte flag_errorhasoccurred
.ret	ret

prompt:
	mov	bx, 1
	call	outtxt
	call	outfilename
	mov	dx, questionmark
	mov	cx, 1
	call	outtxt
	lea	dx, [di + 1]
.px	mov	cx, 1
	xor	bx, bx
	mov	ah, 3Fh
	int	21h
	jc	.no
	test	ax, ax
	jz	.no
	mov	al, [di + 1]
	cmp	al, 'y'
	je	.yes
	cmp	al, 'Y'
	je	.yes
	cmp	al, 's'
	je	.yes
	cmp	al, 'S'
	je	.yes
	cmp	al, 'n'
	je	.no
	cmp	al, 'n'
	je	.no
	jmp	.px
.no	stc
.yes	pushf	; CF already clear on JE
	mov	dx, oopscmd + oopslen - 2
	mov	cx, 2
	mov	bx, 1
	call	outtxt
	popf
	ret

%ifdef DEBUG
outdebug:
	push	bx
	mov	bx, 1
	call	outfilename
	mov	cx, 2
	mov	dx, stackoverflow + stackoverflowlen - 2
	call	outtxt
	pop	bx
	ret
%endif

perror: mov	si, errortable - 4
.nxt	add	si, 4
	mov	bx, [si]
	cmp	bl, al
	je	.h
	cmp	bx, 0
	jne	.nxt
.h	mov	dx, [si + 2]
	call	strlendx
txterror:
	mov	bx, 2
	call	outfilename
	push	cx
	push	dx
	mov	dx, colonspace
	mov	cx, 2
	call	outtxt
	pop	dx
	pop	cx
outtxt:			; BX = handle
	mov	ah, 40h
	int	21h
	jc	.ret
	add	dx, ax
	sub	cx, ax
	jnz	outtxt
.ret	ret
outfilename:		; BX = handle, file name had better not be empty
	push	cx
	push	dx
	push	si
	mov	dx, activepath
	call	strlendx
	call	outtxt
	pop	si
	pop	dx
	pop	cx
	ret
strlendx:
	mov	si, dx
	mov	cx, -1
.len	inc	cx
	lodsb
	cmp	al, 0
	jnz	.len
	ret

align	2, db	 0CCh
errortable:	
	dw	1, .inval
	dw	2, .fnf
	dw	3, .pnf
	dw	15, .idrive
	dw	16, .cdir
	dw	18, .fnf	; No more files becomes file not found
	dw	19, .write
	dw	21, .nready
	dw	23, .crc
	dw	25, .seek
	dw	26, .umedia
	dw	27, .sector
	dw	29, .wfault
	dw	30, .rfault
	dw	31, .gen
	dw	32, .share
	dw	33, .lock
	dw	34, .dchang
	dw	65, .access
	dw	83, .fail
	dw	0, .error
.inval	db	"Invalid function", 13, 10, 0
.fnf	db	"File not found", 13, 10, 0
.pnf	db	"Path not found", 13, 10, 0
.idrive	db	"Invalid drive", 13, 10, 0
.cdir	db	"Current directory or directory not empty", 13, 10, 0
.write	db	"Write protected disk", 13, 10, 0
.nready	db	"Drive not ready", 13, 10, 0
.crc	db	"CRC Error", 13, 10, 0
.seek	db	"Seek error", 13, 10, 0
.umedia	db	"Invalid media descriptor", 13, 10, 0
.sector	db	"Sector not found", 13, 10, 0
.wfault	db	"Write fault", 13, 10, 0
.rfault	db	"Read fault", 13, 10, 0
.gen	db	"General failure", 13, 10, 0
.share	db	"Sharing violation", 13, 10, 0
.lock	db	"Lock violation", 13, 10, 0
.dchang	db	"Invalid disk change", 13, 10, 0
.access	db	"Access denied", 13, 10, 0
.fail	db	"Fail on INT 24H", 13, 10, 0
.error	db	"Error", 13, 10, 0
stackoverflow	db	"stack overflow", 13, 10	
stackoverflowlen	equ	$ - stackoverflow
colonspace	equ	 oopscmd + 5
oopscmd	db	"Usage: RM [options] [--] files", 13, 10
oopslen	equ	$ - oopscmd
	db	" -r  recursive", 13, 10
	db	" -i  prompt always", 13, 10
	db	" -d  remove empty directory", 13, 10
	db	" -f  force", 13, 10
helplen	equ	$ - oopscmd
removedir	db	"remove directory "
removedirlen	equ	$ - removedir
removesystem	db	"remove system file "
removesystemlen	equ	$ - removesystem
removewrite	db	"remove write-protected file "
removewritelen	equ $ - removewrite
notcurrent	db	"not removing current directory", 13, 10
notcurrentlen	equ	$ - notcurrent
notroot		db	"not removing root directory", 13, 10
notrootlen	equ	$ - notroot
questionmark	db	"?"
activepath:

flag_noninteractive	equ	40h
flag_pwildcard		equ	20h
flag_prccall		equ	10h
flag_interactive	equ	1h
flag_emptydir		equ	2h
flag_recursive		equ	4h
flag_force		equ	8h
flag_errorhasoccurred	equ	80h
