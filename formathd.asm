;; Copyright (C) Joshua Hudson 2021
;; Licensed under CC-BY-SA 4.0

BITS 16

ORG 0x100

ioerror		equ	0x078A
ioerrorlen	equ	8
dapatch		equ	0x0792
dapatchlen	equ	6
missingboot16	equ	0x07A4
missingboot16len	equ	8 + 11
boot16		equ	0x07AC
boot16len	equ	11
bootdrive	equ	0x01BE

secondsector	equ	firstsector + 512
searchsector	equ	secondsector + 512
parameter	equ	searchsector + 512
savedsp		equ	parameter + 32
biosdisk	equ	savedsp + 2
sloppypatch	equ	biosdisk + 1	; Currently unused but keeps alignment
dapatchtmp	equ	sloppypatch + 1
;dapatchtmp is 6 bytes long

;Custom (near back) values stored for our convenience
;The original BPB is surprisingly bad; a few more bytes data would have saved a lot of bytes of code
%define mybpb_firstclust	bp + 0x1A2
%define mybpb_firstroot		bp + 0x1A0
%define mybpb_sectsperclust	bp + 0x19E
%define mybpb_lenstockbpb	bp + 0x19C
%define mybpb_physsectormult	bp + 0x19A
%define mybpb_rootdirentries	bp + 0x198
%define mybpbtmp_nheads		bp + 0x194
%define mybpbtmp_nsectors	bp + 0x192


_start:
	mov	sp, 0x3000
	mov	bx, 0x0300
	mov	ah, 0x4A
	int	0x21
	jc	short	.nope
	cld
	mov	bl, [0x0080]
	mov	bh, 0
	mov	[bx + 0x0081], byte 0
	mov	si, 0x0081
.next	lodsb
	cmp	al, 0
	je	short	.nope
	cmp	al, ' '
	je	short	.havediskno
	jmp	short	.next
.nope	mov	dx, msg_usage
	jmp	errormsg
.havediskno:
	lodsb
	cmp	al, ' '
	je	short	.havediskno
	sub	al, '0'
	jb	short	.nope
	cmp	al, 9
	ja	short	.nope
	add	al, 0x80
	mov	[biosdisk], al
	lodsb
	cmp	al, ' '
	jne	short	.nope
.next2	lodsb
	cmp	al, ' '
	je	short	.next2
	cmp	al, '/'
	jne	.notfixboot
	cmp	[si], word '16'
	jne	short	.notprephd16
	jmp	prephd16
.notprephd16:
	cmp	[si], word '32'
	jne	short	.notprephd32
	jmp	prephd32
.notprephd32:
	cmp	[si], byte 'F'
	je	short	.fixboot
	cmp	[si], byte 'f'
	jne	short	.nope
.fixboot:
	jmp	fixboot

.notfixboot:
	cmp	al, 'a'
	jb	short	.nc
	sub	al, 'a' - 'A'
.nc	cmp	al, 'C'
	jb	short	.nope
	cmp	al, 'Z'
	ja	short	.nope
	mov	[formatcmddrive], al
	lodsb
	cmp	al, byte ':'
	jne	short	.nope
	lodsb
	cmp	al, 0
	je	short	.endoptions
	cmp	al, ' '
	jne	short	.nope
	add	si, 2
	;TODO parse options (should any appear)
.endoptions:
	jmp	format

prephd32:
	mov	dx, msg_nofat32
	jmp	errormsg

prephd16:
	; Should we adjust partition type for < 65536 sectors? Nah. use the lastest format.

prephd:
	; Check if drive size is good (no INT13h extensions = must be good)
	mov	dl, [biosdisk]
	mov	dh, 0
	mov	si, parameter
	mov	[si], word 0x1E
	mov	ah, 0x48
	int	0x13
	jc	short	prephdmbr
	cmp	[si + 1], byte 0
	jne	.big
	cmp	[si], byte 0x1A
	jb	short	prephdmbr
.big	cmp	[si + 0x18], word 512
	je	short	prephdmbr
	mov	dx, msg_not512
	jmp	errormsg

	; Build the MBR sector
prephdmbr:
	mov	dl, [biosdisk]		; 0x13 0x1A overwrote DL
	mov	si, _errormbrstart
	mov	di, firstsector
	push	di
	mov	cx, (_errormbrend - _errormbrstart + 1) / 2
	rep	movsw
	mov	cx, firstsector + bootdrive
	sub	cx, di
	shr	cx, 1
	xor	ax, ax
	rep	stosw
	call	makepartitiontable
	mov	dh, 0
	mov	cx, 0x0001
	pop	bx
	mov	ax, 0x0301
	int	0x13
	jc	.io
	mov	bx, secondsector
	push	cx
	mov	di, bx
	xor	ax, ax
	mov	cx, 256
	rep	stosw
	pop	cx
	mov	bp, [firstsector + 0x1C4]
	and	bp, 63
	dec	bp
.nxt	inc	cx
	mov	ax, 0x0301
	int	0x13
	jc	.io
	dec	bp
	jnz	.nxt
	mov	dx, msg_endphase1
	mov	ah, 0x09
	int	0x21
	mov	ax, 0x4C00
	int	0x21
.io	jmp	rawioerror

format:	
	mov	bx, firstsector
	call	loadfirstsector
	call	checkbx
	je	short	already
	call	checkbxmbr
	jc	.badmbr
	mov	si, firstsector + 0xDA
	mov	di, dapatchtmp
	movsw
	movsw
	movsw
	call	callformathd
	mov	bx, secondsector
	call	loadfirstsector
	call	checkbx
	jne	short	nogood
	; TODO: check for space in reserved sectors
	mov	ax, [secondsector + 0x16]
	cmp	[secondsector + 0x16], word 0
	je	fat32noreserve
	call	patchfirstsector_b16
	mov	al, [transflags]
	test	al, 0x80
	jnz	toexiterror
	mov	bp, firstsector
	mov	[secondsector], byte 0xEB	; JMP SHORT
	mov	al, [mybpb_lenstockbpb]
	inc	al
	mov	[secondsector + 1], al
	mov	al, [persistflags]
	mov	[secondsector + 2], byte al	; Only place we can save something (darn!)
	call	saveboot16
	call	savefirstsector
	mov	ax, 0x4C00
	int	0x21
.badmbr mov	dx, msg_badmbr
	jmp	errormsg

errorcode:
	shl	ax, 1
	mov	bx, ax
	mov	dx, [bx + errorcodes]
errormsg:
	mov	ah, 9
	int	0x21
toexiterror:
	jmp	exit_error
fat32noreserve:
fat32noreserve2:
	; TODO change error message when we have FAT32
	mov	dx, msg_nofat32
	jmp	short	errormsg
	
already:
	mov	dx, msg_already
	jmp	short	errormsg

nogood:
	mov	dx, msg_nogood
	jmp	short	errormsg

fixboot:
	mov	bx, secondsector
	call	loadfirstsector
	mov	si, bx
	mov	di, firstsector
	mov	cx, 256
	rep	movsw
	call	checkbx
	; There's two things that can go wrong.
	; 1: It can be overwritten by a new bootsector
	; 2: It can be overwritten by a new MBR loader
	; If they both go wrong at once there's no recovery from here
	je	short	.isbootloader
	or	[transflags], byte 0x60
	mov	bp, firstsector
	; TODO we can streight up check the high data and see if the reserved sector
	; is any good
	jmp	short	.prepboot
.isbootloader:
	;TODO try to find bootloader location in reserved sectors
	cmp	[secondsector + 0x16], word 0
	je	fat32noreserve2
	call	patchfirstsector_b16	; Generate first sector
	mov	al, [transflags]
	test	al, byte 0x80
	jz	short	.prepboot
	jmp	.prepnoboot
.prepboot:
	;bp now points to firstsector
	;find BOOT.16 and check for bootability
	mov	si, firstsector + dapatch - 0x0600
	mov	di, dapatchtmp
	movsw
	movsw
	movsw
	mov	dl, [biosdisk]
	mov	[bp + 0x1BE], dl
	call	fat16lbaprep
	jc	short	.tio1
	call	.loadboot16
	jc	short	.prepnoboot16
	test	[transflags], byte 0xC0
	jnz	.nosaveregswriteback
	push	cx		; Save BIOS parameters for writing it back
	push	dx
.nosaveregswriteback:
	mov	al, [searchsector + 2]
	mov	[persistflags], al
	jmp	short	.prepmore

.noboot16norec:
	mov	dx, str_missing
	jmp	errormsg		; NOT RECOVERABLE
.prepnoboot16:
	test	[transflags], byte 0x20
	jnz	.noboot16norec
	mov	dx, msg_bootfile
	or	[transflags], byte 0x80
.prepnoboot:
	mov	di, [mybpb_lenstockbpb]
	add	di, firstsector + 3
	call	patchfirstsector_noboot_dxmsg
.prepmore:
	test	[transflags], byte 0x20
	jnz	.prepnewmbr
	mov	bx, firstsector
	call	checkbxmbr
	jnc	short	.alreadyfixed
	mov	dl, [biosdisk]
	mov	si, dapatchtmp
	mov	di, firstsector + dapatch - 0x0600
	movsw
	movsw
	movsw
	mov	di, firstsector + 0x1B0
	;TODO check for FAT32 (for setting partition type)
	call	makepartitiontable
	mov	al, [transflags]
	test	al, byte 0xC0
	jnz	short	.nowriteboot16
	pop	dx
	pop	cx
	mov	ax, 0x0301
	mov	bx, secondsector
	int	0x21
.tio1	jc	short	.ioerror
.nowriteboot16:
	mov	dh, 0
	mov	dl, [biosdisk]	; DL Might be trashed
	mov	cx, 0x0001
	mov	ax, 0x0301
	mov	bx, firstsector
	int	0x13
	jc	short	.ioerror
	mov	dx, msg_repairfinished
	mov	ah, 9
	int	0x21
	mov	ax, 0x4C00
	int	0x21
.prepnewmbr:
	;TODO check flag 0x10 for reserved sector
	call	regenfirstsector_b16
	jmp	short	.nowriteboot16

.ioerror:
	jmp	rawioerror
.alreadyfixed:
	jmp	already
.loadboot16:
	;This code is related to the search code in the bootsector but there were too many variables to make them the same
	xor	dx, dx
	mov	ax, [mybpb_firstroot]
	call	fat16logicaltophysical
	mov	cx, [mybpb_rootdirentries]
	jmp	short	.dentrynextsector
.dentryloop:
	cmp	bx, searchsector + 0x0200
	jne	short	.nextdentry
.dentrynextsector:
	mov	bx, searchsector
	push	cx
	push	ax
	push	dx
	call	fat16loadlba
	jc	short	.ioerror
	pop	dx
	pop	ax
	pop	cx
	add	ax, 1
	adc	dx, 0
.nextdentry:
	mov	si, bx
	mov	di, str_boot16
	push	cx
	mov	cx, 11
	repe	cmpsb
	pop	cx
	je	short	.founddentry
	add	bx, 0x20
	loop	.dentryloop
.nop	stc
	ret
.founddentry:
	test	[bx + 0x0B], byte 0xD8
	jnz	short	.nop		; Not a regular file
	mov	ax, word [bx + 0x1A]
	or	ax, ax
	jz	.nop			; Zero sectors
	call	fat16clustertophysical
	jc	short	.ioerror
	mov	bx, searchsector
	call	fat16loadlba
	jc	short	.ioerror
	ret

partitiontype	db	0x06

makepartitiontable:
	; DI = output buffer
	; DL = disk number
	push	dx
	mov	ax, 0x0080
	stosw
	mov	ax, 0x0001
	stosw
	mov	al, [partitiontype]
	stosb
	
	;mov	dl, [biosdisk]
	mov	ah, 0x08
	push	di
	push	es
	xor	di, di
	mov	es, di
	int	0x13
	jc	short	.io
	pop	es
	pop	di
	mov	al, dh
	stosb
	mov	bp, cx
	mov	ax, cx
	stosw
	xor	ax, ax
	stosw
	stosw
	; TODO if FAT32LBA use size data
	call	.complba
	stosw
	xchg	ax, dx
	stosw
	xor	ax, ax
	mov	cx, 24
	rep	stosw
	mov	ax, 0xAA55
	stosw
	pop	dx
	ret
.io	jmp	rawioerror
.complba:
	mov	al, dh
	mov	ah, 0
	inc	ax
	mov	bl, cl
	and	bl, 63
	mov	bh, 0
	mul	bx
	xchg	cl, ch
	and	ch, 0xC0
	rol	ch, 1
	rol	ch, 1
	inc	cx
	mul	cx	; 32 bit output
	ret

callformathd:
	xor	ax, ax
	mov	bx, parameter
	mov	ax, cs
	mov	[bx], word 0
	mov	[bx + 0x02], word formatcmd
	mov	[bx + 4], ax
	mov	[bx + 0x06], word 0x5C
	mov	[bx + 0x08], ax
	mov	[bx + 0x0A], word 0x6C
	mov	[bx + 0x0C], ax
	mov	ax, 0x4B00
	mov	dx, formatexe
	mov	[savedsp], sp
	int	0x21
	cli
	mov	bx, cs
	mov	ds, bx
	mov	es, bx
	mov	ss, bx
	mov	sp, [savedsp]
	sti
	cld
	jc	short	hdrunerror
	;FORMAT.COM occasionally returns nonzero for success :(
	;mov	ah, 0x4D
	;int	0x21
	;or	ax, ax
	;jnz	hdexecerror
	ret

hdrunerror:
	jmp	errorcode

hdexecerror:
exit_error:
	mov	ax, 0x4C01
	int	21h

; As much as I'd like to convert these to use int 0x25 and int 0x26,
; logical sectors might be bigger than physical sectors causing
; this code to absolutely break down.
loadfirstsector:
	;BX = output buffer
	mov	dx, 0
loadnthsector:
	;DX = sector number starting at 0
	mov	ax, 0x0201
	mov	cx, dx
	inc	cx
	mov	dh, 0
	mov	dl, [biosdisk]
	int	0x13
	jc	.rxerror
	ret
.rxerror:
	cmp	cx, 0x01
	je	rawioerror
	stc
	ret

savefirstsector:
	mov	bx, firstsector
	mov	ax, 0x0301
	mov	cx, 0x01
	mov	dh, 0
	mov	dl, [biosdisk]
	int	0x13
	jc	rawioerror
	ret

rawioerror:
	mov	dx, msg_rawioerror
	jmp	errormsg

patchfirstsector_b16:
	;According to the documentation, the first instruction must be a JMP
	;It can either be a JMP SHORT or a JMP NEAR
	mov	bx, secondsector
	mov	bp, firstsector
	cmp	[bx], byte 0xEB	; JMP SHORT
	jne	.try2
	mov	cl, [bx + 1]
	mov	ch, 0
	inc	cx
	inc	cx
	jmp	short	.cont
.try2	mov	cx, [bx + 1]	; must be JMP NEAR
	cmp	al, [bx]
	add	cx, 3
.cont	mov	bx, cx
	sub	bx, 3
	mov	[mybpb_lenstockbpb], bx
	mov	si, secondsector
	mov	di, bp
	rep	movsb
	cmp	di, firstsector + fat16codend - fat16boot + ioerror - 0x0600
	jae	short	patchfirstsector_noboot
	mov	bx, di
	mov	si, fat16boot
	mov	cx, (fat16codend - fat16boot + 1) / 2
	rep	movsw
	mov	si, firstsector + 0xDA
	mov	di, firstsector + dapatch - 0x600
	movsw
	movsw
	movsw
	mov	si, dapatchtmp
	mov	di, firstsector + 0xDA
	movsw
	movsw
	movsw
	mov	si, str_ioerror
	mov	di, firstsector + ioerror - 0x0600
	mov	cx, ioerrorlen
	rep	movsb
	mov	si, str_missing
	mov	di, firstsector + missingboot16 - 0x0600
	mov	cx, missingboot16len
	rep	movsb
	; Generate my BPB parameters (for less code size in boot sector)
	xor	dx, dx
	mov	ax, [bp + 0x0B]
	mov	cl, 9				; If somebody's game they can maybe make 1K sectors work
	shr	ax, cl
	mov	[mybpb_physsectormult], ax
	; Compute distance to root directory
	mov	ah, 0
	mov	al, [bp + 0x10]
	mul	word [bp + 0x16]
	add	ax, [bp + 0x0E]
	mov	[mybpb_firstroot], ax
	mov	ax, [bp + 0x11]
	mov	[mybpb_rootdirentries], ax
	mov	cx, 32
	mul	cx
	div	word [bp + 0x0B]
	or	dx, dx
	jz	.noo
	inc	ax
.noo	add	ax, [mybpb_firstroot]
	mov	[mybpb_firstclust], ax
	mov	ah, 0
	mov	al, [bp + 0x0D]
	mov	[mybpb_sectsperclust], ax
	ret

patchfirstsector_noboot:
	mov	dx, msg_bigbpb
patchfirstsector_noboot_dxmsg:
	mov	ah, 9
	int	0x21
	mov	si, errorbpbjmp
	mov	cx, errorbpbout - errorbpbjmp
	rep	movsb
	;si = errorbpbout already
	mov	di, firstsector + 0x170
	mov	cx, (errorbpboutend - errorbpbout + 1) / 2
	rep	movsw
	mov	di, firstsector + 0x190
	mov	si, dx
	mov	cx, 18		; All the error messagse are 36 bytes
	rep	movsw
	or	[transflags], byte 0x80
	ret

regenfirstsector_b16:
	;This routine only works becasue the W95 routine that trashed the MBR left our data fields in place.
	mov	si, firstsector + 0xDA
	mov	di, dapatchtmp
	movsw
	movsw
	movsw
	mov	si, searchsector
	mov	di, firstsector
	mov	bp, di
	movsw
	mov	al, 0x90
	stosb
	inc	si
	mov	cx, [mybpb_lenstockbpb]
	rep	movsb
	mov	si, fat16boot
	mov	cx, (fat16codend - fat16boot + 1) / 2
	rep	movsw
	mov	si, firstsector + 0xDA
	mov	di, firstsector + dapatch - 0x600
	movsw
	movsw
	movsw
	mov	si, dapatchtmp
	mov	di, firstsector + 0xDA
	movsw
	movsw
	movsw
	ret

	; The only part of the code that uses DOS file handles
saveboot16:
	mov	al, [formatcmddrive]
	mov	bx, searchsector
	mov	di, bx		; This loop is longer than storing the
	stosb			; string constant but makes hex editing
	mov	ax, ':\'	; the binary to change the file name
	stosw			; work reliably
	mov	si, str_boot16
	movsw
	movsw
	movsw
	movsw
.lname	cmp	[di - 1], byte ' '
	jne	short	.ename
	dec	di
	jmp	short	.lname
.ename	mov	al, '.'
	stosb
	movsw
	movsb
.lext	cmp	[di - 1], byte ' '
	jne	short	.eext
	dec	di
	jmp	short	.lext
.eext	cmp	[di - 1], byte '.'
	jne	.nddi
	dec	di
.nddi	mov	[di], byte 0
	;Filename is now built in BX
	mov	ah, 0x3C
	mov	cx, 0x07
	mov	dx, bx
	int	0x21
	jc	.error
	mov	bx, ax
	mov	ah, 0x40
	mov	cx, 512
	mov	dx, secondsector
	int	0x21
	jc	.cerror
	mov	ah, 0x3E
	int	0x21
	ret
.cerror	push	ax
	mov	ah, 0x3E	; Close handle before returning to DOS
	int	0x21
	pop	ax
.error	jmp	errorcode

checkbx:	; Check if sector pointed to by BX is DOS formatted or not
	cmp	[bx], byte 0xEB	; JMP SHORT
	je	.yes
	cmp	[bx], byte 0xE9	; JMP NEAR
.yes	ret

checkbxmbr:	; Check if sector pointed to by BX has a our MBR on it
	cmp	[bx + 0x1BF], byte 0x00
	jne	.nope
	cmp	[bx + 0x1C0], word 0x0001
	jne	.nope
	clc
	ret
.nope:	stc
	ret

%ifdef DEBUG
hexax:	;DEBUG ROUTINE
	push	ax
	push	dx
	push	di
	mov	di, .dope
	mov	dx, di
	call	.gen
	shl	ax, 4
	call	.gen
	shl	ax, 4
	call	.gen
	shl	ax, 4
	call	.gen
	mov	ah, 0x09
	int	0x21
	pop	di
	pop	dx
	pop	ax
	ret
.gen	push	ax
	shr	ax, 12
	add	al, '0'
	cmp	al, '9'
	jbe	.ok
	add	al, 'A' - '0' - 10
.ok	stosb
	pop	ax
	ret
.dope	db	0, 0, 0, 0, ' $'

dumpsi11:
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	mov	dx, 11
.l	lodsb
	mov	bx, 7
	mov	cx, 1
	mov	ah, 0x0E
	int	0x10
	dec	dx
	jnz	.l
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
%endif

		align 2, db 0xCC
;bootloadsig	db	'HDSFBSIG'	; Saved for reserved sector handling
persistflags	db	0
transflags	db	0
msg_nofat32	db	'FAT32 is not implemented yet', 13, 10, '$'
msg_usage	db	'FORMATHD.COM, version 1.0 Copyright (C) Joshua Hudson 2021', 13, 10
		db	'FORMATHD formats the entire hard disk as a big single partition, also', 13, 10
		db	'known as a superfloppy format. This operation must run in two phases', 13, 10
		db	"because DOS can't reread its partition tables. You must know the BIOS", 13, 10
		db	'disk number (typically 0-3) and the drive letter it will end up on.', 13, 10
		db	'For obvious reasons, this tool should only be used from a boot floppy.', 13, 10
		db	"You will need to patch IO.SYS first or the disk won't be usable.", 13, 10
		db	'The disk is made bootable immediately.', 13, 10
		db	'Phase 1: FOMRATHD.COM # /16 where # is the bios number (0-3)', 13, 10
		db	'Phase 2: FORMATHD.COM # C: where C: is the drive that # ended up on.', 13, 10
		db	'Repair: FORMATHD.COM # /F should the OS setup damage the MBR.', 13, 10, '$'
msg_endphase1	db	'Phase 1 has finished. Reboot and run FORMATHD.COM again.', 13, 10, '$'
msg_repairfinished	db	'Repair completed', 13, 10, '$'
msg_already	db	'Boot sector already installed', 13, 10, '$'
msg_nogood	db	'Boot sector no good', 13, 10, '$'
msg_badmbr	db	'MBR is no good', 13, 10, '$'
msg_bigbpb	db	'BPB is too large; disk will not boot', 13, 10, '$'
msg_bootfile	db	'BOOT    16  lost; disk will not boot', 13, 10, '$'
msg_not512	db	'Hard disk sector size is not 512 bytes', 13, 10, '$'
msg_rawioerror	db	'HD IO Error', 13, 10, '$'
formatexe	db	'A:\FORMAT.COM', 0
formatcmd:	db	' '
formatcmddrive	db	'C: /S'
formatcmdend	db	0
boot16file	db	'C:\BOOT.16', 0
		align 2, db 0

%include "errormsg.asm"
		align 2, db $

;; DATA: boot module
;; Code below here must be relocatable; this is no issue except for the far jump
errorbpbjmp:
	jmp	0:0x7D70
errorbpbout:
	xor	bx, bx
	mov	ds, bx
	mov	es, bx
	mov	cx, 36
	mov	si, 0x7D90
	mov	ah, 0x0E
	mov	bl, 0x07
.loop	push	cx
	lodsb
	mov	cx, 1
	int	0x10
	pop	cx
	loop	.loop
	int	0x18
.hlt	hlt
	jmp short .hlt
errorbpboutend:
	align 2, db 0xCC

_errormbrstart:
	xor	ax, ax
	push	ax
	pop	ds
	mov	si, 0x7C00 + .msg - _errormbrstart
	mov	bx, 0x0007
	mov	ah, 0x0E
	mov	cx, 1
	lodsb
.nxt	int	0x10
	lodsb
	or	al, al
	jnz	.nxt
.hlt	hlt
	jmp	short	.hlt
.msg	db	'Run FORMATHD.COM from a DOS boot floppy next'
_errormbrend:
	align 2, db 0x0

fat16boot:
	cld
	xor	bx, bx
	mov	ds, bx
	mov	es, bx
	mov	di, 0x7CDA	; Apply DA patch (ASAP)
	mov	si, 0x7C00 + dapatch - 0x600
	movsw
	movsw
	movsw
	cli			; Put the stack where we want it
	mov	ss, bx
	mov	sp, 0x7C00
	sti
	mov	si, sp
	mov	di, 0x0600
	mov	bp, di
	mov	cx, 256
	rep	movsw
	mov	ax, [mybpb_lenstockbpb]
	add	ax, _fat16start + 0x603 - fat16boot
	push	bx
	push	ax
	retf
_fat16start:
	mov	[bp + bootdrive], dl
	call	fat16lbaprep
	jc	short	.toio
	xor	dx, dx
	mov	ax, [mybpb_firstroot]
	call	fat16logicaltophysical
	mov	cx, [mybpb_rootdirentries]
	jmp	short	.dentrynextsector
.dentryloop:
	cmp	bx, 0x7E00
	jne	short	.nextdentry
.dentrynextsector:
	mov	bx, 0x7C00
	push	cx
	push	ax
	push	dx
	call	fat16loadlba
.toio	jc	short	.ioerror
	pop	dx
	pop	ax
	pop	cx
	add	ax, 1
	adc	dx, 0
.nextdentry:
	mov	si, bx
	mov	di, boot16
	push	cx
	mov	cx, 11
	repe	cmpsb
	pop	cx
	je	short	.founddentry
	add	bx, 0x20
	loop	.dentryloop
.nop	mov	si, missingboot16
	mov	cx, missingboot16len
	jmp	short	fat16error

.founddentry:
	test	[bx + 0x0B], byte 0xD8
	jnz	short	.nop		; Not a regular file
	mov	ax, word [bx + 0x1A]
	or	ax, ax
	jz	.nop			; Zero sectors
	call	fat16clustertophysical
	mov	bx, 0x7C00
	call	fat16loadlba
	jc	short	.ioerror
	mov	cx, 0x0001
	mov	dh, 0
	mov	si, 0x600 + bootdrive ; Points to the first MBR entry
	mov	dl, [si]
	jmp	bx
	
.ioerror:
	mov	si, ioerror
	mov	cx, ioerrorlen

fat16error:
	mov	ah, 0x0E
	mov	bx, 0x0007
.loop	push	cx
	lodsb
	mov	cx, 1
	int	0x10
	pop	cx
	loop	.loop
.hlt	hlt
	jmp short .hlt

	; Tricky routines; used by both boot sector and /F[ix]
fat16clustertophysical:
	; Input: AX
	; Output: DX:AX
	; Destroys everything but BP and DI
	dec	ax
	dec	ax
	mul	word [mybpb_sectsperclust]
	add	ax, [mybpb_firstclust]
	adc	dx, 0
	;Fallthrough
fat16logicaltophysical:
	; Input: DX:AX
	; Output: DX:AX
	; Destroys everything but BP and DI
	push	dx
	mov	cx, [mybpb_physsectormult]
	mul	cx
	mov	bx, dx
	xchg	ax, si	; BX = carry, SI = low output
	pop	ax
	mul	cx
	add	ax, bx	; AX = high out, SI = low output
	mov	dx, si	; AX = high output, DX = low output
	xchg	ax, dx
	ret
fat16lbaprep:
	mov	dl, byte [bp + bootdrive]
	xor	di, di
	mov	ah, 0x08
	int	0x13
	jc	.ret
	mov	dl, dh
	mov	dh, 0
	inc	dx
	mov	[mybpbtmp_nheads], dx
	mov	di, dx		; DI = number of heads
	and	cx, 63		; CX = number of sectors
	mov	[mybpbtmp_nsectors], cx
	; Carry flag is still clear
.ret	ret
fat16loadlba:
	;Input: BP = pointer to BPB, BX = load address, DX:AX = LBA sector address
	;Output: CHS bios parameters are left in registers
	push	bx		; Save address

	; Fun: divide 24 bit DX:AX by 6 bit (number of sectors); 18 bit result means we have to do it in steps
	mov	cx, [mybpbtmp_nsectors]
	push	ax
	xchg	ax, dx
	xor	dx, dx
	div	cx
	xchg	ax, bx		; BX = high division output
	pop	ax
	div	cx
	mov	cx, dx		; CL = sector, CH clear
	inc	cx		; sectors start with 1
	mov	dx, bx		; DX:AX = cylinder * heads + head
	div	word [mybpbtmp_nheads]
	mov	dh, dl		; DH = head
	mov	ch, al		; CH = cylinder
	ror	ah, 1
	ror	ah, 1
	or	cl, ah		; CL gets high 2 bits of cylinder (has to be in the last 5% of the disk)
	mov	dl, [bp + bootdrive]
	pop	bx		; Address we saved earlier
	mov	ax, 0x0201
	int	0x13
.ret	ret
fat16codend:
	align 2, db 0xCC

str_ioerror	db 'HD Error'
str_missing	db 'Missing '
str_boot16	db 'BOOT    16 '
		db 13, 10, '$'	;We print this error from DOS too
	align 4, db 0

firstsector:

