;; Copyright (C) Joshua Hudson 2021
;; Licensed under CC-BY-SA 4.0

BITS 16

CPU 8086

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
savedsp		equ	parameter + 32	; PUN: both saved space or saved sp register depending on context
biosdisk	equ	savedsp + 4
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
%define reserveddx		0x798
%define reservedcx		0x79A


_start:
	mov	sp, 0x3000
	mov	bx, 0x0300
	mov	ah, 0x4A
	int	0x21
	jc	short	.nope
	cld
	mov	bl, [0x0080]
	mov	bh, 0
	mov	si, 0x0081
	mov	[bx + si], byte 0
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
.endoptions:
	jmp	format

prephd32:
	add	si, 2
	lodsb
	cmp	al, 0
	jne	short	.nope
	mov	[partitiontype], byte 0x0B	; FAT32
	jmp	short	prephd
.nope	jmp	_start.nope

prephd16:
	; Should we adjust partition type for < 65536 sectors? Nah. use the lastest format.
	; I don't have patches for really old DOS anyway.
	add	si, 2
.spl	lodsb
	cmp	al, ' '
	je	short	.spl
	cmp	al, 0
	je	short	prephd
	dec	si
	cmp	[si], word '/1'
	jne	short	prephd32.nope
	mov	[persistflags], byte 1

prephd:
	; Check if drive size is good (no INT13h extensions = must be good)
	call	diskparam

	; Build the MBR sector
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
	pop	bx
	call	savefirstsector
	mov	bx, secondsector	; Fill the rest of the first track with zeros
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
	je	short	.alread
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
	jne	short	.nogood
	call	getreservedsectorcount
	; Check for usable reserved sector
	xchg	ax, si
	mov	cx, 2	; Second sector isn't usable no matter what
	mov	dh, 0
	mov	dl, [biosdisk]
	mov	bx, searchsector
.nrsv	inc	cx
	cmp	cx, si
	jae	.noreserved
	mov	ax, 0x0201
	int	0x13
	jc	.nrsv	; Bad sector or sector number > sectors per track
	mov	di, bx
	; Check for rep WORD
	push	cx
	mov	cx, 256
	mov	ax, [di]
	repe	scasw
	pop	cx
	jne	short	.nrsv
	jmp	short	.reserved
.alread	mov	dx, msg_already
	jmp	.emsg
.nogood	mov	dx, msg_nogood
.emsg	jmp	errormsg
.noreserved:
	mov	ax, [secondsector + 0x16]
	cmp	[secondsector + 0x16], word 0	; FAT32
	je	short	fat32noreserve
	call	patchfirstsector_b16
	test	[transflags], byte 0x80
	jnz	short	.toexiterror
	mov	bp, firstsector
	mov	[secondsector], byte 0xEB	; JMP SHORT
	mov	al, [mybpb_lenstockbpb]
	inc	al
	mov	[secondsector + 1], al
.savebootsector:
	mov	al, [persistflags]
	mov	[secondsector + 2], byte al	; Only place we can save something (darn!)
	test	[transflags], byte 0x10
	jnz	.srsv
	call	saveboot16
	jmp	short	.sfs
.srsv	call	savereserved
.sfs	call	savefirstsector
	mov	ax, 0x4C00
	int	0x21
.badmbr mov	dx, msg_badmbr
	jmp	errormsg
.toexiterror:
	jmp	exit_error
.reserved:
	call	savereservedregs
	call	patchfirstsector_reserved
	call	makereservedsig
	cmp	[secondsector], byte 0xE9	; JMP NEAR
	jne	.reservedshort
	mov	ax, [secondsector + 1]
	inc	ax
	mov	[secondsector + 1], al
	mov	[secondsector], byte 0xEB	; JMP SHORT
.reservedshort:
	or	[transflags], byte 0x10
	jmp	short	.savebootsector

errorcode:
	shl	ax, 1
	mov	bx, ax
	mov	dx, [bx + errorcodes]
errormsg:
	mov	ah, 9
	int	0x21
	mov	ax, 0x4C01
	int	0x21

fat32noreserve:
	mov	dx, msg_noreserve
	jmp	short	errormsg
fat32noreserve2:
	mov	dx, msg_noreserve2
	jmp	short	errormsg
	
fixboot:
	mov	bx, secondsector
	call	loadfirstsector
	mov	si, bx
	mov	di, firstsector
	mov	bp, di
	mov	cx, 256
	rep	movsw
	call	checkbx
	; There's two things that can go wrong.
	; 1: It can be overwritten by a new bootsector
	; 2: It can be overwritten by a new MBR loader
	; If they both go wrong at once there's no recovery from here
	je	short	.isbootloader
	or	[transflags], byte 0x40		; Don't write BOOT.16 it's already correct
	; If we find reserved sector data, use it
	xor	ax, ax
	cmp	[bp + boot16 - 0x0600 + 8], ax
	jne	short	.usesboot16
	cmp	[bp + reservedcx - 0x0600], ax
	je	short	.usesboot16
	mov	dl, [biosdisk]
	mov	dh, [firstsector + reserveddx + 1 - 0x0600]
	mov	cx, [firstsector + reservedcx - 0x0600]
	mov	ax, 0x0201
	int	0x13
	jnc	short	.prepbootrsv_mbr
.tio0	jmp	ioerror
.usesboot16:
	call	regenfirstsector_b16
	jmp	.prepboot_b16
.prepbootrsv_mbr:
	; Regenerate BPB from copy saved in second sector and high data
	mov	ch, 0
	mov	cl, [secondsector + 1]
	add	cl, 2		; Has to be <= 0xD4
	mov	si, secondsector
	mov	di, bp
	rep	movsb
	mov	[bp + 2], byte 0x90
	mov	si, firstsector + boot16 - 0x600
	mov	di, firstsector + 4
	movsw
	movsw
	movsw
	movsw
	or	[transflags], byte 0x40		; Don't write back reserved sector
	jmp	short	.prepbootrsv2
.alreadyfixed:
	jmp	format.alread
.isbootloader:
	mov	bx, firstsector
	call	checkbxmbr
	jnc	short	.alreadyfixed
	; Try to find our reserved sector signature
	call	getreservedsectorcount
	; Check for usable reserved sector
	push	bp
	xchg	ax, bp
	mov	cx, 2	; Second sector isn't usable no matter what
	mov	dh, 0
	mov	dl, [biosdisk]
	mov	bx, searchsector
.nrsv	inc	cx
	cmp	cx, bp
	jae	short	.noreserved
	mov	ax, 0x0201
	int	0x13
	jc	short	.nrsv	; Bad sector or sector number > sectors per track
	lea	di, [bx + 4]
	mov	si, bootloadsig
	push	cx
	mov	cx, 4
	rep	cmpsw
	pop	cx
	jne	short	.nrsv
	pop	bp
	push	cx		; Located our reserved sector
	push	dx		; save boot image here
	call	savereservedregs
	call	makereservedsig
	mov	di, firstsector + 0xDA
	xor	ax, ax		; The unique disk is lost -- zero it
	stosw			; So Windows 9x can recreate it
	stosw
	stosw
.prepbootrsv2:
	call	regenfirstsector_reserved
	jmp	short	.tprepmore
.noreserved:
	pop	bp
	cmp	[secondsector + 0x16], word 0	; FAT32
	je	short	.noreservederror
	call	patchfirstsector_b16	; Generate first sector
	test	[transflags], byte 0x80
	jz	short	.prepboot_b16
	jmp	.prepnoboot		; Rebuild from boot sector didn't fit
.noreservederror:
	jmp	fat32noreserve2
.prepboot_b16:
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
	mov	si, dapatchtmp
	mov	di, firstsector + dapatch - 0x0600
	movsw
	movsw
	movsw
	jc	short	.prepnoboot16
	test	[transflags], byte 0x80
	jnz	.nosaveregswriteback
	test	[transflags], byte 0x40
	jz	.notbpbtofirst
	mov	si, secondsector
	mov	di, firstsector
	mov	cx, [mybpb_lenstockbpb]
	add	cl, 3
	rep	movsb
	mov	[firstsector + 2], byte 0x90
	; Will always take the jnz below
.notbpbtofirst:
	test	[transflags], byte 0xC0
	jnz	.nosaveregswriteback
	push	cx		; Save BIOS parameters for writing it back
	push	dx
.nosaveregswriteback:
.tprepmore:
	jmp	short	.prepmore
.prepnoboot16:
	mov	dx, msg_bootfile
	or	[transflags], byte 0x80
.prepnoboot:
	mov	di, [mybpb_lenstockbpb]	; Repaired FAT16 but not bootable
	add	di, firstsector + 3
	call	patchfirstsector_noboot_dxmsg
.prepmore:
	mov	al, [secondsector + 2]
	mov	[persistflags], al
	mov	di, firstsector + 0x1BE
	cmp	[secondsector + 0x16], word 0	; FAT32
	jne	short	.nptype
	mov	[partitiontype], byte 0x0B	; FAT32
.nptype	call	makepartitiontable
	mov	al, [transflags]
	test	al, byte 0xC0			; Flag 0x80 leaves 2 words on the stack; but it doesn't matter
	jnz	short	.nowriteboot16
	pop	dx
	pop	cx
	mov	ax, 0x0301
	mov	bx, secondsector
	int	0x13
.tio1	jc	short	.ioerror
.nowriteboot16:
	call	savefirstsector
	mov	dx, msg_repairfinished
	mov	ah, 9
	int	0x21
	mov	ax, 0x4C00
	test	[transflags], byte 0x80
	jz	.nex
	inc	ax
.nex	int	0x21
.ioerror:
	jmp	rawioerror
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
	mov	bx, secondsector
	call	fat16loadlba
	jc	short	.ioerror
	ret

diskparam:
	mov	dl, [biosdisk]
	mov	dh, 0
	mov	si, parameter
	mov	[si], word 0x1E
	mov	ah, 0x48
	int	0x13
	jc	short	.zh	; Call not supported
	xor	ax, ax
	cmp	[si], byte 0x1A
	jb	short	.nc2
	cmp	[si + 0x18], word 512
	jne	short	.nsp	; int 13h extensions express bigger sectors
				; avoid using LBA, force CHS
.nc2	cmp	[si], byte 0x18
	jb	short	.nsp
.big2	cmp	[si + 0x16], ax
	jne	.bigdsk
	cmp	[si + 0x14], ax
	jne	.bigdsk
	mov	ax, [si + 0x10]
	mov	[savedsp], ax
	mov	ax, [si + 0x12]
	mov	[savedsp + 2], ax
	jmp	short	.ret
.bigdsk	dec	ax
.nsp	mov	[savedsp], ax
	mov	[savedsp + 2], ax
.ret	ret
.zh	xor	ax, ax		; Save zeros so user doesn't trust it
	jmp	short	.nsp

partitiontype	db	0x06

makepartitiontable:
	; DI = output buffer
	push	dx
	mov	ax, 0x0080
	stosw
	mov	ax, 0x0001
	stosw
	mov	al, [partitiontype]
	stosb
	
	mov	dl, [biosdisk]
	mov	ah, 0x08
	push	di
	push	es
	xor	di, di
	mov	es, di
	int	0x13
	jc	short	.io
	pop	es
	pop	di
	test	[persistflags], byte 0x01
	jz	.noinc
	mov	bp, cx
	mov	si, 0xFFC0
	and	bp, si
	cmp	bp, si
	je	.noinc
	add	ch, byte 1
	jnc	.noinc
	add	cl, byte 0x40
.noinc	mov	al, dh
	stosb
	mov	ax, cx
	stosw
	xor	ax, ax
	stosw
	stosw
	call	.complba
	; If FAT32 use LBA size data if available
	cmp	[partitiontype], byte 0x0B
	jne	.nolbadisk
	cmp	dx, [savedsp + 2]
	ja	.nolbadisk
	jb	.islbadisk
	cmp	ax, [savedsp]
	jae	.nolbadisk
.islbadisk:
	mov	dx, [savedsp + 2]	; LBA disk is larger - use LBA
	mov	ax, [savedsp]
	mov	[di - 8], byte 0x0C
	;mov	[partitiontype], 0x0C	; Nobody else will read it
.nolbadisk:
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

getreservedsectorcount:
	mov	ax, [secondsector + 0x0E]
	mul	word [secondsector + 0x0B]
	mov	cx, 512
	div	cx		; AX = number of physical reserved sectors
	mov	cx, 64		; But we will only use the first cylinder's space
	cmp	ax, cx
	jb	.maxok
	mov	ax, cx
.maxok	ret

; As much as I'd like to convert these to use int 0x25 and int 0x26,
; logical sectors might be bigger than physical sectors causing
; this code to absolutely break down.
loadfirstsector:
	;BX = output buffer
	mov	ax, 0x0201
	mov	cx, 0x0001
	mov	dh, 0
	mov	dl, [biosdisk]
	int	0x13
	jc	short	rawioerror
	ret

savefirstsector:
	mov	bx, firstsector
	mov	ax, 0x0301
	mov	cx, 0x01
	mov	dh, 0
	mov	dl, [biosdisk]
	int	0x13
	jc	short	rawioerror
	ret

savereserved:
	mov	bx, secondsector
	mov	ax, 0x0301
	mov	cx, [firstsector + reservedcx - 0x0600]
	mov	dh, [firstsector + reserveddx + 1 - 0x0600]
	mov	dl, [biosdisk]
	int	0x13
	jc	short	rawioerror
	ret

rawioerror:
	mov	dx, msg_rawioerror
	jmp	errormsg

patchfirstsector_b16:
	call	getbpboffset
	mov	bx, cx
	sub	bx, 3
	mov	[mybpb_lenstockbpb], bx
	mov	si, secondsector
	mov	di, bp
	rep	movsb
	cmp	di, firstsector + fat16codend - fat16boot + ioerror - 0x0600
	jae	short	patchfirstsector_noboot
	mov	bx, di
	mov	si, fat16boot
	mov	cx, (fat16codend - fat16boot) / 2
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
	mov	cl, 9
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
	mov	cx, (errorbpboutend - errorbpbout) / 2
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
	mov	di, bp
	mov	cx, [mybpb_lenstockbpb]
	add	cl, 3
	add	di, cx
	mov	si, fat16boot
	mov	cx, (fat16codend - fat16boot) / 2
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

savereservedregs:
	push	dx
	mov	dl, 0x80
	mov	[firstsector + reserveddx - 0x0600], dx
	mov	[firstsector + reservedcx - 0x0600], cx
	pop	dx
	ret

makereservedsig:
	mov	si, secondsector + 4
	mov	di, firstsector + boot16 - 0x0600
	movsw
	movsw
	movsw
	movsw
	mov	si, bootloadsig
	mov	di, secondsector + 4
	movsw
	movsw
	movsw
	movsw
	ret

patchfirstsector_reserved:
	call	getbpboffset
	cmp	cx, 0xD4
	jbe	.szok
	jmp	patchfirstsector_noboot
.szok	push	cx
	mov	si, secondsector
	mov	di, firstsector
	rep	movsb
	pop	cx
	mov	di, firstsector
	mov	si, bx
	rep	movsb
patchfirstsector_reserved_common:
	mov	si, fat32boot
	movsw
	movsw
	movsw
	mov	si, fat32copy
	mov	di, firstsector + 0xE0
	mov	cx, (fat32copyend - fat32copy) / 2
	rep	movsw
	mov	si, fat32absload
	mov	di, firstsector  + 0x100
	mov	cx, (fat32absend - fat32absload) / 2
	rep	movsw
	mov	si, str_ioerror
	mov	di, firstsector + ioerror - 0x0600
	mov	cx, ioerrorlen
	rep	movsb
	xor	ax, ax			; For repair mode switcher
	mov	di, firstsector + boot16 - 0x0600 + 8
	stosw
	stosw
	ret

regenfirstsector_reserved:
	call	getbpboffset
	mov	di, firstsector
	add	di, cx
	jmp	short	patchfirstsector_reserved_common

getbpboffset:
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
.cont	ret

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
	cld
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
.dope	db	80, 80, 80, 80, ' $'

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
bootloadsig	db	'HDSFBSIG'	; Saved for reserved sector handling
persistflags	db	0
transflags	db	0
msg_usage	db	'FORMATHD.COM, version 2.0.1 Copyright (C) Joshua Hudson 2021,23', 13, 10
		db	'FORMATHD formats the entire hard disk as a big single partition, also', 13, 10
		db	'known as a superfloppy format. This operation must run in two phases', 13, 10
		db	"because DOS can't reread its partition tables. You must know the BIOS", 13, 10
		db	'disk number (typically 0-3) and the drive letter it will end up on.', 13, 10
		db	'For obvious reasons, this tool should only be used from a boot floppy.', 13, 10
		db	"You will need to patch IO.SYS first or the disk won't be usable.", 13, 10
		db	'The disk is made bootable immediately.', 13, 10
		db	'Phase 1 FAT16: FOMRATHD.COM # /16 [/1] where # is the bios number (0-3)', 13, 10
		db	'Phase 1 FAT32: FOMRATHD.COM # /32 where # is the bios number (0-3)', 13, 10
		db	'Phase 2: FORMATHD.COM # C: where C: is the drive that # ended up on.', 13, 10
		db	'Repair: FORMATHD.COM # /F should the OS setup damage the MBR.', 13, 10, '$'
		db	'/1 works around a bug in SeaBIOS that hides the last cylinder', 13, 10, '$'
msg_endphase1	db	'Phase 1 has finished. Reboot and run FORMATHD.COM again.', 13, 10, '$'
msg_repairfinished	db	'Repair completed', 13, 10, '$'
msg_already	db	'Boot sector already installed', 13, 10, '$'
msg_nogood	db	'Boot sector no good', 13, 10, '$'
msg_noreserve	db	'FAT32 filesystem has no usable reserved sectors', 13, 10, '$'
msg_noreserve2	db	'FAT32 recovery but reserved boot sector not found', 13, 10, '$'
msg_badmbr	db	'MBR is no good', 13, 10, '$'
msg_bigbpb	db	'BPB is too large; disk will not boot', 13, 10, '$'
msg_bootfile	db	'BOOT    16  lost; disk will not boot', 13, 10, '$'
msg_rawioerror	db	'HD IO Error', 13, 10, '$'
formatexe	db	'A:\FORMAT.COM', 0
formatcmd	db	' '
formatcmddrive	db	'C: /S'
formatcmdend	db	0
		align 2, db 0

%include "errormsg.asm"
		align 2, db '$'

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
	align 2, db 0xCC
errorbpboutend:

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
	align 2, db 0xCC
fat16codend:

fat32boot:
	cld
	jmp	0:0x7CE0
fat32copy:
	xor	ax, ax
	mov	si, 0x7C00
	mov	di, 0x0600
	mov	bx, si
	cli
	mov	ss, ax
	mov	sp, si
	sti
	mov	ds, ax
	mov	es, ax
	mov	cx, 256
	rep	movsw
	jmp	0:0x0700
	align 2, db 0xCC
fat32copyend:
fat32absload:
	mov	[reserveddx], dl
	mov	dh, [reserveddx + 1]
	mov	cx, [reservedcx]
	mov	ax, 0x0201
	int	0x13
	jc	.error
	mov	si, boot16
	mov	di, 0x7C04
	movsw
	movsw
	movsw
	movsw
	mov	si, 0x07BE
	jmp	bx
.error	mov	si, ioerror
	mov	cx, ioerrorlen
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
	align 2, db 0xCC
fat32absend:

str_ioerror	db 'HD Error'
str_missing	db 'Missing '
str_boot16	db 'BOOT    16 '
		db 13, 10, '$'	;We print this error from DOS too
	align 4, db 0

firstsector:

