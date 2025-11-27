CPU 8086

ORG 0100h

;Check FAT12/FAT16/FAT32 partion for errors

;Media descriptor:
; Read boot sector
;  if no boot sector read sectors until finding backup boot sector, give up after 32 sectors
;  read first block of all FATs; check for valid block
; Compute chunk size: take Sectors per FAT % Sectors per cluter;
; chunk size is the smaller of cluster and modulus (if not zero)
; compute how much memory to allocate and allocate it

;File Allocation Table:
; zero fat bitmap table
; FAT32: mark root directory start as 01
; read in FAT and FAT backup at cluster sized chunks
;  check each chunk against its dual/if mismatch take the most likely chunk
;  in almost all cases, the most likely chunk is the one with the fewest duplicate entries
;  in case of tie, take the one with more clusters allocated; we can recover allocated easier than free
;  fill in bitmap 00 = free; 01 = reached = 10 allocated but not reached 11 = allocated and reached
;  bad blocks get value 11 immediately
;  repair a free referenced cluster as a terminal referenced cluster
;  update counters for allocated, inbound references, and outbound references
;  repair (truncate) crosslink
; at the end of this loop we can prove there are no 01 remaining as they would have been repaired

;Directory Structure:
; recursively read in directory structure from root
;  perform basic validation of directory entries; skip any LFS entries and volume label
;  if a file has no clusters its length must be zero
;  mark starting cluster as reached
;  if length flag check set, traverse FAT, checking if chain ends too soon or too late
;   we just fix the size to match the length of the FAT chain

;Recovering lost files (we know by counters if we need to run this or not)
; traverse bitmap array, recover any lost files; they have value 10 in the bitmap array
; FAT16: if root dir is full, make a directory LOST.FND to recover the file into

; Copyright (C) Joshua Hudson, 2024-25

; DOS maximum path length is 80 characters.
; ISO9660 has a maximum directory limit of 8 with root being 1.

; 256 word stacksize for DOS + 128 words for us
stackbottom	equ	(_endbss - _bss + _end - _start + 100h + 768)

; Memory map:
; cs:0        PCP
; cs:_start   program entry point
; cs:_bss     uninitialized data
; cs:_endbss  top of stack
; buf1seg:0   first FAT buffer
; buf2seg:0   second FAT buffer and cluster data buffer
; buf3seg:0   third FAT buffer, if we need it
; buf4seg:0   working buffer (FAT sort and dirwalk)
; bitbufseg:0 bitmap buffer, if it needs to exist
; xmspinseg:0 second bitmap buffer, if it needs to exist
; cmempool:0  bitmap pool in main memory
;
; ... additional bitmap pool in dos-allocated RAM
; ... additional bitmap pool in UMBs if they exist
; ... additional bitmap pool in XMS if it exists

%define FORCE_API
%define NO_HIGH_MEM

_start:
	cld
	; Check size of DOS memory block we are in
	; We're supposed to be able to check the PSP; however it's bugged on DOS4+ when we're very low.
	mov	ax, ds
	dec	ax
	mov	es, ax
	mov	bx, es:[3]
	cmp	bx, word (stackbottom + 32768) / 16
	jae	.mem
.nomem	mov	dx, msg_nocmem
	mov	ah, 9
	int	21h
	mov	ax, 4C04h			; We can *NOT* jump to exit here; this will trash DOS
	int	21h				; as the exit-free memory is unintialized
.mem	mov	sp, stackbottom	
	sub	bx, (stackbottom) / 16
	mov	di, _bss			; Zero initialize BSS (saves total code size)
	mov	cx, (_endbss - _bss) / 2
	xor	ax, ax
	rep	stosw
	mov	[cmem], bx
	mov	si, 81h
.a	lodsb
	cmp	al, ' '
	je	.a
	cmp	al, 9
	je	.a
	and	al, 5Fh
	cmp	al, 'A'
	jb	.arg
	cmp	al, 'Z'
	ja	.arg
	mov	dl, al
	sub	dl, 'A'
	lodsb
	cmp	al, ':'
	jne	.arg
	xor	bx, bx
.b	lodsb
	cmp	al, 0Dh
	je	.go
	cmp	al, ' '
	je	.b
	cmp	al, 9
	je	.b
	cmp	al, '/'
	je	.b
	and	al, 5Fh
	cmp	al, 'F'
	je	.o1
	cmp	al, 'C'
	je	.o2
	cmp	al, 'D'
	je	.o3
	cmp	al, 'Z'
	je	.o4
	jmp	.b
.o1	or	bl, opflag_f
	jmp	.b
.o2	or	bl, opflag_c
	jmp	.b
.o3	or	bl, opflag_d
	jmp	.b
.o4	or	bl, opflag_z
	jmp	.b
.arg	mov	dx, msg_usage
	mov	ah, 9
	int	21h
	mov	ax, 4C10h
	int	21h
.go	xor	cx, cx
	mov	es, cx
	mov	es:[8Ch], word ctrlchandler	; DOS restores these itself: we don't have to
	mov	es:[8Eh], cs
	mov	es:[90h], word iohandler
	mov	es:[92h], cs
	mov	[opflags], bx
	mov	[disk], dl
	mov	bl, dl
	mov	ax, 440Dh
	mov	cx, 0867h
	mov	dx, savedaccessflag
	int	21h
	jnc	.sflag
	mov	dx, endchainlow	; Unlock disk in case it's broken
	mov	bl, [disk]	; Only needed on FreeDOS and DOS 4.
	mov	ax, 440Dh	; endchainlow is currently 0 which is what we want
	mov	cx, 0847h
	int	21h
.sflag:	mov	ah, 0Dh
	int	21h		; Flush all buffers: we want a cleanly written FS.

stage_media_descriptor:
	mov	dx, state_media
	mov	ah, 9
	int	21h
	mov	ax, ds
	add	ax, (stackbottom) / 16
	mov	[buf1seg], ax
	mov	[sectsperchunk], word 1

	xor	ax, ax
	; Get disk sector size
	; Documentation says I can use DOS function 1BH for this, but if the boot sector is trashed that won't work.
	; DOS must have for a source for this information.
	mov	es, [buf1seg]
	mov	[es:256], byte 0A0h
	mov	[es:512], byte 0A0h
	mov	[es:1024], byte 0A0h
	mov	[es:2048], byte 0A0h
	mov	[es:4096], byte 0A0h
	mov	[es:8192], byte 0A0h
	mov	[es:16384], byte 0A0h
	xor	dx, dx
	xor	cx, cx
	xor	bx, bx
	call	diskread
	jnc	.read0
.error0	xor	dx, dx
	xor	cx, cx
	call	diskread0
	jnc	.read0
	call	outax
.errorZ	mov	dx, msg_error0
.errorx	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit
.read0	mov	bx, 16384
.sz0	cmp	[es:bx], byte 0A0h
	jne	.sz1
	shr	bx, 1
	cmp	bx, 128
	jne	.sz0

.sz1	shl	bx, 1
	mov	[bytespersector], bx
	mov	[es:256], byte 0
	mov	[es:512], byte 0
	mov	[es:1024], byte 0
	mov	[es:2048], byte 0
	mov	[es:4096], byte 0
	mov	[es:8192], byte 0
	mov	[es:16384], byte 0
	call	invalidatesector
	xor	dx, dx
	xor	cx, cx
	xor	ax, ax
	xor	bx, bx
	call	diskread
	jc	.errorZ
	mov	bx, 16384
.sz2	cmp	[es:bx], byte 0
	jne	.sz3
	shr	bx, 1
	cmp	bx, 128
	jne	.sz2
.sz3	shl	bx, 1
	mov	ax, [bytespersector]
	cmp	ax, bx
	jae	.szfin
	mov	ax, bx
.szfin	cmp	ax, 256
	jb	.errorZ			; Smallest handled sector size is 256 bytes
	mov	[bytespersector], ax

	xor	ax, ax
.mloop	push	ax
	xor	dx, dx
	xor	cx, cx
	mov	es, [buf1seg]
	cmp	ax, 0
	je	.rda			; Already read in sector 0 (CF clear if this jump is taken)
	call	diskread0
.rda	pop	dx
	jc	.notbpb			; bad sector looking for backup sector - ignore it
	mov	es, [buf1seg]
	; Recognize a BPB
	; There are no magic bytes other than the jmp instruction (which somebody else might use) so we check for impossible values
	mov	al, [es:0]
	cmp	al, 0EBh
	je	.jmp
	cmp	al, 0E9h
	jne	.notbpb
.jmp	cmp	[es:0Bh], byte 0	; Bytes per sector; must be a multiple of 256 or nothing works
	jne	.notbpb
	mov	al, [es:0Dh]
	cmp	al, 0
	je	.notbpb			; Invalid sectors per cluster
	mov	bl, al
	dec	bl
	test	bl, al
	jnz	.notbpb			; Sectors per cluster must be a power of 2
	cmp	[es:0Eh], dx
	jbe	.notbpb			; Number of reserved sectors must be at least this many
	mov	al, [es:10h]		; Number of FATs; 0 is impossible, more than 3 is unreasonable
	dec	al
	cmp	al, 2
	jg	.notbpb
	cmp	[es:15h], byte 0F0h	; Media Descriptor itself
	jnb	.bpb			; DR-DOS has some custom media descriptors with different BPB formats
.notbpb	mov	ax, dx			; below F0h; but we can't handle these.
	inc	ax
	cmp	ax, 97			; Worst case for FAT32 backup boot sector
	jl	.mloop
	mov	dx, msg_noboot
	jmp	stage_media_descriptor.errorx
.bpb	push	dx			; Save boot sector address for .gmdesc
	xor	cx, cx			; Split/join control starts with a 0 in CX

	;TODO acutally debug this block when BOGONDOS is up and running.
	;In theory this would work with logically sectored FAT in some versions of MS-DOS.
	;Initialize logical/physical mismatach
	mov	bx, [bytespersector]
	cmp	bx, [es:0Bh]
	jb	.logicsplit
	ja	.logicjoin
	jmp	.bpbjoin

.impossible:
	mov	dx, msg_notpow2
	jmp	.errorx
.impossible2:
	mov	dx, msg_logfail
	jmp	.errorx

.fixup	or	ch, ch
	jz	.fixupl
	mov	bl, cl
.fr	shr	dx, 1
	rcr	ax, 1
	dec	bl
	jnz	.fr
	ret
.fixupl	mov	bl, cl
	cmp	bl, 0
	jz	.frr
.fl	shl	ax, 1
	rcl	dx, 1
	dec	bl
	jnz	.fl
.frr	ret

.logicjoin:	; Easy case: physical < logical
	inc	cl
	shl	bx, 1
	jz	.impossible
	cmp	bx, [bytespersector]
	jl	.logicjoin
	jg	.impossible
	jmp	.bpbjoin

.logicsplit:	; Delicate case: physical > logical
	stc
	rcl	ch, 1	; Mask
	inc	cl
	shr	bx, 1
	cmp	bx, [bytespersector]
	jg	.logicsplit
	jl	.impossible
	test	[es:0Dh], ch
	jnz	.impossible2
	test	[es:0Eh], ch
	jnz	.impossible2
	test	[es:013h], ch
	jnz	.impossible2
	test	[es:016h], ch
	jnz	.impossible2
	cmp	[es:016h], word 0
	jne	.bpbjoin
	test	[es:024h], ch
	jnz	.impossible2
	test	[es:030h], ch
	jnz	.impossible2
	
.bpbjoin:
	xor	dx, dx
	mov	ah, 0
	mov	al, [es:0Dh]
	test	ch, al
	jnz	.impossible3
	call	.fixup
	mov	[sectsperclust], ax	; Number of sectors per cluster after applying logical sectored fat
	mul	word [bytespersector]	; Cannot overflow
	mov	[bytesperclust], ax
	xor	dx, dx
	mov	ax, [es:0Eh]
	call	.fixup
	or	dx, dx
	jnz	.impossible4	; Really should not happen
	mov	[reservedsects], ax
	mov	al, [es:10h]
	mov	[numfats], al
	mov	ax, [es:016h]
	or	ax, ax
	jnz	.sf
	or	[opflags + 1], byte opflag2_ebpb
	mov	ax, [es:024h]
	mov	dx, [es:026h]
.sf	call	.fixup
	mov	[sectsperfat], ax
	mov	[sectsperfat + 2], dx

	mov	ax, [es:011h]
	mov	[rootdirentries], ax
	or	ax, ax
	jnz	.rs
	;Root dir uses a cluster
	mov	[rootdirsects], ax
	test	[opflags + 1], byte opflag2_ebpb
	je	.rsebpb
	; Check if this is an EBPB by means other than number of root dir entries set
	cmp	[es:026h], word 0	; Since 16h is filled, 24h must be < 65536
	jne	.rscls
	mov	al, [es:42h]
	cmp	al, 0x28
	jb	.rscls
	cmp	al, 0x29
	jbe	.rsdbpb
.rscls	mov	[rootclust], byte 1
	or	[opflags + 1], byte opflag2_rclust
	xor	ax, ax
	jmp	.gtot

	; We have an EBPB
.rsdbpb	or	[opflags + 1], byte opflag2_ebpb
.rsebpb	mov	ax, [es:02Ch]
	mov	dx, [es:02Eh]
	mov	[rootclust], ax
	mov	[rootclust + 2], dx
	mov	ax, [es:30h]
	or	ax, ax
	jne	.noinf
	cmp	ax, [es:0Eh]
	jae	.noinf
	mul	word [bytespersector]
	cmp	dx, [es:0Bh]
	jae	.noinf		; Ludicrus overflow problem
	div	word [es:0Bh]
	or	dx, dx
	jnz	.noinf		; Misaligned!
	mov	[fatinfosect], ax
.noinf	jmp	.gtot
	
.impossible3:
	jmp	.impossible2
.impossible4:
	mov	dx, msg_overflow
	jmp	.errorx

	; So the problem here is we need to figure out how many sectors the root dir takes up
.rs	mov	bx, 32
	mul	bx
	cmp	dx, [es:0Bh]
	jae	.impossible4	; Now that's just bonkers level too large
	div	word [es:0Bh]
	or	dx, dx
	jz	.rse
	inc	ax
	jz	.impossible4
.rse	xor	dx, dx
	test	al, ch
	jnz	.impossible3	; Root sectors not aligned to physical sectors; not recoverable
	call	.fixup
	or	dx, dx
	jnz	.impossible4	; Now that's completely out of range
	mov	bx, ax
	xor	dx, dx
	call	.fixup
	mov	[rootdirsects], ax
	;DX must be zero as rootdirentries is only 2 bytes long to begin with

.gtot	xor	dx, dx
	mov	ax, [es:013h]
	or	ax, ax
	jnz	.tot
	mov	ax, [es:20h]
	mov	dx, [es:22h]
.tot:	; Convert total sectors to sectors per cluster
	sub	ax, [es:0Eh]
	sbb	dx, 0
	jc	.impossible4
	sbb	dx, 0
	mov	ch, 0
	mov	cl, [es:10h]
	xor	di, di
	mov	si, [es:16h]
	or	si, si
	jnz	.totfsl
	mov	si, [es:24h]
	mov	di, [es:25h]
.totfsl	sub	ax, si
	sbb	ax, di
.i4c	jc	.impossible4
	loop	.totfsl
	sub	ax, bx		; bx is still number of root dir sectors in FAT sectors
	sbb	dx, 0
	jc	.i4c
	push	ax
	xor	ax, ax
	mov	ch, 0
	mov	cl, [es:0Dh]
	xchg	ax, dx
	div	cx
	pop	bx
	xchg	ax, bx
	div	cx
	mov	[totalclust + 2], bx
	mov	[totalclust], ax
	mov	[freeclust + 2], bx
	mov	[freeclust], ax
	add	ax, 2
	adc	bx, 0
	mov	[highestclust + 2], bx
	mov	[highestclust], ax
.hncc	mov	dl, [es:015h]	; media descriptor is now in DL
	cmp	bx, 0
	jne	.f32
	cmp	ax, 0FFF6h
	jae	.f32
	cmp	ax, 0FF6h
	jae	.f16
	mov	[fattype], byte 12
	jmp	.gftyp
.f16	call	validatehddescriptor
	mov	[fattype], byte 16
	jmp	.gftyp
.f32	call	validatehddescriptor
	mov	[fattype], byte 32
.gftyp	mov	[descriptor], dl
	cmp	dl, 0F7h
	jne	.gftyp2
	mov	dl, 0FFh
.gftyp2	mov	dh, 0FFh
	mov	[endchainlow], dx
	; We've finished extracting all information from the boot sector

	cmp	[fattype], byte 16
	jbe	.spcnxfr
	mov	ah, 9
	mov	dx, .todofat32
	int	21h
	mov	al, error_norun
	jmp	exit
.todofat32	db	"TODO: Verify FAT32 is sane", 13, 10, '$'
.spcnxfr:

	mov	bx, 1			; Find sectors per chunk
.spcn	test	bx, [sectsperclust]	; So the idea here is if we have an actually aligned
	jnz	.spcf			; filesystem on an SSD it will read and write in SSD-sized
	test	bx, [sectsperfat]	; sectors even while the DOS sector size is smaller
	jnz	.spcf			; Note this is a modulus test; otherwise the FAT checker
	test	bx, [rootdirsects]	; will trash data.
	jnz	.spcf
	shl	bx, 1
	jmp	.spcn
.spcf	mov	[sectsperchunk], bx
	mov	ax, [bytespersector]
	mul	bx			; Cannot ovrflow
	shr	ax, 1
	mov	[wordsperchunk], ax

	xor	dx, dx			; Find offset to first cluster
	xor	ax, ax
	mov	cl, [numfats]
	mov	ch, 0
.fatct	add	ax, [sectsperfat]	; For 1-3 this might actually be faster than the mul series
	adc	dx, [sectsperfat + 2]
	loop	.fatct
	add	ax, [reservedsects]
	adc	dx, 0
	add	ax, [rootdirsects]
	adc	dx, 0
	mov	[firstclustsect], ax
	mov	[firstclustsect + 2], dx
	sub	ax, [sectsperclust]
	sbb	dx, 0
	sub	ax, [sectsperclust]
	sbb	dx, 0
	mov	[zerothclustsect], ax
	mov	[zerothclustsect + 2], dx

	test	[opflags], byte opflag_d
	jz	initpools
display1:
	mov	es, [buf1seg]
	xor	dx, dx
	xor	di, di
	mov	si, dsc_bps
	mov	cx, dsc_spc - dsc_bps
	mov	ax, [bytespersector]
	call	gendigitslblcx
	mov	si, dsc_spc
	mov	cx, dsc_spchk - dsc_spc
	mov	ax, [sectsperclust]
	call	gendigitslblcx
	mov	si, dsc_spchk
	mov	cx, dsc_spfat - dsc_spchk
	mov	ax, [sectsperchunk]
	call	gendigitslblcx
	mov	si, dsc_spfat
	mov	cx, dsc_numfats - dsc_spfat
	mov	ax, [sectsperfat]
	mov	dx, [sectsperfat + 2]
	call	gendigitslblcx
	mov	si, dsc_numfats
	mov	cx, dsc_rootent - dsc_numfats
	mov	al, [numfats]
	call	gendigitslblcx
	mov	si, dsc_rootent
	mov	cx, dsc_firstdata - dsc_rootent
	mov	ax, [rootdirentries]
	call	gendigitslblcx
	mov	si, dsc_firstdata
	mov	cx, dsc_clusters - dsc_firstdata
	mov	ax, [firstclustsect]
	mov	dx, [firstclustsect + 2]
	call	gendigitslblcx
	mov	si, dsc_clusters
	mov	cx, dsc_totalfiles - dsc_clusters
	mov	ax, [totalclust]
	mov	dx, [totalclust + 2]
	call	gendigitslblcx
	mov	ax, 0A0Dh
	stosw
	mov	si, state_media
	mov	cx, state_fat - state_media
	rep	movsb
	push	ds
	push	es
	pop	ds
	mov	ah, 9	; DX is still 0
	int	21h
	pop	ds

	; Initialize buffer pools

initpools:
	push	ds
	pop	es
	mov	cx, fptrcnt
	mov	di, entriestowords
	cmp	[fattype], byte 16
	je	.is16
	cmp	[fattype], byte 32
	je	.is32

	; FAT12: pool size is entire fat
	mov	si, fptrs12
	rep	movsw
	mov	ax, [sectsperfat]
	mul	word [bytespersector]	; Cannot overflow
	mov	cl, 4
	shr	ax, cl			; bytespersector can't be smaller than 16
	mov	bx, 64
	jmp	.initall

.is16	mov	si, fptrs16
	rep	movsw
	mov	ch, 8			; FAT16: every 8 bytes of FAT is 1 byte of bitmap
	jmp	.in12
.is32	mov	si, fptrs32
	rep	movsw
	mov	ch, 16			; FAT32: every 16 bytes of FAT is 1 byte of bitmap
.in12	mov	ax, [wordsperchunk]
	mov	cl, 3
	shr	ax, cl			; ax = paragraphs per chunk
	mov	bx, ax			; bx = ceil(ax / ch)
.dec	shr	bx, 1
	adc	bx, 0
	shr	ch, 1			; works because ch is always a power of 2
	jnz	.dec

.initall:
	mov	cx, [highestclust]	; copy of highestclust for byte clamping
	mov	[savedfilename], cx
	mov	cx, [highestclust + 2]
	mov	[savedfilename + 2], cx
	mov	cx, [buf1seg]
	add	cx, ax
	mov	[buf2seg], cx
	add	cx, ax
	mov	[buf3seg], cx
	cmp	[numfats], byte 2
	jbe	.left2
	add	cx, ax
.left2	mov	[buf4seg], cx
	cmp	ax, 32
	jae	.left3
	mov	ax, 32
.left3	add	cx, ax		; BUF4 must be at least 512 bytes for recursive dirwalk
	mov	[bitbufseg], cx
	; Check if the entire bitmap fits in cmem or not
	mov	ax, [highestclust]		; Wasting two bits is worth it in instructions saved
	mov	dx, [highestclust + 2]
	mov	cx, 6
.dec2	shr	dx, 1
	rcr	ax, 1
	adc	ax, 0
	loop	.dec2	; When loop terminates, DX:AX is the entire size required in paragraphs
	mov	si, bitvectorptrs
%ifdef FORCE_API
	jmp	.cbigr
%endif
	or	dx, dx
	jnz	strict short .cbigr
	mov	di, ds
	add	di, [cmem]
	sub	di, ax
	jb	.cbigr
	mov	[cmempoollen], di	; Remember rest of conventional memory (in case something needs it)
	mov	dx, [bitbufseg]		; Pool fits in conventional memory, bitbufseg unneeded
	add	dx, ax
	mov	[cmempool], dx
	xor	dx, dx
	mov	cx, [bitbufseg]
	mov	bl, b_mem_internal
	call	.mcrec0			; Record single entry for all memory
	jmp	.hmem
.mcrec0	xor	di, di			; BL = type, CX = segment, DX:AX = how many paragraphs
.mcrec	push	cx			; DI = offset
	mov	cx, 6
.mcmm	shl	ax, 1
	rcl	dx, 1
	loop	.mcmm
	pop	cx
.mrec_common:				; BL = type, CX:DI = ptr, DX:AX = how many cells
	cmp	dx, [savedfilename + 2]
	jb	.mcrecl
	ja	.mcrech
	cmp	ax, [savedfilename]
	jbe	.mcrecl
.mcrech	mov	ax, [savedfilename]
	mov	dx, [savedfilename + 2]
.mcrecl	sub	[savedfilename], ax
	sbb	[savedfilename + 2], dx
	mov	[si + bitvectortype], bl
	mov	[si + bitvectorptr], di
	mov	[si + bitvectorptr + 2], cx
	mov	[si + bitvectorlength], ax
	mov	[si + bitvectorlength + 2], dx
	add	si, bitvectorrlen
	ret
.cbigr	mov	cx, [bitbufseg]
	add	cx, bx		; BX preserved the entire time: size of bitbufseg required
	mov	[xmspinseg], cx
	add	cx, bx		; second pool required so we don't thrash too much scanning the FAT
	push	cx
	mov	cl, 6
	shl	bx, cl
	mov	[bitbuflen], bx
	pop	cx
	mov	[cmempool], cx	; There's no pool (len = 0); remember length of bitbufseg = cmempool - bitbufseg
	mov	bx, ds
	add	bx, [cmem]
	sub	bx, cx
%ifndef FORCE_API
	ja	.hcmema
%endif
	jnc	.hcmem
	mov	dx, msg_nocmem2
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit
.hcmema	sub	ax, bx		; Add cmempool to pools
	sbb	dx, 0
	push	ax
	push	dx
	mov	ax, bx
	xor	dx, dx
	mov	bl, b_mem_internal
	call	.mcrec0
	pop	dx
	pop	ax	; DX:AX is how many paragraphs we still need
.hcmem:
%ifndef NO_DOS_MEM
	push	ax
	; Free unused environment
	mov	es, [2Ch]
	mov	ax, es
	test	ax, ax
	jz	.nfenv
	cmp	ax, 0F000h
	jae	.nfenv
	mov	ah, 49h
	int	21h
	;mov	[2Ch], word 0	; Don't actually need to do this, DOS doesn't work like that
	; Link in UMBs as we can use them just fine
.nfenv	mov	ax, 5802h
	cmp	al, 0
	jne	.nlink
	mov	ax, 5803h
	mov	bx, 1
	int	21h
	jc	.nlink
	or	[opflags + 1], byte opflag2_ulink
.nlink	pop	ax
	; Allocate DOS memory; early limit to avoid breaking down in the rare case of excessive fragmentation
	mov	di, bitvectorptrs + bitvectorrlen * 60
	call	.dosalloc
	jz	.hmem
%endif
	push	ax
	push	dx
	push	ds
	pop	es
	mov	ax, 4300h
	int	2fh
	cmp	al, 80h
	jne	.nohmem	; Out of RAM
	mov	ax, 4310h
	int	2fh
	mov	[xmsfunc], bx
	mov	[xmsfunc + 2], es
	pop	dx
	pop	ax
%ifndef NO_HIGH_MEM
	push	ax
	push	dx
	mov	ah, 1
	mov	dx, -1	; Get it all
	mov	ah, 1
	call	far [xmsfunc]
	cmp	ax, 1
	je	.himem
	xor	bx, bx
	mov	ax, 4A01h
	int	2fh
	test	bx, bx	; ES:DI = ptr, BX = len
	jz	.nohim
	mov	ax, di
	neg	ax
	mov	cl, 4
	shr	ax, cl
	jz	.nohim
	push	ax
	call	.a20e
	pop	ax
	jz	.nohim
	push	ax
	xor	dx, dx
	mov	cx, es
	mov	bl, b_mem_internal
	call	.mcrec
	pop	cx
	jmp	.hmpost
.himem	call	.a20e
	je	.noa20h
	xor	dx, dx
	mov	ax, 0FFFh
	mov	cx, 0FFFFh
	mov	di, 10h
	mov	bl, b_mem_high
	call	.mcrec
	mov	cx, 0FFEh
.hmpost	pop	dx
	pop	ax
	sub	ax, cx
	sbb	dx, 0
	jnc	.hmd
.hmz	jmp	.hmem
.hmd	jnz	.himemdone
	test	ax, ax
	jz	.hmz
	jmp	.himemdone
.a20e	mov	ah, 7
	call	far [xmsfunc]
	cmp	al, 0
	jnz	.a20f
	mov	ah, 5
	call	far [xmsfunc]
	cmp	al, 0
	jz	.a20f
	or	[opflags + 1], byte opflag2_a20
.a20f	ret	; returns ZF set = no A20, ZF clear = A20
.noa20h	mov	ah, 2
	call	far [xmsfunc]
.nohim	pop	dx
	pop	ax
.himemdone:
%endif
%ifndef NO_UMB_MEM
	mov	cx, 2000h
.nxtumb	push	ax
	push	dx
	mov	dx, cx
.nxtum2	mov	ah, 10h
	call	far [xmsfunc]
	test	ax, ax
	jz	.noumb
	push	dx
	xor	ax, ax
	mov	cx, bx
	xchg	ax, dx
	mov	bl, b_mem_umb
	call	.mcrec0
	pop	cx
	pop	dx
	pop	ax
	sub	ax, cx
	sbb	dx, 0
	jc	.uhmem
	jnz	.nxtumb
	test	ax, ax
	jnz	.nxtumb
.uhmem	jmp	.hmem
.noumb	cmp	bl, 0B0h
	je	.nxtum2		; There's a smaller UMB and it told us how big
	pop	dx
	pop	ax
%endif
%ifndef FORCE_API
	or	dx, dx
	jnz	.try_xms_for_real
	mov	bx, [cmempool]
	sub	bx, [bitbufseg]
	cmp	ax, bx
	ja	.try_xms_for_real
	mov	ax, bx			; Total XMS memory requirement fits in XMS buffer
	mov	bl, b_mem_internal
	mov	cx, [bitbufseg]
	call	.mcrec0
.xmnuf	jmp	.hmem
.try_xms_for_real:
%endif
%ifndef NO_XMS
	mov	bx, [bitbuflen]		; pre set up for erase XMS memory
	shr	bx, 1
	shr	bx, 1
	mov	[xms_xfer_len], bx
	mov	es, [bitbufseg]
	push	ax
	xor	di, di
	mov	cx, [xms_xfer_len]
	xor	ax, ax
	rep	stosw
	pop	ax
	mov	bp, si
.xmsalloc:				; Note that we don't care at all about over-allocation
	cmp	si, bitvectorptrs + bitvectorrlen * 128	; In case someone really does have >64 handles
	je	.fmem_final
	push	ax
	push	dx
	mov	ah, 8
	call	far [xmsfunc]
	or	ax, ax
	jz	.nomorehmemp		; All out
	mov	cx, dx
	mov	ah, 9
	call	far [xmsfunc]
	cmp	al, 1
	jne	.nomorehmemp
	push	cx			; Need how many kilobytes l8r
	mov	di, dx			; handle
	mov	dx, cx
	xor	ax, ax			; Shift up 12 times
	mov	cx, 4			; best impl as up 16, down 4
.xmssl	shr	ax, 1			; Cannot overflow because
	rcr	dx, 1			; the starting point fits in 1 register
	loop	.xmssl
	mov	bl, b_mem_xms
	call	.mrec_common
	pop	di
	xor	bx, bx
	shr	di, 1			; Shift down 6 times
	rcr	bx, 1			; best done as up 8 down 2
	shr	di, 1
	rcr	bx, 1
	pop	dx
	pop	ax
	sub	ax, bx
	sbb	dx, di
	jc	strict short .xmnuf
	jnz	.xmsalloc
	or	ax, ax
	jnz	.xmsalloc
	jmp	.nomorehmem
%endif
%ifdef FORCE_API
.xmnuf	jmp	.hmem
%endif

.nohmem:				; High memory API not available
	pop	dx
	pop	ax
%ifndef FORCE_API
	push	ax			; Scavenge XMS memory buffers
	push	dx
	mov	ax, [cmempool]
	sub	ax, [bitbufseg]
	mov	bl, b_mem_internal
	mov	cx, [bitbufseg]
	xor	dx, dx
	push	ax
	call	.mcrec0
	pop	cx
	pop	dx
	pop	ax
	sub	ax, cx
	sbb	dx, 0
	jc	.nohmem_vok
	jnz	.nomorehmem
	or	ax, ax
	jnz	.nomorehmem
.nohmem_vok:
	jmp	.hmem
%endif

.nomorehmemp:
	cmp	bp, si
	je	.nohmem			; Never allocated XMS
	pop	dx
	pop	ax
.nomorehmem:
	mov	di, bitvectorptrs + bitvectorrlen * 128
%ifndef NO_DOS_MEM
	call	.dosalloc
	jz	.hmem
%endif
	cmp	si, di
	je	.fmem_final

.nohmem_final:
	mov	dx, msg_noram
.fmemc	mov	ah, 9
	int	21h
	mov	al, error_norun
	jmp	exit
.fmem_final:
	mov	dx, msg_fragram
	jmp	.fmemc

.dosalloc:
	cmp	si, di
	je	.dosalloc_stop
	push	ax
	push	dx
.dosalloc_rtr:
	mov	bx, 0FFFFh
	mov	ah, 48h
	int	21h
	cmp	al, 8
	jne	.dosalloc_zero
	test	bx, bx
	jz	.dosalloc_no
	mov	ax, bx
	mov	ah, 48h
	int	21h
	jc	.dosalloc_rtr	; Did a TSR just allocate memory?
	xchg	ax, cx
	xchg	ax, bx		; CX = seg, AX = para
	xor	dx, dx
	mov	bl, b_mem_dos
	push	ax
	push	di
	call	.mcrec0
	pop	di
	pop	cx
	pop	dx
	pop	ax
	sub	ax, cx
	sbb	dx, 0
	mov	cx, [savedfilename]
	or	cx, [savedfilename + 2]
	jnz	.dosalloc
	ret		; Z flag is set
.dosalloc_no:
	or	di, di	; Clear Z flag
.dosalloc_zero:
	pop	dx
	pop	ax
	ret		; Z flag is set
.dosalloc_stop:
	or	di, di	; clear Z flag
	ret

.hmem:	; All memory allocated
	; Zero memory map
	mov	si, bitvectorptrs
.lfatl	mov	bl, [si + bitvectortype]
	cmp	bl, byte 0
	je	.lfat0
	;DX:AX = number of bytes owned
	mov	ax, [si + bitvectorlength]
	mov	dx, [si + bitvectorlength + 2]
	add	ax, 3
	adc	dx, 0
	shr	dx, 1
	rcr	ax, 1
	shr	dx, 1
	rcr	ax, 1
	cmp	bl, byte b_mem_xms
	jb	.h0seg
	; It's some kind of API-swapped memory
	mov	bx, ax
	mov	di, dx
	mov	cx, [si + bitvectorptr]
	mov	[xms_xfer_dst], cx
.cfbcb	mov	[xms_xfer_dstoff], bx		; Right now the only kind of API-swapped memory is XMS so we do that.
	mov	[xms_xfer_dstoff + 2], di
	push	ax
	mov	ax, [si + bitvectorptr]
	mov	[xms_xfer_src], ax
	call	xmscopy
	pop	ax
	add	bx, [xms_xfer_len]
	add	di, 0
	sub	ax, [xms_xfer_len]
	sbb	dx, 0
	jc	.lfatn
	jnz	.cfbcb
	test	ax, ax
	jnz	.cfbcb
	jmp	.lfatn
.h0seg	mov	es, [si + bitvectorptr + 2]	; Fundamental assumption: a *big* record is paragraph aligned
	inc	ax
	shr	ax, 1
	push	ax
	pushf
	or	dx, dx
	jz	.h0snl
.h0sl	mov	cx, 32768
	xor	ax, ax
	xor	di, di
	rep	stosw
	mov	ax, es
	add	ax, 1000h
	mov	es, ax
	dec	dx
	jnz	.h0sl
.h0snl	popf
	pop	cx
	xor	ax, ax
	mov	di, [si + bitvectorptr]
	rep	stosw
	jnc	.lfatn
	stosb
.lfatn	add	si, bitvectorrlen
	cmp	si, bitvectorptrs + bitvectorrlen * 128
	jl	.lfatl

	; Final Media Descriptor Check; any FAT have a Media Descriptor?
.lfat0	xor	dx, dx
	xor	si, si
	mov	ax, [reservedsects]
	mov	cl, [numfats]
	mov	ch, 0
	xor	bp, bp
	cmp	[fattype], byte 12
	jne	.mdscn
	push	word [sectsperchunk]		; FAT 12 must process the whole at once
	mov	bx, [sectsperfat]
	mov	[sectsperchunk], bx
.mdscn	push	cx
	push	si
	push	di
	mov	es, [buf1seg + si]
	call	diskread0
	pop	di
	pop	si
	pop	cx
	inc	si
	inc	si
	add	ax, [sectsperfat]
	adc	dx, [sectsperfat + 2]
	rcr	bp, 1				; Bitmask of failed FAT reads
	loop	.mdscn
	cmp	[fattype], byte 12
	jne	.npxm
	pop	word [sectsperchunk]
.npxm	mov	cl, 16				; Move bitmask from high bits to low bits
	sub	cl, [numfats]
	shr	bp, cl
	mov	bl, [descriptor]
	test	bp, 1
	jnz	.nmd1
	mov	es, [buf1seg]
	cmp	[es:0], bl
	je	.gmdesc
.nmd1	cmp	[numfats], byte 1
	je	.bmdesc
	test	bp, 2
	jnz	.nmd2
	mov	es, [buf2seg]
	cmp	[es:0], bl
	je	.gmdesc
.nmd2	cmp	[numfats], byte 2
	je	.bmdesc
	test	bp, 4
	jnz	.bmdesc
	mov	es, [buf3seg]
	cmp	[es:0], bl
	je	.gmdesc
.bmdesc	mov	dx, msg_nomdesc		; None of the FATs have a media descriptor; assume not really FAT
	cmp	bp, 0
	je	.bmdsc2
	mov	dx, msg_badmdesc
.bmdsc2	jmp	stage_media_descriptor.errorx
.nbvec	jmp	newbadsectors
.gmdesc	pop	ax			; Got boot sector address back
	cmp	ax, 0
	jnz	.bootbk
	jmp	.mddone
.bootbk	push	ax
	mov	dx, query_bootsect
	mov	cx, state_mediaq
	call	queryfixyn
	pop	dx
	cmp	al, 'y'
	jne	.mddone
	xchg	ax, dx
	xor	dx, dx
	mov	es, [buf1seg]
	call	diskread0
	jc	.nbvec
	xor	ax, ax
	xor	dx, dx
	mov	ch, 0
	call	diskwrite0
	xor	dx, dx			; Repair boot sector clobbered buf1; reload
	mov	ax, [reservedsects]
	call	diskread0
	jnc	.mddone
	or	bp, 1
.mddone	call	checkmark

stage_fat:
	mov	dx, state_fat
	mov	ah, 9
	int	21h
	xor	dx, dx
	mov	ax, [sectsperfat + 2]	; Division is even and only sed for progress bar anyway
	div	word [sectsperchunk]
	mov	[savedfilename + 2], ax
	mov	ax, [sectsperfat]
	div	word [sectsperchunk]
	mov	[savedfilename], ax

	mov	ax, bp
	mov	bp, sp
	sub	sp, 22
	;bp - 1: dirty flags for each FAT
	;bp - 2: bad block flags for each FAT
	;bp - 6: lowest cluster number in FAT block
	;bp - 10: number of entries in a full FAT block
	;bp - 14: one more than last cluster number in FAT block
	;bp - 16: number of words in a FAT block
	;bp - 18: number of entries in a FAT block
	;bp - 20: best FAT so far
	;bp - 22: quality of best FAT so far
	xor	dx, dx
	mov	[bp - 2], ax
	mov	[bp - 4], dx
	mov	[bp - 6], dx
	mov	[bp - 8], dx
	mov	ax, [wordsperchunk]
	call	[entriesperblock]
	mov	[bp - 10], ax
	mov	es, [bitbufseg]

	mov	ax, [rootclust]
	mov	dx, [rootclust + 2]
	test	dx, dx
	jnz	.set_root_clust_top
	cmp	ax, 1
	jb	.fat_block_loop
.set_root_clust_top:
	call	checkrootclustno
	mov	cl, 2		; Root cluster is reached
	call	setbitsclust

.fat_block_loop:
	;FAT blocks are loaded; prepare current counter
	cmp	[fattype], byte 12
	jne	.loop_top_not12
	mov	ax, [sectsperfat]	; Yup all of them
	xor	dx, dx
	jmp	.loop_top_common	; You think I'm going to page a FAT12? You're outta your mind.
.loop_top_not12:
	mov	ax, [bytespersector]
	mul	word [sectsperchunk]	; A chunk can't be bigger than 32KB so DX is always 0
	shr	ax, 1
	cmp	[fattype], byte 16
	je	.loop_top_skip
	shr	ax, 1
.loop_top_skip:
	add	ax, [bp - 6]
	adc	dx, [bp - 4]
.loop_top_common:
	mov	bx, [highestclust]
	mov	cx, [highestclust + 2]
	cmp	dx, cx
	jb	.loop_top_common_short
	cmp	ax, bx
	jbe	.loop_top_common_fullsize
.loop_top_common_short:
	mov	ax, bx
	mov	dx, cx
.loop_top_common_fullsize:
	mov	[bp - 14], ax
	mov	[bp - 12], dx
	sub	ax, [bp - 6]
	call	[entriestowords]
.loop_top_common_store:
	mov	[bp - 16], ax
	call	[entriesperblock]
	mov	[bp - 18], ax
	xor	ax, ax
	mov	[bp - 20], ax	; Nothing selected yet
	mov	[bp - 22], ax	; Unevaluated
	mov	cl, 0

	cmp	[numfats], byte 3
	jb	.quantify_loop
	mov	al, [bp - 2]		; Special case for 3 FATS
	and	al, 6			; Check if 2 and 3 are equal
	jnz	.quantify_loop
	mov	es, [buf3seg]
	push	ds
	mov	ds, [buf2seg]
	call	fat_compare
	pop	ds
	jne	.quantify_loop
	mov	[bp - 20], es
	mov	es, [buf1seg]
	push	ds
	mov	ds, [bp - 20]
	call	fat_compare
	jne	.ncpy31
	call	fat_copy
	or	[bp - 1], byte 1
.ncpy31	pop	ds
	jmp	.quantify_apply_finished

.quantify_loop:
	mov	ch, 0
	shl	ch, cl
	test	[bp - 2], ch
	jnz	.quantify_worse
	mov	ch, 0
	mov	si, buf1seg
	add	si, cx
	add	si, cx
	mov	es, [si]
	cmp	[bp - 20], word 0
	je	.quantify_free
	cmp	[bp - 22], word 0
	jne	.quantify_have
	push	ds
	mov	ds, [bp - 20]
	call	fat_compare
	pop	ds
	jnz	.quantify_need
	jmp	.quantify_have1		; If any matched the first one, use it!
.quantify_need:
	push	es
	mov	es, [bp - 20]
	call	fat_quantify
	mov	[bp - 22], ax
	pop	es
.quantify_have:
	call	fat_quantify
	cmp	ax, [bp - 22]
	jbe	.quantify_worse
.quantify_free:
	mov	[bp - 20], es
	mov	[bp - 22], ax
.quantify_worse:
	inc	cl
	cmp	cl, [numfats]
	jb	.quantify_loop
	cmp	[bp - 20], word 0
	jne	.quantify_have1
	mov	dx, msg_nogoodfat
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit
.nofixfatdiff:
	mov	al, error_nofix
	jmp	exit

.quantify_have1:
	mov	cl, 0
.quantify_apply_loop:
	mov	ch, 0
	mov	si, buf1seg
	add	si, cx
	add	si, cx
	mov	ax, [si]
	mov	bx, [bp - 20]
	cmp	ax, bx
	je	.quantify_apply_bottom
	mov	es, ax
	push	ds
	mov	ds, bx
	mov	ch, 1
	shl	ch, cl
	test	[bp - 2], ch
	jnz	.quantify_mustapply
	call	fat_compare
	jz	.quantify_noapply
.quantify_mustapply:
	call	fat_copy
	or	[bp - 1], ch
.quantify_noapply:
	pop	ds
.quantify_apply_bottom:
	inc	cl
	cmp	cl, [numfats]
	jb	.quantify_apply_loop
.quantify_apply_finished:

	mov	al, [bp - 1]
	or	al, [bp - 2]
	xor	al, [bp - 2]
	jz	.allsame
	mov	cx, state_fat
	mov	dx, query_fatdiff
	call	queryfixyn
	cmp	al, 'y'
	jne	.nofixfatdiff
	; Actual fix happens at writeback time
.allsame:
	xor	di, di
	cmp	[bp - 6], di
	jne	.notfirst
	cmp	[bp - 4], di
	jne	.notfirst
	call	[entryfromblock]
	cmp	al, [descriptor]
	je	.isdesc
	call	[ismdesc]
	je	.isdesc
	mov	cx, state_fat
	mov	dx, query_anomalous
	call	queryfixyn
	cmp	al, 'y'
	je	.fxdesc
	mov	al, error_nofix
	jmp	exit		; Can't risk continuing as this could destroy entire FS
.fxdesc	mov	ah, 0FFh
	mov	dx, 0FFFh
	mov	al, [descriptor]
	call	entrytoblockall
.isdesc	inc	di
	call	[entryfromblock]
	test	[opflags + 1], byte opflag2_rclust
	jz	.secondnormal
	mov	[rootclust], ax
	mov	[rootclust + 2], dx
	call	checkrootclustno
	mov	cl, 2
	call	setbitsclust
	jmp	.secondfinish
.secondnormal:
	cmp	al, 0FFh
	jne	.secondfix
	call	[ismdesc]
	je	.secondfinish
.secondfix:
	mov	cx, state_fat
	mov	dx, query_anomalous
	call	queryfixyn
	cmp	al, 'y'
	jne	.secondfinish
	mov	ax, 0FFFFh
	mov	dx, ax
	call	entrytoblockall
.secondfinish:
	inc	di
.notfirst:
	;*NOW* we can scan the FAT and update bitmaps

	call	[entryfromblock]
	or	dx, dx
	jnz	.notfixendchain
	cmp	ax, 1
	jne	.notfixendchain
	mov	cx, state_fat
	mov	dx, query_clustone
	call	queryfixyn
	cmp	al, 'y'
	jne	.endchain		; We can survive not fixing this one
.setendchain:
	mov	dx, 0FFFh
	mov	ax, [endchainlow]
	call	entrytoblockall
.endchain:
	mov	ax, [bp - 6]
	mov	dx, [bp - 4]
	mov	cl, 2
	call	setbitsclustdi
	jmp	.clustdonenotfree
.notfixendchain:
	or	ax, ax
	jne	.notfree
	or	dx, dx
	jne	.notfree
	mov	ax, [bp - 6]
	mov	dx, [bp - 4]
	call	getbitsclustdi
	test	ch, 1
	jz	.clustdone
	mov	cx, state_fat
	mov	dx, query_freealloc
	call	queryfixyn
	cmp	al, 'y'
	je	.setendchain	; Scan pass is tolerant of this, but the FS is still broken
	jmp	.clustdone	; We won't allocate this cluster because of how cluster alloc works
.notfree:
	sub	[freeclust], word 1
	sbb	[freeclust + 2], word 0
	call	[isbadblock]
	jne	.notbadblock
	mov	ax, [bp - 6]
	mov	dx, [bp - 4]
	mov	cl, 3		; It's bad; therefore it's accounted for all by itself
	call	setbitsclustdi
	jmp	.clustdone
.notbadblock:
	call	[isendchain]
	jae	.endchain
	; Must be allocated block
	push	dx
	push	ax
	mov	ax, [bp - 6]
	mov	dx, [bp - 4]
	mov	cl, 2
	call	setbitsclustdi
	pop	ax
	pop	dx
	; if less than current block, check if free and prompt to repair
	mov	cx, [bp - 6]
	mov	bx, [bp - 4]
	add	cx, di
	adc	bx, 0
	cmp	dx, bx
	ja	.chkoverlarge
	jb	.chkallocbefore
	cmp	ax, cx
	ja	.chkoverlarge
	jb	.chkallocbefore
	jmp	.queryfixxlink	; Block looped on itself
.chkoverlarge:
	cmp	dx, [highestclust + 2]
	jb	.notoverlarge
	ja	.overlarge
	cmp	ax, [highestclust]
	jb	.notoverlarge
.overlarge:
	push	ax
	push	dx
	mov	cx, state_fat
	mov	dx, query_clustout
	call	queryfixyn
	cmp	al, 'y'
	pop	dx
	pop	ax
	je	.setendchain	 ; We are completely tolerant of not fixing this one
	jmp	.clustdonenotfree
.chkallocbefore:
	push	ax
	push	dx
	call	getbitsclust
	test	ch, 2
	jnz	.notoverlargexvector	; Fixes up stack and jmps to .notoverlarge
	mov	cx, state_fat
	mov	dx, query_freealloc
	call	queryfixyn
	cmp	al, 'y'
.notoverlargexvector:
	pop	dx
	pop	ax
	jne	.notoverlarge
	sub	[freeclust], word 1
	sbb	[freeclust + 2], word 0
	push	dx
	push	ax
	mov	cl, 2
	call	setbitsclust
	pop	ax
	pop	dx
	cmp	ax, [bp - 6]
	jne	.fixallocfarbefore
	cmp	dx, [bp - 4]
	jne	.fixallocfarbefore
	push	di
	mov	di, ax
	sub	di, [bp - 6]
	mov	dx, 0FFFh
	mov	ax, [endchainlow]
	call	entrytoblockall
	pop	di
	jmp	.notoverlarge
.fixallocfarbefore:
	push	es
	mov	es, [buf4seg]	; Load prior into buf4 and set end of chain
	push	ax
	push	dx
	call	invalidatesector
	pop	dx
	pop	ax
	mov	bx, 0FFFh
	push	bx
	mov	bx, [endchainlow]
	push	bx
	call	setclusterinfats
	pop	es
.notoverlarge:		; Technically the label is correct, the entry is not over the maximum size
	push	ax	; However it is also not referring to a known-free cluster
	push	dx
	call	getbitsclust
	pop	dx
	pop	ax
	test	ch, 1
	jz	.firstallocfound
.queryfixxlink:
	mov	dx, query_xlink
	mov	cx, state_fat
	call	queryfixyn
	cmp	al, 'y'		; Repair is an exercise in optimism; this will permit copy all files out
	jne	.nofixxlink	; Small chance of directory pass destroying both files
	; The obvious xlink-repair algorithm breaks down in the case of a file xlinked with itself.
	; I spent a long time trying to find an algorithm that worked. I found one, but it's ridiculous
	; and I'm not doing it. Truncate one of the files here.
	mov	dx, 0FFFh
	mov	ax, [endchainlow]
	call	entrytoblockall
.nofixxlink:
	jmp	.clustdonenotfree
.firstallocfound:
	mov	cl, 1
	call	setbitsclust

.clustdonenotfree:
	sub	word [freeclust], 1
	sbb	word [freeclust + 2], 0
.clustdone:
	inc	di
	cmp	di, [bp - 18]
	jb	.notfirst

	; Reached the end of the block; write back
	mov	ax, [bp - 16]
	mov	ax, [bp - 6]
	mov	dx, [bp - 4]
	mov	cx, [bp - 10]
	test	[bp - 1], byte 1
	jz	.notfirstwriteback
	mov	di, 0
	mov	es, [buf1seg]
	call	writebackfat
.notfirstwriteback:
	cmp	[numfats], byte 2
	jb	.advanceblockloop
	test	[bp - 1], byte 2
	jz	.notsecondwriteback
	mov	di, 1
	mov	es, [buf2seg]
	call	writebackfat
.notsecondwriteback:
	cmp	[numfats], byte 3
	jb	.advanceblockloop
	mov	di, 2
	mov	es, [buf3seg]
	call	writebackfat

	;Advance loop
.advanceblockloop:
	mov	ax, [bp - 6]
	mov	dx, [bp - 4]
	add	ax, [bp - 18]
	adc	dx, 0

	cmp	dx, [highestclust + 2]
	jb	.loopmore
	cmp	ax, [highestclust]
	jae	.exitloop
.loopmore:
	mov	[bp - 6], ax
	mov	[bp - 4], dx
	mov	di, savedfilename
	call	incrementprogress

	;Read in sectors for next loop
	mov	cx, [bp - 10]
	call	[getblockaddr]
	xor	cx, cx
	mov	[bp - 2], cx
	add	ax, [reservedsects]
	adc	dx, 0
.loopmorefats:
	push	cx
	mov	bx, cx
	shl	bx, 1
	mov	es, [buf1seg + bx]
	call	diskread0
	pop	cx
	jnc	.loopmorenotbad
	mov	bl, 1
	shl	bl, cl
	or	[bp - 1], bl
.loopmorenotbad:
	add	ax, [sectsperfat]
	adc	dx, [sectsperfat + 2]
	inc	cl
	cmp	cl, [numfats]
	jb	.loopmorefats
	jmp	.loop_top_not12		; Looks like it wants to jump to .fat_block_loop but we already know it's not FAT12
.exitloop:
	mov	sp, bp
	call	checkmark

	;Count up number of (non-empty) files in the FS
	mov	cx, totalfilesinc
	push	cx
	mov	ch, 2
	call	scanbitsclust
stage_dirwalk:
	mov	dx, state_dir
	mov	ah, 9
	int	21h
	xor	ax, ax			; Reset progress
	mov	[progress], ax
	mov	[progress + 2], ax
	mov	[pgdisplay], al
	mov	ax, [rootclust]
	mov	dx, [rootclust + 2]
	mov	si, [reservedsects]
	mov	di, [reservedsects + 2]
	mov	bx, 1
	call	descendtree

	cmp	[pgdisplay], byte 100
	jae	stage_finalize

	; Next: recover directories
stage_recover:
	mov	cx, recoverdirs
	push	cx
	mov	ch, 2
	call	scanbitsclust

	cmp	[pgdisplay], byte 100
	jae	stage_finalize

	; Next: recover files (all in this stage because % measurement)
	mov	cx, recoverfiles
	push	cx
	mov	ch, 2
	call	scanbitsclust

stage_finalize:
	call	checkmark
	mov	ax, [fatinfosect]
	cmp	ax, [reservedsects]
	jae	.ninfo
	;TODO fat info sector check for FAT32

.ninfo	test	[opflags], byte opflag_d
	jz	.done
	mov	es, [buf1seg]
	xor	di, di
	mov	si, dsc_totalfiles
	mov	cx, dsc_usedclust - dsc_totalfiles
	mov	ax, [totalfiles]
	mov	dx, [totalfiles + 2]
	call	gendigitslblcx
	mov	si, dsc_usedclust
	mov	cx, dsc_freeclust - dsc_usedclust
	mov	ax, [totalclust]
	mov	dx, [totalclust + 2]
	sub	ax, [freeclust]
	sbb	dx, [freeclust + 2]
	call	gendigitslblcx
	mov	si, dsc_freeclust
	mov	cx, dsc_end - dsc_freeclust
	mov	ax, [freeclust]
	mov	dx, [freeclust + 2]
	call	gendigitslblcx
	mov	al, '$'
	stosb
	push	ds
	push	es
	pop	ds
	xor	dx, dx
	mov	ah, 9
	int	21h
	pop	ds
.done	call	newline
	mov	al, 0
exit:	mov	ah, 4Ch
	push	ax
	cmp	[savedaccessflag], word 0
	jne	.nac
	mov	dx, savedaccessflag	; Restore prev. access: DOS data structures aren't initialized
	mov	bl, [disk]
	mov	ax, 440Dh
	mov	cx, 0847h
	int	21h
.nac	test	[opflags + 1], byte opflag2_a20
	jz	.na20
	mov	ah, 6
	call	far [xmsfunc]
.na20	mov	si, bitvectorptrs
.free	mov	al, [si + bitvectortype]
	cmp	al, b_mem_internal
	jb	.fdone2
	je	.fdone
	cmp	al, b_mem_umb
	jb	.fdos
	je	.fumb
	cmp	al, b_mem_xms
	jb	.fhi
	jmp	.fxms	; must be XMS (fix this if ever add EMS)
.fdos	mov	es, [si + bitvectorptr]
	mov	ah, 49h
	int	21h
	jmp	.fdone
.fumb	mov	ah, 11h
	mov	dx, [si + bitvectorptr]
	call	far [xmsfunc]
	jmp	.fdone
.fhi	mov	ah, 2h
	call	far [xmsfunc]
	jmp	.fdone
.fxms	mov	ah, 0Ah
	mov	dx, [si + bitvectorptr]
	call	far [xmsfunc]
.fdone	add	si, bitvectorrlen
	cmp	si, bitvectorptrs + bitvectorrlen * 128
	jb	.free
.fdone2	test	[opflags + 1], byte opflag2_ulink
	jne	.numb
	mov	ax, 5803h
	xor	bx, bx
	int	21h
.numb	pop	ax
	int	21h

ctrlchandler:
	push	cs
	pop	ds
	sti
	mov	ah, 9
	mov	dx, msg_cancel
	int	21h
	mov	al, error_cancel
	jmp	exit

iohandler:
	mov	al, 3
	iret

newbadsectors:
	mov	dx, msg_replacer
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit

; CX = state string
; DX = question string
; returns al = 'y' or 'n', preserves all other registers
queryfixyn:
	push	dx
	mov	dx, out_cr
	mov	ah, 9
	int	21h
	pop	dx
queryfixynpostcr:
	mov	ah, 9
	int	21h
	test	[opflags], byte opflag_f
	jnz	.y
.again	mov	ah, 7
	int	21h
	cmp	al, 3
	je	.cc
	cmp	al, 27
	je	.cc
	or	al, 20h
	cmp	al, 'y'
	je	.yn
	cmp	al, 'n'
	jne	.again
.yn	push	ax
	mov	dx, cx
	mov	ah, 9
	int	21h
	pop	ax
	ret
.y	push	bp
	mov	dl, 'y'
	mov	ah, 2
	int	21h
	mov	al, 'y'
	pop	bp
	jmp	.yn
.cc	jmp	ctrlchandler

gendigitslblcx:
	rep	movsb
; Writes DX:AX to ES:DI; clobbers all registers but BP; DX and AH are guaranteed 0 on exit
gendigits:
	xor	cx, cx
	mov	bx, 10
.dig	mov	si, ax
	xor	ax, ax
	xchg	ax, dx
	div	bx
	xchg	ax, si
	div	bx
	add	dl, '0'
	push	dx	; Taking all this stack is fine; we don't call DOS from here
	inc	cx
	mov	dx, si
	test	ax, ax
	jnz	.dig
	test	dx, dx
	jnz	.dig
.loop	pop	ax
	stosb
	loop	.loop
	ret

; Displays AX register; clobbers CX
outax:
	push	ax
	push	dx
	push	bp
	mov	cx, 404h
.nxt	rol	ax, cl
	push	ax
	and	al, 15
	add	al, '0'
	cmp	al, '9'
	jbe	.nadj
	add	al, 7
.nadj	mov	ah, 2
	mov	dl, al
	int	21h
	pop	ax
	dec	ch
	jnz	.nxt
	pop	bp
	pop	dx
	pop	ax
	ret

; Displays a checkmark
checkmark:	ret
	push	ax
	push	dx
	mov	dx, out_check
	mov	ah, 9
	int	21h
	pop	dx
	pop	ax
	ret

newline:
	push	ax
	push	dx
	mov	dx, out_newline
	mov	ah, 9
	int	21h
	pop	dx
	pop	ax
	ret

%ifdef DEBUG
outesline:	; DEBUG: outline pointed to by es
	push	ax
	push	cx
	mov	ax, [es:0]
	call	outax
	mov	ax, [es:2]
	call	outax
	mov	ax, [es:4]
	call	outax
	mov	ax, [es:6]
	call	outax
	mov	ax, [es:8]
	call	outax
	mov	ax, [es:10]
	call	outax
	mov	ax, [es:12]
	call	outax
	mov	ax, [es:14]
	call	outax
	pop	cx
	pop	ax
	ret
%endif

	;Given a cluster number, gets the next cluster number; that is, read fat cluster
	;Input: DX:AX = cluster, ES=buffer     Output: DX:AX = next cluster
	;Destroys BX, CX, DI
getnextcluster:
	push	si
	mov	si, .worker
	call	getsetcluster
	pop	si
	ret
.worker xor	cx, cx
	push	di
.loop	push	cx
	call	diskread0
	pop	cx
	jnc	.found
	inc	cl
	cmp	cl, [numfats]
	jb	.loop
	jmp	newbadsectors	; Oof
.found	pop	di
	jmp	[entryfromblock]

	; Arguments: ES=buffer, DX:AX = cluster to set, SP+2:SP+4 = value to set it to
	; pops argument from stack, destroys BX, CX, SI
setclusterinfats:
	push	bp
	mov	bp, sp
	push	ax	; Preserve actual cluster # for caller
	push	dx
	push	di
	mov	si, .worker
	call	getsetcluster
	pop	di
	pop	dx
	pop	ax
	pop	bp
	ret	4
.worker	xor	cx, cx
.loop	push	cx
	push	ax
	push	dx
	push	di
	call	diskread0
	jc	.skip
	pop	di
	mov	ax, [bp + 4]
	mov	dx, [bp + 6]
	call	[entrytoblock]
	pop	dx
	pop	ax
	push	ax
	push	dx
	push	di
	mov	ch, 40h
	call	diskwrite0
.skip	pop	di
	pop	dx
	pop	ax
	pop	cx
	add	ax, [sectsperfat]
	adc	dx, [sectsperfat + 2]
	inc	cx
	cmp	cl, [numfats]
	jb	.loop
	ret

	; Input: DX:AX = cluster, SI = worker function
	; Output: same as call SI
	; Preserves SI, BP and calls SI with DX:AX = offset into first FAT, DI = offset into block
getsetcluster:
	cmp	[fattype], byte 12
	jne	.not12f
	push	word [sectsperchunk]
	mov	cx, [sectsperfat]
	mov	[sectsperchunk], cx
.not12f	call	blockfromcluster
	call	si
	cmp	[fattype], byte 12
	jne	.not12s
	pop	word [sectsperchunk]
.not12s	ret

	; Input: DX:AX = cluster
	; Ouput: DX:AX = offset into first FAT, DI = offset into block
	; Preserves SI, BP
blockfromcluster:
	push	ax
	mov	ax, [wordsperchunk]
	call	[entriesperblock]
	xchg	ax, bx		; mov bx, ax
	xchg	ax, dx		; mov ax, dx
	xor	dx, dx
	div	bx
	xchg	ax, cx		; mov cx, ax
	pop	ax
	div	bx		; CX:AX = block number
	mov	bx, [sectsperchunk]
	mov	di, dx		; DI = offset into block
	mul	bx
	push	ax
	xchg	ax, cx
	mov	cx, dx
	mul	bx
	mov	dx, cx
	add	dx, ax
	pop	ax
	add	ax, [reservedsects]
	adc	dx, 0
	ret

	; Input: DX:AX = cluster, Output: DX:AX = sector, clobbers CX
sectorfromcluster:
	mov	cx, dx
	mul	word [sectsperclust]
	push	ax
	mov	ax, dx
	xchg	ax, cx
	mul	word [sectsperclust]	; Cannot overflow
	xchg	ax, dx
	pop	ax
	add	dx, cx
	add	ax, [zerothclustsect]
	adc	dx, [zerothclustsect + 2]
	ret

	; Writes a FAT block back to disk
	; DX:AX = offset as cluster no, CX = entries per block, ES = buffer
	; Preserves AX, CX, DX, destroys BX, SI, DI
writebackfat:
	push	ax
	push	dx
	push	cx
	call	[getblockaddr]
	cmp	[fattype], byte 12
	jne	.loop
	push	word [sectsperchunk]		; FAT 12 must process the whole at once
	mov	bx, [sectsperfat]
	mov	[sectsperchunk], bx
.loop	cmp	di, 0
	je	.nom
	add	ax, [sectsperfat]
	adc	dx, [sectsperfat + 2]
	dec	di
	jmp	.loop
.nom	add	ax, [reservedsects]
	adc	dx, 0
	mov	ch, 40h
	call	diskwrite0	; Doesn't return on failure so no pushf needed
	cmp	[fattype], byte 12
	jne	.nopopx
	pop	word [sectsperchunk]
.nopopx pop	cx
	pop	dx
	pop	ax
	ret

checkrootclustno:
	test	dx, dx
	jnz	.notlow
	cmp	ax, 2
	jb	.bad
.notlow	cmp	dx, [highestclust + 2]
	ja	.bad
	jb	.good
	cmp	ax, [highestclust]
	jae	.bad
.good	ret
.bad	mov	dx, msg_badroot
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit

	; Descend a directory tree. Called once with root dir as argument, and once
	; for each recovered directory. DX:AX = cluster (0 = root)
	; DI:SI = caller patchpoint *sector*, BX = caller patchpoint *byte* offset
	; of start of directory entry. If DI:SI points to start of FAT; BX = 1
	; which is a special case for update root directory starting cluster.
	; You don't have to worry about which FAT type it is.
	; Destroys all registers other than DS,SS
descendtree:
	;buf1seg = FAT buffer
	;buf2seg = directory buffer
	;buf3seg = (unused, maybe not allocated)
	;buf4seg = external stack for descent
	mov	bp, sp
	sub	sp, 50
	mov	es, [buf4seg]
	xor	cx, cx
	mov	[bp - 10], cx	; BP - 10 = VFAT entry reset
				; BP - 9 = state flags
				;	1 = invalidate
				;	2 = dirty
				;	4 = skip size prompt
	mov	[bp - 18], di	; BP - 20 = patchpoint sector
	mov	[bp - 22], bx	; BP - 22 = patchpoint byte offset
	mov	[bp - 24], cx	; BP - 24 = overflow from BP-22 when it's FAT
	mov	[bp - 26], cx	; BP - 26 = enumerator state
				;	0 = normal
				;	2 = set not directory and resume
				;	4 = set E5 scan (VFAT or /Z)
				;	6 = fix .. scan
				;	8 = look for nonzero scan
				;	10 = validate . and ..
				;	12 = validate VFAT continuance
				; BP - 30 = enumerator recovery state cluster
				; BP - 32 = enumerator recovery state offset
	mov	[bp - 36], cx	; BP - 38 = sector loaded into buf2seg
	mov	[bp - 38], cx
	dec	cx
	mov	[bp - 32], cx	; BP - 32 = enumerator recovery state offset
	mov	[bp - 30], cx	; BP - 30 = enumerator recovery state cluster
	mov	[bp - 28], cx
	mov	[bp - 12], cx	; BP - 12 = unwind start entry within cluster
	mov	[bp - 14], cx	; BP - 16 = unwind cluster
	mov	[bp - 16], cx
	xor	si, si
	mov	[es:si], ax	; Starting cluster of directory
	mov	[es:si + 2], dx	; TODO: check if FAT32 wants 0 or cluster for .. to root
	mov	[es:si + 4], ax	; Current cluster of directory
	mov	[es:si + 6], dx
	push	ax
	push	dx
	call	invalidatesector	; ES is not sector data
	pop	dx
	pop	ax
	mov	di, 2		; Skip initial . and ..
	cmp	[bp - 22], word 1
	jne	.start_isnotroot
	cmp	ax, [rootclust]
	jne	.start_isnotroot
	cmp	dx, [rootclust + 2]
	jne	.start_isnotroot
	xor	di, di		; But root dir does not have them
.start_isnotroot:
	mov	[bp - 34], di	; BP - 34 = Restart point
	mov	[es:si + 8], di	; Entry within cluster and control flag (0 = manual fix cluster, 1 = auto fix cluster)

	mov	ax, [bytespersector]
	mul	word [sectsperclust]	; cannot overflow
	mov	cl, 5
	shr	ax, cl
	mov	[bp - 8], ax	; BP - 8 = entries per cluster
	
	mov	ax, [sectsperclust]	; DX already 0 because the above mul
	mov	cx, [sectsperchunk]
	div	cx
	mov	[bp - 2], ax	; BP - 2 = chunks per cluster
	xchg	ax, cx
	mul	word [bytespersector]
	mov	cx, 32
	div	cx
	mov	[bp - 4], ax	; BP - 4 = entries per chunk

.readdirentryproc:
	mov	ax, [es:si + 8]
	xor	dx, dx
	cmp	[es:si + 4], dx
	jne	.readdircluster
	cmp	[es:si + 6], dx
	jne	.readdircluster

	div	word [bp - 4]
	mov	di, dx	; Entry within chunk

	; Read root directory entry
	mov	bx, [rootdirentries]
	mov	[bp - 6], bx	; BP - 6 = overflow check value
	xor	dx, dx
	add	ax, [reservedsects]
	adc	dx, 0
	mov	cl, [numfats]
.rfa	add	ax, [sectsperfat]
	adc	dx, [sectsperfat + 2]
	dec	cl
	jnz	.rfa
	jmp	.readdirchunk
.readdircluster:
	and	ax, 7FFFh
	div	word [bp - 4]
	mov	di, dx	; Entry within chunk
	mov	bx, ax	; Chunk within cluster
	mov	ax, [es:si + 4]
	mov	dx, [es:si + 6]
	mov	cx, [bp - 8]
	mov	[bp - 6], cx
	call	sectorfromcluster
	add	ax, bx
	adc	dx, word 0
.readdirchunk:
	test	[bp - 9], byte 1	; If we're rewinding we have to check if
	jnz	.readdirchunkcachemiss	; it's the same chunk or not, but if the
	cmp	[bp - 38], ax		; buffer were trashed, we can force reload
	jne	.readdirchunkcachemiss
	cmp	[bp - 36], dx
	je	.postreaddirbadsector
.readdirchunkcachemiss:
	and	[bp - 9], byte 0FEh
	mov	[bp - 36], dx	; BP - 38 = sector of current dir cluster
	mov	[bp - 38], ax
	mov	es, [buf2seg]
	push	si
	push	di
	call	diskread0
	pop	di
	pop	si
	jnc	.postreaddirbadsector
	mov	cx, [si + 4]
	or	cx, [si + 6]
	jz	.readrootdirbadsector
	jmp	.readdirbadsector
.readrootdirbadsector:
	; A bad sector in the root directory is not recoverable.
	mov	dx, msg_rootbadsect
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit
.postreaddirbadsector:			; means after the bad sector check on readdir

	mov	bx, [bp - 26]
	mov	ax, [descendtreetable + bx]
	mov	ch, bl

	mov	bx, di
	mov	cl, 5
	shl	bx, cl
	mov	es, [buf2seg]
	cmp	ch, 8
	jae	.scan_wants_e5_entries
	cmp	[es:bx], byte 0E5h
	je	.skipentry
.scan_wants_e5_entries:
	;call	.out_entryname		; DEBUG: uncomment when debugging descent routine
	jmp	ax
.skipentry:				; Don't process this one, OR return from processor
	mov	es, [buf4seg]
	inc	word [es:si + 8]
	inc	di
	mov	ax, [es:si + 4]
	mov	dx, [es:si + 6]
	mov	bx, [es:si + 8]
	cmp	bx, [bp - 32]
	jne	.notresetstate
	cmp	ax, [bp - 30]
	jne	.notresetstate
	cmp	dx, [bp - 28]
	jne	.notresetstate
	mov	cx, 0FFFFh	; Hit roll-forward marker to set state back to 0
	mov	[bp - 16], cx
	mov	[bp - 14], cx
	mov	[bp - 12], cx
	inc	cx		; set CX to 0
	mov	[bp - 26], cx
.notresetstate:
	test	si, si
	jnz	.skipentrynotroot
	cmp	[es:si + 4], word 2
	jae	.skipentrynotroot
	cmp	[es:si + 6], word 0
	jne	.skipentrynotroot
	mov	ax, [es:si + 8]
	cmp	ax, [rootdirentries]
	jb	.skipentrynotroot
	jmp	.enddirectory
.advancenorollover:
	test	[bp - 9], byte 1
	jz	.postreaddirbadsector
	jmp	.readdirentryproc	; Cache invalidated: reload
.skipentrynotroot:
	cmp	di, [bp - 4]
	jb	.advancenorollover
	call	.dirwritebackifdirty
	test	si, si
	jnz	.advancenotroot
	cmp	[es:si + 4], si
	jne	.advancenotroot
	cmp	[es:si + 6], si
	jne	.advancenotroot
.advancewithincluster:
	jmp	.readdirentryproc
.advancenotroot:
	mov	ax, [es:si + 8]
	and	ax, 7FFFh
	cmp	ax, [bp - 8]
	jb	.advancewithincluster
	mov	ax, [es:si + 4]
	mov	dx, [es:si + 6]
	mov	es, [buf1seg]
	mov	[bp - 42], ax
	mov	[bp - 40], dx	; BP - 40: undo getnextcluster
	call	getnextcluster
	call	[isbadblock]
	jne	.advanceisnotbad
	; This cluster is bad, but we already hit the bottom of it!
	; However, it could be write-bad only. Tail-migrate it is.
	mov	cx, state_fat
	mov	dx, query_fixtail
	call	queryfixyn
	cmp	al, 'y'
	mov	ax, [bp - 42]
	mov	dx, [bp - 40]
	jne	.enddirectory
	mov	[bp - 26], byte 255
	jmp	.readdirbadsector_recover
.advanceisnotbad:
	mov	es, [buf4seg]
	call	isendchainagnostic
	jc	.enddirectory
.advanceiscluster:
	mov	[es:si + 4], ax
	mov	[es:si + 6], dx
	and	[es:si + 8], word 8000h	;Reset entry within cluster, keep flag
	jmp	.readdirentryproc
.enddirectory:
	call	.dirwritebackifdirty
	mov	[bp - 26], byte 0	; Return to normal state
	sub	si, 10
	jc	.endscan
	or	[bp - 9], byte 1	; invalidate cache and force advancenorollover to recompute
	jmp	.skipentry
.endscan:
	mov	sp, bp
	ret

.scanlfnsubsequent:
	cmp	[es:bx + 0Bh], byte 0Fh
	je	.scanlfnentry
	cmp	[bp - 10], byte 1	; Not an LFN entry; check if LFN ended at end of sequence
	je	.scanlfnaccept
	clc
	jmp	.scanlfnremove
.scanlfnaccept:
	mov	al, [es:bx]
	cmp	al, byte 0
	je	.scanlfnremove	; CF is clear if E
	cmp	al, byte 0E5h
	je	.scanlfnremove
	mov	[bp - 26], byte 0
	mov	[bp - 10], byte 0
	jmp	.scannormal_entry
.scanvolume:
	cmp	[es:bx + 0Bh], byte 0Fh
	jne	.scanlfnskipentryv	; It's not an LFN entry
.scanlfn:			; It's an LFN entry. We remove stale LFN entries.
	mov	[bp - 26], byte 12
	mov	es, [buf4seg]
	mov	ax, [es:si + 4]	; Set up unwind
	mov	dx, [es:si + 6]
	mov	[bp - 16], ax
	mov	[bp - 14], dx
	mov	dx, [es:si + 8]
	mov	[bp - 12], dx
	mov	es, [buf2seg]
.scanlfnentry:
	mov	al, [es:bx]
	cmp	al, 0E5h
	je	.scanlfnremove	; CF is clear if E
	cmp	al, 0
	je	.scanlfnremove
	test	al, byte 40h	; Flag marking first LFN entry
	jz	.scanlfnnext
	test	[bp - 10], byte 0FFh
	jnz	.scanlfnremove	; LFN on top of LFN -- prior LFN chain is bad
	and	al, 3Fh
	jz	.scanlfnremovethis
	mov	[bp - 10], al
.scanlfnskipentryv:
	jmp	.skipentry	; It's a volume label. Nothing to do.

.scanlfnnext:
	mov	ah, [bp - 10]
	dec	ah
	jz	.scanlfnremovethis
	cmp	ah, al
	jne	.scanlfnremovethis
	mov	[bp - 10], ah
	jmp	.scanlfnskipentryv

.scanlfnremovethis:
	stc
.scanlfnremove:
	mov	[bp - 10], byte 0
	mov	es, [buf4seg]
	mov	ax, [es:si + 4]
	mov	dx, [es:si + 6]
	mov	di, [es:si + 8]
	adc	di, 0		; If this, reset after this entry; else before this entry
	mov	[bp - 30], ax
	mov	[bp - 28], dx
	mov	[bp - 32], di
	mov	ax, [bp - 16]
	mov	dx, [bp - 14]
	mov	di, [bp - 12]
	mov	[es:si + 4], ax
	mov	[es:si + 6], dx
	mov	[es:si + 8], di
	mov	[bp - 26], byte 4
	jmp	.readdirentryproc

.scansetnotdir:
	mov	[bp - 26], byte 0
	and	[es:bx + 0Bh], byte 0EFh; Clear directory flag
	or	[bp - 9], byte 2	; set dirty bit
	;fall-through

.scannormal_entry:
	mov	cx, 0FFFFh
	cmp	[es:bx + 0Bh], cl
	jne	.scansetnormal_notallones
	cmp	[es:bx + 1Ah], cx
	jne	.scansetnormal_notallones
	cmp	[es:bx + 14h], cx
	jne	.scansetnormal_notallones
.scansetnormal_delete:
	mov	[es:bx], byte 0E5h	; Auto-repair all ones entry (recovered bad sector *properly*)
	mov	[es:bx + 0Dh], byte 0E5h
	or	[bp - 9], byte 2	; set dirty bit
	jmp	.skipentry
.scansetnormal_notallones:
	test	[es:bx + 0Bh], byte 8
	jnz	.scanvolume
	call	.validate_entryname
	jnc	.scanentryname_valid
	mov	dx, query_badname
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scanentryname_valid
	call	.fix_entryname
.scanentryname_valid:
	call	.validate_notreserved
	jne	.scanentry_notreserved
	mov	dx, query_resname
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scanentry_notreserved
	mov	[es:bx + 3], byte '_'	; Trick: no reserved name contains an _; this slot seems most reasonable given the names
	or	[bp - 9], byte 2	; set dirty bit
.scanentry_notreserved:
	xor	dx, dx
	mov	ax, [es:bx + 1Ah]
	cmp	[fattype], byte 16
	jbe	.scannormal_not32get
	mov	dx, [es:bx + 14h]
.scannormal_not32get:
	test	dx, dx
	jnz	.scannormal_notlow
	cmp	ax, 1
	je	.scannormal_anomalous
.scannormal_notlow:
	cmp	dx, [highestclust + 2]
	jb	.scannormal_notlow2
	ja	.scannormal_anomalous
	cmp	ax, [highestclust]
	jb	.scannormal_notlow2
.scannormal_anomalous:
	mov	dx, query_anomfile
	call	.queryfixentry
	cmp	al, 'y'
	jne	.skipentry
	jmp	.scansetnormal_delete
.scannormal_notlow2:
	test	ax, ax
	jnz	.scannormal_notempty
	test	dx, dx
	jnz	.scannormal_notempty
	; Empty file
	test	[es:bx + 0Bh], byte 10h
	jz	.scannormal_notdirectory_empty
	push	ax
	push	dx
	mov	dx, query_fixemptyd
	call	.queryfixentry
	mov	es, [buf4seg]
	cmp	al, 'y'
	pop	dx
	pop	ax
	jne	.scannormal_notdirectory_empty
	and	[es:bx + 0Bh], byte 0EFh
	or	[bp - 9], byte 2	; set dirty bit
.scannormal_notdirectory_empty:
	mov	cx, [es:bx + 1Ch]
	or	cx, [es:bx + 1Eh]
	jnz	.scannormal_notdirectory_empty0
	mov	dx, query_fixbadlen
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scannormal_notdirectory_empty0
	xor	cx, cx
	mov	[es:bx + 1Ch], cx
	or	[es:bx + 1Eh], cx
	or	[bp - 9], byte 2	; set dirty bit
.scannormal_notdirectory_empty0:
	jmp	.skipentry
.scannormal_notempty:
	push	di
	mov	di, totalfiles
	call	incrementprogress
	pop	di
	push	bx
	push	si	
	push	ax
	push	dx
	call	getbitsclust
	test	ch, 2
	jz	.scanbits_free
	test	ch, 1
	jnz	.scanbits_xlink
	mov	cl, 1
	pop	dx
	pop	ax
	push	ax
	push	dx
	call	setbitsclust
	pop	dx
	pop	ax
	pop	si
	pop	bx
	test	[es:bx + 0Bh], byte 10h
	jz	.scannormal_notdirectory
	jmp	.scannormal_directory
.scannormal_notdirectory:
	test	[opflags], byte opflag_c
	jz	.scannormal_notdirectory_empty0
	jmp	.scannormal_notdirectory_traverse
.scanbits_free:
	pop	dx
	pop	ax
	pop	si
	pop	bx
	mov	dx, query_freefile
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scanbits_nofix
	jmp	.scansetnormal_delete
.scanbits_xlink:
	pop	dx
	pop	ax
	pop	si
	pop	bx
	mov	dx, query_xlinkfile
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scanbits_nofix
	jmp	.scansetnormal_delete
.scanbits_nofix:
	jmp	.skipentry
.scannormal_end:
	test	[opflags], byte opflag_c
	jz	.scannormal_enddirectoryv
	mov	es, [buf4seg]
	mov	ax, [es:si + 4]
	mov	dx, [es:si + 6]
	mov	es, [buf1seg]
	push	si
	push	ax
	push	dx
	call	getnextcluster
	pop	cx
	pop	bx
	call	[isbadblock]
	jne	.scandirtail_notbad
	push	bx
	push	cx
	mov	cx, state_fat
	mov	dx, query_fixtail
	call	queryfixyn
	cmp	al, 'y'
	pop	dx
	pop	ax
	pop	si
	jne	.scannormal_enddirectoryv
	mov	[bp - 26], byte 255
	jmp	.readdirbadsector_recover
.scandirtail_notbad:
	call	isendchainagnostic
	jc	.scannormal_enddirectoryv
	push	ax
	push	dx
	call	getnextcluster
	pop	cx
	pop	bx
	call	[isbadblock]
	jne	.scandirtail_notbad
	push	bx
	push	cx
	mov	cx, state_fat
	mov	dx, query_fixtail
	call	queryfixyn
	cmp	al, 'y'
	pop	dx
	pop	ax
	jne	.scannormal_enddirectoryv
	mov	cx, 0FFFh
	push	cx
	mov	cx, [endchainlow]
	push	cx
	call	setclusterinfats
.scannormal_enddirectoryv:
	jmp	.enddirectory
.scannormal:
	cmp	[es:bx], byte 0
	jne	.scannormal_entry
	test	[opflags], byte opflag_z
	jz	.scannormal_end
	mov	es, [buf4seg]
	mov	ax, [es:si + 4]		; Set up for setting 2E if an entry is found below
	mov	dx, [es:si + 6]
	mov	bx, [es:si + 8]
	mov	[bp - 16], ax
	mov	[bp - 14], dx
	mov	[bp - 12], bx
	mov	[bp - 26], byte 8
	jmp	.skipentry
.scannormal_toodeep:
	mov	dx, msg_toodeep
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit
.scannormal_directory:
	;We encountered a directory, set up to descend
	cmp	si, 500
	ja	.scannormal_toodeep
	push	ax
	push	dx
	call	.dirwritebackifdirty
	pop	dx
	pop	ax
	push	ds			; Need directory name for repair prompts below
	push	es
	push	es
	push	ds
	pop	es
	pop	ds
	push	si
	push	di
	mov	si, bx
	mov	di, savedfilename
	mov	cx, 11
	rep	movsb
	pop	di
	pop	si
	pop	es
	pop	ds
	mov	cx, [es:bx + 16h]
	mov	[savedfilename + 12], cx	; DIR created time
	mov	cx, [es:bx + 18h]
	mov	[savedfilename + 14], cx	; DIR created date
	mov	es, [buf4seg]
	add	si, 10
	mov	[es:si], ax
	mov	[es:si + 2], dx
	mov	[es:si + 4], ax
	mov	[es:si + 6], dx
	mov	[es:si + 8], word 1	; Start processing at the second entry when validate dots finishes
	mov	cx, 0FFFFh
	mov	[bp - 32], cx	; enumerator recovery state offset
	mov	[bp - 30], cx	; enumerator recovery state clsuter
	mov	[bp - 28], cx
	mov	[bp - 12], cx	; unwind start entry within cluster
	mov	[bp - 14], cx	; unwind cluster
	mov	[bp - 16], cx
	mov	[bp - 26], byte 10	; Validate . and .. after load
	jmp	.readdirentryproc
.scannormal_notdirectory_traverse:
	and	[bp - 9], byte 0FBh	; clear no prompt size bit
	mov	cx, [es:bx + 1Ch]
	mov	[bp - 44], cx		; BP - 44: size of file in entry
	mov	cx, [es:bx + 1Eh]
	mov	[bp - 46], cx
	xor	cx, cx
	mov	[bp - 48], cx		; BP - 48: size of file accumulated
	mov	[bp - 50], cx
	push	si
	push	di
	push	bx
	mov	es, [buf1seg]
	call	getnextcluster
	call	[isbadblock]
	jne	.scannormal_notbad
	pop	bx
	mov	es, [buf2seg]
	mov	dx, query_fixbadptr
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scannormal_notdirectory_end2
	xor	cx, cx
	cmp	[fattype], byte 16
	jne	.scannormal_firstbad_not32set
	mov	[bx + 14h], cx
.scannormal_firstbad_not32set:
	mov	[bx + 1Ah], cx
	mov	[bx + 1Ch], cx
	mov	[bx + 1Eh], cx
	jmp	.scannormal_notdirectory_end2
.scannormal_notbad:
	call	isendchainagnostic
	jc	.scannormal_endchain
	mov	cx, [bytesperclust]
	add	[bp - 48], cx
	adc	[bp - 50], word 0
	jc	.scannormal_impossiblybig
	mov	[bp - 40], ax
	mov	[bp - 42], dx
	call	getnextcluster
	call	[isbadblock]
	jne	.scannormal_notbad
	jmp	.scannormal_subsequent_bad
.scannormal_impossiblybig:
	mov	dx, out_crfile		; Too big
	mov	ah, 9
	int	21h
	pop	bx
	mov	es, [buf2seg]
	call	.out_entryname
	mov	dx, msg_2bigfile
	mov	ah, 9
	int	21h
	mov	dx, state_dir
	mov	ah, 9
	int	21h
	jmp	.scannormal_notdirectory_end2
.scannormal_subsequent_bad:
	pop	bx
	mov	es, [buf2seg]
	mov	dx, query_fixbadptr
	call	.queryfixentry
	mov	es, [buf1seg]
	cmp	al, 'y'
	jne	.scannormal_notdirectory_end2
	push	bx
	mov	ax, [bp - 40h]
	mov	dx, [bp - 42h]
	mov	cx, 0FFFh
	push	cx
	mov	cx, [endchainlow]
	push	cx
	call	setclusterinfats
	or	[bp - 9], byte 4
	jmp	.scannormal_endchain
.scannormal_notdirectory_end3:
	pop	bx
.scannormal_notdirectory_end2:
	pop	di
	pop	si
.scannormal_notdirectory_end:
	jmp	.skipentry
.scannormal_endchain:
	mov	si, [bp - 48]
	mov	di, [bp - 50]
	cmp	di, [bp - 46]
	ja	.scannormal_file_tooshort
	jb	.scannormal_file_nottooshort
	cmp	si, [bp - 44]
	jae	.scannormal_file_tooshort
.scannormal_file_nottooshort:
	add	si, [bytesperclust]
	adc	di, 0
	jc	.scannormal_file_nottoolong
	cmp	di, [bp - 46]
	jb	.scannormal_file_nottoolong
	ja	.scannormal_file_toolong
	cmp	si, [bp - 44]
	jb	.scannormal_file_tooshort
.scannormal_file_nottoolong:
	jmp	.scannormal_notdirectory_end3
.scannormal_file_tooshort:
	add	si, [bytesperclust]	; File is too short; take whole clusters
	adc	di, 0
	jc	.scannormal_file_toolong	; naturally occurring pun
	mov	si, 0FFFFh
	mov	di, 0FFFFh
.scannormal_file_toolong:
	mov	es, [buf2seg]
	pop	bx
	test	[bp - 9], byte 4
	jnz	.scannormal_file_setlength
	mov	dx, query_fixbadlen
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scannormal_file_nosetlength
.scannormal_file_setlength:
	mov	[es:bx + 1Ch], si
	mov	[es:bx + 1Eh], di
	or	[bp - 9], byte 2	; set dirty bit
.scannormal_file_nosetlength:
	jmp	.scannormal_notdirectory_end2

.scansete5:
	mov	[es:bx], byte 0E5h
	mov	[es:bx + 0Bh], byte 0
	mov	[es:bx + 0Dh], byte 0E5h
	mov	[es:bx + 14h], word 0
	mov	[es:bx + 1Ah], word 0
	or	[bp - 9], byte 2	; set dirty bit
.scanskipreturn:
	jmp	.skipentry

.scanfixparent:
	test	[es:bx + 0Bh], byte 08h
	jnz	.scanskipreturn
	test	[es:bx + 0Bh], byte 10h
	jz	.scanskipreturn
	mov	ax, [es:bx + 1Ah]
	xor	dx, dx
	cmp	[fattype], byte 16
	jbe	.scanfixparent_notfat32
	mov	dx, [es:bx + 14h]
.scanfixparent_notfat32:
	push	ax
	push	dx
	call	.dirwritebackifdirty
	pop	dx
	pop	ax
	call	sectorfromcluster
	push	si
	push	di
	call	diskread0
	pop	di
	pop	si
	or	[bp - 9], byte 1
	jc	.scanskipreturn		; Not dealing with this nonsense
	mov	es, [buf4seg]
	call	.dirwritebackgetch
	push	cx
	mov	bx, [es:si]
	mov	cx, [es:si + 2]
	mov	es, [buf2seg]
	mov	[es:3Ah], bx
	cmp	[fattype], byte 16
	jbe	.scanfixparent_notfat32dentry
	mov	[es:34h], cx
.scanfixparent_notfat32dentry:
	pop	cx			; from .dirwritebackgetch
	push	si
	push	di
	call	diskwrite0
	pop	di
	pop	si
.scanskipreturn_vnotzero:
	jmp	.scanskipreturn

.scannotzero:
	cmp	[es:bx], byte 0
	je	.scanskipreturn_vnotzero
	mov	bx, [bp - 32]
	mov	ax, [bp - 30]
	mov	dx, [bp - 28]
	xchg	ax, [es:si + 4]			; Set exit point
	xchg	dx, [es:si + 6]
	xchg	bx, [es:si + 8]
	mov	[bp - 32], bx
	mov	[bp - 30], ax
	mov	[bp - 28], dx
	mov	[bp - 26], byte 4
	jmp	.scanskipreturn_vnotzero

	; Rebuild trashed . entries from bad sector recovery
.dotentries_allbits:
	xor	di, di
	mov	es, [buf2seg]
	mov	bx, [es:si + 2]
	mov	cx, [es:si]
	mov	es, [buf4seg]
	mov	ax, '. '
	call	generatedotdirectoryentry
	mov	es, [buf4seg]
	mov	bx, [es:si - 8]
	mov	cx, [es:si - 10]
	mov	es, [buf2seg]
	mov	ax, '..'
	call	generatedotdirectoryentry
	or	[bp - 9], byte 2	; set dirty bit
	jmp	.dotentries_match2

.scanvalidatedotentries:
	; Seems to be going wrong around here; we read cluster 3 but it doesn't contain the expected
	; There's four things that can happen
	; 1) Sector could be all 0 or all 1 bits, in which case we restore the . and .. entries
	; 2) Sector could have . and .. entries in which case we might or might not have to repair
	; 3) Sector could be not a directory in which case we repair by clearing directory bit
	; 4) Sector could be a directory
	xor	ax, ax
	mov	cx, 256
	xor	di, di
	repe	scasw
	je	.dotentries_allbits
	dec	ax
	mov	cx, 256
	xor	di, di
	repe	scasw
	je	.dotentries_allbits
	mov	es, [buf4seg]
	mov	bx, [es:si]
	mov	cx, [es:si + 2]
	mov	es, [buf2seg]
	call	check_dots
	je	.dotentries_match
.dotentries_nomatch:
	; It's not a directory; prompt repair/clear
	mov	dx, out_crfile
	mov	ah, 9
	int	21h
	mov	bx, savedfilename
	call	.out_entrynameds
	mov	dx, query_dirnot
	mov	ah, 9
	int	21h
.again	mov	ah, 7
	int	21h
	cmp	al, 3
	je	.cc
	cmp	al, 27
	je	.cc
	or	al, 20h
	cmp	al, 'f'
	je	.yn
	cmp	al, 'd'
	jne	.again
.yn	push	ax
	mov	dx, state_dir
	mov	ah, 9
	int	21h
	pop	ax
	cmp	al, 'f'
	je	.dotentries_isfile
	jmp	.dotentries_allbits
.cc	jmp	ctrlchandler
.dotentries_match:
	; Validate parent pointer in ..
	mov	es, [buf4seg]
	mov	bx, [es:si - 10]
	mov	cx, [es:si - 8]
	mov	es, [buf2seg]
	cmp	[fattype], byte 16
	jbe	.dotentries_fat16b
	cmp	[es:14h + 32], cx
	jne	.dotentries_nomatchparent
.dotentries_fat16b:
	cmp	[es:1Ah + 32], bx
	je	.dotentries_match2
.dotentries_nomatchparent:
	mov	es, [buf4seg]
	cmp	[es:si - 10], word 0
	jne	.dotentries_maybeautofixparent
	cmp	[es:si - 8], word 0
	je	.dotentries_noautofixparent	; Flag only exists on not root directory
.dotentries_maybeautofixparent:			; Otherwise its bit is reused as offset into root
	test	[es:si - 1], byte 80h
	mov	es, [buf2seg]
	je	.dotentries_fixparent
.dotentries_noautofixparent:
	push	ds
	pop	es
	mov	bx, savedfilename
	mov	dx, query_fixdotdot
	call	.queryfixentry
	mov	es, [buf4seg]
	cmp	al, 'y'
	jne	.dotentries_match2
.dotentries_fixparent:
	cmp	[fattype], byte 16
	jbe	.dotentries_fat16c
	mov	[es:14h + 32], cx
.dotentries_fat16c:
	mov	[es:1Ah + 32], bx
	or	[bp - 9], byte 2	; set dirty bit
.dotentries_match2:
	; Start processing normally at the second entry
	mov	es, [buf4seg]
	mov	di, 1
	mov	[bp - 26], byte 0
	jmp	.skipentry
.dotentries_isfile:
	sub	si, 10
	mov	[bp - 26], byte 2
	mov	es, [buf4seg]
	jmp	.readdirentryproc

.dirwritebackifdirty:
	test	[bp - 9], byte 2
	jnz	.dirwriteback
	ret
	
.dirwriteback:
	and	[bp - 9], byte 0FDh
	mov	dx, [bp - 36]	; BP - 38 = sector of current dir cluster
	mov	ax, [bp - 38]
	mov	es, [buf4seg]
	call	.dirwritebackgetch
	mov	es, [buf2seg]
	push	si
	push	di
	call	diskwrite0
	pop	di
	pop	si
	ret

.queryfixentry:
	push	dx
	mov	dx, out_crfile
	mov	ah, 9
	int	21h
	pop	dx
	call	.out_entryname
	mov	cx, state_dir
	jmp	queryfixynpostcr

.validate_entryname:
	push	si
	xor	si, si
	mov	cx, 11
.validate_entryname_loop:
	call	.validatechar
	jc	.validate_entryname_ret
	inc	si
	loop	.validate_entryname_loop
	clc
.validate_entryname_ret:
	pop	si
	ret

.fix_entryname:
	push	si
	xor	si, si
	mov	cx, 11
.fix_entryname_loop:
	call	.validatechar
	jnc	.nofix_char
	cmp	al, 'a'
	jb	.underscore_char
	cmp	al, 'z'
	ja	.underscore_char
	and	al, 'a' - 'A'
	jmp	.fix_char
.underscore_char:
	mov	al, '_'
.fix_char:
	mov	[es:bx + si], al
.nofix_char:
	inc	si
	loop	.fix_entryname_loop
	pop	si
	or	[bp - 9], byte 2	; set dirty bit
	ret

.validatechar:
	mov	al, [es:bx + si]
	cmp	al, 32
	jae	.validate_notc
	cmp	al, 5
	jne	.validate_no
	test	si, si
	jne	.validate_no
	clc
	ret
.validate_notc:
	cmp	al, 'a'
	jb	.validate_notl
	cmp	al, 'z'
	ja	.validate_notl
.validate_no:
	stc
	ret
.validate_notl:
	cmp	al, '.'
	je	.validate_no
	cmp	al, '"'
	je	.validate_no
	cmp	al, '*'
	je	.validate_no
	cmp	al, '+'
	je	.validate_no
	cmp	al, ','
	je	.validate_no
	cmp	al, '/'
	je	.validate_no
	cmp	al, ':'
	je	.validate_no
	cmp	al, ';'
	je	.validate_no
	cmp	al, '<'
	je	.validate_no
	cmp	al, '='
	je	.validate_no
	cmp	al, '>'
	je	.validate_no
	cmp	al, '?'
	je	.validate_no
	cmp	al, '\'
	je	.validate_no
	cmp	al, '['
	je	.validate_no
	cmp	al, ']'
	je	.validate_no
	cmp	al, '|'
	je	.validate_no
	clc
	ret

.validate_notreserved:
	push	si
	push	di
	mov	si, reservednames
.validate_notreserved_loop:
	mov	di, bx
	mov	cx, 4
	repe	cmpsw
	je	.validate_isreserved
	cmp	[si], byte 0
	jne	.validate_notreserved_loop
	cmp	[si], byte 1	; force NE
.validate_isreserved:
	pop	di
	pop	si
	ret

.out_entryname:
	push	ds
	push	es
	pop	ds
	call	.out_entrynameds
	pop	ds
	ret

.out_entrynameds:
	push	ax
	push	bx
	push	cx
	push	dx
	push	bp		; Some DOS apparently depends on BIOS saving BP but some BIOS doesn't
	mov	cl, 0
.out_el	mov	dl, [bx]
	test	cl, cl
	jnz	.out_en0
	cmp	dl, 05h
	jne	.out_en0
	mov	dl, 0e5h
.out_en0:
	cmp	dl, 7
	je	.out_en0a
	cmp	dl, 8
	je	.out_en0a
	cmp	dl, 9
	je	.out_en0a
	cmp	dl, 10
	je	.out_en0a
	cmp	dl, 13
	jne	.out_en0b
.out_en0a:
	mov	dl, '?'
.out_en0b:
	mov	ah, 2
	int	21h
	inc	bx
	inc	cx
	cmp	cl, 8
	jne	.out_en8
	mov	dl, '.'
	mov	ah, 2
	int	21h
.out_en8:
	cmp	cl, 11
	jne	.out_el
	pop	bp
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret

.dirwritebackgetch:
	mov	ch, 0C0h
	cmp	[es:si + 4], word 0
	jne	.dirwritebackcluster
	cmp	[es:si + 6], word 0
	jne	.dirwritebackcluster
	mov	ch, 80h
.dirwritebackcluster:
	ret

.readdirnofixbad:		; We can't recover from skipping this one
	mov	al, error_nofix
	jmp	exit
	; Routine for repairing bad sector in directory
.readdirbadsector:
	mov	cx, state_dir
	mov	dx, query_dirbadsec
	call	queryfixyn
	cmp	al, 'y'
	jne	.readdirnofixbad
.readdirbadsector_recover:
	mov	es, [buf4seg]
	mov	ax, [es:si + 4]
	mov	dx, [es:si + 6]
	mov	cx, .recbadsectorfill
	cmp	ax, [es:si]
	jne	.recbadsector_notfirstchildclust
	cmp	dx, [es:si + 4]
	jne	.recbadsector_notfirstchildclust
	test	ax, ax
	jnz	.recbadsector_firstchildclust
	test	dx, dx
	jz	.recbadsector_notfirstchildclust
.recbadsector_firstchildclust:
	mov	cx, .recbadsectorfillfirst
.recbadsector_notfirstchildclust:
	push	si
	push	di
	push	cx
	mov	ax, [es:si]
	mov	dx, [es:si + 2]
	cmp	ax, [es:si + 4]
	jne	.readdirbadsector_subsequent
	cmp	dx, [es:si + 6]
	jne	.readdirbadsector_subsequent
	test	si, si
	jnz	.readdirbadsector_childcall
	push	word [bp - 18]
	push	word [bp - 20]
	push	word [bp - 22]
	push	word [bp - 24]
	jmp	.readdirbadsector_es
.readdirbadsector_subsequent:
	xor	ax, ax
	push	ax
	inc	ax
	push	ax
	push	word [bp - 40]
	push	word [bp - 42]
.readdirbadsector_childcall:
	;Generate patchpoint from unwind data
	mov	ax, [es:si + 8 - 10]
	and	ax, 7FFFh
	xor	dx, dx
	mov	cx, 32
	mul	cx
	div	word [bytespersector]
	xchg	ax, cx	; CX = sector offset within cluster
	xchg	bx, dx	; BX = byte offset within sector
	mov	ax, [es:si + 4 - 10]
	mov	dx, [es:si + 6 - 10]
	push	cx
	call	sectorfromcluster
	pop	cx
	add	ax, cx	; Add sector to starting sector in cluster
	xor	cx, cx
	adc	dx, cx
	push	dx
	push	ax
	push	cx	; 0
	push	bx	; byte offset within sector
.readdirbadsector_es:
	mov	es, [buf2seg]
	call	recoverbadsector
	mov	es, [buf4seg]
	pop	di
	pop	si
	mov	bx, [es:si]
	mov	cx, [es:si + 2]
	cmp	bx, [es:si + 4]
	jne	.recbadnotfirst
	cmp	cx, [es:si + 6]
	jne	.recbadnotfirst
	mov	[bp - 30], cx
	mov	[bp - 28], bx
	mov	[bp - 26], byte 6
	or	di, 8000h
	mov	[bp - 32], di
	mov	[es:si + 8], word 8000h
	jmp	.readdirentryproc
.recbadnotfirst:
	mov	bx, ax
	mov	cx, dx
	mov	[es:si], ax
	mov	[es:si + 2], dx
	cmp	[bp - 26], byte 255
	je	.recbadenddir
	jmp	.readdirentryproc
.recbadenddir:
	jmp	.enddirectory

.recbadsectorfillfirst:
	cmp	si, [sectsperclust]	; SI counts *down* to loop terminator
	jne	.recbadsectorfill
	; Construct . and .. entries
	; This code is dirty
	mov	bx, [bp - 10]
	mov	cx, [bp - 12]
	mov	ax, '. '
	call	generatedotdirectoryentry
	; And this is even dirtier
	push	si
	push	es
	mov	si, [bp + 16]
	mov	es, [buf4seg]
	mov	bx, [si - 8]
	mov	cx, [si - 10]
	pop	es
	pop	si
	mov	ax, '..'
	call	generatedotdirectoryentry
	; From here it's the normal filler
.recbadsectorfill:
	mov	ax, 20E5h
	stosw			; 0
	mov	ax, '  '
	stosw			; 2-8
	stosw
	stosw
	stosw
	mov	ax, ' '
	stosw			; A
	mov	ah, 0E5h	; C (0D = E5 = can't undelete)
	stosw
	mov	ah, 0
	mov	cx, 9		; E-1E
	stosw
	cmp	di, [bytespersector]
	jb	.recbadsectorfill
	ret

	; Given a bad cluster, swaps it for a new cluster and updates pointers
	; DX:AX = cluster, ES = working buffer
	; On stack: BP + 4 (SP + 2) target cluster/offset
	;           BP + 8 (SP + 6) target sector
	;           BP + 12 (SP + 10) sector builder (called with DI=0; must preserve SI)
	; Destroys BX, CX, SI, DI
	; Returns DX:AX = new cluster
recoverbadsector:
	push	bp
	mov	bp, sp
	sub	sp, 16
	mov	[bp - 4], ax	; BP - 4 = cluster being replaced
	mov	[bp - 2], dx
	call	sectorfromcluster
	mov	[bp - 8], ax	; BP - 6 = sector being replaced
	mov	[bp - 6], dx
	call	getnextcluster
	call	[isbadblock]
	je	.isalreadymarked
	push	ax		; Preserve
	push	dx
	mov	ax, 0FFF7h
	mov	dx, 0FFFh
	push	dx
	push	ax
	mov	ax, [bp - 8]	; Location being marked bad
	mov	dx, [bp - 6]
	call	setclusterinfats
	pop	dx
	pop	ax
	jmp	.recover_alloc
.isalreadymarked:
	mov	dx, 0FFFh		; Since the target is already marked, the replacement must
	mov	ax, [endchainlow]	; be end chain as we don't know what comes next.
.recover_alloc:
	call	alloccluster	; Argument to alloccluster is the value to set it to
	mov	[bp - 12], ax	; BP - 12 = replaced cluster
	mov	[bp - 10], dx
	call	sectorfromcluster
	mov	[bp - 16], ax	; BP - 16 = replaced sector
	mov	[bp - 14], dx
	
	mov	si, [sectsperclust]
	push	word [sectsperchunk]
	mov	[sectsperchunk], word 1
.recbadsectorloop:
	push	si
	mov	ax, [bp - 8]
	mov	dx, [bp - 6]
	call	diskread0
	jnc	.recgoodsector
	xor	di, di
	call	[bp + 12]
.recgoodsector:
	mov	ch, 0C0h
	mov	ax, [bp - 16]
	mov	dx, [bp - 14]
	call	diskwrite0
	pop	si
	add	[bp - 8], word 1
	adc	[bp - 6], word 0
	add	[bp - 16], word 1
	adc	[bp - 14], word 0
	dec	si
	jnz	.recbadsectorloop
	pop	word [sectsperchunk]

	;update previous reference
	mov	ax, [bp + 8]
	mov	dx, [bp + 10]
	test	dx, dx
	jnz	.issector
	cmp	ax, [reservedsects]
	jae	.issector
	mov	ax, [bp + 4]
	mov	dx, [bp + 6]
	test	[opflags + 1], byte opflag2_rclust
	jne	.iscluster
	cmp	dx, 0
	jne	.isboot
	cmp	ax, 1
	jbe	.isboot
.iscluster:
	push	word [bp - 10]
	push	word [bp - 12]
	call	setclusterinfats
	jmp	.epilog
.isboot:
	; If we get here, must be a FAT32 EBPB
	push	word [sectsperchunk]
	mov	[sectsperchunk], word 1
	xor	ax, ax
	xor	dx, dx
	call	diskread0
	jc	newbadsectors
	mov	bx, [bp - 10]
	mov	cx, [bp - 12]
	mov	[es:2Ch], bx
	mov	[es:2Eh], cx
	xor	ax, ax
	xor	dx, dx
	mov	ch, 0
	call	diskwrite0
	mov	ax, [es:032h]
	cmp	ax, 0
	je	.nobackupbootsector
	cmp	ax, [es:0Eh]
	jae	.nobackupbootsector
	xor	dx, dx
	call	diskread0
	jc	.nobackupbootsector
	mov	bx, [bp - 10]
	mov	cx, [bp - 12]
	mov	[es:2Ch], bx
	mov	[es:2Eh], cx
	xor	dx, dx
	mov	ch, 0
	call	diskwrite0
.nobackupbootsector:
	pop	word [sectsperchunk]
	jmp	.epilog
.issector:	; Operates on chunk at once
	call	diskread0
	jc	newbadsectors
	mov	bx, [bp + 4]
	mov	cx, [bp - 12]
	mov	[es:bx + 1Ah], cx
	cmp	[fattype], byte 32
	jne	.dirnot32
	mov	cx, [bp - 10]
	mov	[es:bx + 14h], cx
.dirnot32:
	mov	ch, 0C0h
	call	diskwrite0
.epilog:
	; return new cluster
	mov	ax, [bp - 12]
	mov	dx, [bp - 10]
	mov	sp, bp
	ret	10

	;Allocate cluster. ES = buffer, DX:AX = value to set it to. Trashes all registers except ES, BP
alloccluster:
	push	dx	; See setclusterinfats
	push	ax
	push	es
	mov	ch, 0
	mov	ax, .found
	push	ax
	call	scanbitsclust
	mov	dx, msg_outofspace
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit
.found	call	scanbitsclust.unwind
	mov	sp, bp		; longjmp!
	pop	bp
	pop	cx		; Trash
	pop	cx
	pop	es
	;DX:AX is allocated cluster
	;on stack is cluster to point it to
	sub	[freeclust], word 1
	sbb	[freeclust + 2], word 0
	push	dx
	push	ax
	mov	ch, 3
	call	setbitsclust
	pop	ax
	pop	dx
	call	setclusterinfats
	ret

	; Checks if this sector is the first sector of a directory, by checking . and .. entries
	; ES:0 = buffer, CX:BX = cluster number of buffer
	; Returns; ZF set if match, clear if no match
check_dots:
	cmp	[es:0], word '. '
	jne	.ret
	cmp	[es:2], word '  '
	jne	.ret
	cmp	[es:4], word '  '
	jne	.ret
	cmp	[es:6], word '  '
	jne	.ret
	cmp	[es:8], word '  '
	jne	.ret
	cmp	[es:10], byte ' '
	jne	.ret
	cmp	[fattype], byte 16
	jbe	.fat16
	cmp	[es:14h], cx
	jne	.ret
.fat16	cmp	[es:1Ah], bx
	jne	.ret
	cmp	[es:32], word '..'
	jne	.ret
	cmp	[es:2 + 32], word '  '
	jne	.ret
	cmp	[es:4 + 32], word '  '
	jne	.ret
	cmp	[es:6 + 32], word '  '
	jne	.ret
	cmp	[es:8 + 32], word '  '
	jne	.ret
	cmp	[es:10 + 32], byte ' '
.ret	ret

	;Arguments: AX = '. ' or '..'
	;BX:CX = target cluster
	;ES:DI = output buffer
generatedotdirectoryentry:
	stosw			; 0
	mov	ax, '  '
	stosw
	stosw
	stosw
	stosw
	mov	ah, 10h
	stosw			; A
	xor	ax, ax
	stosw			; C
	stosw
	stosw
	stosw			; 12
	mov	ax, bx
	stosw			; 14
	xor	ax, ax
	mov	ax, [savedfilename + 12]
	stosw			; 16
	mov	ax, [savedfilename + 14]
	stosw
	mov	ax, cx
	stosw			; 1A
	xor	ax, ax
	stosw
	stosw
	ret

; Checks if target is directory. If so, recovers it
recoverdirs:
	push	ax
	push	dx
	push	bp
	mov	bp, sp
	sub	sp, 12
	mov	[bp - 2], dx	; Cluster
	mov	[bp - 4], ax
	call	sectorfromcluster
	mov	es, [buf1seg]
	call	diskread0
	jc	.nope
	mov	cx, [bp - 2]
	mov	bx, [bp - 4]
	xor	di, di
	call	check_dots
	jz	.found
.nope	mov	sp, bp
	pop	bp
	pop	dx
	pop	ax
	ret
.foundn	inc	di
	cmp	di, 49
	je	.foundc
	mov	dx, [bp - 10]
	mov	ax, [bp - 12]
	mov	[bp - 2], dx
	mov	[bp - 4], ax
.found	mov	ax, [es:3Ah]
	xor	dx, dx
	cmp	[fattype], byte 16
	jbe	.fat16
	mov	dx, [es:34h]
.fat16	mov	bx, [es:16h]
	mov	cx, [es:18h]
	mov	[bp - 8], bx	; Date+time
	mov	[bp - 6], cx
	test	dx, dx
	jnz	.nroot1
	cmp	ax, 1
	jbe	.foundc		; Got to root or bogus!
	test	ax, ax
	jnz	.nroot1
	test	dx, dx
	jz	.foundc		; Got to root!
.nroot1	cmp	dx, [rootclust + 2]
	jne	.nroot2
	cmp	ax, [rootclust]
	je	.foundc		; Got to root!
.nroot2	cmp	dx, [highestclust + 2]
	jb	.lowc
	ja	.foundc		; Bogus!
	cmp	ax, [highestclust]
	jae	.foundc		; Bogus!
.lowc	mov	[bp - 10], dx	; Parent cluster
	mov	[bp - 12], ax
	push	di
	call	sectorfromcluster
	call	diskread0
	pop	di
	jc	.foundc
	jne	.foundc
	mov	cx, [bp - 10]
	mov	bx, [bp - 12]
	call	check_dots
	jnz	.foundc		; Parent cluster not a directory
	mov	ax, [bp - 12]
	mov	dx, [bp - 10]
	call	getbitsclust
	cmp	ch, 2
	je	.foundn		; Ascend one more
.foundc	mov	dx, query_recdir1	; Prompt
	mov	ah, 9
	int	21h
	mov	ax, [bp - 4]
	mov	dx, [bp - 2]
	call	.pname
	mov	dx, query_recdir2
	mov	ah, 9
	int	21h
	mov	ax, [bp + 4]
	mov	dx, [bp + 2]
	call	.pname
	call	queryrecovercommon
	cmp	al, 'y'
	je	.rec
	mov	al, error_nofix
	jmp	exit		; Attempting to continue would make hash of things.
.rec	call	mklostfnd
	mov	[bp - 10], dx
	mov	[bp - 12], ax
	mov	dx, [bp - 2]
	mov	ax, [bp - 4]
	mov	bx, [bp - 8]
	mov	cx, [bp - 6]
	mov	[savedfilename], ax
	mov	[savedfilename + 2], dx
	mov	[savedfilename + 4], bx
	mov	[savedfilename + 6], cx
	mov	dx, [bp - 10]	; LOST.FND
	mov	ax, [bp - 12]
	mov	bx, matchemptyslot.ret
	mov	si, matchemptyslot
	mov	cx, .create
	call	mkdirentry
	mov	ax, [savedfilename]		; First fix parent cluster ref
	mov	dx, [savedfilename + 2]
	call	sectorfromcluster
	call	diskread0
	jc	newbadsectors
	mov	cx, [bp - 12]
	mov	[es:3Ah], cx
	cmp	[fattype], byte 16
	jbe	.fat16b
	mov	cx, [bp - 10]
	mov	[es:34h], cx
.fat16b	mov	ch, 0C0h
	call	diskwrite0
	mov	sp, bp	; Will now call into descendtree
	mov	ax, [savedfilename]
	mov	dx, [savedfilename + 2]
	push	ax
	push	dx
	mov	cl, 1
	call	setbitsclust	; It's reached now
	pop	dx
	pop	ax
	mov	si, [savedfilename + 8]
	mov	di, [savedfilename + 10]
	mov	bx, [savedfilename + 12]
	call	descendtree
	pop	bp	; Finally pop bp (can't earlier as descendtree does not save)
	pop	dx	; Check if we finished or not
	pop	ax
	push	ax
	push	dx
	call	getbitsclust
	pop	dx
	pop	ax
	cmp	ch, 2
	jne	strict short	.f_ret
	jmp	recoverdirs		; Still hanging: something up the chain must have not been a directory after all
.create	mov	[savedfilename + 8], ax		; Save location for descendtree
	mov	[savedfilename + 10], dx
	mov	[savedfilename + 12], di
	mov	ax, [savedfilename]
	mov	dx, [savedfilename + 2]
	mov	bx, [savedfilename + 4]
	mov	cx, [savedfilename + 6]
	mov	[es:di + 14h], dx
	mov	[es:di + 1Ah], ax
	mov	[es:di + 16h], bx
	mov	[es:di + 18h], cx
	xor	cx, cx
	mov	[es:di + 08h], word "CH"
	mov	[es:di + 0Ah], word 104Bh	;K, dir
	mov	[es:di + 0Ch], cx
	mov	[es:di + 0Eh], cx
	mov	[es:di + 10h], cx
	mov	[es:di + 12h], cx
	mov	[es:di + 1Ch], cx
	mov	[es:di + 1Eh], cx
	lea	si, [di + 8]
	jmp	recovernamedir
.pname	mov	di, savedfilename
	mov	si, savedfilename + 8
	cmp	[fattype], byte 17
	sbb	si, 0		; !FAT32: si -= 1
	push	es
	push	ds
	pop	es
	call	recovernamedir
	mov	al, '$'
	stosb
	pop	es
	mov	dx, savedfilename
	mov	ah, 9
	int	21h
.f_ret	ret

recoverfiles:
	push	ax
	push	dx
	mov	di, savedfilename
	mov	si, savedfilename + 8
	push	ds
	pop	es
	call	recovernamefile
	mov	al, '$'
	stosb
	mov	dx, query_recfile
	mov	ah, 9
	int	21h
	mov	dx, savedfilename
	mov	ah, 9
	int	21h
	call	queryrecovercommon
	cmp	al, 'y'
	pop	dx
	pop	ax
	jne	recoverdirs.f_ret	; Declined to recover this one; we can continue
	mov	[savedfilename], ax
	mov	[savedfilename + 2], dx
	xor	cx, cx
	mov	[savedfilename + 4], cx
	mov	[savedfilename + 6], cx
	mov	es, [buf1seg]
.szlp	call	getnextcluster
	mov	cx, [bytesperclust]
	add	[savedfilename + 4], cx
	adc	[savedfilename + 6], word 0
	call	isendchainagnostic
	jnc	.szlp
	call	mklostfnd
	mov	bx, matchemptyslot.ret
	mov	si, matchemptyslot
	mov	cx, .create
	jmp	mkdirentry
.create	mov	ax, [savedfilename]
	mov	dx, [savedfilename + 2]
	call	getdatetime
	mov	[es:di + 14h], dx
	mov	[es:di + 1Ah], ax
	mov	[es:di + 16h], bx
	mov	[es:di + 18h], cx
	xor	cx, cx
	mov	[es:di + 08h], word "CH"
	mov	[es:di + 0Ah], word 204Bh	;K, archive
	mov	[es:di + 0Ch], cx
	mov	[es:di + 0Eh], cx
	mov	[es:di + 10h], cx
	mov	[es:di + 12h], cx
	mov	cx, [savedfilename + 4]
	mov	[es:di + 1Ch], cx
	mov	cx, [savedfilename + 6]
	mov	[es:di + 1Eh], cx
	lea	si, [di + 8]
	jmp	recovernamefile

recovernamedir:
	cmp	[fattype], byte 16
	ja	recovernamecommon
	push	ax
	mov	ax, 'DI'
	stosw
	mov	al, 'R'
	stosb
	pop	ax
	jmp	recovernamecommon

recovernamefile:
	cmp	[fattype], byte 16
	ja	recovernamecommon
	push	ax
	mov	ax, 'FI'
	stosw
	mov	ax, 'LE'
	stosw
	pop	ax

recovernamecommon:
	cmp	[fattype], byte 16
	jbe	.fat16
	xchg	ax, dx
	call	.hex4
	xchg	ax, dx
.fat16	call	.hex4
	mov	al, ' '
	mov	cx, si
	sub	cx, di
	rep	stosb
	ret
.hex4	mov	bx, ax
	mov	cx, 0404h
.hex4l	rol	bx, cl
	mov	al, bl
	and	al, 15
	add	al, '0'
	cmp	al, '9'
	jbe	.hexn
	add	al, 'A' - '0' - 10
.hexn	stosb
	dec	ch
	jnz	.hex4l
	ret

queryrecovercommon:
	mov	cx, state_dir
	mov	dx, query_recq
	jmp	queryfixynpostcr

matchemptyslot:
	cmp	[es:di], byte 0E5h
.ret	ret

	;mklostfnd: gets or creates LOST.FND
	;Returns cluster in DX:AX
mklostfnd:
	mov	ax, [mklostfndclust]
	mov	dx, [mklostfndclust + 2]
	mov	cx, ax
	or	cx, dx
	jnz	.ret
	call	getdatetime
	mov	[savedfilename + 12], cx	; Time
	mov	[savedfilename + 14], bx	; Date
	mov	ax, [rootclust]
	mov	dx, [rootclust + 2]
	mov	si, .mklostfnd_match
	mov	bx, .mklostfnd_popclust
	mov	cx, .mklostfnd_popentry
	jmp	mkdirentry
.mklostfnd_match:
	mov	cl, [es:di + 11]
	and	cl, 10h
	cmp	cl, 10h
	jne	.ret
	push	di
	mov	cx, 11
	mov	si, lostfnd
	repe	cmpsb
	pop	di
.ret	ret
.mklostfnd_popclust:
	mov	si, .initcluster
	jmp	initdircluster
.initcluster:
	mov	[mklostfndclust], ax
	mov	cx, ax
	mov	[mklostfndclust + 2], dx
	mov	bx, dx
	mov	ax, '. '
	call	generatedotdirectoryentry
	mov	ax, '..'
	mov	cx, [rootclust]		; TODO does FAT32 want 0 here?
	mov	bx, [rootclust + 2]
	call	generatedotdirectoryentry
	ret
.mklostfnd_popentry:
	mov	si, lostfnd	; Create entry
	mov	cx, 11
	rep	movsb
	mov	al, 10h
	stosb		; B
	xor	ax, ax
	stosw		; C
	stosw		; E
	stosw		; 10
	stosw		; 12
	mov	dx, [mklostfndclust + 2]
	mov	ax, dx
	stosw		; 14
	mov	ax, [savedfilename + 12]	; Time
	stosw		; 16
	mov	ax, [savedfilename + 14]	; Date
	stosw		; 18
	mov	ax, [mklostfndclust]
	stosw		; 1A
	xor	ax, ax
	stosw		; 1C
	stosw		; 1E
	mov	ax, [mklostfndclust]
	ret

	; Makes or matches a directory entry
	; DX:AX = directory cluster (0 = root)
	; BX = cluster create logic (no arguments)
	; CX = entry create logic (argument: ES:DI = pointer to write to)
	; SI = match logic	(argument: ES:DI = match to evaluate, must preserve ES,DI,BP)
	; Returns: cluster if SI matches, otherwise return value of CX
mkdirentry:
	push	bp
	mov	bp, sp
	sub	sp, 26
	mov	[bp - 26], cx	; entry create logic
	xor	cx, cx
	mov	[bp - 2], cx	; empty slot sector low
	mov	[bp - 4], cx	; empty slot sector high
	mov	[bp - 6], cx	; empty slot byte offset
	mov	[bp - 8], cx	; empty slot counter
	mov	[bp - 10], cx	; counter
	mov	[bp - 12], ax	; Current cluster
	mov	[bp - 14], dx
	mov	[bp - 22], si	; match logic
	mov	[bp - 24], bx	; cluster create logic
	mov	es, [buf1seg]
	mov	cx, ax
	or	cx, dx
	jnz	.mkentry_clust
	mov	ax, [reservedsects]
	mov	cl, [numfats]
.rpl	add	ax, [sectsperfat]
	adc	dx, [sectsperfat + 2]
	loop	.rpl
	mov	cx, [rootdirentries]
	mov	[bp - 16], cx	; loop control
	call	.mkentry_scan
	jnc	.gentry		; got it!
	mov	cx, [bp - 10]
	cmp	cx, [bp - 16]
	je	.rootdirfull
.centry	call	.mkentry
.gentry	mov	sp, bp
	pop	bp
.ret	ret

.rootdirfull:
	mov	dx, msg_outofslots
	mov	ah, 9
	int	21h
	mov	dx, msg_goodenuf
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit

.mkentry_clust:
	mov	ax, [bytespersector]
	mul	word [sectsperclust]
	mov	cl, 5
	shr	ax, cl
	mov	[bp - 16], ax	; loop control
	mov	ax, [bp - 12]
	mov	dx, [bp - 14]
	mov	[bp - 18], ax	; current cluster
	mov	[bp - 20], dx
.cloop	call	sectorfromcluster
	call	.mkentry_scan
	jnc	.gentry
	mov	cx, [bp - 10]
	cmp	cx, [bp - 16]
	jb	.centry
	push	es
	mov	es, [buf2seg]
	mov	ax, [bp - 18]
	mov	dx, [bp - 20]
	call	getnextcluster
	call	isendchainagnostic
	jc	.newcluster
	mov	[bp - 18], ax
	mov	[bp - 20], dx
	pop	es
	mov	[bp - 10], word 0
	jmp	.cloop
.newcluster:
	push	es
	mov	es, [buf2seg]
	mov	si, .ret
	call	initdircluster
	push	dx
	push	ax
	push	dx
	push	ax
	mov	ax, [bp - 18]
	mov	dx, [bp - 20]
	call	setclusterinfats
	pop	ax
	pop	dx
	pop	es
	call	sectorfromcluster
	mov	[bp - 2], ax
	mov	[bp - 4], dx
	mov	[bp - 6], word 0
	mov	[bp - 8], word 0
	xor	ax, ax
	xor	dx, dx
	jmp	.centry

.mkentry:
	call	[bp - 24]
	mov	di, [bp - 6]
	mov	cx, [bp - 8]
	cmp	cx, [bp - 16]
	jne	.ismidslot
	mov	cx, [wordsperchunk]
	shl	cx, 1
	sub	cx, 16
	cmp	di, cx
	jb	.isthisslot
	mov	ax, [bp - 2]	; Read in the *next* slot, see if it's good.
	mov	dx, [bp - 4]
	add	ax, [sectsperchunk]
	adc	dx, 0
	push	di
	call	diskread0
	jc	.b0n		; If the bad sector is persistent we haven't damaged anything yet
	cmp	[es:0], byte 0
	je	.b0n		; No need to bother
	mov	[es:0], byte 0
	call	diskwrite0d
.b0n	pop	di
	jmp	.ismidslot2
.isthisslot:
	call	.mkentryreadthis
	mov	[es:di + 16], byte 0
	jmp	.ismidslot2
.ismidslot:
	call	.mkentryreadthis
.ismidslot2:
	push	ax
	push	dx
	call	[bp - 26]
	pop	bx
	pop	cx
	push	ax
	push	dx
	xchg	ax, cx
	mov	dx, bx
	call	diskwrite0d
	pop	dx
	pop	ax
	ret
.mkentryreadthis:
	mov	ax, [bp - 2]	; Read in storage slot (buffer read check will dedupe read)
	mov	dx, [bp - 4]
	push	di
	call	diskread0
	pop	di
	jc	.nbad
	ret

.nbad	jmp	newbadsectors
.mkentry_scan:
	call	diskread0
	jc	.nbad
	xor	di, di
.loops	cmp	[es:di], byte 0
	je	.ends1
	call	[bp - 22]
	je	.founds
	cmp	[es:di], byte 0E5h
	jne	.shl
	call	.rslot
.shl	mov	bx, [bp - 10]
	inc	bx
	mov	[bp - 10], bx
	cmp	bx, [bp - 16]
	je	.ends2
	add	di, 32
	mov	cx, [wordsperchunk]
	shl	cx, 1
	cmp	di, cx
	jb	.loops
	add	ax, [sectsperchunk]
	adc	dx, 0
	jmp	.mkentry_scan
.founds	cmp	[es:di], byte 0E5h
	je	.ends1			; Search rule was "empty slot"
	xor	dx, dx
	mov	ax, [es:di + 1Ah]
	cmp	[fattype], byte 16
	jbe	.found6
	mov	dx, [es:di + 14h]
.found6	clc
	ret
.ends1	call	.rslot
.ends2	stc
	ret
.rslot	xor	cx, cx
	cmp	[bp - 2], cx
	jne	.rslotr
	cmp	[bp - 4], cx
	jne	.rslotr
	mov	[bp - 2], ax
	mov	[bp - 4], dx
	mov	[bp - 6], di
	mov	bx, [bp - 10]
	mov	[bp - 8], bx
.rslotr	ret

;	Allocates and initializes a directory cluster
;	es = buffer, si = 0 chunk initializer argument
;	returns DX:AX = cluster
;	Ordered writes: zeros from back to front to avoid causing damage if interrupted
;	We need this due to an LKNL bug.
initdircluster:
	push	si
	mov	dx, 0FFFh
	mov	ax, [endchainlow]
	call	alloccluster
	add	[totalfiles], word 1
	adc	[totalfiles + 2], word 0
	mov	di, totalfiles
	call	incrementprogress
	push	ax
	push	dx
	mov	cx, [wordsperchunk]
	xor	ax, ax
	xor	di, di
	rep	stosw
	xor	dx, dx
	mov	ax, [sectsperclust]
	div	word [sectsperchunk]
	xchg	ax, cx		; cx = chunks per cluster
	pop	dx
	pop	ax
	push	ax
	push	dx
	push	cx
	call	sectorfromcluster
	pop	cx
	dec	cx
	jz	.last
	push	ax
.next	add	ax, [sectsperchunk]	; Trick: cannot actaully overflow
	push	ax
	push	dx
	push	si
	push	cx
	mov	ch, 0Ch
	call	diskwrite0
	pop	cx
	pop	si
	pop	dx
	pop	ax
	dec	cx
	jnz	.next
	pop	ax
.last	xor	di, di
	mov	bx, ax
	mov	cx, dx
	pop	dx		; Ugly. Ugly.
	pop	ax		; Need to pass cluster in DX:AX to SI
	pop	si		; Preserved at start
	push	ax		; but need to save sector
	push	dx
	push	bx
	push	cx
	call	si
	pop	dx
	pop	ax
	mov	ch, 0C0h
	call	diskwrite0
	pop	dx
	pop	ax
	ret

totalfilesinc:
	add	[totalfiles], word 1
	adc	[totalfiles + 2], word 0
	ret

	; Returns: Date in CX, time in BX, preserves all other registers
getdatetime:
	push	ax
	push	dx
	mov	ah, 2Ch
	int	21h
	mov	al, ch
	mov	ch, cl
	mov	cl, 5
	shl	ax, cl
	or	al, ch
	mov	cl, 6
	shl	ax, cl
	shr	dh, 1
	or	al, dh
	push	ax	; TIME
	mov	ah, 2Ah
	int	21h
	xchg	ax, cx
	mov	cl, 4
	shl	ax, cl
	or	al, dh
	mov	ax, 5
	shl	ax, cl
	or	al, dl
	xchg	ax, cx
	pop	bx
	pop	dx
	pop	ax
	ret

incrementprogress:	; DI=pointer to total, destroys nothing
	push	ax
	push	dx
	mov	ax, [progress]
	mov	dx, [progress + 2]
	add	ax, 100
	adc	dx, 0
	cmp	dx, [di + 2]
	ja	.u
	jb	.n
	cmp	ax, [di]
	jae	.u
.n	mov	[progress], ax
	mov	[progress + 2], dx
.r	pop	dx
	pop	ax
	ret
.u	cmp	[pgdisplay], byte 99	; Prevent infinite loop on division by 0 (empty fs)
	je	.n
	inc	byte [pgdisplay]
	sub	ax, [di]
	sbb	dx, [di + 2]
	cmp	dx, [di + 2]
	ja	.u
	jb	.d
	cmp	ax, [di]
	jae	.u
.d	mov	al, [pgdisplay]
	aam
	xchg	al, ah
	xchg	ax, dx
	push	bp
	cmp	dl, 0
	jne	.p
	mov	dl, ' ' - '0'
.p	add	dl, '0'
	mov	ah, 2
	int	21h
	mov	dl, dh
	add	dl, '0'
	mov	ah, 2
	int	21h
	mov	dx, out_percentpost
	mov	ah, 9
	int	21h
	pop	bp
	jmp	.r

	; Entry point; DX:AX = base cluster no, DI = offset
	; Result in CH; destroys everything but DI and ES
getbitsclustdi:
	add	ax, di
	adc	dx, 0
	mov	ch, 1
	jmp	getbitsclust.e2
	;DX:AX = base cluster
getbitsclust:
	mov	ch, 0
.e2	push	es
	call	bitsclustcommon
	mov	ch, [es:bx]
	shr	ch, cl
	pop	es
	and	ch, 3
	ret

	; bits to set in bottom two bits of CL
setbitsclustdi:
	add	ax, di
	adc	dx, 0
	mov	ch, 3
	jmp	setbitsclust.e2
setbitsclust:
	mov	ch, 2
.e2	push	es
	push	cx
	call	bitsclustcommon
	pop	ax
	shl	al, cl
	or	[es:bx], al
	pop	es
	ret

	; bits to clear are set in bottom two bits of CL
clearbitsclustdi:
	add	ax, di
	adc	dx, 0
	mov	ch, 3
	jmp	clearbitsclust.e2
clearbitsclust:
	mov	ch, 2
.e2	push	es
	push	cx
	call	bitsclustcommon
	pop	ax
	and	al, 3
	shl	al, cl
	not	al
	and	[es:bx], al
	pop	es
	ret

bitsclustcommon:
	mov	si, bitbufsi
	call	isbitsclustcommon
	jc	.cache
	mov	si, pinsi
	call	isbitsclustcommon
	jc	.cache
	push	dx
	push	ax
	mov	si, bitvectorptrs
.loop	cmp	[si + bitvectorlength + 2], dx
	ja	.this
	jb	.next
	cmp	[si + bitvectorlength], ax
	ja	.this
.next	sub	ax, [si + bitvectorlength]
	sbb	dx, [si + bitvectorlength + 2]
	add	si, bitvectorrlen
	jmp	.loop
.this	push	si
	push	cx
	mov	si, bitbufsi
	test	ch, byte 1
	jz	.fbuf
	mov	si, pinsi
.fbuf	call	swapbitsclustcommon
	pop	cx
	test	ch, byte 2
	jz	.fcln
	or	[si], byte 1
.fcln	pop	ax		; Not used
	pop	ax
	pop	dx
.cache	test	ch, byte 2
	jz	.cnr
	or	[si], byte 1
.cnr	sub	ax, [si + 6]
	sbb	dx, [si + 8]
	mov	cl, al
	and	cl, 3
	shl	cl, 1
	les	bx, [si + 2]			; It's addressible memory
	shr	dx, 1
	rcr	ax, 1
	shr	dx, 1
	rcr	ax, 1
	add	bx, ax
	adc	dx, 0
	push	cx
	mov	ax, es
	mov	cl, 12
	shl	dx, cl
	add	ax, dx
	pop	cx
	mov	es, ax
	ret

	; DX:AX = cluster, SI = pointer to cache control record
isbitsclustcommon:
	cmp	dx, [si + 8]
	jb	.no
	ja	.ab
	cmp	ax, [si + 6]
	jb	.no
.ab	cmp	dx, [si + 12]
	ja	.no
	jb	.yes
	cmp	ax, [si + 10]
	jae	.no
.yes	stc
	ret
.no	clc
	ret

	; DX:AX = offset into selected, SI = pointer to cache control record, on stack = skipped, new record pointer, requested cluster
	; Stack arguments remain on stack, trashes BX, CX
swapbitsclustcommon:
	push	bp
	mov	bp, sp
	push	dx
	push	ax
	mov	bx, [si]
	test	bx, 1
	jz	.clean
	dec	bx
	cmp	[bx + bitvectortype], byte b_mem_xms
	jb	.clean
	les	ax, [si + 2]	; XMS write up
	mov	[xms_xfer_src], word ax
	mov	[xms_xfer_srcoff], word ax
	mov	[xms_xfer_srcoff + 2], es
	mov	ax, [bx + bitvectorptr]
	mov	[xms_xfer_dst], ax
	mov	ax, [si + 14]
	mov	dx, [si + 16]
	mov	[xms_xfer_dstoff], ax
	mov	[xms_xfer_dstoff + 2], dx
	call	xmscopy
.clean	mov	bx, [bp + 6]
	cmp	[bx + bitvectortype], byte b_mem_xms
	je	.xms
	ja	.fault
	call	.load
	call	.base
	mov	[si + 6], ax		; requested - offset = starting
	mov	[si + 8], dx
	add	ax, [bx + bitvectorlength]
	adc	dx, [bx + bitvectorlength + 2]
	mov	[si + 10], ax
	mov	[si + 12], dx
	jmp	.ret
.fault	mov	si, bx
	jmp	scanbitsclust.fault
.xms:	mov	ax, [bp - 4]		; Compute offset alignment, so that we can use equality comparison
	mov	dx, [bp - 2]		; rather than overlap comparison for XMS buffers
	mov	cx, [bitbuflen]
	dec	cx
	or	ax, cx
	xor	ax, cx
	inc	cx
	mov	[si + 6], ax		; Normalized offset & offset + length
	mov	[si + 8], dx
	add	ax, cx		; Cannot overflow
	mov	[si + 10], ax
	mov	[si + 12], dx
	sub	ax, cx
	call	.load
	mov	es, [si - 2]	; Buffer sector
	shr	dx, 1		; Compute offset into XMS buffer
	ror	ax, 1
	shr	dx, 1
	ror	ax, 1
	mov	[si + 14], ax
	mov	[si + 16], dx
	mov	[xms_xfer_srcoff], ax
	mov	[xms_xfer_srcoff + 2], dx
	xor	ax, ax
	mov	[xms_xfer_dst], ax	; XMS read down
	mov	[xms_xfer_dstoff], ax
	mov	[xms_xfer_dstoff + 2], es
	mov	[si + 2], ax
	mov	[si + 4], es
	mov	ax, [bx + bitvectorptr]
	mov	[xms_xfer_src], ax
	call	xmscopy
	call	.base
	add	[si + 6], ax		; But we need the whole for comparisons
	adc	[si + 8], dx
	add	[si + 10], ax
	adc	[si + 12], dx
.ret	pop	ax			; We saved them for ourselves
	pop	dx
	pop	bp
	ret
.base	mov	ax, [bp + 8]		; base = requested - offset
	mov	dx, [bp + 10]
	sub	ax, [bp - 4]
	sub	dx, [bp - 2]
	ret
.load	mov	[si], bx
	mov	cx, [bx + bitvectorptr]
	mov	[si + 2], cx
	mov	cx, [bx + bitvectorptr + 2]
	mov	[si + 4], cx
	ret

	; Locate all entries matching a pattern
	; Arguments: CH = pattern, stack = callback (called with bp + 4 = address of callback)
	; Destroys all registers other than CS,DS,SS,BP
	; Are we having fun yet?
	; stage_recover -> scanbitsclust -> recoverdirs -> mklostfnd -> mkdirentry -> alloccluster -> scanbitsclust
	; stage_recover -> scanbitsclust -> recoverdirs -> mkdirentry -> alloccluster -> scanbitsclust
	; stage_recover -> scanbitsclust -> recoverdirs -> descendtree -> recoverbadsector -> alloccluster -> scanbitsclust
	; stage_recover -> scanbitsclust -> recoverfiles -> mklostfnd -> mkdirentry -> alloccluster -> scanbitsclust
	; stage_recover -> scanbitsclust -> recoverfiles -> mkdirentry -> alloccluster -> scanbitsclust
scanbitsclust:
	push	bp
	mov	bp, sp
	sub	sp, 12
	mov	si, bitvectorptrs
	xor	dx, dx
	mov	ax, 2
	mov	cl, 4
	shl	ch, cl		; We start at 2
	mov	[bp - 6], byte 0
	test	[opflags + 1], byte opflag2_sbcpin
	jnz	.outer
	mov	[bp - 6], byte opflag2_sbcpin
	or	[opflags + 1], byte opflag2_sbcpin

	; Outer loop -- get bitvector to enumerate
.outer	cmp	[si + bitvectortype], byte 0
	je	.retouter
	cmp	[si + bitvectortype], byte b_mem_xms
	ja	.fault
	je	.xms
	les	bx, [si + bitvectorptr]
	mov	[bp - 4], bx	; small-size offset
	mov	bx, [si + bitvectorlength + 2]
	mov	di, [si + bitvectorlength]
	call	.div4ceil	; 4 entries per byte
	; Tricky part: do big chunks first
	test	bx, bx
	jz	.mide
	push	di
.mid	xor	di, di
	call	.inner
	push	ax
	mov	ax, es
	add	ax, 1000h
	mov	es, ax
	pop	ax
	dec	bx
	jnz	.mid
	jmp	.mid
	pop	di
.mide	add	di, [bp - 4]
	call	.inner
.cont	add	si, bitvectorrlen
	cmp	si, bitvectorptrs + bitvectorrlen * 128
	jb	.outer
.retouter:
	call	.unwind
	mov	sp, bp
	pop	bp
	ret	2
.fault	mov	ax, [si + bitvectortype]
	call	outax
	jmp	exit
.xms	mov	[bp - 4], word 0	; Always use seg pointers for XMS
	mov	[bp - 8], ax		; Initial offset, used later
	mov	[bp - 6], dx
	mov	di, ax
	mov	bx, dx
	add	di, [si + bitvectorlength]
	adc	bx, [si + bitvectorlength + 2]
	mov	[bp - 12], di		; Final offset
	mov	[bp - 10], bx
.xmsnxt	push	si
	mov	si, bitbufsi
	call	isbitsclustcommon
	jc	.cache
	mov	si, pinsi
	call	isbitsclustcommon
	jc	.cache
.scan	pop	si
	push	si		; preserve
	push	dx		; cluster
	push	ax
	push	si		; record selected
	push	cx		; preserved
	sub	ax, [bp - 8]	; offset
	sbb	dx, [bp - 6]
	mov	si, bitbufsi
	test	[bp - 6], byte opflag2_sbcpin
	jz	.scan2
	mov	si, pinsi
.scan2	call	swapbitsclustcommon
	pop	cx
	pop	dx		; we need cache (still in si) not record selected
	pop	ax
	pop	dx
.cache	mov	di, [si + 10]
	mov	bx, [si + 12]
	sub	di, [si + 6]
	sbb	bx, [si + 8]
	call	.div4ceil	; 4 entries per byte
	mov	es, [si + 4]
	pop	si		; get record selected back
	call	.inner
	cmp	dx, [bp - 10]
	jb	.xmsnxt
	ja	.xmslst
	cmp	ax, [bp - 12]
	jb	.xmsnxt
.xmslst	jmp	.cont
.unwind	test	[bp - 6], byte opflag2_sbcpin	; alloccluster depends on this not trashing DX:AX
	jz	.uret
	and	[opflags + 1], byte ~opflag2_sbcpin
.uret	ret

.div4ceil:			; The last entry might not be a multiple of 4
	add	di, 3
	adc	bx, 0
	shr	bx, 1
	rcr	di, 1
	shr	bx, 1
	rcr	di, 1
	ret

.inner	push	bx		; .inner destroys CL only
	mov	bx, [bp - 4]	; offset, saved above
	test	al, 2		; Loop is unrolled -- must be first entry
	jnz	.inner2

.innerl	mov	cl, [es:bx]
	and	cl, 3
	cmp	ch, cl
	jne	.nm0
	call	.match		; Extra layer of indrection on match
.nm0	rol	ch, 1
	rol	ch, 1
	inc	ax		; Alignment: can only overflow at the last part of the loop
	mov	cl, [es:bx]
	and	cl, 0Ch
	cmp	ch, cl
	jne	.nm1
	call	.match
.nm1	rol	ch, 1
	rol	ch, 1
	inc	ax
.inner2	mov	cl, [es:bx]
	and	cl, 30h
	cmp	ch, cl
	jne	.nm2
	call	.match
.nm2	rol	ch, 1
	rol	ch, 1
	inc	ax
	mov	cl, [es:bx]
	and	cl, 0C0h
	cmp	ch, cl
	jne	.nm3
	call	.match
.nm3	rol	ch, 1
	rol	ch, 1
	add	ax, 1
	adc	dx, 0
	inc	bx
	cmp	bx, di
	jne	.innerl
	pop	bx
	ret

.match	cmp	dx, [highestclust + 2]	; Skip garbage entries at end of list (up to 3 possible)
	jb	.matchx
	cmp	ax, [highestclust]
	je	.matchr
.matchx	push	ax			; Yes we save every single register. We're using them.
	push	bx			; Caller will make absolute hash of them repairing
	push	cx			; stuff anyway. Might as well write register save once.
	push	dx
	push	si
	push	di
	push	es
	call	[bp + 4]
	pop	es
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
.matchr	ret

;Routines used by FAT repair
;bp - 16: number of words in a FAT block
;Destroys no registers, output in Z flag
fat_compare:
	push	si
	push	di
	push	cx
	xor	si, si
	mov	cx, [bp - 16]
	xor	di, di
	repe	cmpsw
	pop	cx
	pop	di
	pop	si
	ret

fat_copy:
	push	si
	push	di
	push	cx
	xor	si, si
	mov	cx, [bp - 16]	; words per block
	xor	di, di
	rep	movsw
	pop	cx
	pop	di
	pop	si
	ret

	;[bp - 18] = entries per block
fat_quantify:
	push	es
	mov	es, [buf4seg]
	pop	ds
	call	fat_copy
	push	cs
	pop	ds
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	mov	ax, [bp - 18]
	push	bp
	mov	bp, sp
	sub	sp, 10
	mov	[bp - 2], ax		; Entries per block

	;Shell sort
	mov	bx, shelltabs_end - 2
.loop1	mov	[bp - 4], bx		; Advancer
	mov	si, [bp - 4]		; Current
	mov	[bp - 6], si		; Current gap
	cmp	si, [bp - 2]
	jae	.l2end			; Gap too big, nothing to do
.loop2	mov	di, si			; Ahead
	call	[entryfromblock]
	mov	[bp - 8], ax
	mov	[bp - 10], dx
.loop3	sub	di, [bp - 6]
	jb	.l3end
	call	[entryfromblock]
	cmp	dx, [bp - 10]
	jb	.l3end
	ja	.a
	cmp	ax, [bp - 8]
	jbe	.l3end
.a	add	di, [bp - 6]
	call	[entrytoblock]		; Shift entry up one
	sub	di, [bp - 6]
	jmp	.loop3
.l3end	add	di, [bp - 6]
	mov	ax, [bp - 8]
	mov	dx, [bp - 10]
	call	[entrytoblock]		; Write removed entry where it goes
	inc	si
	cmp	si, [bp - 2]
	jb	.loop2
.l2end	mov	bx, [bp - 4]
	dec	bx
	dec	bx
	cmp	bx, shelltabs
	jae	.loop1

	xor	di, di	; Advancer
	xor	si, si	; Counter
	xor	ax, ax
	xor	dx, dx
.aloop	push	dx			; Ended up being tight because I have to rotate registers anyway
	push	ax
	call	[entryfromblock]
	pop	bx
	pop	cx
	test	dx, dx
	jnz	.nz
	cmp	ax, 1
	jbe	.laa
.nz	cmp	dx, 0FFFFh
	jne	.nf
	cmp	ax, 0FFFFh
	je	.laa
.nf	cmp	ax, bx
	jne	.ns
	cmp	dx, cx
	je	.laa
.ns	cmp	dx, [highestclust + 2]
	ja	.laa
	jb	.nh
	cmp	ax, [highestclust]
	ja	.laa
.nh	inc	si
.laa	inc	di
	cmp	di, [bp - 2]
	jb	.aloop
	mov	ax, si
	or	ax, ax
	jnz	.hdat
	cmp	[es:0], word 0		; Causes all tiny to win over all empty (recovered bad block in FAT)
	je	.ndat
.hdat	inc	ax
.ndat:

	mov	sp, bp
	pop	bp

	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	ret

entrytoblockall:	; Writes value to all FAT buffers
	push	es
	xor	cx, cx
.loop	push	cx
	mov	bx, cx
	add	bx, cx
	add	bx, buf1seg
	mov	es, [bx]
	call	[entrytoblock]
	pop	cx
	inc	cx
	cmp	cl, [numfats]
	jb	.loop
	pop	es
	mov	[bp - 1], byte 7	; All same, ergo all changed
	ret

validatehddescriptor:
	cmp	dl, byte 0F8h	; See caller
	je	.ret
	cmp	dl, byte 0FFh
	jne	.error
.ret	ret
.error	mov	ah, 9
	mov	dx, msg_hdbaddesc
	int	21h
	mov	al, error_nofix
	jmp	exit

; Checks for end chain, handles somebody saying "no" to fix out of range entry in FAT
isendchainagnostic:
	test	dx, dx
	jnz	.high
	cmp	ax, 2
	jb	.yes
.high	cmp	dx, [highestclust + 2]
	jb	.no
	cmp	ax, [highestclust]
	jae	.yes
.no	clc
	ret
.yes	stc
	ret

totalfileinc:
	add	[totalfiles], word 1
	adc	[totalfiles + 2], word 0
	ret

; FAT12 has entries that span sectors but we don't play that game; we loaded the whole FAT in that case
entryfromblock12:
	xor	dx, dx
	call	calcoffset12
	mov	ax, [es:bx]
	jc	.odd
	and	ax, 0FFFh
	ret
.odd	mov	cl, 4
	shr	ax, cl
	ret

entrytoblock12:
	and	ax, 0FFFh		; Stupid trick: set media descriptor sends too many bits
	call	calcoffset12
	jc	.odd
	and	[es:bx], word 0FFFh
	or	[es:bx], ax
	ret
.odd	mov	cl, 4
	shl	ax, cl
	and	[es:bx], byte 0Fh
	or	[es:bx], ax
	shr	ax, cl			; effective bits of AX are preserved
	ret

calcoffset12:
	mov	bx, di
	add	bx, di
	add	bx, di
	shr	bx, 1
	ret

entriestowords12:
	shl	ax, 1
	mov	di, 3
	div	di
	or	dx, dx
	jz	.ret
	inc	ax
.ret	ret

entriesperblock12:
	mov	ax, [highestclust]
	ret

getblockaddr12:
	xor	dx, dx
	xor	ax, ax
	ret

isendchain12:
	cmp	ah, 0Fh
	jne	.ret
	cmp	al, [descriptor]
	je	.ret
	cmp	ax, 0FF7h
.ret	ret

isbadblock12:
	cmp	ax, 0FF7h
	ret

ismdesc12:
	cmp	ah, 00Fh
	ret

entryfromblock16:
	xor	dx, dx
	mov	bx, di
	add	bx, di
	mov	ax, [es:bx]
	ret

entrytoblock16:
	mov	bx, di
	add	bx, di
	mov	[es:bx], ax
entriesperblock16:
entriestowords16:
	ret

getblockaddr16:		; the difference between this function between F16 and F32 is 16/16 vs 32/16
	div	cx
	mul	word [sectsperchunk]
	ret

isendchain16:
	; A FAT16 media descriptor must be F8h or FFh
	cmp	ax, 0FFF7h
	ret

isbadblock16:
	cmp	ax, 0FFF7h
	ret

ismdesc16:
	cmp	ah, 0FFh
	ret

entryfromblock32:
	mov	bx, di
	shl	bx, 1
	shl	bx, 1
	mov	ax, [es:bx]
	mov	dx, [es:bx + 2]
	and	dh, 0Fh		; It's called FAT32 but it's really FAT28
	ret

entrytoblock32:
	mov	bx, di
	shl	bx, 1
	shl	bx, 1
	mov	[es:bx], ax
	mov	[es:bx + 2], dx
	ret

entriestowords32:
	shl	ax, 1
	ret

entriesperblock32:
	shr	ax, 1
	ret

getblockaddr32:
	xchg	ax, bx
	xchg	ax, dx
	xor	dx, dx
	div	cx
	xchg	ax, bx
	div	cx
	mul	word [sectsperchunk]
	push	ax
	xchg	ax, bx
	mov	bx, dx
	mul	word [sectsperchunk]
	xchg	ax, dx
	add	dx, bx
	pop	ax
	ret

isendchain32:
	cmp	dx, 0FFFh
	jnb	isendchain16
	ret

isbadblock32:
	cmp	dx, 0FFFh
	je	isbadblock16
	ret

ismdesc32:
	cmp	dx, 0FFFFh
	jne	.ret
	cmp	ah, 0FFh
.ret	ret

	; Directory variant of diskwrite0; sets CH itself but can't write to FAT
diskwrite0d:
	mov	ch, 0C0h
	cmp	dx, [firstclustsect + 2]
	ja	diskwrite0
	jb	.root
	cmp	ax, [firstclustsect]
	jae	diskwrite0
.root	mov	ch, 80h

	; Writes buffer to sector; trashes all registers but ES, BP
diskwrite0:
	call	setbufsector
	xor	bx, bx
diskwrite:
	mov	cl, 1
	call	diskreadwrite
	jc	.bad
	ret
.bad	mov	dx, msg_replace
	mov	ah, 9
	int	21h
	mov	al, error_nofix
	jmp	exit

	; Reads sector to buffer if not already in buffer; trashes BX, CX, SI, DI
	; CF is set on error
diskread0:
	xor	bx, bx
	push	ax
	push	dx
	call	diskread
	pop	dx
	pop	ax
	jc	.cbad
	call	setbufsector
	clc
	ret
.cbad	push	ax
	push	dx
	call	invalidatesector
	pop	dx
	pop	ax
	stc
	ret

invalidatesector:
	mov	ax, 0FFFFh
	mov	dx, ax
setbufsector:
	mov	bx, es
	cmp	bx, [buf1seg]
	jne	.n1
	mov	[buf1sector], ax
	mov	[buf1sector + 2], dx
	jmp	.n4
.n1	cmp	bx, [buf2seg]
	jne	.n2
	mov	[buf2sector], ax
	mov	[buf2sector + 2], dx
	jmp	.n4
.n2	cmp	bx, [buf3seg]
	jne	.n3
	mov	[buf3sector], ax
	mov	[buf3sector + 2], dx
	jmp	.n4
.n3	cmp	bx, [buf4seg]
	jne	.n4
	mov	[buf4sector], ax
	mov	[buf4sector + 2], dx
.n4	ret

diskread:
	xor	cx, cx

;DX:AX = sector address
;ES:BX = read address
;CX = read/write flag: 0 = read, 1 = write; when writing bits 14-13 are
;	00 - unknown (must be boot sector or reserved sectors)
;	01 - FAT data
;	10 - root directory data
;	11 - file data
;Always reads one chunk size, which may or may not be a whole cluster
;The zeroth sector can't be cached for numerous reasons; it's always accessed directly.
;This does clobber all registers other than segment registers and bp despite appearing not to
diskreadwrite:
	push	bp
	mov	bp, sp
	sub	sp, 16
	mov	[bp - 16], ax
	mov	[bp - 14], dx
	mov	[bp - 10], bx	; Buffer
	mov	[bp - 8], es
	mov	bx, [sectsperchunk]
	mov	[bp - 12], bx
	mov	[bp - 6], ds
	mov	[bp - 4], cx
	test	[opflags + 1], byte opflag2_7305
	jnz	.fat32
	test	[opflags + 1], byte opflag2_bigdisk
	jnz	.big
	add	bx, ax		; Switch to big if a big offset is requested
	jc	.bigf
	or	dx, dx
	jnz	.bigf
	lds	bx, [bp - 10]
	mov	cx, [bp - 12]
	mov	dx, [bp - 16]
	call	.i2526
	jnc	.exit
	cmp	ax, 0207h
	stc
	jnz	.exit
	mov	ds, [bp - 6]
.bigf	or	[opflags + 1], byte opflag2_bigdisk
.big	mov	cx, 0FFFFh
	lea	bx, [bp - 16]
	call	.i2526
	jnc	.exit
	cmp	ax, 0207h
	stc
	jnz	.exit
	or	[opflags + 1], byte opflag2_7305
.fat32	mov	si, [bp - 4]
	lea	bx, [bp - 16]
	mov	dl, [cs:disk]
	inc	dl
	mov	ax, 7305h
	int	21h
.exit	mov	ds, [bp - 6]
	mov	es, [bp - 8]
	mov	sp, bp
	pop	bp
	ret
.i2526	mov	al, [cs:disk]
	mov	[cs:preserve_bp], bp
	sub	sp, 2
	mov	[cs:preserve_sp], sp
	add	sp, 2
	test	[bp - 4], byte 1
	jnz	.write
	int	25h
	mov	sp, [cs:preserve_sp]	; DR-DOS bugfix, must immediately follow int instruction
	jmp	.cont
.write	int	26h
	mov	sp, [cs:preserve_sp]
.cont	mov	bp, [cs:preserve_bp]
	jc	.keepc
	popf
	clc
	ret
.keepc	popf
	stc
	ret

xmscopy	push	si
	mov	si, xms_xfer_len
	mov	ah, 0Bh
	call	far [xmsfunc]
	cmp	ax, 1
	jne	.err
	pop	si
	ret
.err	xchg	ax, bx
	call	outax
	mov	dx, msg_xmserr
	mov	ah, 9
	int	21h
	jmp	exit

	align 2, db 0CCh

shelltabs	dw	1, 3, 6, 13, 28, 61, 134, 294, 646, 1421, 3126, 6877
shelltabs_end:

descendtreetable:
	dw	descendtree.scannormal
	dw	descendtree.scansetnotdir
	dw	descendtree.scansete5
	dw	descendtree.scanfixparent
	dw	descendtree.scannotzero
	dw	descendtree.scanvalidatedotentries
	dw	descendtree.scanlfnsubsequent

fptrcnt	equ	8
fptrs12:
	dw	entriestowords12
	dw	entryfromblock12
	dw	entrytoblock12
	dw	entriesperblock12
	dw	getblockaddr12
	dw	isendchain12
	dw	isbadblock12
	dw	ismdesc12

fptrs16:
	dw	entriestowords16
	dw	entryfromblock16
	dw	entrytoblock16
	dw	entriesperblock16
	dw	getblockaddr16
	dw	isendchain16
	dw	isbadblock16
	dw	ismdesc16

fptrs32:
	dw	entriestowords32
	dw	entryfromblock32
	dw	entrytoblock32
	dw	entriesperblock32
	dw	getblockaddr32
	dw	isendchain32
	dw	isbadblock32
	dw	ismdesc32

reservednames	db	"AUX     "
		db	"CON     "
		db	"CLOCK$  "
		db	"NUL     "
		db	"PRN     "
		db	"COM1    "
		db	"COM2    "
		db	"COM3    "
		db	"COM4    "
		db	"COM5    "
		db	"COM6    "
		db	"COM7    "
		db	"COM8    "
		db	"COM9    "
		db	"LPT1    "
		db	"LPT2    "
		db	"LPT3    "
		db	"LPT4    "
		db	"LPT5    "
		db	"LPT6    "
		db	"LPT7    "
		db	"LPT8    "
		db	"LPT9    "
		db	0
lostfnd		db	"LOST    FND"

msg_usage	db	'SSDSCAN pre-alpha, for 16 bit FAT only', 13, 10
		db	'Usage: SSDSCAN DRIVE: [/F] [/C] [/D]', 13, 10
		db	'/F   Fix errors without prompting', 13, 10
		db	'/C   Check chain length against file length', 13, 10
		db	'/Z   Recovery directories that have zeroed sectors in the middle', 13, 10
		db	'     (caution: 0 is normally the directory terminator; can damage FS instead)', 13, 10
		db	'/D   Describe filesystem'
out_newline	db	13, 10, '$'
out_crfile	db	13, 'File $'
msg_nocmem	db	'Insufficient Conventional Memory available to check any disk.', 13, 10, '$'
msg_nocmem2	db	'Insufficient Conventional Memory available to check this disk.', 13, 10, '$'
msg_noram	db	'Insufficinet memory available to check this disk.', 13, 10, '$'
msg_fragram	db	'Memory is too fragmented, try rebooting.', 13, 10, '$'
msg_xmserr	db	'XMS Error', 13, 10, '$'
msg_cancel	db	"^C: Cancelled", 13, 10, '$'
state_mediaq	db	13, 10
state_media	db	        'Media Descriptor     : $'
state_fat	db	13, 10, 'File Allocation Table: $'
state_dir	db	13, 10, 'Directory Structure  : $'
out_cr		db	13, '$'
out_check	db	251, "  $"
out_percentpost	db	'%', 8, 8, 8, '$'
dsc_bps		db	13,     'Bytes per sector           : '
dsc_spc		db	13, 10, 'Sectors per cluster        : '
dsc_spchk	db	13, 10, 'Sectors per chunk          : '
dsc_spfat	db	13, 10, 'Sectors per FAT            : '
dsc_numfats	db	13, 10, 'Number of FATs             : '
dsc_rootent	db	13, 10, 'Number of root dir entries : '
dsc_firstdata	db	13, 10, 'Sector of first cluster    : '
dsc_clusters	db	13, 10, 'Number of clusters         : '
dsc_totalfiles	db	13, 10, 'Total files/directories    : '
dsc_usedclust	db	13, 10, 'Number of used clusters    : '
dsc_freeclust	db	13, 10, 'Number of free clusters    : '
dsc_end:
msg_error0	db	'Read error accessing boot sector.', 13, 10
		db	'This might be recoverable using SSDFIXBT after copying to a new SSD.', 13, 10, '$'
msg_notpow2	db	"Bytes per sector isn't a power of 2", 13, 10, '$'
msg_logfail	db	"Unable to initialize logical sectored FAT", 13, 10, "because it isn't aligned to physical sectors.", 13, 10, '$'
msg_overflow	db	'Overflow computing filesystem offsets', 13, 10, '$'
msg_noboot	db	'Failed to find a valid boot sector', 13, 10, '$'
msg_badmdesc	db	'Encountered a bad sector trying to read media descriptor.', 13, 10
msg_nomdesc	db	"Media Descriptor does not correspond to boot sector.", 13, 10, "Most likely this isn't a FAT filesystem after all.", 13, 10, '$'
msg_badroot	db	"Root cluster out of range.", 13, 10, '$'
query_bootsect	db	"Boot sector damaged, however a backup boot sector was found. Restore it?$"
query_fatdiff	db	"FATs disagree, repair with best guess?$"
query_anomalous	db	"Anomalous record in FAT, repair?$"
query_clustone	db	"Encountered mid-allocation cluster value, repair?$"
query_freealloc	db	"Encountered free block in use, allocate?$"
query_clustout	db	"Encountered out-of-range cluster value, repair?$"
query_xlink	db	"Encountered cross-linked cluster, repair?$"
query_badname	db	" has invalid filename characters, fix?$"
query_resname	db	" is a reserved name, auto rename?$"
query_anomfile	db	" is an anomalous directory entry, delete?$"
query_freefile	db	" refers to a free cluster, delete?$"
query_xlinkfile	db	" refers to a crosslinked cluster, delete?$"
query_dirnot	db	" is marked as a directory but seems to not be, select(F,D)$"
query_fixemptyd	db	" is marked as a directory but is an empty file, fix?$"
query_fixdotdot	db	" has a broken .. entry, fix?$"
query_dirbadsec	db	"Bad sector encountered reading directory, replace?$"
query_fixtail	db	"Bad sector marker encountered at end of directory, repair directory?$"
query_fixbadlen	db	" has an impossible length, fix?$"
query_fixbadptr	db	" has a bad block in chain, truncate?$"
query_recdir1	db	"Found lost directory $"
query_recdir2	db	".CHK via $"
query_recfile	db	"Found lost file $"
query_recq	db	".CHK, recover?$"
msg_hdbaddesc	db	"Invalid media descriptor; FAT16 and FAT32 must use F8 or FF", 13, 10, '$'
msg_nogoodfat	db	"Correlated bad sectors destroyed FAT.", 13, 10, '$'
msg_rootdirlost	db	"Root directory lost", 13, 10, '$'
msg_toodeep	db	"Directory tree depth exceed 51; cannot ccontinue.", 13, 10, '$'
msg_2bigfile	db	" exceeds 4GB by traversal. In the vain hope", 13, 10
		db	"you have an OS that can read this, repair (truncate) will not be performed.", 13, 10, '$'
msg_rootbadsect	db	"Bad sector encountered in root directory", 13, 10, '$'
msg_replacer	db	13, 10, "New bad sector encountered reading SSD; advise immediate replacement", 13, 10, '$'
msg_replace	db	13, 10, "Bad sector encountered writing to SSD; advise immediate replacement", 13, 10, '$'
msg_outofslots	db	"Ran out of root directory entries, free some up.", 13, 10, '$'
msg_outofspace	db	"Ran out of space repairing disk, free some up.", 13, 10
msg_goodenuf	db	"The file system should be good enough to move a file off the disk.", 13, 10, '$'

	align	16, db 0		; Everything depends on _bss being aligned!

_end:

section .bss

_bss:
;The code will collapse if bss isn't a multiple of 16 bytes
;Allocations broken up in to chunks that correspond
xms_xfer_len	resb	4
xms_xfer_src	resb	2
xms_xfer_srcoff	resb	4
xms_xfer_dst	resb	2
xms_xfer_dstoff	resb	4

bitvectorptrs	resb	bitvectorrlen * 128	; Use bitvector* constants as index offsets

entriestowords	resb	2		; Function pointers
entryfromblock	resb	2
entrytoblock	resb	2
entriesperblock	resb	2		; on call, AX = number of words in block
getblockaddr	resb	2		; on call, CX = number of entries in full block
isendchain	resb	2
isbadblock	resb	2
ismdesc		resb	2

cmem		resb	2		; Number of usable paragraphs of conventional memory in our memory block beyond the stack
opflags		resb	2
disk		resb	1
numfats		resb	1
fatinfosect	resb	2
buf1seg		resb	2
buf2seg		resb	2
buf3seg		resb	2
buf4seg		resb	2

buf1sector	resb	4
buf2sector	resb	4
buf3sector	resb	4
buf4sector	resb	4

reservedsects	resb	2
bytespersector	resb	2
sectsperchunk	resb	2
sectsperclust	resb	2
totalclust	resb	4
freeclust	resb	4

rootclust	resb	4
rootdirentries	resb	4
sectsperfat	resb	4
firstclustsect	resb	4

zerothclustsect	resb	4		; Optimization hack: direct multiply and add to get sector
highestclust	resb	4
endchainlow	resb	2
savedaccessflag	resb	2
mklostfndclust	resb	4

rootdirsects	resb	2
bytesperclust	resb	2
		resb	2
cmempool	resb	2
cmempoollen	resb	2
fattype		resb	1
descriptor	resb	1
preserve_sp	resb	2		; Disk access slaughters these registers but we need them back _immediately_
preserve_bp	resb	2		; These are accessed from cs but we build with cs = ds

xmsfunc		resb	4
fatinbuf	resb	4
fatinbitmask	resb	4
bitmasklen	resb	2
wordsperchunk	resb	2

totalfiles	resb	4
progress	resb	4
bitbuflen	resb	2		; In entries
pgdisplay	resb	1
		resb	1
		resb	4

bitbufseg	resb	2
bitbufsi	resb	2
bitbufbx	resb	2
bitbufes	resb	2
bitbuflow	resb	4
bitbufhigh	resb	4
bitbufoff	resb	4
xmspinseg	resb	2
pinsi		resb	2
pinbx		resb	2
pines		resb	2
pinlow		resb	4
pinhigh		resb	4
pinoff		resb	4
		resb	4
		resb	4

savedfilename	resb	16		; Saved file name and time (keep me on bottom)

_endbss:

bitvectorlength	equ	0
bitvectorptr	equ	4
bitvectortype	equ	8
bitvectorrlen	equ	10

b_mem_internal	equ	1
b_mem_dos	equ	2
b_mem_umb	equ	3
b_mem_high	equ	4
b_mem_xms	equ	5

opflag_f	equ 1
opflag_c	equ 2
opflag_d	equ 4
opflag_z	equ 8
opflag2_bigdisk	equ 1
opflag2_7305	equ 2
opflag2_ebpb	equ 4
opflag2_rclust	equ 8
opflag2_sbcpin	equ 32
opflag2_ulink	equ 64
opflag2_a20	equ 128

error_nofix	equ 4
error_norun	equ 8
error_usage	equ 16
error_cancel	equ 32
