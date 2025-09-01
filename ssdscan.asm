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
;    if too soon, ask extend or free
;    if too late, must truncate
;    as we traverse each cluster, change its bitmap entry to 01; if it already is 01 it must be a bad sector

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
; buf1:0      first FAT buffer
; buf2:0      second FAT buffer and cluster data buffer
; buf3:0      third FAT buffer, if we need it
; buf4:0      working buffer (FAT sort and dirwalk)
; buf4:0      XMS bitmap pool buffer, if it exists
; bitbufseg:0 bitmap buffer, if it needs to exist
; cmempool:0  bitmap pool in main memory
;
; ... additional bitmap pool in dos-allocated RAM
; ... additional bitmap pool in UMBs if they exist
; ... additional bitmap pool in XMS if it exists

_start:
	cld
	; Check size of DOS memory block we are in
	; We're supposed to be able to check the PSP; however it's bugged on DOS4+ when we're very low.
	mov	ax, ds
	dec	ax
	mov	es, ax
	mov	ax, es:[3]
	cmp	ax, word (stackbottom + 32768) / 16
	jae	.mem
.nomem	mov	dx, msg_nocmem
	mov	ah, 9
	int	21h
	mov	ax, 4C04h			; We can *NOT* jump to exit here; this will trash DOS
	int	21h				; as the exit-free memory is unintialized
.mem	mov	sp, stackbottom	
	sub	ax, (stackbottom) / 16
	mov	[cmem], ax
	mov	di, _bss			; Zero initialize BSS (saves total code size)
	mov	cx, (_endbss - _bss) / 2
	xor	ax, ax
	rep	stosw
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
	mov	ax, 4C01h	; FIXME normalize error codes
	int	21h
.go	mov	[opflags], bx
	mov	[disk], dl
	;TODO disable ^C handler and IO error handler as these will permanently leak memory

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
	call	diskread0
	jnc	.read0
.error0	xor	dx, dx
	xor	cx, cx
	call	diskread0
	jnc	.read0
	call	outax
.errorZ	mov	dx, msg_error0
.errorx	mov	ah, 9
	int	21h
	mov	ax, 4C03h
	int	21h
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
	xor	dx, dx
	xor	cx, cx
	xor	ax, ax
	call	diskread0
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
	cmp	[es:15h], byte 0E0h	; Media Descriptor itself
	jnb	.bpb
.notbpb	mov	ax, dx
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
	cmp	bx, 0
	jne	.f32
	cmp	ax, 0FFF6h
	jae	.f32
	cmp	ax, 0FF6h
	jae	.f16
	mov	[fattype], byte 12
	jmp	.gftyp
.f16	mov	[fattype], byte 16
	jmp	.gftyp
.f32	mov	[fattype], byte 32
.gftyp	mov	bl, [es:015h]
	mov	[descriptor], bl
	; We've finished extracting all information from the boot sector

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
	mov	[zerothclustsect], dx

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
	mov	cx, dsc_end - dsc_clusters
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
	mov	bx, ax			; bx = ceil(bx / ch)
.dec	shr	bx, 1
	adc	bx, 0
	shr	ch, 1			; works because ch is always a power of 2
	jnz	.dec

.initall:
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
	or	dx, dx
	jnz	.cbigr
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
	call	.mcrec			; Record single entry for all memory
	jmp	.hmem
.mcrec	push	cx			; BL = type, CX = pointer, DX:AX = how many paragraphs
	mov	cx, 6
.mcmm	shl	ax, 1
	rcl	dx, 1
	loop	.mcmm
	pop	cx
	mov	[si + bitvectortype], bl
	mov	[si + bitvectorptr], cx
	mov	[si + bitvectorlength], ax
	mov	[si + bitvectorlength + 2], dx
	ret
.cbigr	mov	cx, [bitbufseg]
	add	cx, bx		; BX preserved the entire time: size of bitbufseg required
	mov	[cmempool], cx	; There's no pool (len = 0); remember length of bitbufseg = cmempool - bitbufseg
	mov	bx, ds
	add	bx, [cmem]
	sub	bx, cx
	ja	.hcmema
	jae	.hcmem
	mov	dx, msg_nocmem2
	mov	ah, 9
	int	21h
	mov	ax, 4C04h	; TODO normalize exit codes
	int	21h
.hcmema	sub	ax, bx
	sbb	dx, 0
	push	ax
	push	dx
	mov	ax, bx
	xor	dx, dx
	mov	bl, b_mem_internal
	call	.mcrec
	pop	dx
	pop	ax
.hcmem	db	0xCC		; TODO allocate memory from DOS, UMB, and XMS
	hlt

.hmem:
	; Zero memory map
	mov	si, bitvectorptrs
.lfatl	mov	al, [si + bitvectortype]
	cmp	al, byte 0
	je	.lfat0
	cmp	al, byte b_mem_xms
	jb	.h0seg
	; It's some kind of API-swapped memory
	test	[opflags + 1], byte opflag2_cbf
	jnz	.cfbc
	mov	es, [bitbufseg]
	xor	di, di
	mov	cx, [cmempool]
	sub	cx, [bitbufseg]
	shl	cx, 1
	shl	cx, 1
	shl	cx, 1
	xor	ax, ax
	rep	stosw
	mov	[xms_xfer_len + 2], word 0
	call	xmssegsrc
	or	[opflags + 1], byte opflag2_cbf
.cfbc	mov	ax, [si + 4]
	mov	dx, [si + 6]
	mov	bx, ax
	mov	di, dx
	mov	cx, [cmempool]
	sub	cx, [bitbufseg]
.cfbcl	or	dx, dx
	jnz	.cfbcb
.cfbcl2	cmp	ax, cx
	ja	.cfbcb
	mov	cx, ax
.cfbcb	mov	[xms_xfer_len], cx		; Right now the only kind of API-swapped memory is XMS so we do that.
	mov	[xms_xfer_dstoff], bx
	mov	[xms_xfer_dstoff + 2], di
	push	si
	push	ax
	mov	si, [si + 2]
	mov	[xms_xfer_src], si
	mov	ah, 0bh
	call	far [xmsaddr]
	pop	ax
	pop	si
	add	bx, cx
	add	di, 0
	sub	ax, cx
	sbb	dx, 0
	jnz	.cfbcl2
	test	ax, ax
	jnz	.cfbcl
	jmp	.lfatn
.h0seg	mov	es, [si + bitvectorptr]
	mov	ax, [si + bitvectorlength]
	mov	dx, [si + bitvectorlength + 2]
	shr	dx, 1
	rcr	ax, 1
	adc	ax, 0
	shr	dx, 1
	rcr	ax, 1
	adc	ax, 0
	;DX:AX = number of bytes owned
	shr	ax, 1
	adc	ax, 0
	push	ax
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
.h0snl	pop	cx
	xor	ax, ax
	xor	di, di
	rep	stosw
.lfatn	add	si, 8
	cmp	si, bitvectorptrs + (8 * 128)
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
	push	dx
	push	ax
	mov	es, [buf1seg + si]
	call	diskread0
	pop	ax
	pop	dx
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
	mov	al, 3		; TODO normalize exit codes
	jmp	exit
.nofixfatdiff:
	mov	al, 3		; TODO normalize exit codes
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
	mov	al, 4		; TODO normalize exit codes
	jmp	exit		; Can't risk continuing as this could destroy entire FS
.fxdesc	mov	ah, 0FFh
	mov	dx, 0FFFFh
	mov	al, [descriptor]
	call	entrytoblockall
.isdesc	inc	di
	call	[entryfromblock]
	test	[opflags + 1], byte opflag2_rclust
	jz	.secondnormal
	;FIXME this code blows up if DX:AX is out of range -- this isn't a recoverable situation so we can only raise an error
	mov	[rootclust], ax
	mov	[rootclust + 2], dx
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
	mov	al, [descriptor]
	mov	ah, dh
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
	je	.setendchain	 ; If the user says no, file will be truncated on scan pass
	jmp	.clustdone
.notfree:
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
	mov	ah, 0FFh
	mov	al, [descriptor]
	call	entrytoblockall
	pop	di
	jmp	.notoverlarge
.fixallocfarbefore:
	push	es
	mov	es, [buf4seg]	; Load prior into buf4 and set end of chain
	mov	bx, 0FFFh
	push	bx
	mov	bh, 0FFh
	mov	bl, [descriptor]
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
	mov	ah, 0FFh
	mov	al, [descriptor]
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
	; TODO percentage

	;Read in sectors for next loop
	mov	cx, [bp - 10]
	call	[getblockaddr]
	xor	cx, cx
	mov	[bp - 2], cx
	add	ax, [reservedsects]
	adc	dx, 0
.loopmorefats:
	push	ax
	push	dx
	push	cx
	mov	bx, cx
	shl	bx, 1
	mov	es, [buf1seg + bx]
	call	diskread0
	pop	cx
	pop	dx
	pop	ax
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


stage_dirwalk:
	mov	dx, state_dir
	mov	ah, 9
	int	21h
	mov	ax, [rootclust]
	mov	dx, [rootclust + 2]
	mov	si, [reservedsects]
	mov	di, [reservedsects + 2]
	mov	bx, 1

	call	descendtree
	
	; Next: recover directories

	; Next: recover files (all in this stage because % measurement)
	call	checkmark

	mov	al, 0
exit:	mov	ah, 4Ch
	; There will be free XMS, etc. here
	int	21h

newbadsectors:
	mov	dx, msg_replacer
	mov	ah, 9
	int	21h
	mov	al, 3	; TODO error normalization
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
.again	mov	ah, 7
	int	21h
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
	pop	dx
	pop	ax
	ret

; Displays a checkmark
checkmark:
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

;DEBUG: out line
%ifdef NOTDEF
outesline:
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

invalidatenextcluster:
	mov	cx, 0FFFFh
	mov	[clustinbuffer], cx
	mov	[clustinbuffer + 2], cx
	ret
	
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
	push	ax
	push	dx
	call	diskread0
	pop	dx
	pop	ax
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
	push	cx
	xchg	ax, dx
	mov	cx, dx
	mul	word [sectsperclust]	; Cannot overflow
	add	cx, ax
	xchg	ax, dx
	pop	ax
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
	call	invalidatenextcluster
	mov	bp, sp
	sub	sp, 42
	mov	es, [buf4seg]
	xor	cx, cx
	mov	[bp - 10], cx	; BP - 10 = VFAT entry reset
				; BP - 9 = state flags
	mov	[bp - 18], di	; BP - 20 = patchpoint sector
	mov	[bp - 20], si
	mov	[bp - 22], bx	; BP - 22 = patchpoint byte offset
	mov	[bp - 24], cx	; BP - 24 = overflow from BP-22 when it's FAT
	mov	[bp - 26], cx	; BP - 26 = enumerator state
				;	0 = normal
				;	2 = set not directory and resume
				;	4 = set E5 scan (VFAT or /Z)
				;	6 = fix .. scan
				;	8 = look for nonzero scan
				;	10 = validate . and ..
				; BP - 30 = enumerator recovery state cluster
				; BP - 32 = enumerator recovery state offset
	mov	[bp - 36], cx	; BP - 38 = sector loaded into buf2seg
	mov	[bp - 38], cx
	dec	cx
	mov	[bp - 32], cx	; BP - 32 = enumerator recovery state offset
	mov	[bp - 30], cx	; BP - 30 = enumerator recovery state clsuter
	mov	[bp - 28], cx
	mov	[bp - 12], cx	; BP - 12 = unwind start entry within cluster
	mov	[bp - 14], cx	; BP - 16 = unwind cluster
	mov	[bp - 16], cx
	xor	si, si
	mov	[es:si], ax	; Starting cluster of directory
	mov	[es:si + 2], dx	; TODO: check if FAT32 wants 0 or cluster for .. to root
	mov	[es:si + 4], ax	; Current cluster of directory
	mov	[es:si + 6], dx
	mov	di, 2		; Skip initial . and ..
	cmp	bx, 1
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
	;call	checkmark
	;mov	ax, [es:si + 6]
	;call	outax
	;mov	ax, [es:si + 4]
	;call	outax
	;call	newline
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
	call	invalidatenextcluster	; TODO ???
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
	mov	al, 3	; TODO normalize exit codes
	jmp	exit
.postreaddirbadsector:			; means after the bad sector check on readdir

	mov	bx, [bp - 26]
	mov	ax, [descendtreetable + bx]

	mov	bx, di
	mov	cl, 5
	shl	bx, cl
	mov	es, [buf2seg]
	cmp	[es:bx], byte 0E5h
	je	.skipentry
	jmp	ax
.skipentry:				; Don't process this one, OR return from processor
	mov	es, [buf4seg]
	inc	word [es:si + 8]
	inc	di
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
	mov	ax, [bp - 10]
	test	[bp - 9], byte 1
	jz	.postreaddirbadsector
	jmp	.readdirentryproc	; Cache invalidated: reload
.skipentrynotroot:
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
	call	getnextcluster
	mov	es, [buf4seg]
	cmp	dx, [highestclust + 2]
	jb	.advanceiscluster
	cmp	ax, [highestclust]
	jae	.enddirectory
.advanceiscluster:
	mov	[es:si + 4], ax
	mov	[es:si + 6], dx
	and	[es:si + 8], word 8000h	;Reset entry within cluster, keep flag
	test	si, si
	je	.advanceisnottoplevel
	; PSYCH; "special case" on input is general case on advance
	mov	[bp - 22], ax
	mov	[bp - 24], dx
	mov	bx, [reservedsects]
	mov	[bp - 20], si
	mov	[bp - 18], word 0
.advanceisnottoplevel:
	jmp	.readdirentryproc
.enddirectory:
	test	si, si
	jz	.endscan
	mov	[bp - 26], byte 0	; Return to normal state
	sub	si, 10
	jmp	.skipentry
.endscan:
	mov	sp, bp
	ret

.scanvolume:
	cmp	[es:bx + 0Bh], byte 0Fh
	je	.scanlfn
.scanlfnskipentryv:
	jmp	.skipentry	; It's a volume label. Nothing to do.
.scanlfn:			; It's an LFN entry. We remove stale LFN entries.
	mov	al, [es:bx]
	test	al, byte 20h
	jne	.scanlfnnext
	test	[bp - 10], byte 0FFh
	jnz	.scanlfnremovethis
	and	al, 1Fh
	jz	.scanlfnremovethis
	mov	[bp - 10], al
	jmp	.scanlfnskipentryv

.scanlfnnext:
	mov	ah, [bp - 10]
	dec	ah
	jz	.scanlfnremovethis
	cmp	ah, al
	jne	.scanlfnremovethis
	mov	[bp - 10], ah
	jmp	.scanlfnskipentryv

.scanlfnremoveprev:
	clc
	jmp	.scanlfnremove
.scanlfnremovethis:
	stc
.scanlfnremove:
	mov	[bp - 10], byte 0
	mov	es, [buf2seg]
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
	mov	[bp - 26], byte 6
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
	cmp	[fattype], byte 16
	je	.scansetnormal_allones
	cmp	[es:bx + 14h], cx
	jne	.scansetnormal_notallones
.scansetnormal_allones:
	mov	[es:bx], byte 0E5h	; Auto-repair all ones entry (recovered bad sector *properly*)
	mov	[es:bx + 0Dh], byte 0E5h
	or	[bp - 9], byte 2	; set dirty bit
	jmp	.skipentry
.scansetnormal_notallones:
	test	[es:bx + 0Bh], byte 8
	jnz	.scanvolume
	test	[bp - 10], byte 0FFh
	jnz	.scanlfnremoveprev
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
	push	ax
	mov	ax, dx
	call	outax
	pop	ax
	call	outax
	call	newline
	mov	dx, query_anomfile
	call	.queryfixentry
	cmp	al, 'y'
	jne	.skipentry
	jmp	.scansetnormal_allones	; Fix (delete) anomalous file
.scannormal_notlow2:
	test	ax, ax
	jnz	.scannormal_notempty
	test	dx, dx
	jnz	.scannormal_notempty
	; Empty file
	test	[es:bx + 0Bh], byte 10h
	jne	.scannormal_notdirectory
	mov	dx, query_fixdotdot
	call	.queryfixentry
	mov	es, [buf4seg]
	cmp	al, 'y'
	jne	.scannormal_notdirectory
	and	[es:bx + 0Bh], byte 0EFh
	or	[bp - 9], byte 2	; set dirty bit
.scannormal_notdirectory:
	jmp	.skipentry
.scannormal_notempty:
	push	bx
	push	si	
	push	ax
	push	dx
	call	getbitsclust
	test	ch, 2
	jz	.scanbits_free
	test	ch, 1
	jnz	.scanbits_xlink
	mov	cl, 2
	pop	dx
	pop	ax
	call	setbitsclust
	pop	si
	pop	bx
	test	[es:bx + 0Bh], byte 10h
	jz	.scannormal_notdirectory
	jmp	.scannormal_directory
.scanbits_free:
	mov	dx, query_freefile
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scanbits_nofix
	jmp	.scanbits_delete
.scanbits_xlink:
	mov	dx, query_xlinkfile
	call	.queryfixentry
	cmp	al, 'y'
	jne	.scanbits_nofix
.scanbits_delete:
	pop	dx
	pop	ax
	pop	si
	pop	bx
	jmp	.scansetnormal_allones
.scanbits_nofix:
	pop	dx
	pop	ax
	pop	si
	pop	bx
	jmp	.skipentry
.scannormal_end:
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
	mov	[bp - 32], bx
	mov	[bp - 30], ax
	mov	[bp - 28], dx
	mov	[bp - 26], byte 8
	jmp	.skipentry
.scannormal_toodeep:
	mov	dx, msg_toodeep
	mov	ah, 9
	int	21h
	mov	ax, 4C04h		; TODO normalize exit codes
	int	21h
.scannormal_directory:
	;We encountered a directory, set up to descend
	cmp	si, 500
	ja	.scannormal_toodeep
	call	.dirwritebackifdirty
	push	ds			; Need directory name for repair prompts below
	push	es
	push	ds
	pop	es
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
	mov	[bp + 40], cx	; DIR created date
	mov	cx, [es:bx + 18h]
	mov	[bp + 42], cx	; DIR created time
	mov	es, [buf4seg]
	add	si, 10
	mov	[es:si], ax
	mov	[es:si + 2], dx
	mov	[es:si + 4], ax
	mov	[es:si + 6], dx
	xor	cx, cx
	mov	[es:si + 8], cx
	dec	cx
	mov	[bp - 32], cx	; enumerator recovery state offset
	mov	[bp - 30], cx	; enumerator recovery state clsuter
	mov	[bp - 28], cx
	mov	[bp - 12], cx	; unwind start entry within cluster
	mov	[bp - 14], cx	; unwind cluster
	mov	[bp - 16], cx
	mov	[bp - 26], byte 10	; Validate . and .. after load
	jmp	.readdirentryproc

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
	cmp	[fattype], byte 32
	jne	.scanfixparent_notfat32
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
	push	ax
	push	dx
	call	diskread0
	pop	dx
	pop	ax
	pop	di
	pop	si
	or	[bp - 9], byte 1
	jc	.scanskipreturn		; Not dealing with this nonsense
	mov	es, [buf4seg]
	mov	bx, [es:si]
	mov	cx, [es:si + 2]
	mov	es, [buf2seg]
	mov	[es:3Ah], bx
	cmp	[fattype], byte 32
	jne	.scanfixparent_notfat32dentry
	mov	[es:34h], cx
.scanfixparent_notfat32dentry:
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

.dotentries_generatebaselineentry:
	stosw
	mov	ax, '  '
	stosw
	stosw
	stosw
	stosw
	mov	ah, 10h
	stosw
	mov	ah, 0
	mov	cx, 6
	rep	stosw
	mov	ax, [bp - 40]
	stosw
	mov	ax, [bp - 42]
	stosw
	xor	ax, ax
	stosw
	stosw
	ret

	; Rebuild trashed . entries from bad sector recovery
.dotentries_allbits:
	xor	di, di
	mov	ax, '..'
	call	.dotentries_generatebaselineentry
	mov	ax, '. '
	call	.dotentries_generatebaselineentry
	or	[bp - 9], byte 2	; set dirty bit
	jmp	.dotentries_match2

.scanvalidatedotentries:
	; There's five things that can happen
	; 1) Sector could be all 0 or all 1 bits, in which case we restore the . and .. entries
	; 2) Sector could have . and .. entries in which case we might or might not have to repair
	; 3) Sector could be not a directory in which case we repair by clearing directory bit
	; 4) Sector could be a recovered bad sector -- we will have placed E5 entries
	; 5) Sector could be a directory
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
	mov	bx, [es:0]
	mov	cx, [es:2]
	mov	es, [buf2seg]
	cmp	[es:0], word 20E5h
	je	.dotentries_trybadrec
	cmp	[es:0], word '. '
	jne	.dotentries_nomatch
	cmp	[es:2], word '  '
	jne	.dotentries_nomatch
	cmp	[es:4], word '  '
	jne	.dotentries_nomatch
	cmp	[es:6], word '  '
	jne	.dotentries_nomatch
	cmp	[es:8], word '  '
	jne	.dotentries_nomatch
	cmp	[es:10], word '  '
	jne	.dotentries_nomatch
	cmp	[es:11], byte ' '
	jne	.dotentries_nomatch
	cmp	[fattype], byte 16
	jbe	.dotentries_fat16
	cmp	[es:14h], cx
	jne	.dotentries_nomatch
.dotentries_fat16:
	cmp	[es:1Ah], bx
	jne	.dotentries_nomatch
	cmp	[es:32], word '..'
	jne	.dotentries_nomatch
	cmp	[es:2 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:4 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:6 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:8 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:10 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:11 + 32], byte ' '
	jne	.dotentries_nomatch
	jmp	.dotentries_match
.dotentries_trybadrec:
	jne	.dotentries_nomatch
	cmp	[es:2], word '  '
	jne	.dotentries_nomatch
	cmp	[es:4], word '  '
	jne	.dotentries_nomatch
	cmp	[es:6], word '  '
	jne	.dotentries_nomatch
	cmp	[es:8], word '  '
	jne	.dotentries_nomatch
	cmp	[es:10], word '  '
	jne	.dotentries_nomatch
	cmp	[es:11], byte ' '
	jne	.dotentries_nomatch
	cmp	[es:32], word 20E5h
	jne	.dotentries_nomatch
	cmp	[es:2 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:4 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:6 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:8 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:10 + 32], word '  '
	jne	.dotentries_nomatch
	cmp	[es:11 + 32], byte ' '
	jne	.dotentries_nomatch
	jmp	.dotentries_allbits	; Clearly it's a rebuild
.dotentries_nomatch:
	; It's not a directory; prompt repair/clear
	mov	dx, out_crfile
	mov	ax, 9
	int	21h
	mov	bx, savedfilename
	call	.out_entrynameds
	mov	dx, query_dirnot
	mov	ah, 9
	int	21h
.again	mov	ah, 7
	int	21h
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
	test	[es:si - 1], byte 80h
	mov	es, [buf2seg]
	je	.dotentries_fixparent
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
	mov	[es:si + 8], di
	jmp	.skipentry
.dotentries_isfile:
	sub	si, 10
	mov	[bp - 26], byte 2
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
	mov	ch, 0C0h
	cmp	[es:si + 4], word 0
	jne	.dirwritebackcluster
	cmp	[es:si + 6], word 0
	jne	.dirwritebackcluster
	mov	ch, 80h
.dirwritebackcluster:
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

.readdirnofixbad:		; We can't recover from skipping this one
	mov	ax, 4C04h	; TODO normalize exit codes
	int	21h
	; Routine for repairing bad sector in directory
.readdirbadsector:
	mov	cx, state_dir
	mov	dx, query_dirbadsec
	call	queryfixyn
	cmp	al, 'y'
	jne	.readdirnofixbad
	mov	es, [buf4seg]
	mov	ax, [es:si + 4]
	mov	dx, [es:si + 6]
	push	si
	push	di
	mov	cx, .recbadsectorfill
	push	cx
	test	si, si
	jnz	.readdirbadsector_subsequent
	push	word [bp - 18]
	push	word [bp - 20]
	push	word [bp - 22]
	push	word [bp - 24]
	jmp	.readdirbadsector_es
	;Generate patchpoint from unwind data
	mov	ax, [es:si + 8]
	and	ax, 7FFFh
	xor	dx, dx
	mov	cx, 32
	mul	cx
	div	[bytespersector]
	xchg	ax, cx	; CX = sector offset within cluster
	xchg	bx, dx	; BX = byte offset within sector
	mov	ax, [es:si + 4]
	mov	dx, [es:si + 6]
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
	call	invalidatenextcluster
	mov	es, [buf4seg]
	pop	di
	pop	si
	mov	bx, [es:si]
	mov	cx, [es:si + 2]
	cmp	bx, [es:si + 4]
	jne	.recbadnotfirst
	cmp	cx, [es:si + 6]
	jne	.recbadnotfirst
	mov	bx, ax
	mov	cx, dx
	mov	[es:si], ax
	mov	[es:si + 2], dx
	jmp	.readdirentryproc
.recbadnotfirst:
	mov	[bp - 30], cx
	mov	[bp - 28], bx
	mov	[bp - 26], byte 6
	or	di, 8000h
	mov	[bp - 32], di
	mov	[es:si + 8], word 8000h
	jmp	.readdirentryproc
	
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
	push	ax
	call	diskread0
	pop	ax
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
	push	ax
	push	dx
	call	diskread0
	pop	dx
	pop	ax
	jc	newbadsectors
	mov	bx, [bp + 4]
	mov	cx, [bp - 12]
	mov	[es:bx + 1Ah], cx
	cmp	[fattype], byte 32
	jne	.dirnot32
	mov	cx, [bp - 10]
	mov	[es:bx + 14h], cx
.dirnot32:
	mov	ch, 0Ch
	call	diskwrite0
.epilog:
	; update old cluster -> bad
	mov	bx, 0FFFFh
	push	bx
	mov	bl, 0F7h
	push	bx
	call	setclusterinfats
	; return new cluster
	mov	ax, [bp - 12]
	mov	dx, [bp - 10]
	mov	bp, sp
	ret	10

	;Allocate cluster. ES = buffer, DX:AX = value to set it to. Trashes all registers except ES, BP
alloccluster:
	push	ax
	push	dx
	push	es
	mov	ch, 0
	mov	ax, .found
	push	ax
	call	scanbitsclust
	mov	dx, msg_outofspace
	mov	ah, 9
	int	21h
	mov	ax, 4C03h	; TODO Normalize exit codes
	int	21h
.found	mov	sp, bp		; longjmp!
	pop	bp
	pop	cx		; Trash
	pop	es
	;DX:AX is allocated cluster
	;on stack is cluster to point it to
	push	dx
	push	ax
	mov	ch, 3
	call	setbitsclust
	pop	ax
	pop	dx
	call	setclusterinfats
	ret
	
	; Entry point; DX:AX = base cluster no, DI = offset
	; Result in CH; destroys everything but DI and ES
getbitsclustdi:
	add	ax, di
	adc	dx, 0
	;DX:AX = base cluster
getbitsclust:
	push	es
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
setbitsclust:
	push	es
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
clearbitsclust:
	push	es
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
	mov	si, bitvectorptrs
.loop	cmp	[si + bitvectorlength + 2], dx
	ja	.this
	jb	.next
	cmp	[si + bitvectorlength], ax
	jae	.this
.next	sub	ax, [si + bitvectorlength]
	sbb	dx, [si + bitvectorlength + 2]
	add	si, 8
	jmp	.loop
.this	cmp	[si + bitvectortype], byte b_mem_xms
	je	.xms
	ja	.fault
	mov	es, [si + bitvectorptr]	; It's addressible memory
	mov	cl, al
	and	cl, 3
	shl	cl, 1
	shr	dx, 1
	rcr	ax, 1
	shr	dx, 1
	rcr	ax, 1
	mov	bx, ax
	ret
.xms	int3	; TODO fetch from XMS
.fault	int3	; FIXME

	; Locate all entries matching a pattern
	; Arguments: CH = pattern, stack = callback (called with bp + 4 = address of callback)
	; Destroys all registers other than CS,DS,SS,BP
scanbitsclust:
	push	bp
	mov	bp, sp
	sub	sp, 4		; May be used later for xms routine
	mov	si, bitvectorptrs
	xor	dx, dx
	mov	ax, 2
	mov	cl, 4
	shl	ch, cl

	; Outer loop -- get bitvector to enumerate
.outer	cmp	[si + bitvectortype], byte 0
	je	.retouter
	cmp	[si + bitvectortype], byte b_mem_xms
	je	.xms
	ja	.fault
	mov	es, [si + bitvectorptr]
	mov	bx, [si + bitvectorlength + 2]
	mov	di, [si + bitvectorlength]
	call	.div2ceil	; 4 entries per byte
	call	.div2ceil
	; Tricky part: do big chunks first
	push	di
	xor	di, di
.mid	test	bx, bx
	jnz	.mide
	call	.inner
	push	ax
	mov	ax, es
	add	ax, 1000h
	mov	es, ax
	pop	ax
	dec	bx
	jmp	.mid
.mide	pop	di
	call	.inner
.cont	add	si, 8
	jmp	.outer
.retouter:	
	add	sp, 4
	pop	bp
	ret
.xms	int3	;TODO fetch from XMS
.fault	int3	;FIXME

.div2ceil:
	shr	bx, 1
	rcr	di, 1
	adc	di, 0		; The last entry might not be a multiple of 4
	adc	bx, 0
	ret

.inner	push	bx
	xor	bx, bx
	test	al, 2		; Loop is unrolled -- must be first entry
	je	.inner2

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

entrytoblockall:
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
	cmp	ax, 0FF7h
	ret

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
	

diskwrite0:
	xor	bx, bx
diskwrite:
	mov	cl, 1
	call	diskreadwrite
	jc	.bad
	ret
.bad	mov	dx, msg_replace
	mov	ah, 9
	int	21h
	mov	al, 3	; TODO error normalization
	jmp	exit

diskread0:
	xor	bx, bx
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
	test	[opflags + 1], byte opflag2_bigdisk
	jnz	.fat32
	add	bx, ax		; Check if we know it's big already
	jc	.big
	or	dx, dx
	jnz	.big
	test	[opflags + 1], byte opflag2_bigdisk
	jnz	.big
	lds	bx, [bp - 10]
	mov	cx, [bp - 12]
	mov	dx, [bp - 16]
	call	.i2526
	jnc	.exit
	cmp	ax, 0207h
	jne	.exitc
	mov	ds, [bp - 6]
	or	[opflags + 1], byte opflag2_bigdisk
.big	mov	cx, 0FFFFh
	lea	dx, [bp - 16]
	call	.i2526
	jnc	.exit
	cmp	ax, 0207h
	jne	.exitc
.fat32	mov	si, [bp - 4]
	mov	dl, [cs:disk]
	inc	dl
	mov	ax, 7305h
	int	21h
.exit	mov	ds, [bp - 6]
	mov	es, [bp - 8]
	mov	sp, bp
	pop	bp
	ret
.exitc	stc
	jmp	.exit
.i2526	mov	al, [cs:disk]
	lds	bx, [bp - 10]
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

xmssegsrc:	; ES -> XMS SRC; destroys AX, CX, DX
	mov	ax, es
	mov	cx, 16
	mul	ax
	mov	[xms_xfer_src], word 0
	mov	[xms_xfer_srcoff], ax
	mov	[xms_xfer_srcoff + 2], dx
	ret

xmssegdst:	; ES -> XMS DST; destroys AX, CX, DX
	mov	ax, es
	mov	cx, 16
	mul	ax
	mov	[xms_xfer_dst], word 0
	mov	[xms_xfer_dstoff], ax
	mov	[xms_xfer_dstoff + 2], dx
	ret

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

msg_usage	db	'SSDSCAN pre-alpha unusable', 13, 10
		db	'Usage: SSDSCAN DRIVE: [/F] [/C] [/D]', 13, 10
		db	'/F   Fix errors without prompting', 13, 10
		db	'/C   Check chain length against file length', 13, 10, '$'
		db	'/Z   Recovery directories that have zeroed sectors in the middle', 13, 10, '$'
		db	'     (caution: 0 is normally the directory terminator; can damage FS instead', 13, 10, '$'
		db	'/D   Describe filesystem'
out_newline	db	13, 10, '$'
out_crfile	db	13, 'File $'
msg_nocmem	db	'Insufficient Conventional Memory available to check any disk.', 13, 10, '$'
msg_nocmem2	db	'Insufficient Conventional Memory available to check this disk.', 13, 10, '$'
state_mediaq	db	13, 10
state_media	db	        'Media Descriptor     : $'
state_fat	db	13, 10, 'File Allocation Table: $'
state_dir	db	13, 10, 'Directory Structure  : $'
out_cr		db	13, '$'
out_check	db	251, "  $"
dsc_bps		db	13,     'Bytes per sector           : '
dsc_spc		db	13, 10, 'Sectors per cluster        : '
dsc_spchk	db	13, 10, 'Sectors per chunk          : '
dsc_spfat	db	13, 10, 'Sectors per FAT            : '
dsc_numfats	db	13, 10, 'Number of FATs             : '
dsc_rootent	db	13, 10, 'Number of root dir entries : '
dsc_firstdata	db	13, 10, 'Sector of first cluster    : '
dsc_clusters	db	13, 10, 'Number of clusters         : '
dsc_end:
msg_error0	db	'Read error accessing boot sector.', 13, 10
		db	'This might be recoverable using SSDFIXBT after copying to a new SSD.', 13, 10, '$'
msg_notpow2	db	"Bytes per sector isn't a power of 2", 13, 10, '$'
msg_logfail	db	"Unable to initialize logical sectored FAT", 13, 10, "because it isn't aligned to physical sectors.", 13, 10, '$'
msg_overflow	db	'Overflow computing filesystem offsets', 13, 10, '$'
msg_noboot	db	'Failed to find a valid boot sector', 13, 10, '$'
msg_badmdesc	db	'Encountered a bad sector trying to read media descriptor.', 13, 10
msg_nomdesc	db	"Media Descriptor does not correspond to boot sector.", 13, 10, "Most likely this isn't a FAT filesystem after all.", 13, 10, '$'
query_bootsect	db	"Boot sector damaged, however a backup boot sector was found. Restore it?$"
query_fatdiff	db	"FATs disagree, repair with best guess?$"
query_anomalous	db	"Anomalous record in FAT, repair?$"
query_clustone	db	"Encountered mid-allocation cluster value, repair?$"
query_freealloc	db	"Encountered free block in use, allocate?$"
query_clustout	db	"Encountered out-of-range cluster value, repair?$"
query_xlink	db	"Encountered cross-linked cluster, repair?$"
query_badname	db	"has invalid filename characters, fix?$"
query_resname	db	"is a reserved name, auto rename?$"
query_anomfile	db	"is an anomalous directory entry, delete?$"
query_freefile	db	"refers to a free cluster, delete?$"
query_xlinkfile	db	"refers to a crosslinked cluster, delete?$"
query_dirnot	db	" is marked as a directory but seems to not be, select(F,D)$"
query_fixdotdot	db	" has a broken .. entry, fix?", 13, 10, '$'
query_dirbadsec	db	"Bad sector encountered reading directory, replace?$"
msg_nogoodfat	db	"Correlated bad sectors destroyed FAT.", 13, 10, '$'
msg_rootdirlost	db	"Root directory lost", 13, 10, '$'
msg_toodeep	db	"Directory tree depth exceed 51; cannot ccontinue.", 13, 10, '$'
msg_rootbadsect	db	"Bad sector encountered in root directory", 13, 10, '$'
msg_replacer	db	"New bad sector encountered reading SSD; advise immediate replacement", 13, 10, '$'
msg_replace	db	"Bad sector encountered writing to SSD; advise immediate replacement", 13, 10, '$'
msg_outofspace	db	"Ran out of space repairing disk, free some up.", 13, 10
		db	"The file system should be good enough to move a file off the disk.", 13, 10, '$'

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

bitvectorptrs	resb	8 * 128		; Use bitvector* constants as index offsets

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
clustinbuffer	resb	4
		resb	4

rootdirsects	resb	2
		resb	2
bitbufseg	resb	2
cmempool	resb	2
cmempoollen	resb	2
fattype		resb	1
descriptor	resb	1
preserve_sp	resb	2		; Disk access slaughters these registers but we need them back _immediately_
preserve_bp	resb	2		; These are accessed from cs but we build with cs = ds

xmsaddr		resb	4
fatinbuf	resb	4
fatinbitmask	resb	4
bitmasklen	resb	2
wordsperchunk	resb	2

savedfilename	resb	16		; Saved file name (keep me on bottom)
					; Technically it's only 11 long so I can squeeze later if need be

_endbss:

bitvectorlength	equ	0
bitvectorptr	equ	4
bitvectortype	equ	6

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
opflag2_cbf	equ 8
opflag2_rclust	equ 16
