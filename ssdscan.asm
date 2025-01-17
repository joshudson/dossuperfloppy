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
;  set flag on crosslink, don't repair yet
; at the end of this loop we can prove there are no 01 remaining as they would have been repaired

;Directory Structure:
; recursively read in directory structure from root
;  perform basic validation of directory entries; skip any LFS entries and volume label
;  if a file has no clusters its length must be zero
;  mark starting cluster as reached
;  if length flag check set, or xlink flag is set, traverse FAT, checking if chain ends too soon or too late
;    if too soon, ask extend or free
;    if too late, must truncate
;    as we traverse each cluster, change its bitmap entry to 01; if it already is 01 repair the xlink
;      options are duplicate, truncate this file, truncate other file, get other file name
;      get other file name starts a traversal looking for the other file's identity

;Recovering lost files (we know by counters if we need to run this or not)
; traverse bitmap array, recover any lost files; they have value 10 in the bitmap array
; FAT16: if root dir is full, make a directory LOST.FND to recover the file into

; Copyright (C) Joshua Hudson, 2024-25

; 256 word stacksize for DOS + 128 words for us
stackbottom	equ	(_endbss - _bss + _end - _start + 100h + 768)

; Memory map:
; cs:0        PCP
; cs:_start   program entry point
; cs:_bss     uninitialized data
; cs:_endbss  top of stack
; buf1:0      first FAT buffer
; buf2:0      second FAT buffer and cluster data buffer
; buf3:0      third FAT buffer and dirwalk state buffer
; buf4:0      XMS bitmap pool buffer, if it exists
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
	mov	ax, 4C04h
	int	21h
.mem	mov	sp, stackbottom	
	sub	ax, (stackbottom) / 16
	mov	[cmem], ax
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
.o1	or	bl, opflag_f
	jmp	.b
.o2	or	bl, opflag_c
	jmp	.b
.o3	or	bl, opflag_d
	jmp	.b
.arg	mov	dx, msg_usage
	mov	ah, 9
	int	21h
	mov	ax, 4C01h	; FIXME normalize error codes
	int	21h
	db	0xCC
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
	xor	bx, bx
	xor	dx, dx
	xor	cx, cx
	call	diskread
	jnc	.read0
.error0	xor	bx, bx
	xor	dx, dx
	xor	cx, cx
	call	diskread
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
	xor	bx, bx
	xor	dx, dx
	xor	cx, cx
	xor	ax, ax
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
	xor	bx, bx
	xor	dx, dx
	xor	cx, cx
	mov	es, [buf1seg]
	cmp	ax, 0
	je	.rda			; Already read in sector 0 (CF clear if this jump is taken)
	call	diskread
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
	;jmp	.bpb
.notbpb	mov	ax, dx
	inc	ax
	cmp	ax, 97			; Worst case for FAT32 backup boot sector
	jl	.mloop
	mov	dx, msg_noboot
	jmp	stage_media_descriptor.errorx
.bpb	xor	cx, cx			; Initialize FAT variables
	mov	[fatinfosect], cx	; We don't know if there is one yet
	mov	[rootclust], cx
	mov	[rootclust + 2], cx
	mov	[rootdirsects], cx
	mov	[rootdirsects + 2], cx
	push	dx			; Save boot sector address for .gmdesc

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
	mov	[rootdirsects + 2], ax
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
	mov	[rootdirsects + 4], dx

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
	add	ax, 2
	adc	bx, 0
	mov	[totalclust + 2], bx
	mov	[totalclust], ax
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
.spcn	shl	bx, 1			; So the idea here is if we have an actually aligned
	cmp	bx, 32			; filesystem on an SSD it will read and write in SSD-sized
	je	.spc2			; sectors even when the DOS sector size is smaller.
	cmp	bx, [sectsperclust]
	ja	.spc0
	cmp	[sectsperfat + 2], word 0
	jne	.spc1
	cmp	bx, [sectsperfat]
	ja	.spc0
.spc1	cmp	bx, [rootdirsects]
	jbe	.spcn
	ja	.spc0
.spc0	shr	bx, 1
.spc2	mov	[sectsperchunk], bx

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
	cmp	[fattype], byte 16
	je	.is16
	cmp	[fattype], byte 32
	je	.is32

	; FAT12: pool size is entire fat
	mov	ax, [sectsperfat]
	mul	word [bytespersector]	; Cannot overflow
	mov	cl, 4
	shr	ax, cl			; bytespersector can't be smaller than 16
	mov	bx, 64
	jmp	.initall

.is16	mov	ch, 8			; FAT16: every 8 bytes of FAT is 1 byte of bitmap
	jmp	.in12
.is32	mov	ch, 16			; FAT32: every 16 bytes of FAT is 1 byte of bitmap
.in12	mov	ax, [sectsperchunk]
	mul	word [bytespersector]	; Cannot overflow
	mov	cl, 4
	shr	ax, cl
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
	cmp	ax, 32
	jb	.left2
	add	cx, ax		; Typical case for sizing BUF3
	jmp	.have3
.left2	add	cx, 32		; BUF3 must be at least 512 bytes for recursive dirwalk
.have3	mov	[buf4seg], cx
	; Check if the entire bitmap fits in cmem or not
	mov	ax, [totalclust]
	mov	dx, [totalclust + 2]
	mov	cx, 8
.dec2	shr	dx, 1
	rcr	ax, 1
	adc	ax, 0
	loop	.dec2	; When loop terminates, DX:AX is the entire size required in paragraphs
	or	dx, dx
	jnz	.cbigr
	mov	dx, ds
	add	dx, [cmem]
	sub	dx, [buf4seg]
	jb	.cbigr
	mov	dx, [buf4seg]
	mov	[cmempool], dx	; Pool fits in conventional memory, no block needed
	mov	[cmempoollen], ax
	jmp	.hmem
.cbigr	mov	cx, [buf4seg]
	add	cx, bx		; BX preserved the entire time: size of buf4 required
	mov	[cmempool], cx
	mov	bx, ds
	add	bx, [cmem]
	sub	bx, cx
	jae	.hcmem
	mov	dx, msg_nocmem2
	mov	ah, 9
	int	21h
	mov	ax, 4C04h	; TODO normalize exit codes
	int	21h
.hcmem:
	db	0x33		; TODO allocate XMS memory

.hmem:
	; Might as well read into all buffers now we will need the reads anyway
	xor	dx, dx
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
	xor	bx, bx
	mov	es, [buf1seg + si]
	call	diskread
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
	mov	cx, state_media
	call	queryfixyn
	pop	dx
	cmp	al, 'y'
	jne	.mddone
	xchg	ax, dx
	xor	dx, dx
	xor	bx, bx
	mov	es, [buf1seg]
	call	diskread
	jc	.nbvec
	xor	ax, ax
	xor	dx, dx
	xor	bx, bx
	call	diskwrite
	xor	dx, dx			; Repair boot sector clobbered buf1; reload
	mov	ax, [reservedsects]
	xor	bx, bx
	call	diskread
	jnc	.mddone
	or	bp, 1
.mddone	call	checkmark

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
; returns al = 'y' or 'n'
queryfixyn:
	push	dx
	mov	dx, out_cr
	mov	ah, 9
	int	21h
	pop	dx
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
	mov	dx, out_cr
	mov	ah, 9
	int	21h
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

diskread:
	xor	cx, cx

;DX:AX = sector address
;ES:BX = read address
;CX = read/write flag: 0 = read, 1 = write; when writing bits 14-13 are
;	00 - unknown
;	01 - FAT data
;	10 - root directory data
;	11 - file data
;Always reads one chunk size, which may or may not be a whole cluster
;The zeroth sector can't be cached for numerous reasons; it's always accessed directly.
;This does clobber all registers other than segment registers despite appearing not to
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
	;jnz	.fat32
	add	bx, ax		; Check if we know it's big already
	;jc	.big
	or	dx, dx
	;jnz	.big
	test	[opflags + 1], byte opflag2_bigdisk
	;jnz	.big
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
	;jnz	.write
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


msg_usage	db	'Usage: SSDSCAN DRIVE: [/F] [/C] [/D]', 13, 10
		db	'/F   Fix errors without prompting', 13, 10
		db	'/C   Check chain length against file length', 13, 10, '$'
		db	'/D   Describe filesystem', 13, 10, '$'
msg_nocmem	db	'Insufficient Conventional Memory available to check any disk.', 13, 10, '$'
msg_nocmem2	db	'Insufficient Conventional Memory available to check this disk.', 13, 10, '$'
state_media	db	'Media Descriptor     : $'
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
query_bootsect	db	"Boot sector damaged, however a backup boot sector was found. Restore it?", 13, 10, '$'
msg_replacer	db	"New bad sector encountered reading SSD; advise immediate replacement", 13, 10, '$'
msg_replace	db	"Bad sector encountered writing to SSD; advise immediate replacement", 13, 10, '$'

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

bitvectorptrs	resb	8 * 128

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

rootdirsects	resb	4
bitbufseg	resb	2
cmempool	resb	2
cmempoollen	resb	2
fattype		resb	1
descriptor	resb	1
preserve_sp	resb	2		; Disk access slaughters these registers but we need them back _immediately_
preserve_bp	resb	2		; These are accessed from cs but we build with cs = ds

_endbss:

bitvectoroffset	equ	0
bitvectorptr	equ	4
bitvectortype	equ	6

opflag_f	equ 1
opflag_c	equ 2
opflag_d	equ 4
opflag2_bigdisk	equ 1
opflag2_7305	equ 2
opflag2_ebpb	equ 4
