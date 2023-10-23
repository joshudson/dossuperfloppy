ORG 0

CPU 8086

	db	'MZ'
	dw	48	; Number of bytes in the last 512 byte block
	dw	1	; Number of 512 byte blocks to load
	dw	0	; Number of relocations
	dw	4	; Size of MZ header in paragraphs
	dw	0	; Number of paragraphs of uninitialized data
	dw	32	; Number of paragraphs max stack + heap
	dw	0	; Relative initial SS
	dw	0280h	; Initial SP
	dw	0	; Checksum (not used)
	dw	0	; Relative initial IP
	dw	0	; Relative initial CS
	dw	0	; Offset of first relocation
	dw	0	; Overlay number
	dd	0, 0	; Reserved
	dw	0	; OEM ID
	dw	0	; OEM INFO
	dd	0, 0, 0, 0, 0
	dd	pehdr
align	16, db 0

_start16:
	mov	dx, reqnt - _start16 + 0100h
	mov	ah, 9
	int	21h
	mov	ax, 4C03h
	int	21h
reqnt	db	'This program requires Windows NT.', 13, 10, '$'

fatparttypes	db	1, 4, 6, 0Bh, 0Ch, 0Eh
fatparttypeslen	equ	$ - fatparttypes
align	4, db 0

; Application messages stored here (otherwise wasted space)
completemsg	db	'OK', 13, 10
completemsglen	equ	$ - completemsg
align	4, db 0
errormsg	db	'Error!', 13, 10
errormsglen	equ	$ - errormsg
align	4, db 0
nothingmsg	db	'Nothing to do', 13, 10
nothingmsglen	equ	$ - nothingmsg
align	4, db 0

align 32, db 0

CPU 386
BITS	32

imgbase		equ	400000h
startcode	equ	1000h
startimport	equ	2000h
imgsize		equ	startimport + 4096; ((importsectionend - import + 4095) & ~4096)

pehdr:
	db	'PE', 0, 0
	dw	014Ch	; 386
	dw	2	; Number of sections
	dd	0	; Would be build date but reproducible builds says no
	dd	0, 0	; No symbols for you!
	dw	optend - optstart	; Size of optional header
	dw	323h	; Characteristics
optstart:
	dw	010Bh	; PE32 image
	dw	0	; Linker version (what linker?)
	dd	_end32 - _start32	; Size of code
	dd	importsectionend - import	; Size of data (import table)
	dd	0	; Size of uninitialized data (none)
	dd	startcode	; Address of entry point
	dd	startcode	; base of code
	dd	startimport	; Base of data
	dd	imgbase	; Image base
	dd	4096	; Section alignment
	dd	512	; File alginment
	dw	1	; Major Operating System Version
	dw	0	; Minor Operating System Version
	dw	1	; Image major version
	dw	0	; Image minor version
	dw	3	; Major Subsystem Version
	dw	10	; Minor Subsystem Version
	dd	0	; Win32 Version value
	dd	imgsize	; Image size
	dd	endhdr	; Size of headers
	dd	0	; PE Checksum
	dw	3	; Windows Console
	dw	8F00h	; DLL Characteristics
	dd	65536	; Size of stack reserve
	dd	65536	; Size of stack commit
	dd	0	; Size of heap reserve
	dd	0	; Size of heap commit
	dd	0	; Loader flags
	dd	16	; number of RVA and sizes

	dd	0	; Pointer to export directory
	dd	0	; Size of export directory
	dd	startimport	; Pointer to import directory
	dd	importend - import	; Size of import directory
	dd	0, 0	; Resource directory
	dd	0, 0	; Exception Directory
	dd	0, 0	; Security Directory
	dd	0, 0	; Base relocation table
	dd	0, 0	; Debug
	dd	0, 0	; Arch-specific
	dd	0, 0	; RVA of GP
	dd	0, 0	; TLS
	dd	0, 0	; Load Config
	dd	0, 0	; Bound Import
	dd	startimport + importaddressestranslated - import	; Pointer to import addresses table
	dd	importaddressestranslatedend - importaddressestranslated ; Size of import addresses table
	dd	0, 0	; Delay Load
	dd	0, 0	; .NET Runtime
	dd	0, 0	; Reserved
optend:

	db	'.text', 0, 0, 0	; .text section
	dd	(_end32 - _start32 + 511) & ~511	; Length of section
	dd	startcode		; Load address (offset)
	dd	(_end32 - _start32 + 511) & ~511	; Size of section in image
	dd	_start32		; Pointer to data
	dd	0			; Pointer to relocations
	dd	0			; Pointer to line numbers (not used)
	dw	0			; Number of relocations
	dw	0			; Number of line numbers
	dd	060000020h		; Characteristics

	db	'.idata', 0, 0		; .idata section
	dd	importsectionend - import
	dd	startimport
	dd	importsectionend - import
	dd	import
	dd	0, 0, 0
	dd	0C0000040h
endhdr:

align	512, db 0

_start32:
	sub	esp, 1024
	mov	ebp, esp
	push	eax		; Pointer argument to SetFilePointer
	call	[getcommandline + imgbase + startimport - import]
	;Parse command line
	xchg	eax, esi
	call	extractargument
	call	extractargument
	or	ebx, ebx
	jnz	.go

	; Wrong number of arguments: display usage and exit
	xor	esi, esi
	inc	esi
	push	usagelen
	push	usage + imgbase + startimport - import
	jmp	short	.exitv

.go	xor	eax, eax
	not	eax
	push	eax
	push	0A0000000h
	push	3
	not	eax
	push	eax
	push	7
	push	0C0000000h
	push	ebx
	call	[createfile + imgbase + startimport - import]
	inc	eax
	jz	short	.errv
	dec	eax
	xchg	eax, ebx

	push	512
	push	ebp
	mov	edi, [readfile + imgbase + startimport - import]
	call	readwrite
.errv	jz	.error

	mov	edi, ebp
	cmp	[edi], byte 0EBh
	je	.found
	inc	edi
	inc	edi
	cmp	[edi], byte 0EBh
	je	.found
.nothn	push	nothingmsglen
	push	nothingmsg + imgbase
	xor	esi, esi
.exitv	jmp	.exit

.found	cmp	[ebp + 015h], byte 0F8h
	jne	.nothn		; Already cleared

	; Clean MBR now; save only if needed
	inc	edi
	movzx	ecx, byte [edi]
	mov	al, 0
	inc	edi
	rep	stosb

	; If there's a FAT partition, don't clean
	lea	esi, [ebp + 512 - 2 - 64 + 4]
	mov	cl, 4
	xor	edx, edx
.mbrlp	mov	al, [esi]
	mov	edi, imgbase + fatparttypes
	push	ecx
	mov	cl, fatparttypeslen
	repne	scasb
	pop	ecx
	je	short .nothn
	cmp	al, 0B0h
	jne	.bs1
	mov	edx, esi
.bs1	cmp	al, 0B5h
	jne	.bs2
	mov	edx, esi
.bs2	add	esi, 16
	dec	cl
	jnz	.mbrlp

	; Seek to SSD Check Partition
	or	edx, edx
	jz	short .nobk
	mov	eax, dword [edx + 8 - 4]
	mov	edi, 512
	mul	edi
	mov	[esp], edx
	mov	edx, esp
	xor	ecx, ecx
	push	ecx
	push	edx
	push	eax
	push	ebx
	call	[setfilepointer + imgbase + startimport - import]
	;Cannot fail

	lea	esi, [ebp + edi]
	push	edi	; Still 512
	push	esi
	mov	edi, [readfile + imgbase + startimport - import]
	call	readwrite
	jz	.error
	cmp	[esi], byte 0EBh
	jne	.nobk
	cmp	[esi + 2], word 'SS'
	jne	.nobk
	cmp	[esi + 4], dword 'D->H'
	jne	.nobk

	;Seek to backup MBR
	mov	edi, 512
	movzx	eax, byte [esi + 8]
	dec	eax
	js	.error
	mul	edi
	mov	[esp], edx
	mov	edx, esp
	xor	ecx, ecx
	inc	ecx
	push	ecx
	push	edx
	push	eax
	push	ebx
	call	[setfilepointer + imgbase + startimport - import]
	; Cannot fail

	;Write MBR to backup MBR
	push	edi	; Still 512
	push	ebp
	mov	edi, [writefile + imgbase + startimport - import]
	call	readwrite
	jz	.error

.nobk:	; Write MBR back
	xor	edx, edx
	push	edx
	push	edx
	push	edx
	push	ebx
	call	[setfilepointer + imgbase + startimport - import]
	push	512
	push	ebp
	mov	edi, [writefile + imgbase + startimport - import]
	call	readwrite
	jz	.error

	push	completemsglen
	push	completemsg + imgbase
	xor	esi, esi
	jmp	.exit
.error	push	errormsglen		; Something went wrong, have a nice day
	push	errormsg + imgbase
	xor	esi, esi
	inc	esi
.exit	mov	edi, [writefile + imgbase + startimport - import]
	push	-12
	call	[getstdhandle + imgbase + startimport - import]
	xchg	eax, ebx
	call	readwrite
	push	esi
	call	[exitprocess + imgbase + startimport - import]
	db	0CCh

extractargument:
	xor	ebx, ebx
	or	esi, esi
	jz	.rtn
	mov	edi, esi
	mov	ebx, esi
.nquot	lodsw
	cwde
	or	eax, eax
	jz	.end
	cmp	eax, ' '
	je	.enda
	cmp	eax, 9
	je	.enda
	cmp	eax, '"'
	je	.quot
	stosw
	jmp	.nquot
.enda	lodsw
	cwde
	or	eax, eax
	jz	.end
	cmp	eax, ' '
	je	.enda
	cmp	eax, 9
	je	.enda
	dec	esi
	dec	esi
	mov	[edi], word 0
.rtn	ret
.quot	lodsw
	cwde
	or	eax, eax
	je	.end
	cmp	eax, '"'
	je	.nquot
	stosw
	jmp	.quot
.end	stosw
	xor	esi, esi
	ret

readwrite:
	push	eax
	mov	eax, esp
	xor	ecx, ecx
	push	ecx
	push	eax
	push	dword [eax + 12]
	push	dword [eax + 8]
	push	ebx
	call	edi
	pop	ecx
	or	eax, eax
	jz	.rtn
	or	ecx, ecx
	jz	.rtn
	add	[esp + 4], ecx
	sub	[esp + 8], ecx
	jnz	readwrite
	or	edi, edi		; Clear ZF
.rtn	ret	8
_end32:

align	512, db 0CCh

import:
	dd	importaddresses + startimport - import
	dd	0
	dd	0
	dd	kernel32 + startimport - import
	dd	importaddressestranslated + startimport - import
	dd	0, 0, 0, 0, 0
importend:

importaddresses:
	dd	import_exitprocess + startimport - import
	dd	import_getcommandline + startimport - import
	dd	import_createfile + startimport - import
	dd	import_setfilepointer + startimport - import
	dd	import_readfile + startimport - import
	dd	import_writefile + startimport - import
	dd	import_getstdhandle + startimport - import
	dd	0

importaddressestranslated:
exitprocess:		dd	import_exitprocess + startimport - import
getcommandline:		dd	import_getcommandline + startimport - import
createfile:		dd	import_createfile + startimport - import
setfilepointer:		dd	import_setfilepointer + startimport - import
readfile:		dd	import_readfile + startimport - import
writefile:		dd	import_writefile + startimport - import
getstdhandle:		dd	import_getstdhandle + startimport - import
			dd	0
importaddressestranslatedend:

kernel32:	db	'KERNEL32.DLL', 0
align 2, db 0
import_exitprocess:	dw	0
	db	'ExitProcess', 0
align 2, db 0
import_getcommandline:	dw	0
	db	'GetCommandLineW', 0
align 2, db 0
import_createfile:	dw	0
	db	'CreateFileW', 0
align 2, db 0
import_setfilepointer:	dw	0
	db	'SetFilePointer', 0
align 2, db 0
import_readfile:	dw	0
	db	'ReadFile', 0
align 2, db 0
import_writefile:	dw	0
	db	'WriteFile', 0
align 2, db 0
import_getstdhandle:	dw	0
	db	'GetStdHandle', 0
align 2, db 0

usage		db	'Removes the overlay FAT BPB from the MBR and backup MBR created by SSDFMT.', 13, 10
		db	'Usage: POSTNTFS.EXE \\.\PHYSYCALDRIVEn', 13, 10
		db	'where n is the drive number, usually 0.', 13, 10
usagelen	equ	$ - usage

importsectionend:
