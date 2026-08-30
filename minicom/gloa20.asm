; GLOA20: for testing FIXCALL5

_start:
	mov	ax, 4300h
	int	2Fh
	cmp	al, 80h
	jne	.noxms
	mov	ax, 4310h
	int	2Fh
	mov	[100h], bx
	mov	[102h], es
	mov	ah, 3
	call	far [100h]
	cmp	ax, 1
	jne	.oops
	ret
.noxms	mov	dx, noxms
.exit	mov	ah, 9
	int	21h
	ret
.oops	cmp	bl, 80h
	mov	dx, nimp
	je	.exit
	mov	dx, error
	jmp	.exit
noxms	db	"No XMS driver", 13, 10, '$'
nimp	db	"Function not implemented", 13, 10, '$'
error	db	"A20 error", 13, 10, '$'
