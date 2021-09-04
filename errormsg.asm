errorcodes:
	dw	.e0
	dw	.e1
	dw	.e2
	dw	.e3
	dw	.e4
	dw	.e5
	dw	.e6
	dw	.e7
	dw	.e8
	dw	.e9
	dw	.e10
	dw	.e11
	dw	.e12
	dw	.e13
	dw	.e14
	dw	.e15
	dw	.e16
	dw	.e17
	dw	.e18
.e0	db	'Error 0', 13, 10, '$'
.e1	db	'Invalid Function', 13, 10, '$'
.e2	db	'File not found', 13, 10, '$'
.e3	db	'Path not found', 13, 10, '$'
.e4	db	'Too many open files', 13, 10, '$'
.e5	db	'Access Denied', 13, 10, '$'
.e6	db	'Invalid Handle', 13, 10, '$'
.e7	db	'Memory ctl blocks destroyed', 13, 10, '$'
.e8	db	'Out of memory', 13, 10, '$'
.e9	db	'Invalid memory block address', 13, 10, '$'
.e10	db	'Invalid environment', 13, 10, '$'
.e11	db	'Invalid format', 13, 10, '$'
.e12	db	'Invalid access code', 13, 10, '$'
.e13	db	'Invalid data', 13, 10, '$'
.e14	db	'Error 14', 13, 10, '$'
.e15	db	'Invalid drive', 13, 10, '$'
.e16	db	"Can't remove current dir", 13, 10, '$'
.e17	db	'Not same device', 13, 10, '$'
.e18	db	'No more matching files', 13, 10, '$'
