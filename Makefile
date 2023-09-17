all: FORMATHD.ZIP

LICENSE.TXT: LICENSE
	todos < LICENSE > LICENSE.TXT

README.TXT: README
	todos < README > README.TXT

PATCHDOS.COM: patchdos.asm errormsg.asm
	nasm -f bin -o PATCHDOS.COM patchdos.asm

FORMATHD.COM: formathd.asm errormsg.asm
	nasm -f bin -o FORMATHD.COM formathd.asm

FORMATHD.ZIP: LICENSE.TXT README.TXT PATCHDOS.COM FORMATHD.COM
	rm -f FORMATHD.ZIP
	zip -9 FORMATHD.ZIP README.TXT LICENSE.TXT PATCHDOS.COM FORMATHD.COM

ISSF.COM: issf.asm
	nasm -f bin -o ISSF.COM issf.asm

SSDFMT.COM: ssdfmt.asm
	nasm -f bin -o SSDFMT.COM ssdfmt.asm

SSDFIXBT.COM: ssdfixbt.asm
	nasm -f bin -o SSDFIXBT.COM ssdfixbt.asm

hdgeometry: hdgeometry.asm
	nasm -f bin -o hdgeometry hdgeometry.asm

GEOMETRY.COM: geometry.asm
	nasm -f bin -o GEOMETRY.COM geometry.asm

CMP.COM: cmp.asm
	nasm -f bin -o CMP.COM cmp.asm
	
NEWIMAGE.COM: newimage.asm
	nasm -f bin -o NEWIMAGE.COM newimage.asm

ERRORLVL.COM: errorlvl.asm
	nasm -f bin -o ERRORLVL.COM errorlvl.asm
