all: FORMATHD.ZIP SSDFMT.ZIP

LICENSE.TXT: LICENSE
	todos < LICENSE > LICENSE.TXT

FORMATHD.TXT: formathd
	todos < formathd > FORMATHD.TXT

PATCHDOS.COM: patchdos.asm errormsg.asm
	nasm -f bin -o PATCHDOS.COM patchdos.asm

FORMATHD.COM: formathd.asm errormsg.asm
	nasm -f bin -o FORMATHD.COM formathd.asm

FORMATHD.ZIP: LICENSE.TXT FORMATHD.TXT PATCHDOS.COM FORMATHD.COM ISSF.COM
	rm -f FORMATHD.ZIP
	zip -9 FORMATHD.ZIP FORMATHD.TXT LICENSE.TXT PATCHDOS.COM FORMATHD.COM ISSF.COM

ISSF.COM: issf.asm
	nasm -f bin -o ISSF.COM issf.asm

SSDFMT.TXT: ssdfmt
	todos < ssdfmt > SSDFMT.TXT

SSDFMT.COM: ssdfmt.asm
	nasm -f bin -o SSDFMT.COM ssdfmt.asm

SSDFIXBT.COM: ssdfixbt.asm
	nasm -f bin -o SSDFIXBT.COM ssdfixbt.asm

POSTNTFS.EXE: postntfs.asm
	nasm -f bin -o POSTNTFS.EXE postntfs.asm

SSDFMT.ZIP: LICENSE.TXT SSDFMT.TXT SSDFMT.COM SSDFIXBT.COM ISSF.COM POSTNTFS.EXE
	rm -f SSDFMT.ZMP
	zip -9 SSDFMT.ZIP SSDFMT.TXT LICENSE.TXT SSDFMT.COM SSDFIXBT.COM ISSF.COM POSTNTFS.EXE

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
