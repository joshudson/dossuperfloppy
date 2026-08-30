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

SSDSCAN.TXT: ssdscan
	todos < ssdscan > SSDSCAN.TXT

SSDSCAN.COM: ssdscan.asm
	nasm -f bin -o SSDSCAN.COM ssdscan.asm

RENAME1.COM: rename1.asm
	nasm -f bin -o RENAME1.COM rename1.asm

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

NEWIMAGE.COM: newimage.asm
	nasm -f bin -o NEWIMAGE.COM newimage.asm

BIOSRAM.COM: biosram.asm
	nasm -f bin -o BIOSRAM.COM biosram.asm

CPU.COM: cpu.asm
	nasm -f bin -o CPU.COM cpu.asm

REMAP25.COM: remap25.asm
	nasm -f bin -o REMAP25.COM remap25.asm

RBPB.COM: rbpb.asm
	nasm -f bin -o RBPB.COM rbpb.asm

minicom/CMP.COM: minicom/cmp.asm
	nasm -f bin -o minicom/CMP.COM minicom/cmp.asm

minicom/ERRORLVL.COM: minicom/errorlvl.asm
	nasm -f bin -o minicom/ERRORLVL.COM minicom/errorlvl.asm

minicom/MINICOM.COM: minicom/minicom.asm
	nasm -f bin -o minicom/MINICOM.COM minicom/minicom.asm

minicom/DIR.COM: minicom/dir.asm
	nasm -f bin -o minicom/DIR.COM minicom/dir.asm

minicom/FIXCALL5.COM: minicom/fixcall5.asm
	nasm -f bin -o minicom/FIXCALL5.COM minicom/fixcall5.asm

minicom/GLOA20.COM: minicom/gloa20.asm
	nasm -f bin -o minicom/GLOA20.COM minicom/gloa20.asm

minicom/LOADFIX2.COM: minicom/loadfix2.asm
	nasm -f bin -o minicom/LOADFIX2.COM minicom/loadfix2.asm

minicom/MKDIR.COM: minicom/mkdir.asm
	nasm -f bin -o minicom/MKDIR.COM minicom/mkdir.asm

minicom/REBOOT.COM: minicom/reboot.asm
	nasm -f bin -o minicom/REBOOT.COM minicom/reboot.asm

clean:
	rm -f LICENSE.TXT FORMATHD.TXT PATCHDOS.COM FORMATHD.COM ISSF.COM FORMATHD.ZIP
	rm -f SSDFMT.TXT SSDFMT.COM SSDFIXBT.COM POSTNTFS.EXE SSDFMT.ZIP RENAME1.COM REMAP25.COM
	rm -f CPU.COM BIOSRAM.COM NEWIMAGE.COM FDEMUINF.COM hdgeometry
	rm -f minicom/CMP.COM minicom/ERRORLVL.COM minicom/MINICOM.COM minicom/LOADFIX2.COM minocom/DIR.COM
	rm -f minicom/FIXCALL5.COM minicom/GLOA20.COM minicom/MKDIR.COM minicom/REBOOT.COM minicom/RM.COM

hostclean:
	rm -f host/sparsify32

host/sparsify32: host/sparsify32.asm host/syscalls32.inc
	nasm -f bin -o host/sparsify32 -I host host/sparsify32.asm
	chmod +x host/sparsify32
