all: LICENSE.TXT README.TXT PATCHDOS.COM FORMATHD.COM

LICENSE.TXT: LICENSE
	todos < LICENSE > LICENSE.TXT

README.TXT: README
	todos < README > README.TXT

PATCHDOS.COM: patchdos.asm errormsg.asm
	nasm -f bin -o PATCHDOS.COM patchdos.asm

FORMATHD.COM: formathd.asm errormsg.asm
	nasm -f bin -o FORMATHD.COM formathd.asm
