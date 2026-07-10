#!/usr/bin/tcc -run

#include <stdio.h>
#include <string.h>

typedef unsigned char byte;
typedef unsigned short word;

int main(int argc, char **argv)
{
	byte fbuffer[512];
	byte pracbuf[512];
	byte type = 0;
	byte captive1 = 0;
	byte captive2 = 0;
	byte mediadescriptor = 0;
	byte sectors = 0;
	byte heads = 0;
	byte tracks = 0;
	word totalsectors = 0;
	word bpblen;
	
	if (argc > 1 && (!strcmp(argv[1], "-ro") || !strcmp(argv[1], "-readonly"))) {
		type |= 0x80;
		argc -= 1;
		argv += 1;
	}
	if (argc != 4) {
		fprintf(stderr, "Usage: ssdhostpack [-readonly] ssdhost.bin floppy.img hdstarter.img\n");
		return 2;
	}
	FILE *bin = fopen(argv[1], "rb");
	if (!bin) {
		fprintf(stderr, "Can't open %s\n", argv[1]);
		return 1;
	}
	FILE *floppy = fopen(argv[2], "rb");
	if (!floppy) {
		fprintf(stderr, "Can't open %s\n", argv[2]);
		return 1;
	}
	if (fread(pracbuf, 512, 1, bin) != 1) {
		fprintf(stderr, "Can't read %s\n", argv[1]);
		return 1;
	}
	if (fread(fbuffer, 512, 1, floppy) != 1) {
		fprintf(stderr, "Can't read %s\n", argv[2]);
		return 1;
	}
	mediadescriptor = fbuffer[0x15];
	if (mediadescriptor < 0xE0) {
		fprintf(stderr, "Obviously invalid media descriptor\n");
		return 3;
	}
	if (mediadescriptor == 0xE5) {
		fprintf(stderr, "Media descriptor is for 128 byte sectors\n");
		return 3;
	}
	totalsectors = fbuffer[0x13] | ((word)fbuffer[0x14] << 8);
	if (fbuffer[0] == 0x69) fbuffer[0] = 0xEB; /* broken 8086 JMP--yes it booted on the original */
	if (fbuffer[0] == 0xE9)
		bpblen = fbuffer[1] + ((word)fbuffer[2] << 1) + 3;
	else if (fbuffer[0] == 0xEB)
		bpblen = fbuffer[1] + 2;
	else
		fprintf(stderr, "Initial JMP instruction missing; cannot analyze BPB");
	if (bpblen >= 0x2B) bpblen = 0x2B;
	if (fbuffer[0xB] != 0 && fbuffer[0xC] != 2) {
		/* floppy disks don't use logical bytes per sector */
		fprintf(stderr, "Bytes per sector is not 512\n");
		return 3;
	}
	if (bpblen >= 0x1C) {
		sectors = fbuffer[0x18];
		heads = fbuffer[0x1A];
		tracks = totalsectors / (sectors * (word)heads);
		if (sectors * (word)heads * tracks != totalsectors) {
			fprintf(stderr, "Floppy disk image is not rectangular\n");
			return 3;
		}
		switch (mediadescriptor) {
			case 0xED:
			case 0xF4: /* just says double density, Altos MS-DOS 2.01 */
			case 0xFA:
			case 0xFB: /* guess but more likely to work everywhere */
			case 0xFC:
			case 0xFD:
			case 0xFE:
			case 0xFF:
				type |= 1;
				break;
			case 0xF0:
			case 0xF8:
				/* designated for use geometry; need decision ladder */
				if (tracks <= 40)
					if (sectors <= 9)
						type |= 1; /* 5.25 double density */
					else if (sectors <= 15)
						type |= 2; /* 5.25 high density */
					else
						type |= 4; /* weirdo formatting: hardware must be 3.5 high */
				else if (sectors <= 9)
					type |= 3; /* 3.5 double density */
				else if (sectors <= 15)
					type |= 2; /* 5.25 high density */
				else if (sectors < 21)
					type |= 4; /* 3.5 high density */
				else
					type |= 5; /* 3.5 extended density */
				break;
			case 0xF9:
				if (sectors <= 9)
					type |= 3;
				else if (sectors <= 15)
					type |= 3;
				else
					type |= 4;
				break;
			/* all others; media type is nonsenical but disk is sane, type 0 it is */
		}
	} else {
		switch (mediadescriptor) {
			case 0xED:
				type |= 1;
				sectors = 9;
				heads = 2;
				tracks = 80;
				break;
			case 0xF0:
				/* in theory this could be 36 sectors but all such FAT need the geo filled */
				type |= 4;
				sectors = 18;
				heads = 2;
				tracks = 80;
				break;
			case 0xF4:
				fprintf(stderr, "Media descriptor F4 doesn't uniquely specify geometry\n");
				return 3;
			case 0xF5:
				/* type 0 */
				sectors = 12;
				heads = 4;
				/* must compute tracks */
				tracks = totalsectors / (sectors * (word)heads);
				if (sectors * (word)heads * tracks != totalsectors) {
					fprintf(stderr, "Floppy disk image is not rectangular\n");
					return 3;
				}
				break;
			case 0xF8:
				type |= 3;
				sectors = 9;
				heads = 1;
				tracks = 80;
				break;
			case 0xF9:
				/* 3.5 9,80,2 (type 3) */
				/* 3.5 18,80,2 (type 4) */
				/* 5.25 15,80,2 (type 2) */
				switch (totalsectors) {
					case 9*80*2:
						type |= 3;
						sectors = 9;
						heads = 2;
						tracks = 80;
						break;
					case 18*80*2:
						type |= 4;
						sectors = 18;
						heads = 2;
						tracks = 80;
						break;
					case 15*80*2:
						type |= 2;
						sectors = 15;
						heads = 2;
						tracks = 80;
						break;
					default:
						fprintf(stderr, "Nonsensical size for media type F9\n");
						return 3;
				}
			case 0xFA:
				type |= 1; /* guessing */
				sectors = 8;
				heads = 1;
				tracks = 80;
				break;
			case 0xFB:
				type |= 1; /* guessing */
				sectors = 8;
				heads = 2;
				tracks = 80;
				break;
			case 0xFC:
				type |= 1;
				sectors = 9;
				heads = 1;
				tracks = 40;
				break;
			case 0xFD:
				type |= 1;
				sectors = 9;
				heads = 2;
				tracks = 40;
				break;
			case 0xFE:
				type |= 1;
				sectors = 8;
				heads = 1;
				tracks = 40;
			case 0xFF:
				type |= 1;
				sectors = 8;
				heads = 2;
				tracks = 40;
				break;
			defaut:
				fprintf(stderr, "Unrecognized media descriptor %x\n", mediadescriptor);
				return 3;
		}
		if (sectors * (word)heads * tracks != totalsectors) {
			fprintf(stderr, "Floppy disk media descriptor does not match total sectors\n");
			return 3;
		}
	}
	FILE *out = fopen(argv[3], "wb");
	if (!out) {
		fprintf(stderr, "Can't open %s for write\n", argv[3]);
		return 3;
	}
	/* make run */
	for (word sectorsleft = 5; sectorsleft --> 0;) {
		if (fwrite(pracbuf, 512, 1, out) != 1) {
			fprintf(stderr, "Can't write %s\n", argv[3]);
			return 1;
		}
		if (sectorsleft) {
			if (fread(pracbuf, 512, 1, bin) != 1) {
				fprintf(stderr, "Can't read %s\n", argv[1]);
				return 1;
			}
		}
		if (sectorsleft == 4) {
			word reserved = fbuffer[0x0E] + ((word)fbuffer[0x0F] << 8) + 4;
			word len = totalsectors + 4;
			pracbuf[3] = type;
			pracbuf[4] = sectors;
			pracbuf[6] = heads;
			pracbuf[8] = tracks;
			if (bpblen > pracbuf[1] + 3) bpblen = pracbuf[1] + 3;
			memcpy(pracbuf + 11, fbuffer + 11, bpblen - 11);
			pracbuf[0x0E] = reserved & 0xFF;
			pracbuf[0x0F] = (reserved >> 8);
			pracbuf[0x13] = len & 0xFF;
			pracbuf[0x14] = (len >> 8);
			pracbuf[0x18] = 0;
			pracbuf[0x1A] = 0;
			pracbuf[0x20] = len & 0xFF;
			pracbuf[0x21] = (len >> 8);
			pracbuf[0x24] = 0x80;
			pracbuf[0x26] = 0x28;
			captive1 = pracbuf[510];
			captive2 = pracbuf[511];
			pracbuf[510] = 0x55;
			pracbuf[511] = 0xAA;
		} else if (sectorsleft == 3) {
			pracbuf[510] = captive1;
			pracbuf[511] = captive2;
		}
	}
	for (word sectorsleft = totalsectors; sectorsleft --> 0;) {
		if (fwrite(fbuffer, 512, 1, out) != 1) {
			fprintf(stderr, "Can't write %s\n", argv[3]);
			return 1;
		}
		if (sectorsleft) {
			if (fread(fbuffer, 512, 1, floppy) != 1) {
				fprintf(stderr, "Can't read %s\n", argv[1]);
				return 1;
			}
		}
	}
	if (fclose(out)) {
		fprintf(stderr, "Can't finish writing %s\n", argv[3]);
		return 1;
	}
	return 0;
}
