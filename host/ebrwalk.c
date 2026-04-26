#!/usr/bin/tcc -run

#include <stdio.h>
#include <string.h>

static void getchsstr(char *s, unsigned char *buf, unsigned int offset)
{
	unsigned short cyl = buf[offset + 3] | (unsigned short)(buf[offset + 2] >> 6) << 8;
	unsigned short head = buf[offset + 1];
	unsigned short sect = buf[offset + 2] & 63;
	if ((head == 0xFF || head == 0xFE) && cyl == 1023 && sect == 63)
		strcpy(s, "-----------");
	else
		sprintf(s, "%4u-%03u-%02u", cyl, head, sect);
}

static void fatwalk(FILE *f, char *chs1, char *chs2, unsigned char type, unsigned offset, unsigned length)
{
	unsigned char buf[512];
	fseek(f, offset * 512, SEEK_SET);
	fread(buf, 512, 1, f);
	unsigned int length2 = *(unsigned int *)(buf + 0x20);
	printf("%2X %s %8u %s %8u %8u %8u %2u\n", type, chs1, offset, chs2, length, length2, offset + length, buf[0x0D]);
}

static void partwalk(FILE *f, unsigned ebrbasis, unsigned fatbasis)
{
	unsigned char buf[512];
	fseek(f, fatbasis * 512, SEEK_SET);
	fread(buf, 512, 1, f);
	for (unsigned int offset = 0x1BE; offset < 0x1FE; offset += 16)
	{
		unsigned int offset2 = *(unsigned int*)(buf + offset + 8);
		unsigned int length2 = *(unsigned int*)(buf + offset + 12);
		char chs1[12];
		char chs2[12];
		getchsstr(chs1, buf, offset);
		getchsstr(chs2, buf, offset + 4);
		switch (buf[offset + 4])
		{
			case 0: break;
			case 1:
			case 4:
			case 6:
			case 0x0B:
			case 0x0C:
			case 0x0E:
				fatwalk(f, chs1, chs2, buf[offset + 4], fatbasis + offset2, length2);
				break;
			case 5:
			case 0x0F:
				printf("%2X %s %8u %s %8u %17u\n", buf[offset + 4], chs1, ebrbasis + offset2, chs2, length2, offset2 + length2);
				partwalk(f, ebrbasis == 0 ? offset2 : ebrbasis, ebrbasis + offset2);
				break;
			default:
				printf("%2X %s %8u %s %8u %17u\n", buf[offset + 4], chs1, ebrbasis + offset2, chs2, length2, offset2 + length2);
				break;
		}
	}
}

int main(int argc, char **argv)
{
	if (argc < 2) return 1;
	FILE *f = fopen(argv[1], "rb");
	if (!f) return 1;
	printf("TP    C-  H- S   offset C-   H- S       size  secsize    end+1 SPC\n");
	partwalk(f, 0, 0);
}
