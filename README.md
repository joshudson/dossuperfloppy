## Fixed Disk Superfloppy Tools

This repository contains tools for preparing and installing DOS (including
FreeDOS and Windows 9x) on a fixed disk using a superfloppy format; that is,
the the disk as a whole appears as one filesystem, which is convenient for
accessing the disk from the host operating system when running in a VM. As
a side effect, this saves disk space.

There are two sets of tools, one for [rotating head disks](https://raw.githubusercontent.com/joshudson/dossuperfloppy/master/formathd)
and one for [solid state disks and SD cards](https://raw.githubusercontent.com/joshudson/dossuperfloppy/master/ssdfmt).

All tools are cross-compiled from a Linux host. Build tools required:
    make, nasm, tofrodos, info-zip

If you unpack both sets of tools to the same directory, you will get
name collisions. Some files are common to both tools. Always keep newest.

With the recent rise in interest in retro-computers being remade and
installing DOS on sold state media, I have high hopes that these tools
are found useful to people besides just me.

Copyright (C) Joshua Hudson -- Licensed under CC-BY-SA 4.0