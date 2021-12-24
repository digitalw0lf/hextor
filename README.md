# Hextor

<img title="Disarm_Dark" src="https://user-images.githubusercontent.com/63538674/147139570-2d1b66d4-dd2d-4ee1-aef5-34a6116ce3e2.png" height=130 /> &nbsp; &nbsp; &nbsp; <img title="DepthmapInsideJpeg" src="https://user-images.githubusercontent.com/63538674/147140091-45a191f1-cc9f-4d12-b090-b78ed52bbb50.png" height=130 /> &nbsp; &nbsp; &nbsp; <img title="TextMode_Search" src="https://user-images.githubusercontent.com/63538674/147140112-b66c0d13-0f37-452a-b1a0-eaaef4777db3.png" height=130 />

More [screenshots and use cases >>](https://github.com/digitalw0lf/hextor/wiki/Hextor-Use-cases)


Download: https://github.com/digitalw0lf/hextor/releases/latest

Hextor is a Hexadecimal editor and binary data analyzing toolkit.

It was created to make working with binary file formats as simple and convenient as with plain text. Although it is staffed with a powerful set of data analyzing tools, it starts instantly and rapidly works with files of any size.

Main features are:

- View data as different basic types (text/hex/int8/16/32/float/etc.)
- Interpret and edit data as [structure with C-like description](https://github.com/digitalw0lf/hextor/wiki/Structure-analyzer)
- View/edit logical volumes, physical disks, process memory
- Unlimited Undo/Redo
- Bookmarks
- ANSI/ASCII encoding (other SBCS and MBCS planned)
- Flexible text/hex/unicode search and replace with [wildcards and value search](https://github.com/digitalw0lf/hextor/wiki/Search-and-replace)
- Search and replace in files/directories
- Advanced binary file comparison with insert/delete detection
- View data as bitmap
- "Text editor" mode for huge text files and text sections inside of binary files
- Hash calculation (MD/SHA/CRC etc.)
- Disassembler (using Zydis library)
- Scripting support (via IActiveScript)
- Planned feature: Plugin support (via OleAutomation API)
- Dark and Light UI themes

As with any other low-level tool, use with caution and always make backups of valuable data!

Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>

Hextor is a Freeware Source-Available software. See LICENSE.txt for details

### Credits

Hextor uses the following open-source libraries.

**FastMM4** A memory manager for Delphi applications  
https://github.com/pleriche/FastMM4  
Copyright (c) Professional Software Development / Pierre le Riche  
Licence: MPL 1.1 / LGPL 2.1  

**HashLib4Pascal** Checksum and hash library  
https://github.com/Xor-el/HashLib4Pascal  
Copyright (c) Ugochukwu Mmaduekwe  
Licence: MIT  

**JCL** JEDI Code Library  
https://www.delphi-jedi.org  
Copyright (c) Project JEDI contributors  
Licence: MPL 1.1  

**superobject** JSON library  
https://github.com/hgourvest/superobject  
(fixed version from fork https://github.com/fainspace/superobject)  
Copyright (c) Henri Gourvest  
Licence: LGPL / MPL 1.1  

**SynEdit** Syntax highlighting editor component  
https://github.com/SynEdit/SynEdit  
Licence: MPL 1.1 / GPL 2  

**Virtual-TreeView** Delphi treeview control  
https://www.jam-software.com/virtual-treeview  
Copyright (c) 1999-2001 digital publishing AG  
Copyright (c) 2000-2009 Mike Lischke  
Copyright (c) JAM Software  
License:  MPL 1.1 / LGPL 2.1  

**Zydis** Fast and lightweight x86/x86-64 disassembler library  
https://github.com/zyantific/zydis  
Copyright (c) 2014-2020 Florian Bernd  
Copyright (c) 2014-2020 Joel Honer  
Licence: MIT  

