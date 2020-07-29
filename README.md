# Hextor

Hextor - Hexadecimal editor and binary data analyzing toolkit

Aim of the project is to create a powerful and expandable binary data analyzing toolkit while keeping it lightweight and comprehensible like a conventional hex editor.

Main features are:

- Fast view/edit files of any size
- View/edit logical volumes, physical disks, process memory
- Unlimited Undo/Redo
- Bookmarks
- ANSI/ASCII encoding (other SBCS and MBCS planned)
- View data as different basic types (int8/16/32/float/etc.)
- Text/hex/unicode search and replace (RegEx planned)
- Search and replace in files/directories
- Interpret and edit data as structure with C-like description
- Binary file comparison
- View data as bitmap
- Hash calculation (MD/SHA/CRC etc.)
- Disassembler (using Zydis library)
- Scripting support (via MSScriptControl)
- Plugin support planned (via OleAutomation API)

Project is in early development stage. Use with caution and always make backups of valuable data!

Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>

Hextor is a Freeware Source-Available software. See LICENSE.txt for details

### Credits

Hextor uses the following open-source libraries.

**HashLib!** Checksum and hash library  
Copyright (c) Alex Demchenko  
http://www.cobans.net  
(Downloaded from https://torry.net/authorsmore.php?id=3749)  
Licence: Freeware  

**JCL** JEDI Code Library  
https://www.delphi-jedi.org  
Copyright (c) Project JEDI contributors  
Licence: MPL 1.1  

**KControls** component suite for Delphi and Lazarus  
https://github.com/kryslt/KControls  
Copyright (c) Tomas Krysl  
Licence: Freeware  

**superobject** JSON library  
https://github.com/hgourvest/superobject  
(fixed version from fork https://github.com/fainspace/superobject)  
Copyright (c) Henri Gourvest  
Licence: LGPL / MPL 1.1  

**Zydis** Fast and lightweight x86/x86-64 disassembler library  
https://github.com/zyantific/zydis  
Copyright (c) 2014-2020 Florian Bernd  
Copyright (c) 2014-2020 Joel Honer  
Licence : MIT  

