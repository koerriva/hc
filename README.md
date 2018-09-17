# hc

compile `Java` Source File to native.

Target Triple

构造 Target Triple方法:

e.g : x86_64-pc-none-gnu 64位PC平台通用格式

`格式` `<arch><sub>`-`<vendor>`-`<sys>`-`<abi>`
- `arch` = x86_64, i386, arm, thumb, mips, etc.
- `sub` = for ex. on ARM: v5, v6m, v7a, v7m, etc.
- `vendor` = pc, apple, nvidia, ibm, etc.
- `sys` = none, linux, win32, darwin, cuda, etc.
- `abi` = eabi, gnu, android, macho, elf, etc.