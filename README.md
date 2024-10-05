# gbcdis
Game Boy disassembler written in OCaml.

# Overview

`gbcdis` starts the disassembly from the `0x0` address, meaning it correctly
disassembles the boot ROM, for other ROMs it needs to read the first `0x100`
bytes as instructions that take up `0x100` bytes as a sum in order to start
disassembling the correct code.
In addition it doesn't try to differentiate code and data, producing gibberish
instructions in place of the data. If the last of these instructions
doesn't take more bytes than is left in the data block and doesn't overlap
on the real code, the disassembly should function and continue correctly, aside
from producing some useless jump target labes (for the jump instructions
in the data block).

# Building & usage

To build the project use `dune build`

To run the disassembler, use `dune exec gbcdis -- <bin_path> <out_path>`
