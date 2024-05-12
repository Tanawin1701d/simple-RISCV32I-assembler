# simple RISC-V 32I Assembler

## Introduction
This document provides an overview and usage instructions for the simple RISC-V assembler 
implemented in Scala. In addition, This project was used by Kathryn 
(The hardware construction framework with hybrid design flow) to show a simple pipeline RISC-V implementation

## Specification
- Version : ```RISC-V 32I```
- Exclude : ```Fence and Csr instruction```

## code example

```asm

; op reg
add  r4 r14 r13 ; r4 <- r14 + r13

; op imm
addi r8 r14 -1  ; r8 <- r14 + -1

; branch
.loop
addi r2 r2 1
blt .loop r2 r1   ; pc <-  (r2 < r1) ? .loop : pc+4

; pc & jump instruction
lui   r25 8192
auipc r26 8192
jal   r27 8

; load and store memory
lb r20 r10 4  ; r20 <- mem[r10 + 4]    8 bits load
sh r10  0 r4  ; mem[r10 + 0] <- r4    16 bits store

```

## usage
### run example [merge sort example]
1. Clone the repository from GitHub: `git clone https://github.com/Tanawin1701d/simple-RISCV32I-assembler`
2. modify ```example/sort/compileGroup``` file as follow :
   - first line is abs path to ```sort.asm``` which was provided in the same directory with ```compileGroup```
   - second line is abs path to ```<your output with file name>```
   - you may compile more than 1 file at a time by append your src and des file to this file

6. compile program: ```sbt compile```
7. run program: ```sbt "run <abs path to compileGroup>"```