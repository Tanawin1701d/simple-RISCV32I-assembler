; this is a comment


addi r1 r1 -1
addi r2 r2 2
addi r3 r3 2
addi r0 r0 0

beq 8 r2 r3
addi r20 r0 1
bne 8 r1 r2
addi r21 r0 1
blt 8 r1 r2
addi r22 r0 1
bge 8 r2 r1
addi r23 r0 1
bLTu 8 r1 r2
addi r24 r0 1
bGEU 8 r1 r2
addi r25 r0 1
bGEU -4 r1 r2