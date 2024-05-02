 ; this is a comment
addi r1 r1 255
addi r2 r2 1
slli r2 r2 15
addi r3 r3 7
addi r10 r2 1024
; store
sb r10 -4 r1
sh r10  0 r2
sW r10  4 r3
; load
lb r20 r10 -4
lh r21 r10 0
lw r22 r10 4
; load with unsign
lbu r25 r10 -4
lbu r26 r10 0