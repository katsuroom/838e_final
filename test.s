;; DEBUG read-all ⇒ ((let ((x (cons 0 0))) (let ((y (box 8))) 2)))
;; DEBUG parse-closed ⇒ #s(Prog () #s(Let x #s(Prim2 cons #s(Lit 0) #s(Lit 0)) #s(Let y #s(Prim1 box #s(Lit 8)) #s(Lit 2))))
.text
.globl entry
.extern peek_byte
.extern read_byte
.extern write_byte
.extern raise_error
.extern collect_garbage
entry:
        addi s0, s0, -8
        sw s1, 0(s0)
        addi s0, s0, -8
        sw s0, 0(s0)
        addi s0, s0, -8
        sw t3, 0(s0)
        mv s0, sp
        mv s1, a0
        li t0, 0
        addi s0, s0, -8
        sw t0, 0(s0)
        li t0, 0
        sw t0, 0(s1)
        lw t0, 0(s0)
        addi s0, s0, 8
        sw t0, 8(s1)
        mv t0, s1
        xori t0, t0, 2
        addi s1, s1, 16
        addi s0, s0, -8
        sw t0, 0(s0)
        li t0, 128
        sw t0, 0(s1)
        mv t0, s1
        xori t0, t0, 1
        addi s1, s1, 8
        addi s0, s0, -8
        sw t0, 0(s0)
        li t0, 32
        addi sp, sp, 8
        addi sp, sp, 8
        lw t3, 0(s0)
        addi s0, s0, 8
        lw s0, 0(s0)
        addi s0, s0, 8
        lw s1, 0(s0)
        addi s0, s0, 8
        ret
err:
        mv t3, sp
        andi t3, t3, 8
        sub sp, sp, t3
        jal ra, raise_error
