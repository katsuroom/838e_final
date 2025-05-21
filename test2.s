;; DEBUG read-all ⇒ ((+ 1 2))
;; DEBUG parse-closed ⇒ #s(Prog () #s(Prim2 + #s(Lit 1) #s(Lit 2)))
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
        li t0, 16
        addi s0, s0, -8
        sw t0, 0(s0)
        li t0, 32
        lw t1, 0(s0)
        addi s0, s0, 8
        mv t2, t1
        andi t2, t2, 15
        sub t3, t2, x0
        bne t3, x0, err
        mv t2, t0
        andi t2, t2, 15
        sub t3, t2, x0
        bne t3, x0, err
        add t0, t0, t1
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
