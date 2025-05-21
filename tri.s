;; DEBUG read-all ⇒ ((((λ (t) ((λ (f) (t (λ (z) ((f f) z)))) (λ (f) (t (λ (z) ((f f) z)))))) (λ (tri) (λ (n) (if (zero? n) 0 (+ n (tri (sub1 n))))))) 36))
;; DEBUG parse-closed ⇒ #s(Prog () #s(App #s(App #s(Lam lambda6854 (t) #s(App #s(Lam lambda6851 (f) #s(App #s(Var t) (#s(Lam lambda6850 (z) #s(App #s(App #s(Var f) (#s(Var f))) (#s(Var z))))))) (#s(Lam lambda6853 (f) #s(App #s(Var t) (#s(Lam lambda6852 (z) #s(App #s(App #s(Var f) (#s(Var f))) (#s(Var z)))))))))) (#s(Lam lambda6856 (tri) #s(Lam lambda6855 (n) #s(If #s(Prim1 zero? #s(Var n)) #s(Lit 0) #s(Prim2 + #s(Var n) #s(App #s(Var tri) (#s(Prim1 sub1 #s(Var n)))))))))) (#s(Lit 36))))
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
        # unhandled: lea
        addi s0, s0, -8
        sw t0, 0(s0)
        # unhandled: lea
        addi s0, s0, -8
        sw t0, 0(s0)
        # unhandled: lea
        sw t0, 0(s1)
        mv t0, s1
        xori t0, t0, 5
        addi s1, s1, 8
        addi s0, s0, -8
        sw t0, 0(s0)
        # unhandled: lea
        sw t0, 0(s1)
        mv t0, s1
        xori t0, t0, 5
        addi s1, s1, 8
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
ret6858:
        addi s0, s0, -8
        sw t0, 0(s0)
        li t0, 576
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
ret6857:
        lw t3, 0(s0)
        addi s0, s0, 8
        lw s0, 0(s0)
        addi s0, s0, 8
        lw s1, 0(s0)
        addi s0, s0, 8
        ret
        # unhandled: dq
label_lambda6854_301f:
        lw t0, 8(sp)
        # unhandled: lea
        sw t0, 0(s1)
        lw t1, 0(sp)
        sw t1, 8(s1)
        mv t0, s1
        xori t0, t0, 5
        addi s1, s1, 16
        addi s0, s0, -8
        sw t0, 0(s0)
        # unhandled: lea
        sw t0, 0(s1)
        lw t1, 8(sp)
        sw t1, 8(s1)
        mv t0, s1
        xori t0, t0, 5
        addi s1, s1, 16
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t1, 8(sp)
        sw t1, 24(sp)
        lw t1, 0(sp)
        sw t1, 16(sp)
        addi sp, sp, 16
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
        addi sp, sp, 16
        ret
        # unhandled: dq
label_lambda6851_301c:
        lw t0, 8(sp)
        lw t2, 3(t0)
        addi s0, s0, -8
        sw t2, 0(s0)
        lw t0, 0(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        # unhandled: lea
        sw t0, 0(s1)
        lw t1, 16(sp)
        sw t1, 8(s1)
        mv t0, s1
        xori t0, t0, 5
        addi s1, s1, 16
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t1, 8(sp)
        sw t1, 32(sp)
        lw t1, 0(sp)
        sw t1, 24(sp)
        addi sp, sp, 24
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
        addi sp, sp, 24
        ret
        # unhandled: dq
label_lambda6850_301b:
        lw t0, 8(sp)
        lw t2, 3(t0)
        addi s0, s0, -8
        sw t2, 0(s0)
        # unhandled: lea
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 8(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 16(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
ret6859:
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 16(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t1, 8(sp)
        sw t1, 32(sp)
        lw t1, 0(sp)
        sw t1, 24(sp)
        addi sp, sp, 24
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
        addi sp, sp, 24
        ret
        # unhandled: dq
label_lambda6853_301e:
        lw t0, 8(sp)
        lw t2, 3(t0)
        addi s0, s0, -8
        sw t2, 0(s0)
        lw t0, 0(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        # unhandled: lea
        sw t0, 0(s1)
        lw t1, 16(sp)
        sw t1, 8(s1)
        mv t0, s1
        xori t0, t0, 5
        addi s1, s1, 16
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t1, 8(sp)
        sw t1, 32(sp)
        lw t1, 0(sp)
        sw t1, 24(sp)
        addi sp, sp, 24
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
        addi sp, sp, 24
        ret
        # unhandled: dq
label_lambda6852_301d:
        lw t0, 8(sp)
        lw t2, 3(t0)
        addi s0, s0, -8
        sw t2, 0(s0)
        # unhandled: lea
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 8(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 16(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
ret6860:
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 16(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t1, 8(sp)
        sw t1, 32(sp)
        lw t1, 0(sp)
        sw t1, 24(sp)
        addi sp, sp, 24
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
        addi sp, sp, 24
        ret
        # unhandled: dq
label_lambda6856_3021:
        lw t0, 8(sp)
        # unhandled: lea
        sw t0, 0(s1)
        lw t1, 0(sp)
        sw t1, 8(s1)
        mv t0, s1
        xori t0, t0, 5
        addi s1, s1, 16
        addi sp, sp, 16
        ret
        # unhandled: dq
label_lambda6855_3020:
        lw t0, 8(sp)
        lw t2, 3(t0)
        addi s0, s0, -8
        sw t2, 0(s0)
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 15
        sub t3, t2, x0
        bne t3, x0, err
        sub t3, t0, x0
        li t0, 56
        li t2, 24
        # unhandled: cmove
        sub t3, t0, x56
        beq t3, x0, if6861
        li t0, 0
        # unhandled: jmp
if6861:
        lw t0, 8(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        # unhandled: lea
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 16(sp)
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 32(sp)
        mv t2, t0
        andi t2, t2, 15
        sub t3, t2, x0
        bne t3, x0, err
        addi t0, t0, -16
        addi s0, s0, -8
        sw t0, 0(s0)
        lw t0, 8(sp)
        mv t2, t0
        andi t2, t2, 7
        sub t3, t2, x5
        bne t3, x0, err
        lw t0, -5(t0)
        # unhandled: jmp
ret6863:
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
if6862:
        addi sp, sp, 24
        ret
err:
        mv t3, sp
        andi t3, t3, 8
        sub sp, sp, t3
        jal ra, raise_error
