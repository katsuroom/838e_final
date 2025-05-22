#lang racket
(provide asm-display)
(require a86/ast racket/match "types.rkt")

;; Register remapping: x86 → RISC-V
(define remap-reg
  (hash 'rax 't0 'r8 't1 'r9 't2 'r15 't3
        'rdi 'a0 'rsp 'sp 'rbp 'fp 'rbx 's1 'eax 't0))

(define (rename-reg r)
  (cond
    [(hash-has-key? remap-reg r) (hash-ref remap-reg r)]
    [(symbol? r) r]
    [(integer? r) (string->symbol (string-append (number->string r)))]
    [else (error "rename-reg: unexpected register" r)]))

(define indent "        ")

;; Format argument string
(define (arg->string e)
  (match e
    [(? register?) (symbol->string (rename-reg e))]
    [(? integer?)  (number->string e)]
    [(? symbol?)   (symbol->string e)]
    [($ label)     (symbol->string label)]
    [(Offset off)
     (match off
       [(list '+ base imm)
        (string-append (number->string imm)
                       "(" (symbol->string (rename-reg base)) ")")]
       [(? integer?)
        (string-append (number->string off)
                       "(" (symbol->string (rename-reg 'fp)) ")")]
       [_ (error "unsupported Offset expression" off)])]
    [_ (error "unsupported operand" e)]))

;; Convert a single instruction to string
(define (instr->string instr)
  (match instr
    ;; Segments & labels
    [(Text)         ""]
    [(Data)         ""]
    [(Global ($ l)) ""]
    [(Extern ($ l)) ""]
    [(Label ($ l))  (string-append (symbol->string l) ":")]

    ;; Memory store
    [(Mov (Offset off) src)
     (match off
       [(list '+ base imm)
        (string-append indent "sw " (symbol->string (rename-reg src))
                       ", " (number->string imm)
                       "(" (symbol->string (rename-reg base)) ")")]
       [(? integer?)
        (string-append indent "sw " (symbol->string (rename-reg src))
                       ", " (number->string off) "(fp)")]
       [_ (error "Mov-store: unsupported Offset" off)])]

    ;; Memory load
    [(Mov dst (Offset off))
     (match off
       [(list '+ base imm)
        (string-append indent "lw " (symbol->string (rename-reg dst))
                       ", " (number->string imm)
                       "(" (symbol->string (rename-reg base)) ")")]
       [(? integer?)
        (string-append indent "lw " (symbol->string (rename-reg dst))
                       ", " (number->string off) "(fp)")]
       [_ (error "Mov-load: unsupported Offset" off)])]

    ;; Arithmetic / logic
    [(Add dst src)
     (match src
       [(? register?)
        (string-append indent "add " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg src)))]
       [(? integer?)
        (if (= src 0) ""
            (string-append indent "addi " (symbol->string (rename-reg dst))
                           ", " (symbol->string (rename-reg dst))
                           ", " (number->string src)))]
       [_ (error "Add: unsupported" src)])]

    [(Sub dst src)
     (match src
       [(? register?)
        (string-append indent "sub " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg src)))]
       [(? integer?)
        (if (= src 0) ""
            (string-append indent "addi " (symbol->string (rename-reg dst))
                           ", " (symbol->string (rename-reg dst))
                           ", " (number->string (- src))))])]

    [(And dst src)
     (match src
       [(? register?)
        (string-append indent "and " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg src)))]
       [(? integer?)
        (string-append indent "andi " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg dst))
                       ", " (number->string src))])]

    [(Or dst src)
     (match src
       [(? register?)
        (string-append indent "or " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg src)))]
       [(? integer?)
        (string-append indent "ori " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg dst))
                       ", " (number->string src))])]

    [(Xor dst src)
     (match src
       [(? register?)
        (string-append indent "xor " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg src)))]
       [(? integer?)
        (string-append indent "xori " (symbol->string (rename-reg dst))
                       ", " (symbol->string (rename-reg dst))
                       ", " (number->string src))])]

    ;; Shift ops
    [(Sar dst amt)
     (string-append indent "srai " (arg->string dst) ", "
                    (arg->string dst) ", " (number->string amt))]

    [(Sal dst amt)
     (string-append indent "slli " (arg->string dst) ", "
                    (arg->string dst) ", " (number->string amt))]

    [(Cmove dst src)
     (string-append
      indent "beq t3, x0, cmove_then\n"
      indent "j cmove_end\n"
      "cmove_then:\n"
      indent "mv " (arg->string dst) ", " (arg->string src) "\n"
      "cmove_end:")]

    ;; lea (only label)
    [(Lea dst ($ label))
     (string-append indent "la " (arg->string dst)
                    ", " (symbol->string label))]


    ;; jmp to register
    [(Jmp ($ label)) (string-append indent "j " (arg->string label))]
    [(Jmp reg) (string-append indent "jalr " (arg->string reg))]


    [(Dq val)
      (if (= val 1)
        (string-append indent "addi x0, x0, 0")
        (string-append indent ""))]

    ;; cons
    [(Call ($ 'cons))
     (string-append
      indent "sw a0, 0(s1)\n"
      indent "sw a1, 8(s1)\n"
      indent "addi a0, s1, 0\n"
      indent "xori a0, a0, " (number->string type-cons) "\n"
      indent "addi s1, s1, 16")]

    ;; box
    [(Call ($ 'box))
     (string-append
      indent "sw a0, 0(s1)\n"
      indent "addi a0, s1, 0\n"
      indent "xori a0, a0, " (number->string type-box) "\n"
      indent "addi s1, s1, 8")]

    ;; mv / li
    [(Mov dst src)
     (match src
       [(? register?)
        (if (eq? dst src) ""
            (string-append indent "mv " (symbol->string (rename-reg dst))
                           ", " (symbol->string (rename-reg src))))]
       [(? integer?)
        (string-append indent "li " (symbol->string (rename-reg dst))
                       ", " (number->string src))])]

    ;; push / pop
    [(Push r)
     (string-append indent "addi sp, sp, -8\n"
                    indent "sw " (symbol->string (rename-reg r)) ", 0(sp)")]
    [(Pop r)
     (string-append indent "lw " (symbol->string (rename-reg r)) ", 0(sp)\n"
                    indent "addi sp, sp, 8")]

    ;; comparisons / branches
    [(Cmp r1 r2)
     (match r2
       [(? register?)
        (string-append indent "sub t3, "
                    (symbol->string (rename-reg r1)) ", "
                    (symbol->string (rename-reg r2)))]
       [(? integer?)
        (string-append indent "addi t3, "
                    (symbol->string (rename-reg r1)) ", "
                    (number->string (- r2)))])]

     
    [(Je ($ l))  (string-append indent "beq t3, x0, " (symbol->string l))]
    [(Jne ($ l)) (string-append indent "bne t3, x0, " (symbol->string l))]
    [(Jl ($ label)) (string-append indent "blt t3, x0, " (symbol->string label))]

    ;; call / ret
    [(Call ($ l)) (string-append indent "jal x0, " (symbol->string l))]
    [(Ret) (string-append indent "ret")]

    ;; fallback
    [_ (string-append indent "# unhandled: " (instruction-name instr))]))

(define (asm-display instrs)
  ;; print once
  (printf ".section .text\n")
  (printf ".global entry\n")

  ;; main
  (for ([i instrs])
    (define s (instr->string i))
    (when (and s (not (string=? s "")))
      (printf "~a\n" s)))
      
  (printf "raise_error:\n")
  (printf "\taddi a1, a1, -1\n")
  (printf "\tret")
  )

  