#lang racket
(provide compile-op0 compile-op1 compile-op2 compile-op3 pad-stack assert-proc)
(require "ast.rkt")
(require "types.rkt")
(require "assert.rkt")

;; RISC-V register mapping
(define t0 'x5)
(define t1 'x6)
(define t2 'x7)
(define t3 'x28)
(define t4 'x29)
(define a0 'x10)
(define s0 'x8) ; stack ptr / frame ptr
(define s1 'x9) ; heap ptr

;; Helper: memory offset as list
;; (Offset base offset) → string like "8(x8)"
(define (mem->str m)
  (match m
    [(list 'Offset base off)
     (format "~a(~a)" off base)]
    [_ (format "(invalid-offset ~a)" m)]))

;; Instruction builders
(define (Mov rd rs)        `(mv ,rd ,rs))
(define (Add rd r1 r2)     `(add ,rd ,r1 ,r2))
(define (Addi rd r imm)    `(addi ,rd ,r ,imm))
(define (Sub rd r1 r2)     `(sub ,rd ,r1 ,r2))
(define (Xor rd imm)       `(xori ,rd ,rd ,imm))
(define (And rd imm)       `(andi ,rd ,rd ,imm))
(define (Sd rs addr)       `(sd ,rs ,(mem->str addr)))
(define (Ld rd addr)       `(ld ,rd ,(mem->str addr)))
(define (Cmp r1 r2)        `(slt x0 ,r1 ,r2)) ; dummy comparison
(define (Jmp l)            `(j ,l))
(define (Jal l)            `(jal ,l))
(define (Je l)             `(beq ,t0 x0 ,l))
(define (Ret)              '(ret))
(define (Label l)          `(label ,l))

;; Op0
(define (compile-op0 p)
  (match p
    ['void
     `((li ,t0 ,(value->bits (void))))]
    ['read-byte
     `((call read_byte))]
    ['peek-byte
     `((call peek_byte))]
    ['collect-garbage
     `((mv ,a0 ,s0)
       (mv ,a0 ,s0)
       (mv ,a0 ,s1)
       (call collect_garbage)
       (mv ,s1 ,t0)
       (li ,t0 ,(value->bits (void))))]))

;; Op1
(define (compile-op1 p)
  (match p
    ['add1
     `((addi ,t0 ,t0 ,(value->bits 1)))]
    ['sub1
     `((addi ,t0 ,t0 ,(value->bits -1)))]
    ['zero?
     `((seqz ,t0 ,t0))]
    ['box
     `((sd ,t0 0(x9))
       (mv ,t0 ,x9)
       (xori ,t0 ,t0 ,type-box)
       (addi ,x9 ,x9 8))]
    ['unbox
     `((ld ,t0 ,(mem->str (list 'Offset t0 (- type-box)))))]
    ['car
     `((ld ,t0 ,(mem->str (list 'Offset t0 (- 8 type-cons)))))]
    ['cdr
     `((ld ,t0 ,(mem->str (list 'Offset t0 (- type-cons)))))]
    ['eof-object?
     `((li ,t1 ,(value->bits eof))
       (beq ,t0 ,t1 if-true)
       (li ,t0 ,(value->bits #f))
       (j if-end)
       (label if-true)
       (li ,t0 ,(value->bits #t))
       (label if-end))]))

;; Op2
(define (compile-op2 p)
  (match p
    ['+
     `((ld ,t1 ,(mem->str (list 'Offset s0 0)))
       (addi ,s0 ,s0 8)
       (add ,t0 ,t0 ,t1))]
    ['-
     `((ld ,t1 ,(mem->str (list 'Offset s0 0)))
       (addi ,s0 ,s0 8)
       (sub ,t0 ,t1 ,t0))]
    ['=
     `((ld ,t1 ,(mem->str (list 'Offset s0 0)))
       (addi ,s0 ,s0 8)
       (sub ,t2 ,t1 ,t0)
       (seqz ,t0 ,t2))]
    ['<
     `((ld ,t1 ,(mem->str (list 'Offset s0 0)))
       (addi ,s0 ,s0 8)
       (slt ,t0 ,t1 ,t0))]
    ['cons
     `((sd ,t0 0(x9))
       (ld ,t0 ,(mem->str (list 'Offset s0 0)))
       (addi ,s0 ,s0 8)
       (sd ,t0 8(x9))
       (mv ,t0 ,x9)
       (xori ,t0 ,t0 ,type-cons)
       (addi ,x9 ,x9 16))]))

;; Op3
(define (compile-op3 p)
  (match p
    ['vector-set!
     `((ld ,t4 ,(mem->str (list 'Offset s0 0)))
       (addi ,s0 ,s0 8)
       (ld ,t1 ,(mem->str (list 'Offset s0 0)))
       (addi ,s0 ,s0 8)
       (slli ,t4 ,t4 3)
       (add ,t1 ,t1 ,t4)
       (sd ,t0 8(t1))
       (li ,t0 ,(value->bits (void))))]))

;; Pad stack (placeholder)
(define pad-stack '())
(define assert-proc (λ (x) '()))
