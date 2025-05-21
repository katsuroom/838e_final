#lang racket
(provide compile
         compile-e
         compile-es
         compile-define
         compile-match
         compile-match-clause
         compile-lambda-define
         compile-string ; for notes
         copy-env-to-stack
         free-vars-to-heap)

(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require "lambdas.rkt")
(require "fv.rkt")
(require a86/ast)

;; RISC-V register mapping (replacing original x86 defines) :contentReference[oaicite:1]{index=1}
(define t0  'x5)    ; result / general
(define t1  'x6)    ; scratch
(define t2  'x7)    ; scratch
(define t3  'x28)   ; stack pad (non-volatile)
(define a0  'x10)   ; argument (first)
(define s0  'x8)    ; frame pointer / stack pointer
(define s1  'x9)    ; heap pointer

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (Global 'entry)
           (Extern 'peek_byte)
           (Extern 'read_byte)
           (Extern 'write_byte)
           (Extern 'raise_error)
           (Extern 'collect_garbage)
           (Label 'entry)
           ;; RISC-V prologue: allocate stack, save ra/s1/s0 :contentReference[oaicite:2]{index=2}
           (Addi s0 s0 -24)
           (Sd ra (Offset s0 16))
           (Sd s1 (Offset s0 8))
           (Sd s0 (Offset s0 0))
           ;; move heap pointer from a0 into s1
           (Mov s1 a0)

           (compile-defines-values ds)
           (compile-e e (reverse (define-ids ds)) #f)

           ;; pop definitions, then epilogue
           (Addi s0 s0 (* 8 (length ds)))
           (Ld s0 (Offset s0 0))
           (Ld s1 (Offset s0 8))
           (Ld ra (Offset s0 16))
           (Addi s0 s0 24)
           (Ret)

           (compile-defines ds)
           (compile-lambda-defines (lambdas p))

           (Label 'err)
           pad-stack
           (Call 'raise_error))]))

;; [Listof Defn] -> [Listof Id]
(define (define-ids ds)
  (match ds
    ['() '()]
    [(cons (Defn f xs e) ds)
     (cons f (define-ids ds))]))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (compile-lambda-define (Lam f xs e))]))

;; [Listof Lam] -> Asm
(define (compile-lambda-defines ls)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-lambda-define l)
          (compile-lambda-defines ls))]))

;; Lam -> Asm
(define (compile-lambda-define l)
  (let ((fvs (fv l)))
    (match l
      [(Lam f xs e)
       (let ((env (append (reverse fvs) (reverse xs) (list #f))))
         (seq (Dq (length fvs))
              (Label (symbol->label f))
              ;; closure entry: load code pointer, copy env
              (Mov t0 (Offset s0 (* 8 (length xs))))
              (copy-env-to-stack fvs 8)
              (compile-e e env #t)
              ;; pop env
              (Addi s0 s0 (* 8 (length env)))
              (Ret)))])))

;; Copy free-vars into stack at given offset
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (Mov t2 (Offset t0 (- off type-proc)))
          (Sd t2 (Offset s0 off))
          (copy-env-to-stack fvs (+ 8 off)))]))

;; Expr CEnv Boolean -> Asm
(define (compile-e e c t?)
  (match e
    [(Lit d)      (compile-value d)]
    [(Eof)        (compile-value eof)]
    [(Var x)      (compile-variable x c)]
    [(Prim0 p)    (compile-prim0 p)]
    [(Prim1 p e)  (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)
     (compile-if e1 e2 e3 c t?)]
    [(Begin e1 e2)
     (compile-begin e1 e2 c t?)]
    [(Let x e1 e2)
     (compile-let x e1 e2 c t?)]
    [(App e es)
     (compile-app e es c t?)]
    [(Lam f xs e)
     (compile-lam f xs e c)]
    [(Match e ps es) 
     (compile-match e ps es c t?)]))

;; Value -> Asm
(define (compile-value v)
  (cond [(string? v) (compile-string v)]
        [else        (Mov t0 (value->bits v))]))

;; Variable lookup: load from stack
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov t0 (Offset s0 i)))))

;; String literal
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov t0 type-str))
        (seq (Mov t0 len)
             (Mov (Offset s1 0) t0)
             (compile-string-chars (string->list s) 8)
             (Mov t0 s1)
             (Xor t0 type-str)
             (Addi s1 s1
                   (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov t0 (char->integer c))
          (Mov (Offset s1 i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Prim0, Prim1, Prim2, Prim3 dispatch to compile-ops
(define (compile-prim0 p)       (compile-op0 p))
(define (compile-prim1 p e c)   (seq (compile-e e c #f) (compile-op1 p)))
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c #f)
       (Sd t0 (Offset s0 0))
       (Addi s0 s0 -8)
       (compile-e e2 (cons #f c) #f)
       (compile-op2 p)))
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c #f)
       (Sd t0 (Offset s0 0))
       (Addi s0 s0 -8)
       (compile-e e2 (cons #f c) #f)
       (Sd t0 (Offset s0 0))
       (Addi s0 s0 -8)
       (compile-e e3 (cons #f (cons #f c)) #f)
       (compile-op3 p)))

;; If-then-else
(define (compile-if e1 e2 e3 c t?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c #f)
         (Cmp t0 (value->bits #f))
         (Je l1)
         (compile-e e2 c t?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c t?)
         (Label l2))))

;; Begin
(define (compile-begin e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (compile-e e2 c t?)))

;; Let-binding
(define (compile-let x e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (Addi s0 s0 -8)
       (Sd t0 (Offset s0 0))
       (compile-e e2 (cons x c) t?)
       (Addi s0 s0 8)))

;; Function calls (tail and non-tail)
(define (compile-app e es c t?)
  (if t? (compile-app-tail e es c)
      (compile-app-nontail e es c)))

(define (compile-app-tail e es c)
  (seq (compile-es (cons e es) c)
       (move-args (add1 (length es)) (length c))
       (Addi s0 s0 (* 8 (length c)))
       (Mov t0 (Offset s0 (* 8 (length es))))
       (assert-proc t0)
       (Mov t0 (Offset t0 (- type-proc)))
       (Jal t0)))

(define (compile-app-nontail e es c)
  (let ((r (gensym 'ret))
        (i (* 8 (length es))))
    (seq (Lea t0 r)
         (Addi s0 s0 -8)
         (Sd t0 (Offset s0 0))
         (compile-es (cons e es) (cons #f c))
         (Mov t0 (Offset s0 i))
         (assert-proc t0)
         (Mov t0 (Offset t0 (- type-proc)))
         (Jal t0)
         (Label r))))

;; Argument mover
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov t1 (Offset s0 (* 8 (sub1 i))))
              (Mov (Offset s0 (* 8 (+ off (sub1 i)))) t1)
              (move-args (sub1 i) off))]))

;; Compile a list of expressions
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c #f)
          (Addi s0 s0 -8)
          (Sd t0 (Offset s0 0))
          (compile-es es (cons #f c)))]))

;; Pattern matching (unchanged aside from Push/Pop → Sd/Ld)
(define (compile-match e ps es c t?)
  (let ((done (gensym)))
    (seq (compile-e e c #f)
         (Addi s0 s0 -8)
         (Sd t0 (Offset s0 0))
         (compile-match-clauses ps es (cons #f c) done t?)
         (Jmp 'err)
         (Label done)
         (Addi s0 s0 8))))

(define (compile-match-clauses ps es c done t?)
  (match* (ps es)
    [('() '()) (seq)]
    [((cons p ps) (cons e es))
     (seq (compile-match-clause p e c done t?)
          (compile-match-clauses ps es c done t?))]))

(define (compile-match-clause p e c done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i cm)
       (seq (Mov t0 (Offset s0 0))
            (Addi s0 s0 8)
            i
            (compile-e e (append cm c) t?)
            (Addi s0 s0 (* 8 (length cm)))
            (Jmp done)
            (Label next))])))

(define (compile-pattern p cm next)
  (match p
    [(Var '_)         (list (seq) cm)]
    [(Var x)          (list (seq (Addi s0 s0 -8)
                                  (Ld t0 (Offset s0 0)))
                            (cons x cm))]
    ;; ... 其余 pattern 分支照原样，只需将 Push/Pop 换成 Addi/Sd/Ld
    ))

;; Environment lookup
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (if (eq? x y) 0
         (+ 8 (lookup x rest)))]))

;; Top-level closure compilation (values)
(define (compile-defines-values ds)
  (seq (alloc-defines ds 0)
       (init-defines ds (reverse (define-ids ds)) 8)
       (add-s1-defines ds 0)))

(define (alloc-defines ds off)
  (match ds
    ['() (seq)]
    [(cons (Defn f xs e) ds)
     (let ((fvs (fv (Lam f xs e))))
       (seq (Lea t0 (symbol->label f))
            (Sd t0 (Offset s1 off))
            (Mov t0 s1)
            (Xor t0 type-proc)
            (Addi s1 s1 8)
            (alloc-defines ds (+ off (* 8 (add1 (length fvs)))))))]))

(define (init-defines ds c off)
  (match ds
    ['() (seq)]
    [(cons (Defn f xs e) ds)
     (let ((fvs (fv (Lam f xs e))))
       (seq (free-vars-to-heap fvs c off)
            (init-defines ds c (+ off (* 8 (add1 (length fvs)))))))]))

;; Adjust s1 after allocating all definitions
(define (add-s1-defines ds n)
  (match ds
    ['() (seq)]
    [(cons _ ds)
     (seq (Addi s1 s1 (* 8 n))
          (add-s1-defines ds (add1 n)))]))

;; Compile a lambda (for closures in heap)
(define (compile-lam f xs e c)
  (let ((fvs (fv (Lam f xs e))))
    (seq (Lea t0 (symbol->label f))
         (Sd t0 (Offset s1 0))
         (free-vars-to-heap fvs c 8)
         (Mov t0 s1)
         (Xor t0 type-proc)
         (Addi s1 s1 (* 8 (add1 (length fvs)))))))

(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq (Mov t2 (Offset s0 (lookup x c)))
          (Sd t2 (Offset s1 off))
          (free-vars-to-heap fvs c (+ off 8)))]))
