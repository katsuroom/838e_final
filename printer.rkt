#lang racket
(require a86/ast)
(provide/contract
 [asm-display (-> (listof instruction?) any)])

;; ------------------------------------------------------------------
;; Register name mapping: internal symbols → RISC-V names
;; ------------------------------------------------------------------
(define reg-map
  (hash
   [t0  "x5"]   ; result / general
   [t1  "x6"]   ; scratch
   [t2  "x7"]   ; scratch
   [t3  "x28"]  ; stack pad
   [t4  "x29"]  ; scratch (for op3)
   [a0  "x10"]  ; arg0
   [a1  "x11"]  ; arg1
   [a2  "x12"]  ; arg2
   [s0  "x8"]   ; frame pointer / sp
   [s1  "x9"])) ; heap pointer

(define tab (make-string 8 #\space))

;; ------------------------------------------------------------------
;; Comment formatting (unchanged)
;; ------------------------------------------------------------------
(define (comment->string c)
  (match c
    [(% s)   (string-append (make-string 32 #\space) "; " s)]
    [(%% s)  (string-append tab ";; " s)]
    [(%%% s) (string-append ";;; " s)]))

(define current-extern-labels (make-parameter '()))

;; ------------------------------------------------------------------
;; Label and section formatting (unchanged)
;; ------------------------------------------------------------------
(define label-symbol->string
  (match (system-type 'os)
    ['macosx (λ (s) (string-append "$_" (symbol->string s)))]
    [_
     (λ (s)
       (if (and (current-shared?) (memq s (current-extern-labels)))
           (string-append "$" (symbol->string s) " wrt ..plt")
           (string-append "$" (symbol->string s))))]))

(define extern-label-decl-symbol->string
  (match (system-type 'os)
    ['macosx (λ (s) (string-append "$_" (symbol->string s)))]
    [_
     (λ (s)
       (string-append "$" (symbol->string s)))]))

(define (text-section n)
  (match (system-type 'os)
    ['macosx (format "section __TEXT,~a align=16" n)]
    [_       (format "section ~a progbits alloc exec nowrite align=16" n)]))

(define (data-section n)
  (match (system-type 'os)
    ['macosx (format "section __DATA,~a align=8" n)]
    [_       (format "section ~a progbits alloc noexec write align=8" n)]))

;; ------------------------------------------------------------------
;; Convert an instruction to a simple string
;; ------------------------------------------------------------------
(define (common-instruction->string i)
  (let ((as (instruction-args i)))
    (string-append tab
                   (instruction-name i)
                   (apply string-append
                          (if (empty? as) "" " ")
                          (add-between (map arg->string as)
                                       ", ")))))

(define (fancy-instr->string i)
  (let ((s (simple-instr->string i)))
    (if (instruction-annotation i)
        (if (< (string-length s) 40)
            (format "~a~a; ~.s"
                    s
                    (make-string (- 40 (string-length s)) #\space)
                    (instruction-annotation i))
            (format "~a ; ~.s"
                    s
                    (instruction-annotation i)))
        s)))

;; ------------------------------------------------------------------
;; Argument → String (modified for RISC-V)
;; ------------------------------------------------------------------
(define (arg->string e)
  (match e
    ;; Register
    [(? register?) (hash-ref reg-map e (symbol->string e))]
    ;; Memory access: Offset base disp → "disp(base)"
    [(Offset base disp)
     (string-append (number->string disp)
                    "("
                    (arg->string base)
                    ")")]
    ;; Fallback to expression
    [_ (exp->string e)]))

;; ------------------------------------------------------------------
;; Expression → String (unchanged)
;; ------------------------------------------------------------------
(define (exp->string e)
  (match e
    [(? register?) (symbol->string e)]
    [(? integer?) (number->string e)]
    [($ x)          (label-symbol->string x)]
    [(list '? e1 e2 e3)
     (string-append "(" (exp->string e1)
                    " ? " (exp->string e2)
                    " : " (exp->string e3) ")")]
    [(list (? exp-unop? o) e1)
     (string-append "(" (symbol->string o)
                    " " (exp->string e1) ")")]
    [(list (? exp-binop? o) e1 e2)
     (string-append "(" (exp->string e1)
                    " " (symbol->string o)
                    " " (exp->string e2) ")")]))
                    
;; ------------------------------------------------------------------
;; Simple instruction → String (unchanged)
;; ------------------------------------------------------------------
(define (simple-instr->string i)
  (match i
    [(Text)         (string-append tab "section .text")]
    [(Text n)       (string-append tab (text-section n))]
    [(Data)         (string-append tab "section .data align=8")]
    [(Data n)       (string-append tab (data-section n))]
    [(Extern ($ l)) (string-append tab "extern " (extern-label-decl-symbol->string l))]
    [(Label ($ l))  (string-append (label-symbol->string l) ":")]
    [(Lea d e)
     (string-append tab "lea "
                    (arg->string d) ", [rel "
                    (arg->string e) "]")]
    [(Equ x c)
     (string-append tab
                    (symbol->string x)
                    " equ "
                    (number->string c))]
    [(Db (? bytes? bs))
     (apply string-append tab "db " (add-between (map number->string (bytes->list bs)) ", "))]
    [_ (common-instruction->string i)]))

;; ------------------------------------------------------------------
;; Display a list of instructions with comments (unchanged)
;; ------------------------------------------------------------------
(define (instrs-display a)
  (match a
    ['() (void)]
    [(cons (? Comment? c) a)
     (begin (write-string (comment->string c))
            (write-char #\newline)
            (instrs-display a))]
    [(cons i (cons (% s) a))
     (begin (write-string (fancy-instr->string i))
            (write-char #\newline)
            (instrs-display a))]
    [(cons i a)
     (begin (write-string (fancy-instr->string i))
            (write-char #\newline)
            (instrs-display a))]))

;; ------------------------------------------------------------------
;; Entry point: Asm → Void (unchanged)
;; ------------------------------------------------------------------
(define (extern-labels a)
  (match a
    ['() '()]
    [(cons (Extern ($ l)) a)
     (cons l (extern-labels a))]
    [(cons _ a)
     (extern-labels a)]))

(define (asm-display a)
  (define (go)
    (match (findf Label? a)
      [(Label g)
       (begin
         (write-string (string-append
                        tab "default rel\n"
                        tab "section .text\n"))
         (instrs-display a))]
      [_ (instrs-display a)]))
  (parameterize ([current-extern-labels (extern-labels a)])
    (go)))
