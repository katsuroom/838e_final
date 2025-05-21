#lang racket
;; compile-stdin.rkt
;; 从 stdin 读取 Racket 源码，编译成 x86-AST，再调用 printer 打印汇编

(provide main)
(require "parse.rkt"
         "compile.rkt"
         "read-all.rkt"
         "printer.rkt")

;; -> Void
;; Compile contents of stdin and emit asm code on stdout
(define (main)
  (read-line)  ;  #lang racket
  (define sexprs (read-all))
  (define parsed (apply parse-closed sexprs))
  ;; (printf ";; DEBUG read-all ⇒ ~s\n" sexprs)
  ;; (printf ";; DEBUG parse-closed ⇒ ~s\n" parsed)
  (asm-display (compile parsed)))

;; 让本模块可直接运行
;; (module+ main
;;   (main))


