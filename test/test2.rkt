#lang racket

(define (equal x y)
    (if (= x y) #t #f))

(equal 2 2)