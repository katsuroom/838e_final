#lang racket

(let ((x (cons 4 5)))
    (let ((y (box 6)))
        (box (car x))
    )
)