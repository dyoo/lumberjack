#lang racket/base

(define (f x)
  (* x x))

(define (g x)
  (* (f x) (h x)))

(define (h x)
  (+ x 2))

(printf "Running!\n")
(f (g (h 3)))