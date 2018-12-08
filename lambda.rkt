#lang plai

;; pair
(define (pair A B)
  (lambda (selector) (selector A B)))

(define left (lambda (A B) A))
(define right (lambda (A B) B))

(define (my-first e) (e left))
(define (my-rest e) (e right))

;; if
(define (my-if C T F) (C T F))
(define yes (lambda (T F) T))
(define no (lambda (T F) F))

;; numbers
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(define sum
  (lambda (m)
    (lambda (n)
      ((n succ) m))))

(define prod
  (lambda (m)
    (lambda (n)
      ((n (sum m)) zero))))

(define pred
  (lambda (n)
    (left ((n (lambda (p)
                (pair (rest p)
                      (succ (rest p)))))
           (pair zero zero)))))

(define iszero
  (lambda (n)
    ((n (lambda (ignore) no)) yes)))