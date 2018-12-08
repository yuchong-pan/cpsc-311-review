#lang plai

(define mk-fact
  (lambda (f)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((f f) (sub1 n)))))))

(define fix
  (lambda (p)
    ((lambda (f)
       (f f))
     (lambda (f)
       (p (lambda (x) ((f f) x)))))))

(define fact
  (fix
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n (fact (sub1 n))))))))