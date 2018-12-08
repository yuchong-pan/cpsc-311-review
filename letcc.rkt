#lang plai

;; Escapers
(let/cc k
  (k 3))

(+ 1
   (let/cc k
     (k 3)))

;; Exception
(define (f n)
  (+ 10
     (* 5
        (let/cc k
          (/ 1 (if (zero? n)
                   (k 1)
                   n))))))
(+ 3 (f 0))

;; Producers and Consumers
(define route-producer
  (local [(define resume (box false))]
    (lambda (real-send)
      (local [(define send-to (box real-send))
              (define send (lambda (value-to-send)
                             (set-box! send-to
                                       (let/cc k
                                         (begin
                                           (set-box! resume k)
                                           ((unbox send-to) value-to-send))))))]
        (if (unbox resume)
            ((unbox resume) real-send)
            (begin
              (send 'providence)
              (send 'houston)
              (send 'bangalore)))))))
(define (get producer)
  (let/cc k (producer k)))

(list (get route-producer)
      (get route-producer)
      (get route-producer))

(define (route-producer-body send)
  (begin
    (send 'providence)
    (send 'houston)
    (send 'bangalore)))

(define (general-producer body)
  (local [(define resume (box false))]
    (lambda (real-send)
      (local [(define send-to (box real-send))
              (define send (lambda (value-to-send)
                             (set-box! send-to
                                       (let/cc k
                                         (begin
                                           (set-box! resume k)
                                           ((unbox send-to) value-to-send))))))]
        (if (unbox resume)
            ((unbox resume) real-send)
            (body send))))))

(define (odds-producer-body send)
  (local [(define (loop n)
            (begin
              (send n)
              (loop (+ n 2))))]
    (loop 1)))

(define odds-producer (general-producer odds-producer-body))

(+ (get odds-producer)
   (get odds-producer)
   (get odds-producer)
   (get odds-producer)
   (get odds-producer))