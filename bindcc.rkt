#lang plai

(define-type KCFAE
  [num (n number?)]
  [id (v symbol?)]
  [add (l KCFAE?) (r KCFAE?)]
  [fun (arg symbol?) (body KCFAE?)]
  [app (fe KCFAE?) (ae KCFAE?)]
  [if0 (c KCFAE?) (t KCFAE?) (e KCFAE?)]
  [bindcc (name symbol?) (body KCFAE?)])

(define-type KCFAE-V
  [numV (n number?)]
  [closureV (p procedure?)]
  [contV (p procedure?)])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value KCFAE-V?) (next Env?)])

(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'lookup "unbound variable")]
    [anEnv (this value next)
           (if (symbol=? name this)
               value
               (lookup name env))]))

(define (num+ lv rv)
  (numV (+ (numV-n lv) (numV-n rv))))

(define (num-zero? n)
  (zero? (numV-n n)))

(define (interp e env k)
  (type-case KCFAE e
    [num (n) (k (numV n))]
    [id (v) (k (lookup v env))]
    [fun (param body)
         (k (closureV (lambda (av dyn-k)
                        (interp body (anEnv param av env) dyn-k))))]
    [add (l r)
         (interp l
                 env
                 (lambda (lv)
                   (interp r
                           env
                           (lambda (rv)
                             (k (num+ lv rv))))))]
    [if0 (c t e)
         (interp c
                 env
                 (lambda (cv)
                   (if (num-zero? cv)
                       (interp t env k)
                       (interp e env k))))]
    [app (fe ae)
         (interp fe
                 env
                 (lambda (fv)
                   (interp ae
                           env
                           (lambda (av)
                             (type-case KCFAE-V fv
                               [closureV (p) (p av k)]
                               [contV (p) (p av)]
                               [else (error 'interp "non-applicable")])))))]
    [bindcc (name body)
            (interp body
                    (anEnv name
                           (contV k)
                           env)
                    k)]))

(define (interp-test e)
  (begin (interp e
                 (mtEnv)
                 (lambda (v) v))))

(interp-test (bindcc 'k (num 3)))
(interp-test (bindcc 'k (app (id 'k) (num 3))))
(interp-test (bindcc 'k (add (num 1) (app (id 'k) (num 3)))))
(interp-test (add (num 1) (bindcc 'k (add (num 1) (app (id 'k) (num 3))))))
(interp-test (app (bindcc 'k (app (id 'k) (fun 'dummy (num 3))))
                  (num 1729)))
(interp-test (bindcc 'k (app (id 'k) (app (id 'k) (app (id 'k) (num 3))))))
(interp-test (app (app (bindcc 'k (id 'k)) (fun 'x (id 'x))) (num 3)))