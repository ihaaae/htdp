#lang racket
(require rackunit)

;; Exercise 345
;bsl is
; mul
; add
; Number
(struct add (left right))
(struct mul (left right))

;; Exercise 346
;; BSL-value is
;; Number

;; Exercise 347
;; BSL -> BSL-value
;; ...
(define (eval-expression bsl-expr)
  (cond
    [(number? bsl-expr) bsl-expr]
    [(mul? bsl-expr) (* (eval-expression (mul-left bsl-expr))
                        (eval-expression (mul-right bsl-expr)))]
    [(add? bsl-expr) (+ (eval-expression (add-left bsl-expr))
                        (eval-expression (add-right bsl-expr)))]))

(module+ test
  (define be1 (add (mul -2 -3) 33))
  (check-equal? (eval-expression be1) 39))

;; Exercise 348
;; Boolean BSL is
;; Boolean
;; (And Boolean)
;; (Or Boolean)
;; (Not Boolean)
(struct And [left right])
(struct Or [left right])
(struct Not [value])

;; Boolean-value is
;; Boolean

(define (eval-bool-expression bsl-bool)
  (cond
    [(boolean? bsl-bool) bsl-bool]
    [(And? bsl-bool) (and (eval-bool-expression (And-left bsl-bool))
                          (eval-bool-expression (And-right bsl-bool)))]
    [(Or? bsl-bool) (or (eval-bool-expression (Or-left bsl-bool))
                          (eval-bool-expression (Or-right bsl-bool)))]
    [(Not? bsl-bool) (not (eval-bool-expression (Not-value bsl-bool)))]))

(module+ test
  (define bb1 (Or (Not #true) (And #false #false)))
  (check-equal? (eval-bool-expression bb1) #false))

;; Exercise 349, 350
(define (atom? s) (number? s))
(define WRONG "wrong input")

;; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

(module+ test
 (check-equal? (parse 1) 1)
 (check-equal? (parse '(+ 1 2)) (add 1 2)))
  
; SL -> BSL-expr 
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))

(module+ test
  (check-equal? (parse-sl '(+ 1 2)) (add 1 2))
  (check-equal? (parse-sl '(* 1 2)) (mul 1 2)))

; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(module+ test
  (check-equal? (parse-atom 1) 1))

;; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

;; Exercise 351
;; S-expr -> BSL-value
(define (interpreter-expr a-sexpr)
  (eval-expression (parse a-sexpr)))

;; Exercise 352
; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

;BSL-var-expr symbol number -> BSL-var-expr
(define (subst ex x v)
  (cond
    [(symbol? ex) (if (symbol=? x ex) v ex)]
    [(number? ex) ex]
    [(add? ex) (add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]
    [else (error WRONG)]))

(module+ test
  (define bve1 (add (mul 'x 2) 1))
  (check-equal? (subst 'x 'x 3) 3)
  (check-equal? (subst 'x 'y 2) 'x)
  (check-equal? (subst 2 'y 3) 2)
  (check-equal? (subst bve1 'x 3) (add (mul 3 2) 1)))

;; Exercise 353
;BSL-var-expr -> Boolean
(define (numeric? ex)
  (cond
    [(symbol? ex) #false]
    [(number? ex) #true]
    [(add? ex) (and (numeric? (add-left ex)) (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex)) (numeric? (mul-right ex)))]))

(module+ test
  (check-equal? (numeric? be1) #true)
  (check-equal? (numeric? bve1) #false))

;; Exercise 354
;BSL-var-expr -> Number
(define (eval-variable bsl-var-expr)
  (if (numeric? bsl-var-expr)
      (eval-expression bsl-var-expr)
      (error WRONG)))

;; BSL-var-expr AL -> Number
(define (eval-variable* ex da)
  (define (subst/al ex al)
    (match al
      ['() ex]
      [(cons head tail) (subst/al (subst ex (first head) (second head)) tail)]))
  (eval-variable (subst/al ex da)))

(module+ test
  (check-equal? (eval-variable* bve1 (list (list 'x 3))) 7))
