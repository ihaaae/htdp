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
 
;; (define (atom? s) (number? s))
;; (define WRONG "wrong input")
;; 
;; ; S-expr -> BSL-expr
;; (define (parse s)
;;   (cond
;;     [(atom? s) (parse-atom s)]
;;     [else (parse-sl s)]))
;; 
;; (check-expect (parse 1) 1)
;; (check-expect (parse '(+ 1 2)) (make-add 1 2))
;;  
;; ; SL -> BSL-expr 
;; (define (parse-sl s)
;;   (cond
;;     [(and (consists-of-3 s) (symbol? (first s)))
;;      (cond
;;        [(symbol=? (first s) '+)
;;         (make-add (parse (second s)) (parse (third s)))]
;;        [(symbol=? (first s) '*)
;;         (make-mul (parse (second s)) (parse (third s)))]
;;        [else (error WRONG)])]
;;     [else (error WRONG)]))
;; 
;; (check-expect (parse-sl '(+ 1 2)) (make-add 1 2))
;; (check-expect (parse-sl '(* 1 2)) (make-mul 1 2))
;; (check-error (parse-sl '(- 1 2)))
;; (check-error (parse-sl '(+ 1 2 3)))