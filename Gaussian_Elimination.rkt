#lang racket

(require rackunit)

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations

; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers.
; interpretation if (list a1 ... an b) is an Equation,
; a1, ..., an are the left-hand-side variable coefficients
; and b is the right-hand side

; A Solution is a [List-of Number]

(define M ; an SOE
  (list (list 2 2  3 10) ; an Equation
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(module+ test
  (check-equal? (lhs (first M)) '(2 2 3)))
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(module+ test
  (check-equal? (rhs (first M)) 10))
(define (rhs e)
  (first (reverse e)))

; SOE Solution -> boolean
(define (check-solution equations solution)
  ; X [list-of X] [list-of X] -> Number
  ; _assume_ (length solution) >= (length lhs-equaiton)
  (define (plug-in lhs-equation solution)
    (cond
      [(empty? lhs-equation) 0]
      [else (+ (* (first lhs-equation) (first solution))
               (plug-in (rest lhs-equation) (rest solution)))]))
  (cond
    [(empty? equations) #t]
    [else (if (= (rhs (first equations))
                 (plug-in (reverse (lhs (first equations))) (reverse solution)))
              (check-solution (rest equations) solution)
              #f)]))

(module+ test
  (check-equal? (check-solution M S) #t))

;; Exercise 463
(define triangular-M
  (list (list 2 2  3 10)
        (list   3  9 21)
        (list      1  2)))

(module+ test
  (check-equal? (check-solution triangular-M S) #t))
