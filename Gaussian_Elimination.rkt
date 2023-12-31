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

; X [list-of X] [list-of X] -> Number
; _assume_ (length solution) >= (length lhs-equaiton)
(define (plug-in lhs-equation solution)
  (define (plug-in-reverse r-lhs-equation r-solution)
    (cond
      [(empty? r-lhs-equation) 0]
      [else (+ (* (first r-lhs-equation) (first r-solution))
               (plug-in-reverse (rest r-lhs-equation) (rest r-solution)))]))
  (plug-in-reverse (reverse lhs-equation) (reverse solution)))

; SOE Solution -> boolean
(define (check-solution equations solution)
  (cond
    [(empty? equations) #t]
    [else (if (= (rhs (first equations))
                 (plug-in (lhs (first equations)) solution))
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

;; Exercise 464
(define triangular-M.v2
 (list (list 2  2  3   10)
       (list    3  9   21)
       (list   -3 -8  -19)))

(module+ test
  (check-equal? (check-solution triangular-M.v2 S) #t))

;; Exercise 465
; equation equation -> equation
; return a modified equation-one so that it starts with 0
; _assume_ ~equation-one~ and ~equation-two~ has the same length
(define (subtract equation-one equation-two)
  (cond
    [(= 0 (first equation-one)) (rest equation-one)]
    [else
     (define times (/ (first equation-one) (first equation-two)))
     (rest (foldr (lambda (one two r)
                    (cons (- one (* times two)) r))
                  '()
                  equation-one equation-two))]))

(module+ test
  (check-equal? (subtract (list 2 5 12 31) (list 2 2 3 10))
                (list 3 9 21)))

;; Exercise 466
; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length:
;   n + 1, n, n - 1, ..., 2.
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations
(define (triangulate M)
  (cond
    [(empty? (rest M)) M]
    [else (cons (first M) (triangulate (map (lambda (e) (subtract e (first M)))
                                            (rest M))))]))

(module+ test
  (define M-one (list (list 1 2)))
  (define TM-one (list (list 1 2)))
  (check-equal? (triangulate M-one) TM-one)
  (define M-two (list (list 3 9 21) (list -3 -8 -19)))
  (define TM-two (list (list 3 9 21) (list 1 2)))
  (check-equal? (triangulate M-two) TM-two)
  (check-equal? (triangulate M) triangular-M))

;; Exercise 467
(define (triangulate.v2 M)
  ; SOE -> SOE
  ; _generative_
  (define (keep-rotate M)
    (cond
      [(not (= 0 (first (first M)))) M]
      [else (keep-rotate (append (rest M) (list (first M))))]))
  (cond
    [(empty? (rest M)) M]
    [else (let ([rotated-M (keep-rotate M)])
            (cons (first rotated-M)
                  (triangulate.v2 (map (lambda (e) (subtract e (first rotated-M)))
                                       (rest rotated-M)))))]))

(module+ test
  (define original-M (list (list   0 -5  -5)
                           (list  -8 -4 -12)))
  (define rotated-M (list (list  -8 -4 -12)
                          (list   0 -5  -5)))
  (check-equal? (triangulate.v2 (list (list 2  3  3 8)
                                      (list 2  3 -2 3)
                                      (list 4 -2  2 4)))
                (list (list 2  3  3   8)
                      (list   -8 -4 -12)
                      (list      -5  -5))))

;; Exercise 468
(define (triangulate.v3 M)
  ; SOE -> SOE
  ; _generative_
  (define (keep-rotate M)
    (cond
      [(not (= 0 (first (first M)))) M]
      [else (keep-rotate (append (rest M) (list (first M))))]))
  (cond
    [(andmap (lambda (e) (= 0 (first e))) M) (error "no solution")]
    [(empty? (rest M)) M]
    [else (let ([rotated-M (keep-rotate M)])
            (cons (first rotated-M)
                  (triangulate.v3 (map (lambda (e) (subtract e (first rotated-M)))
                                       (rest rotated-M)))))]))

(module+ test
  (define no-solution-M (list (list 2 2 2 6)
                              (list 2 2 4 8)
                              (list 2 2 1 2))))

;; Exercise 409
;; TM -> solution
(define (solve TM-one)
  (let ([current-lhs (lhs (first TM-one))]
        [current-rhs (rhs (first TM-one))])
  (cond
    [(empty? (rest TM-one)) (list (/ current-rhs
                                     (first current-lhs)))]
    [else (let ([rest-solution (solve (rest TM-one))])
            (cons (/ (- current-rhs
                        (plug-in rest-solution (rest current-lhs)))
                     (first current-lhs))
                  rest-solution))])))

(module+ test
  (check-equal? (solve triangular-M) S))

;; Exercise 409
;; TM -> solution
(define (solve.v2 TM-one)
  (foldr (lambda (e r) (cons (/ (- (rhs e)
                                   (plug-in r (rest (lhs e))))
                                (first (lhs e))) r))
         '()
         TM-one))

(module+ test
  (check-equal? (solve.v2 triangular-M) S))

;; Exercise 410
;; SOE -> solution
(define (gauss M-one)
  (solve.v2 (triangulate M-one)))

(module+ test
  (check-equal? (gauss M) S))