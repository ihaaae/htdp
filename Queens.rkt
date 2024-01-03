#lang racket
(require rackunit)

(struct posn [x y])

(define QUEENS 8)
; A QP is a structure:
;   (posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

;; Exercise 479
;QP QP -> Boolean
;; whether ~qpa~ on the lines ~qpb~ could radiate
(define (threatening? qpa qpb)
  (cond
    [(= (posn-x qpa) (posn-x qpb)) #t]
    [(= (posn-y qpa) (posn-y qpb)) #t]
    [(= (+ (posn-x qpa) (posn-y qpa)) (+ (posn-x qpb) (posn-y qpb))) #t]
    [(= (- (posn-x qpa) (posn-y qpa)) (- (posn-x qpb) (posn-y qpb))) #t]
    [else #f]))

(module+ test
  (define 2-3 (posn 2 3))
  (define 2-4 (posn 2 5))
  (define 6-3 (posn 6 3))
  (define 3-2 (posn 3 2))
  (define 3-4 (posn 3 4))
  (check-equal? (threatening? 2-3 2-4) #true)
  (check-equal? (threatening? 2-3 6-3) #true)
  (check-equal? (threatening? 2-3 3-2) #true)
  (check-equal? (threatening? 2-3 3-4) #true))

;; Exercise 481
;; [List-of QP] -> Boolean
;; N -> [[List-of QP] -> Boolean]
(define (n-queens-solution? n)
  ;; QP [List-of QP] -> Boolean
  (define (threatening?/list qp-one qp-list)
    (ormap (lambda (qp-two) (threatening? qp-one qp-two)) qp-list))
  (lambda (qp-list)
    (cond
      [(not (= (length qp-list) n)) #false]
      [else (not (ormap (lambda (qp-one)
                          (threatening?/list qp-one (remove qp-one qp-list)))
                        qp-list))])))

(module+ test
  (define 0-1 (posn 0 1))
  (define 1-3 (posn 1 3))
  (define 2-0 (posn 2 0))
  ;; (define 3-2 (make-posn 3 2))
  (check-equal? ((n-queens-solution? 4) (list 0-1 1-3 2-0 3-2)) #true))

