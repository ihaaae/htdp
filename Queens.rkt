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
