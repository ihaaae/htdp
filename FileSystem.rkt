#lang racket
(require rackunit)
(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

;; Exercise 330
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

(define hang "hang")
(define draw "draw")
(define read! "read!")
(define Docs (cons read! '()))
(define Code (cons hang (cons draw '())))
(define Libs (cons Docs (cons Code '())))
(define read!-1 "read!")
(define part1 "part1")
(define part2 "part2")
(define part3 "part3")
(define Text (cons part1 (cons part2 (cons part3 '()))))
(define TS (cons Text (cons read! (cons Libs '()))))

;; Exercise 331
;; Dir.v1 -> Number
;; return the number of files in dir
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(string? (first dir)) (+ 1 (how-many (rest dir)))]
    [else (+ (how-many (first dir))
             (how-many (rest dir)))]))

(module+ test
  (check-equal? (how-many Libs) 3)
  (check-equal? (how-many TS) 7))