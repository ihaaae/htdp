#lang racket

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