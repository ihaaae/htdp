#lang racket
(require lang/htdp-intermediate-lambda)
(require 2htdp/batch-io)
(require test-engine/racket-tests)

(define LOCATION "words.txt")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

;letter dictionary -> number
;the number of words in _dict_ that start with Letter _l_
(define (start-with# l dict)
  (cond
    [(empty? dict) 0]
    [else (+
           (if (string-ci=? (substring (first dict) 0 1) l)
               1
               0)
           (start-with# l (rest dict)))]))

(define dict (list "apple" "after" "ban"))
(check-expect (start-with# "a" dict) 2)
(check-expect (start-with# "c" dict) 0)
(test)
