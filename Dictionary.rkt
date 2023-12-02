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

(define-struct letter-count [letter count])
(define (update-letter-counts lcs)
  (cons
   (make-letter-count
    (letter-count-letter (first lcs))
    (+ 1 (letter-count-count (first lcs))))
   (rest lcs)))

(define olcs (list (make-letter-count "a" 1) (make-letter-count "b" 1)))
(define nlcs (list (make-letter-count "a" 2) (make-letter-count "b" 1)))

(check-expect (update-letter-counts olcs) nlcs)

;dictionary -> [list-of letter-count]
(define (count-by-letter dict)
  (cond
    [(empty? dict) '()]
    [else (let
              ([count-result (count-by-letter (rest dict))]
               [current-letter (string-ith (first dict) 0)])
              (cond
                [(empty? count-result)
                 (cons (make-letter-count current-letter 1) '())]
                [else
                 (let
                     ([current-result-letter (letter-count-letter (first count-result))])
                  (cond
                   [(string=? current-letter current-result-letter)
                    (update-letter-counts count-result)]
                   [else (cons (make-letter-count current-letter 1)
                               count-result)]))]))]))

(check-expect (count-by-letter '()) '())
(check-expect (count-by-letter (list "apple")) (list (make-letter-count "a" 1)))
(check-expect (count-by-letter dict) (list (make-letter-count "a" 2) (make-letter-count "b" 1)))

(test)
