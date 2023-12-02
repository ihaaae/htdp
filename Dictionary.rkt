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

(struct letter-count (letter count) #:transparent)


;dictionary -> [list-of letter-count]
;list of letter-count for count showed in _dict_
(define (count-by-letter dict)
  (match dict
    ['() '()]
    [(cons head '()) (cons (letter-count (string-ith head 0) 1) '())]
    [(cons head tail)
    (match-let* ([result (count-by-letter tail)]
                 [dict-letter (string-ith head 0)]
                 [(cons (letter-count lc-letter lc-count) result-tail) result])
      (if (string=? dict-letter lc-letter)
          (cons (letter-count lc-letter (+ lc-count 1))
                result-tail)
          (cons (letter-count dict-letter 1)
                result)))]))

(check-expect (count-by-letter '()) '())
(check-expect (count-by-letter (list "apple")) (list (letter-count "a" 1)))
(check-expect (count-by-letter dict) (list (letter-count "a" 2) (letter-count "b" 1)))

(test)
