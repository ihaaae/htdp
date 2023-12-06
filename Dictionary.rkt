#lang racket
(require lang/htdp-intermediate-lambda)
(require 2htdp/batch-io)
(require test-engine/racket-tests)
(require racket/trace)

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
  (match dict
    ['() 0]
    [(cons head tail)
     (let ([dict-letter (substring head 0 1)]
           [result (start-with# l tail)])
       (if (string-ci=? dict-letter l)
           (+ 1 result)
           result))]))

(define dict (list "apple" "after" "ban"))
(check-expect (start-with# "a" dict) 2)
(check-expect (start-with# "c" dict) 0)

(struct letter-count (letter count) #:transparent)

;[list-of string] [list-of string] -> [list-of letter-count]
;letter-count-list whose letter in _letters_ start the word in _dict_
(define (count-by-letter-help letters dict)
  (match letters
    ['() '()]
    [(cons head tail)
     (let ([count (start-with# head dict)]
           [result (count-by-letter-help tail dict)])
       (if (= count 0)
           result
           (cons (letter-count head count) result)))]))

;dictionary -> [list-of letter-count]
;list of letter-count for count showed in _dict_
(define (count-by-letter dict)
  (count-by-letter-help LETTERS dict))

(check-expect (count-by-letter '()) '())
(check-expect (count-by-letter (list "apple")) (list (letter-count "a" 1)))
(check-expect (count-by-letter dict) (list (letter-count "a" 2) (letter-count "b" 1)))

(define (lc>=? one two)
  (>= (letter-count-count one) (letter-count-count two)))

;[NElist-of letter-count] -> letter-count
;the letter-count in _lcs_ whose count is bigger 
(define (most-frequent-help lcs)
  (match lcs
    [(cons head '()) head]
    [(cons head tail)
     (let ([result (most-frequent-help tail)])
       (if (lc>=? head result) head result))]))

;dictionary -> letter-count
;letter-count most frequent letter that start-with words in _dict_
(define (most-frequent dict)
  (match dict
    ['() '()]
    (most-frequent-help (count-by-letter dict))))

(check-expect (most-frequent (list "banana")) (letter-count "b" 1))
(check-expect (most-frequent dict) (letter-count "a" 2))

;Dictionary -> [list-of Dictionary]
(define (words-start-with l dict)
  (match dict
    ['() '()]
    [(cons head tail)
     (let ([result (words-start-with l tail)])
       (if (string-ci=? l (string-ith head 0))
           (cons head result)
           result))]))

(define (words-by-first-letter dict)
  (words-by-first-letter-help LETTERS dict))

(define (words-by-first-letter-help letters dict)
  (match letters
    ['() '()]
    [(cons head tail)
     (let ([num (start-with# head dict)]
           [result (words-by-first-letter-helper tail dict)])
       (if (== num 0)
           result
           (cons (words-start-with head dict) result)))]))

(check-expect (words-by-first-letter (list "apple" "after" "banana"))
              (list (list "apple" "after") (list "banana")))

(define (most-frequent.v2-help dics)
  (match dics
    ['() '()]
    [(cons head '()) (letter-count (string-ith (first head) 0) (length head))]
    [(cons head tail)
     (let ([result (most-frequent.v2-help tail)])
       (if (>= (length head) (letter-count-count result))
           (letter-count (string-ith (first head) 0) (length head))
           result))]))

(define (most-frequent.v2 dict)
  (most-frequent.v2-help (words-by-first-letter dict)))

(check-expect (most-frequent.v2 (list "banana")) (letter-count "b" 1))
(check-expect (most-frequent.v2 dict) (letter-count "a" 2))

(test)
