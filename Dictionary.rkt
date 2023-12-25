#lang racket
;(require lang/htdp-intermediate-lambda)
(require 2htdp/batch-io)
(require test-engine/racket-tests)
(require racket/trace)

;(: explode (-> String (Listof String)))
; a list of 1String in _string_
(define (explode string)
  (cond
    [(= 1 (string-length string)) (list (substring string 0 1))]
    [else (cons (substring string 0 1) (explode (substring string 1)))]))
; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS
   (explode "abcdefghijklmnopqrstuvwxyz"))

;(: start-with# (-> String (Listof String)))
; number of words in dict that starts with _l_
(define (start-with# letter dict)
  (define (start-with? word)
    (string-ci=? (substring word 0 1) letter))
  (length (filter start-with? dict)))

(define dict-one (list "apple" "after" "ban"))
(check-expect (start-with# "a" dict-one) 2)
(check-expect (start-with# "c" dict-one) 0)

(struct letter-count (letter count) #:transparent)
;dictionary -> [list-of letter-count]
;list of letter-count for count showed in _dict_
(define (count-by-letter dict)
  (define (exist? letter)
    (not (= 0 (start-with# letter dict))))
  (define (letter-count-by-letter letter)
    (letter-count letter (start-with# letter dict)))
  (map letter-count-by-letter (filter exist? LETTERS)))

(check-expect (count-by-letter '()) '())
(check-expect (count-by-letter (list "apple")) (list (letter-count "a" 1)))
(check-expect (count-by-letter dict-one) (list (letter-count "a" 2) (letter-count "b" 1)))

;[NElist-of letter-count] -> letter-count
;the letter-count in _lcs_ whose count is bigger
(define (most-frequent-letters list-letcounts)
  (define (letcount>? letcount-a letcount-b)
    (> (letter-count-count letcount-a) (letter-count-count letcount-b)))
  (first (sort list-letcounts letcount>?)))

(check-expect (most-frequent-letters (list (letter-count "a" 2))) (letter-count "a" 2))
(check-expect (most-frequent-letters (list (letter-count "a" 2) (letter-count "b" 3)))
              (letter-count "b" 3))

;dictionary -> [Maybe letter-count]
;letter-count most frequent letter that start-with words in _dict_
(define (most-frequent dict)
  (cond
    [(empty? dict) #f]
    [else (most-frequent-letters (count-by-letter dict))]))

(check-expect (most-frequent (list "banana")) (letter-count "b" 1))
(check-expect (most-frequent dict-one) (letter-count "a" 2))

(define (words-by-first-letter-help letters dict)
  (define (exist? letter)
    (not (= 0 (start-with# letter dict))))
  (define (words-start-with letter)
    (filter (lambda (word)
              (string-ci=? (substring word 0 1) letter))
            dict))
  (map words-start-with (filter exist? letters)))

(check-expect (words-by-first-letter-help (list "a" "c") dict-one) (list (list "apple" "after")))
(check-expect (words-by-first-letter-help (list "a" "b" "c") dict-one) (list (list "apple" "after") (list "ban")))

; Dictionary -> [list-of Dictionary]
(define (words-by-first-letter dict)
  (words-by-first-letter-help LETTERS dict))

(check-expect (words-by-first-letter (list "apple" "after" "banana"))
              (list (list "apple" "after") (list "banana")))

;; dictionary -> [Maybe letter-count]
(define (most-frequent.v2 Dict)
  (define (greatest-count-dict dict biggest)
    (cond
      [(boolean? biggest)
       (letter-count (substring (first dict) 0 1) (length dict))]
      [(>= (length dict) (letter-count-count biggest))
       (letter-count (substring (first dict) 0 1) (length dict))]
      [else biggest]))
  (foldr greatest-count-dict #f (words-by-first-letter Dict)))

(check-expect (most-frequent.v2 (list "banana")) (letter-count "b" 1))
(check-expect (most-frequent.v2 dict-one) (letter-count "a" 2))

(test)
