;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |2.195|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/batch-io)

(define LOCATION "words.txt")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

;letter dictionary -> number
(define (start-with# l dict)
  (cond
    [(empty? dict) 0]
    [else (+
           (if (string-ci=? (string-ith (first dict) 0) l)
               1
               0)
           (start-with# l (rest dict)))]))

(check-expect (start-with# "e" AS-LIST) 576)
