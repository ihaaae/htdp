#lang racket

(require rackunit)

(struct db (schema content))
; A DB is a structure: (db Schema Content)

; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions

; integrity constraint In (db sch con),
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

;; Name    Age     Present
;; String  Integer Boolean
;; "Alice" 35      #true
;; "Bob"   25      #false
;; "Carol" 30      #true
;; "Dave"  32      #false

(define school-schema
  `(("Name" ,string?)
    ("Age"  ,integer?)
    ("Present" ,boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob" 25 #false)
    ("Carol" 30 #true)
    ("Dave" 32 #false)))

(define school-db (db school-schema school-content))

;; Present Description
;; Boolean String
;; #true   "presence"
;; #false  "absence"

(define presence-schema
  `(("Present" ,boolean?)
    ("Description" ,string?)))

(define presence-content
  `((#true "presence")
    (#false "absence")))

(define presence-db (db presence-schema presence-content))

;; Exercise 403
(struct spec (label predicat))

(define school-schema/a
  (list (spec "Name" string?)
        (spec "Age" integer?)
        (spec "Present" boolean?)))

(define school-db/a (db school-schema/a school-content))

(define presence-schema/a
  (list (spec "Present" boolean?)
        (spec "Description" string?)))

(define presence-db/a (db presence-schema/a presence-content))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)

(module+ test
  (check-equal? (integrity-check school-db) #true)
  (check-equal? (integrity-check presence-db) #true))

(define (integrity-check db)
  #false)

;; Exercise 404
; [X Y] (X Y -> Boolean) [X] [Y] -> Boolean
; if all results of (~pred~ X Y) is true, return #t;
; otherwise #f
; _assume_ the two lists are of equal length
(define (andmap2 pred list-one list-two)
  (cond
    [(empty? list-one) #t]
    [else
     (if (pred (first list-one) (first list-two))
         (andmap2 pred (rest list-one) (rest list-two))
         #f)]))

(module+ test
  (define x-one (list 1 2 3))
  (define y-one (list "a" "ab" "abc"))
  (check-equal? (andmap2 (lambda (x y) (= x (string-length y))) x-one y-one) #t))