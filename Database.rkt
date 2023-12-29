#lang racket

(require rackunit)
(require racket/trace)

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

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)

(module+ test
  (check-equal?
   (integrity-check (db (list (list "Present" boolean?)) (list (list "Alice")))) #false)
  (check-equal? (integrity-check school-db) #true)
  (check-equal? (integrity-check presence-db) #true))

(define (integrity-check db)
  (local (; Row -> Boolean
          ; does row satisfy (I1) and (I2)
          (define (row-integrity-check row)
            (and (= (length row)
                    (length (db-schema db)))
                 (andmap (lambda (s c) [(second s) c])
                         (db-schema db)
                         row))))
    (andmap row-integrity-check (db-content db))))

;; Exercise 405
(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))

(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))

(define projected-db
  (db projected-schema projected-content))
;  Stop! Read this test carefully. What's wrong?
(module+ test
  (check-equal?
   (db-content (project.v1 school-db '("Name" "Present")))
   projected-content))

(define (member? x ys)
  (if (boolean? (member x ys))
      #f
      #t))

(define (project.v1 db-one labels)
  (let ([schema  (db-schema db-one)]
        [content (db-content db-one)])
        ; Spec -> Boolean
        ; does this spec belong to the new schema
        (define (keep? c) (member? (first c) labels))
        ; Row -> Row
        ; retains those columns whose name is in labels
        (define (row-project row) (row-filter row (map first schema)))
        ; Row [List-of Label] -> Row
        ; retains those cells whose corresponding element
        ; in names is also in labels
        ;; _assume_ row and names has the same length
        (define (row-filter row names)
          (cond
            [(empty? row) '()]
            [else
             (if (member? (first names) labels)
                 (cons (first row)
                       (row-filter (rest row) (rest names)))
                 (row-filter (rest row) (rest names)))]))
    (db (filter keep? schema)
        (map row-project content))))

(module+ test
  (define row-one '("Alice" 35 #true))
  (define names-one '("Name" "present"))
  (define projected-row-one '("Alice" #true)))

;; Exercise 406
(define (project.v2 db-one labels)
  (let* ([schema  (db-schema db-one)]
         [content (db-content db-one)]
         [old-labels (map first schema)])
        ; Spec -> Boolean
        ; does this spec belong to the new schema
        (define (keep? c) (member? (first c) labels))
        ; Row -> Row
        ; retains those columns whose name is in labels
        (define (row-project row) (row-filter row old-labels))
        ; Row [List-of Label] -> Row
        ; retains those cells whose corresponding element
        ; in names is also in labels
        ;; _assume_ row and names has the same length
        (define (row-filter row names)
          (cond
            [(empty? row) '()]
            [else
             (if (member? (first names) labels)
                 (cons (first row)
                       (row-filter (rest row) (rest names)))
                 (row-filter (rest row) (rest names)))]))
    (db (filter keep? schema)
        (map row-project content))))

  (module+ test
    (check-equal?
     (db-content (project.v2 school-db '("Name" "Present")))
     projected-content))

;; Exercise 407
(define (project.v3 db-one labels)
  (let* ([schema  (db-schema db-one)]
         [content (db-content db-one)]
         [old-labels (map first schema)])
        ; Spec -> Boolean
        ; does this spec belong to the new schema
        (define (keep? c) (member? (first c) labels))
        ; Row -> Row
        ; retains those columns whose name is in labels
        (define (row-project row)
          (foldr (lambda (cell name r)
                   (if (member? name labels) (cons cell r) r))
                 '()
                 row old-labels))
    (db (filter keep? schema)
        (map row-project content))))

(module+ test
  (check-equal?
   (db-content (project.v3 school-db '("Name" "Present")))
   projected-content))

(define (project db-one labels)
  (define schema  (db-schema db-one))
  (define content (db-content db-one))

  ; Spec -> Boolean
  ; does his column belong to the new schema
  (define (keep? c)
    (member? (first c) labels))

  ; Row -> Row
  ; retains those columns whose name is in labels
  (define (row-project row)
    (foldr (lambda (cell m c) (if m (cons cell c) c))
           '()
           row
           mask))
  (define mask (map keep? schema))
  (db (filter keep? schema)
           (map row-project content)))

(module+ test
  (check-equal?
   (db-content (project school-db '("Name" "Present")))
   projected-content))

;; Exercise 408
;; db [label] (row -> boolean) -> [row]
(define (select db-one labels pred)
  (define schema (db-schema db-one))
  (define content (db-content db-one))
  (db-content
   (project
    (db schema
        (filter pred content))
    labels)))

(module+ test
  (define selected-content
    `(("Alice" #true)
      ("Carol" #true)))
  (check-equal?
   (select school-db '("Name" "Present") (lambda (row) (third row)))
   selected-content))

; Exercise 409
;; db [label] -> db
;; reorder all columns following the sequence in ~labels~
(define (reorder db-one labels)
  (define schema (db-schema db-one))
  (define content (db-content db-one))
  (define old-labels (map first schema))
  ;; X [X] -> number
  (define (index-of item a-list)
    (foldl (lambda (i a r) (if (string=? item a) i r))
           0
           (build-list (length a-list) (lambda (x) x)) a-list))
  ;; row -> row
  (define (reorder/row row)
    (map second (sort (map (lambda (l c) (list l c)) old-labels row)
                      comp/x)))
  ;; (list label X) (list label X) -> boolean
  (define (comp/x a b)
    (< (index-of (first a) labels) (index-of (first b) labels)))
  (db (sort schema comp/x)
      (map reorder/row content)))

;; Present Description
;; Boolean String
;; #true   "presence"
;; #false  "absence"

(module+ test
  (define labels-one (list "Description" "Present"))
  (define reordered-schema (list (list "Description" string?) (list "Present" boolean?)))
  (define reordered-content (list (list "presence" #true) (list "absence" #false)))
  (check-equal? (db-schema (reorder presence-db labels-one)) reordered-schema)
  (check-equal? (db-content (reorder presence-db labels-one)) reordered-content))

;; db [label] -> db
;; reorder all columns following the sequence in ~labels~
;; only column appear both in ~db~ and ~labels~
(define (reorder.v2 db-one labels)
  (define old-labels (map first (db-schema db-one)))
  (define filtered-labels (filter (lambda (l) (member? l old-labels)) labels))
  (define filtered-db (project db-one filtered-labels))
  (reorder filtered-db filtered-labels))

;; Name    Age     Present
;; String  Integer Boolean
;; "Alice" 35      #true
;; "Bob"   25      #false
;; "Carol" 30      #true
;; "Dave"  32      #false
(module+ test
  (define labels-two (list "Present" "Name" "Race"))
  (define reordered-schema-two (list (list "Present" boolean?) (list "Name" string?)))
  (define reordered-content-two (list (list #t "Alice")
                                      (list #f "Bob")
                                      (list #t "Carol")
                                      (list #f "Dave")))
  (check-equal? (db-schema (reorder.v2 school-db labels-two)) reordered-schema-two)
  (check-equal? (db-content (reorder.v2 school-db labels-two)) reordered-content-two))