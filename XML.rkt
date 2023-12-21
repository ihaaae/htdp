#lang racket
(require test-engine/racket-tests)
;; An Xexpr.v2 is a list:
;; -(cons Symbol Body)
;; where Body is the following
;; [List-of Xexpr.v2]
;; (cons [list-of Attribute] [List-of Xexpr.v2])
;; An Atrribute is a list of two items:
;; (cons Symbol (cons String '()))


;; <transition from="seen-e" to="seen-f" /> in Xexpr.v2
(define transition (cons 'transition
                         (cons (list (cons 'from (cons "seen-e" '()))
                                     (cons 'to (cons "seen-f" '())))
                               '())))
(define transition-two '(transition ((from "seen-e") (to "seen-f"))))

;; <word /> in Xexpr.v2
(define word (cons 'word '()))
;; <ul><li><word /><word /></li><li><word /></li></ul> in Xexpr.v2
(define xexpr-one (cons 'ul
                        (list (cons 'li
                                    (list word word))
                              (cons 'li
                                    (list word)))))
(define xexpr-one-two '(ul (li (word) (word)) (li (word))))

(check-expect transition transition-two)
(check-expect xexpr-one xexpr-one-two)

(define xexpr-two'(server ((name "example.org"))))
;; correspond to <server name="example.org">
(define xexpr-three '(carcas (board (grass))
                         (player ((name "sam"))))) ; xexpr.v2
(define xexpr-three-two (cons 'carcas (list (cons 'board
                                                  (list (cons 'grass '())))
                                            (cons 'player
                                                  (cons (list (cons 'name (cons "sam" '())))
                                                        '())))))
(check-expect xexpr-three xexpr-three-two)
;; correspond to <carcas> <board><grass /></ board> <player name="same"></ carcas>
(define xexpr-b '(start)) ;xexpr.v0
;; correspond to <start />

(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2 -> symbol
(define (xexpr-name xe)
  (first xe))

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)

; Xerpr.v2 -> [List-of Xexpr.v2]
(define (xexpr-content ex)
  (let [(optional-loa+content (rest ex))]
    (cond
      [(empty? optional-loa+content) '()]
      [else (let [(loa-or-x (first optional-loa+content))]
              (if (list-of-attributes? loa-or-x)
                  (rest optional-loa+content)
                  optional-loa+content))])))

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

;; Excercise 367
(define (xexpr-attr/self-ref xex)
  (let ([optional-loa+content (rest xex)])
    (cond
      [(empty? optional-loa+content) '()]
      [else (let ([loa-or-x (first optional-loa+content)])
              (if (list-of-attributes? loa-or-x)
                  loa-or-x
                  (map xexpr-attr/self-ref (rest xex))))])))
;; This is a self-reference version of xexpr-attr.
;; I think the answer is pretty obvious.
;; The attributes of the son xexpr is not your attributes.
;; Do I misunderstand the question.

;; Exercise 368
;; formulate a data definition of function to replace _informal_ 'or'
;; As function get either a [list-of attributes] or Xexpr.v2
;; it should just be a formal forumlation
;; Body-head is
;; - [List-of-Attributes]
;; - Xexpr.v2
; Body-head -> Boolean
; is x a list of attributes
(define (list-of-attributes?.v2 x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))
;; If there is a better way to distinguish the xexpr.v2 and [list-of att]
;; like pattern match it would be better.
;; use struct instead of list to represent Xexpr.v2 may be better

;; [List-of Attributes] Symbol -> {string or #false}
(define (find-attr attributes-list the-attribute)
  (let [(result (assq the-attribute attributes-list))]
    (if (boolean? result)
        #false
        (second result))))

(check-expect (find-attr (xexpr-attr e0) 'initial) #false)
(check-expect (find-attr (xexpr-attr e1) 'initial) "X")
(check-expect (find-attr (xexpr-attr e2) 'initial) #false)
(check-expect (find-attr (xexpr-attr e3) 'initial) #false)
(check-expect (find-attr (xexpr-attr e4) 'initial) "X")

(test)