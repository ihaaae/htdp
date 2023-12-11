#lang racket
; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; -(cons Symbol bodies)
;bodies:
; body
; (cons attributes bodies)

<transition from="seen-e" to="seen-f" />
'(trainsition ((from "seen-e") (to "seen-f")))

<ul><li><word /><word /></li><li><word /></li></ul>
'(ul (li (word) (word)) (li (word))) xexpr.v1


'(server ((name "example.org")))
<server name="example.org">
'(carcas (board (grass)) (player ((name "sam"))))
<carcas> <board><grass /></ board> <player name="same"></ carcas>
'(start) xexpr.v0
<start />

;Xexpr.v2:
;(cons Symbol optionalAttributes-and-Body)
;optionalAttributes-and-Body:
;(cons attributes body)
;body
;'()
;body: '() or [List-of Xexpr.v2]
;attributes: '() or [List-of attribute]
;attribute: (cons Symbol (cons String '()))

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

;listofattruibutes-or-Xexpr
;[List-of Attribute]
;Xexpr.v2
; listofattruibutes-or-Xexpr -> Boolean
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
  (cond
    [(empty? (rest ex)) '()]
    [(body? (rest ex)) (rest ex)]
    [else (rest (rest ex))]))

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

;listofattruibutes-or-Xexpr
;(first x) is Xerpr.v2 or [List-of attributes]
(define (body? x)
  (cond
    [(list-of-attributes? (first x)) #false]
    [else #true]))

;[List-of Attributes] Symbol -> {string or #false}
(define (find-attr attributes-list the-attribute)
  (if (boolean? (assq the-attribute attributes-list))
      #false
      (second (assq the-attribute attributes-list))))

(check-expect (find-attr (xexpr-attr e0) 'initial) #false)
(check-expect (find-attr (xexpr-attr e1) 'initial) "X")
(check-expect (find-attr (xexpr-attr e2) 'initial) #false)
(check-expect (find-attr (xexpr-attr e3) 'initial) #false)
(check-expect (find-attr (xexpr-attr e4) 'initial) "X")
