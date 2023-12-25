#lang racket
(require lang/htdp-intermediate-lambda)
(require test-engine/racket-tests)

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define node-a (make-node 15 'd NONE
                          (make-node
                           24 'i NONE NONE)))


(define node-b (make-node 15 'd
                          (make-node
                           87 'h NONE NONE)
                          NONE))

;; node number -> boolean
;; whether _a-node_ contains _n_
(define (contains-bt? a-node n)
  (cond
    [(no-info? a-node) #false]
    [else (or (= (node-ssn a-node) n)
              (contains-bt? (node-left a-node) n)
              (contains-bt? (node-right a-node) n))]))

(check-expect (contains-bt? (make-node 0 'd NONE NONE) 1) #false)
(check-expect (contains-bt? node-a 24) #true)
(check-expect (contains-bt? node-b 100) #false)

;; node number -> [Maybe Symbol]
;; name of node in _a-bt_ whose ssn is _n_
(define (search-bt a-bt n)
  (cond
    [(not (contains-bt? a-bt n)) #false]
    [else (cond
            [(= n (node-ssn a-bt)) (node-name a-bt)]
            [(contains-bt? (node-left a-bt) n)
             (search-bt (node-left a-bt) n)]
            [else (search-bt (node-right a-bt) n)])]))

(check-expect (search-bt node-a 24) 'i)
(check-expect (search-bt node-b 100) #false)

;; Node -> [List-of Number]
;; all ssn in _a-tree_ ordered by their position in _a-tree_ from left to right
(define (inorder a-tree)
  (cond
    [(no-info? a-tree) '()]
    [else (append (inorder (node-left a-tree))
                  (list (node-ssn a-tree))
                  (inorder (node-right a-tree)))]))

(define node-c (make-node 15 'd
                          (make-node
                           87 'h NONE NONE)
                          (make-node
                           24 'i NONE NONE)))
(check-expect (inorder node-c) '(87 15 24))

;; Node Number -> Symbol
;; the name of node in _a-bst_ whose ssn is _n_, return NONE if such ssn doesn't exist
(define (search-bst a-bst n)
  (cond
    [(no-info? a-bst) NONE]
    [else (cond
            [(= n (node-ssn a-bst)) (node-name a-bst)]
            [(< n (node-ssn a-bst)) (search-bst (node-left a-bst) n)]
            [(> n (node-ssn a-bst)) (search-bst (node-right a-bst) n)])]))

(define bst-1 (make-node 15 'd
                          (make-node
                           4 'h NONE NONE)
                          (make-node
                           87 'i NONE NONE)))

(check-expect (search-bst bst-1 4) 'h)
(check-expect (search-bst bst-1 24) NONE)

;; BST(Node) N S -> BST
;; doesn't change the structure of _a-bst_, and add (make-node _n_ _s_ NONE NONE) to a NONE substring of _a-bst_
(define (create-bst a-bst n s)
  (cond
    [(no-info? a-bst) (make-node n s NONE NONE)]
    [else (cond
            [(> n (node-ssn a-bst))
             (make-node (node-ssn a-bst)
                        (node-name a-bst)
                        (node-left a-bst)
                        (create-bst (node-right a-bst) n s))]
            [(< n (node-ssn a-bst))
             (make-node (node-ssn a-bst)
                        (node-name a-bst)
                        (create-bst (node-left a-bst) n s)
                        (node-right a-bst))])]))

(define bst-2 (make-node 15 'd
                          (make-node
                           4 'h
                           (make-node 1 'a NONE NONE) NONE)
                          (make-node
                           87 'i NONE NONE)))

(check-expect (create-bst bst-1 1 'a) bst-2)

(define bst-left-one (make-node 29 'a
                                (make-node 15 'a
                                           (make-node 10 'a NONE NONE)
                                           (make-node 24 'a NONE NONE))
                                NONE))
(define bst-right-one (make-node 89 'a
                                 (make-node 77 'a NONE NONE)
                                 (make-node 95 'a
                                            NONE
                                            (make-node 99 'a NONE NONE))))
(define bst-one (make-node 63 'a bst-left-one bst-right-one))
(check-expect (inorder bst-one) '(10 15 24 29 63 77 89 95 99))

; [NEList-of [List N S]] -> BST
; build a BST with the list of _a-list_ in a way of create-bst
(define (create-bst-from-list a-list)
  (cond
    [(empty? a-list) NONE]
    {else
     (create-bst (create-bst-from-list (rest a-list))
                 (first (first a-list))
                 (second (first a-list)))}))

(define l-1 '((99 a) (77 a) (24 a) (10 a) (95 a) (15 a) (89 a) (29 a) (63 a)))
(check-expect (create-bst-from-list l-1) bst-one)

(test)
