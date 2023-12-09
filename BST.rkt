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

;node number -> boolean
(define (contains-bt? a-node n)
  (cond
    [(no-info? a-node) #false]
    [else (or (= (node-ssn a-node) n)
              (contains-bt? (node-left a-node) n)
              (contains-bt? (node-right a-node) n))]))

(check-expect (contains-bt? node-a 24) #true)
(check-expect (contains-bt? node-b 100) #false)

;node number -> [Maybe Symbol]
(define (search-bt a-bt n)
  (cond
    [(no-info? a-bt) #false]
    [else (cond
            [(= n (node-ssn a-bt)) (node-name a-bt)]
            [(not (boolean? (search-bt (node-left a-bt) n)))
             (search-bt (node-left a-bt) n)]
            [(not (boolean? (search-bt (node-right a-bt) n)))
             (search-bt (node-right a-bt) n)]
            [else #false])]))

(check-expect (search-bt node-a 24) 'i)
(check-expect (search-bt node-b 100) #false)

;Node -> [List-of Number]
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

;Node Number -> Symbol
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

; BST(Node) N S -> BST
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

; [NEList-of [List N S]] -> BST
(define (create-bst-from-list a-list)
  (cond
    [(empty? a-list) NONE]
    {else
     (create-bst (create-bst-from-list (rest a-list))
                 (first (first a-list))
                 (second (first a-list)))}))

(define l-1 '((99 o) (77 l) (24 i) (10 h) (95 g) (15 d) (89 c) (29 b) (63 a)))
