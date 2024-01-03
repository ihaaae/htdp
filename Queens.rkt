#lang racket
(require rackunit)

(struct posn [x y])

(define QUEENS 8)
; A QP is a structure:
;   (posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

;; Exercise 479
;QP QP -> Boolean
;; whether ~qpa~ on the lines ~qpb~ could radiate
(define (threatening? qpa qpb)
  (cond
    [(= (posn-x qpa) (posn-x qpb)) #t]
    [(= (posn-y qpa) (posn-y qpb)) #t]
    [(= (+ (posn-x qpa) (posn-y qpa)) (+ (posn-x qpb) (posn-y qpb))) #t]
    [(= (- (posn-x qpa) (posn-y qpa)) (- (posn-x qpb) (posn-y qpb))) #t]
    [else #f]))

(module+ test
  (define 2-3 (posn 2 3))
  (define 2-4 (posn 2 5))
  (define 6-3 (posn 6 3))
  (define 3-2 (posn 3 2))
  (define 3-4 (posn 3 4))
  (check-equal? (threatening? 2-3 2-4) #true)
  (check-equal? (threatening? 2-3 6-3) #true)
  (check-equal? (threatening? 2-3 3-2) #true)
  (check-equal? (threatening? 2-3 3-4) #true))

;; Exercise 481
;; [List-of QP] -> Boolean
;; N -> [[List-of QP] -> Boolean]
(define (n-queens-solution? n)
  ;; QP [List-of QP] -> Boolean
  (define (threatening?/list qp-one qp-list)
    (ormap (lambda (qp-two) (threatening? qp-one qp-two)) qp-list))
  (lambda (qp-list)
    (cond
      [(not (= (length qp-list) n)) #false]
      [else (not (ormap (lambda (qp-one)
                          (threatening?/list qp-one (remove qp-one qp-list)))
                        qp-list))])))

(module+ test
  (define 0-1 (posn 0 1))
  (define 1-3 (posn 1 3))
  (define 2-0 (posn 2 0))
  ;; (define 3-2 (make-posn 3 2))
  (check-equal? ((n-queens-solution? 4) (list 0-1 1-3 2-0 3-2)) #true))

;; Exercise 483
;; the second interpretaiton
; N -> Board
; creates the initial n by n board
(struct board [dimension added])
(define (board0 n) (board n '()))

; Board QP -> Board
; places a queen at qp on a-board
(define (add-queen a-board qp)
  (board (board-dimension a-board) (cons qp (board-added a-board))))

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  (cond
    [(empty? (board-added a-board)) (n-board (board-dimension a-board))]
    [else (filter (lambda (open-spot)
                    (not (threatening? (first (board-added a-board))
                                       open-spot)))
                  (find-open-spots (board (board-dimension a-board)
                                          (rest (board-added a-board)))))]))

 (module+ test
   (check-equal? (map (lambda (p) (list (posn-x p) (posn-y p)))
                      (find-open-spots (board 4 (list 0-1 1-3))))
                 '((3 0) (3 2) (2 0))))

;; Number Number -> [List-of QP]
;; return a list where qp's position is (0*0 to n * n)
(define (n-m-row n m)
  (cond
    [(= 1 n) (build-list m (lambda (y) (posn (- n 1) y)))]
    [else (append (build-list m (lambda (y) (posn (- n 1) y)))
                  (n-m-row (- n 1) m))]))
(define (n-board n)
  (n-m-row n n))

;; n -> ([List-of QP] -> Boolean)
(define (n-board? n)
  (lambda (a-board)
    (cond
      [(not (= (* n n) (length a-board))) #false]
      [else (andmap (lambda (p) (cons? (member
                                        p
                                        (map (lambda (x) (list (posn-x x) (posn-y x)))
                                             a-board))))
                    (for*/list ((i n) (j n))
                      (list i j)))])))

(module+ test
  (check-equal? ((n-board? 4) (n-board 4)) #true))

;; Exercise 482
; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
;; _generative_ place (n-1) queens to a smaller board ~(add-queen queen-one a-board)~
;; ~queen-one~ is any position for ~(find-open-spots a-board)~
(define (place-queens a-board n)
  ;; Board [Maybe [List-of QP]] -> [Maybe [List-of QP]]
  ;; return r if r is list; return #false if couldn't place ~n-1~ queens to ~a-board~
  ;; return solutions otherwise
  (define (place-queens/spot spot placed-board r)
    (let ([result (place-queens placed-board (- n 1))])
      (cond
        [(cons? r) r]
        [(boolean? result) #f]
        [else (cons spot result)])))
  (let* ([open-spots (find-open-spots a-board)]
         [open-boards (map (lambda (spot) (add-queen a-board spot)) open-spots)])
    (cond
      [(= n 0) '()]
      [else  (foldr place-queens/spot
                    #false
                    open-spots open-boards)])))

(module+ test
  (check-equal? ((n-queens-solution? 4) (place-queens (board0 4) 4)) #true))


