#lang racket
(require rackunit)

;; Data Analysis and Definitions:

;; A letter is a symbol in: 'a ... 'z plus '_

;; A word is a (listof letter).

;; A body-part is one of the following symbols:
(define PARTS '(head body right-arm left-arm right-leg left-leg))

;; Constants:
;; some guessing words:
(define WORDS
  '((h e l l o)
    (w o r l d)
    (i s)
    (a)
    (s t u p i d)
    (p r o g r a m)
    (a n d)
    (s h o u l d)
    (n e v e r)
    (b e)
    (u s e d)
    (o k a y)))

;; the number of words we can choose from
(define WORDS# (length WORDS))

 ;; chosen-word : word
;; the word that the player is to guess
(define chosen-word (first WORDS))

;; status-word : word
;; represents which letters the player has and hasn't guessed
(define status-word (first WORDS))

;; body-parts-left : (listof body-part)
;; represents the list of body parts that are still "available"
(define body-parts-left PARTS)

;; hangman :  ->  void
;; effect: initialize chosen-word, status-word, and body-parts-left
(define (hangman)
  (begin
    (set! chosen-word (list-ref WORDS (random (length WORDS))))
    (set! status-word (make-status))
    (set! body-parts-left PARTS)))

;; Exercise 37.2.1
(define (make-status)
  (build-list (length chosen-word) (lambda (x) '_)))

(module+ test
  (begin
    (hangman)
    (check-equal? (length status-word) (length chosen-word))))

;; hangman-guess : letter  ->  response
;; to determine whether the player has won, lost, or may continue to play
;; and, if so, which body part was lost, if no progress was made
;; effects: (1) if the guess represents progress, update status-word
;; (2) if not, shorten the body-parts-left by one
(define (hangman-guess guess)
  (local ((define new-status (reveal-list chosen-word status-word guess)))
    (cond
      [(equal? new-status status-word)
       (local ((define next-part (first body-parts-left)))
         (begin
           (set! body-parts-left (rest body-parts-left))
           (cond
             [(empty? body-parts-left) (list "The End" chosen-word)]
             [else (list "Sorry" next-part status-word)])))]
      [else
       (cond
         [(equal? new-status chosen-word) "You won"]
         [else
          (begin
            (set! status-word new-status)
            (list "Good guess!" status-word))])])))

;; reveal-list : word word letter  ->  word
;; to compute the new status word
(define (reveal-list chosen-word status-word guess)
  (local ((define (reveal-one chosen-letter status-letter)
	    (cond
	      [(symbol=? chosen-letter guess) guess]
	      [else status-letter])))
    (map reveal-one chosen-word status-word)))

;; Exercise 37.2.4
(begin
  (set! chosen-word '(a))
  (set! status-word (make-status))
  (set! body-parts-left PARTS)
  (check-equal? ((lambda (result) (string=? result "You won"))
                 (hangman-guess 'a))
                #true))

(begin
  (set! chosen-word '(i s))
  (set! status-word (make-status))
  (set! body-parts-left PARTS)
  (check-equal? ((lambda (result) (string=? (first result) "Good guess!"))
                 (hangman-guess 'i))
                #true)
  (check-equal? ((lambda (result) (string=? (first result) "Sorry"))
                 (hangman-guess 'a))
                #true))

(begin
  (set! chosen-word '(i s))
  (set! status-word (make-status))
  (set! body-parts-left '(head))
  (check-equal? ((lambda (result) (string=? (first result) "The End"))
                 (hangman-guess 'a))
                #true))

;; Exercise 37.2.6
;; hangman-guess : letter  ->  response
;; to determine whether the player has won, lost, or may continue to play
;; and, if so, which body part was lost, if no progress was made
;; effects: (1) if the guess represents progress, update status-word
;; (2) if not, shorten the body-parts-left by one
;; (3) if guess in ~seen~ shorten the body-parts-left-by-one
(define seen '())
(define (hangman-guess.v2 guess)
  (local ((define new-status (reveal-list chosen-word status-word guess)))
    (cond
      [(cons? (member guess seen))
       (let ([next-part (first body-parts-left)])
        (begin
          (set! body-parts-left (rest body-parts-left))
          (list "You have used this guess before." next-part status-word)))]
      [(equal? new-status status-word)
       (local ((define next-part (first body-parts-left)))
         (begin
           (set! body-parts-left (rest body-parts-left))
           (set! seen (cons guess seen))
           (cond
             [(empty? body-parts-left) (list "The End" chosen-word)]
             [else (list "Sorry" next-part status-word)])))]
      [else
       (cond
         [(equal? new-status chosen-word) "You won"]
         [else
          (begin
            (set! status-word new-status)
            (set! seen (cons guess seen))
            (list "Good guess!" status-word))])])))
