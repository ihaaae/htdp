#lang racket
(require rackunit)
(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

;; Exercise 330
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

(define hang "hang")
(define draw "draw")
(define read! "read!")
(define Docs (cons read! '()))
(define Code (cons hang (cons draw '())))
(define Libs (cons Docs (cons Code '())))
(define read!-1 "read!")
(define part1 "part1")
(define part2 "part2")
(define part3 "part3")
(define Text (cons part1 (cons part2 (cons part3 '()))))
(define TS (cons Text (cons read! (cons Libs '()))))

;; Exercise 331
;; Dir.v1 -> Number
;; return the number of files in dir
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(string? (first dir)) (+ 1 (how-many (rest dir)))]
    [else (+ (how-many (first dir))
             (how-many (rest dir)))]))

(module+ test
  (check-equal? (how-many Libs) 3)
  (check-equal? (how-many TS) 7))

;; Exercise 332

(struct dir (name content))

; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

(define hang.v2 "hang")
(define draw.v2 "draw")
(define Code.v2 (dir "Code" (list hang.v2 draw.v2)))
(define read!.v2 "read!")
(define Docs.v2 (dir "Docs" (list read!.v2)))
(define Libs.v2 (dir "Libs" (list Code.v2 Docs.v2)))
(define read!-1.v2 "read!")
(define part1.v2 "part1")
(define part2.v2 "part2")
(define part3.v2 "part3")
(define Text.v2 (dir "Text" (list part1.v2 part2.v2 part3.v2)))
(define TS.v2 (dir "TS" (list Text.v2 read!-1.v2 Libs.v2)))

;; Exercise 333

;; Dir.v2 -> Number
;; ...
(define (how-many.v2 dir)
  (let ([content (dir-content dir)])
    (how-many-LOFD content)))

;; LOFD -> Number
(define (how-many-LOFD lofd)
  (cond
    [(empty? lofd) 0]
    [(string? (first lofd)) (+ 1 (how-many-LOFD (rest lofd)))]
    [else (+ (how-many.v2 (first lofd))
             (how-many-LOFD (rest lofd)))]))

(module+ test
  (check-equal? (how-many.v2 Libs.v2) 3)
  (check-equal? (how-many.v2 TS.v2) 7))

;; Exercise 334

; A Dir.v2a is a structure:
;   (dir String Number Boolean LOFD)

; same below

;; Exercise 335

(struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)

(struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 [List-of Dir.v3])
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

(define hang.v3 (file "hang" 8 ""))
(define draw.v3 (file "draw" 2 ""))
(define Code.v3 (dir.v3 "Code" '() (list hang.v3 draw.v3)))
(define read!.v3 (file "read!" 19 ""))
(define Docs.v3 (dir.v3 "Docs" '() (list read!.v3)))
(define Libs.v3 (dir.v3 "Libs" (list Code.v3 Docs.v3) '()))
(define read!-1.v3 (file "read!" 10 ""))
(define part1.v3 (file "part1" 99 ""))
(define part2.v3 (file "part2" 52 ""))
(define part3.v3 (file "part3" 17 ""))
(define Text.v3 (dir.v3 "Text" '() (list part1.v3 part2.v3 part3.v3)))
(define TS.v3 (dir.v3 "TS" (list Text.v3 Libs.v3) (list read!-1.v3)))

;; Exercise 336 and 337