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
; Dir.v3 -> Number
(define (how-many.v3 dir)
  (+ (foldl (lambda (one-dir n)
              (+ (how-many.v3 one-dir) n))
            0
            (dir.v3-dirs dir))
     (length (dir.v3-files dir))))

(module+ test
  (check-equal? (how-many.v3 Libs.v3) 3)
  (check-equal? (how-many.v3 TS.v3) 7))

;; Exercise 338
;; skip

;; Exercise 339
; Dir.v3 string -> Boolean
(define (find? a-dir a-file-name)
  (or (ormap (lambda (one-dir) (find? one-dir a-file-name))
             (dir.v3-dirs a-dir))
      (ormap (lambda (a-file) (string=? (file-name a-file) a-file-name))
             (dir.v3-files a-dir))))

(module+ test
  (check-equal? (find? TS.v3 "hang") #true)
  (check-equal? (find? Text.v3 "pycharm-log.txt") #false))

;; exercise 340
; Dir.v3 -> [List-of String]
(define (ls a-dir)
  (append (map (lambda (sub-dir) (dir.v3-name sub-dir)) (dir.v3-dirs a-dir))
          (map (lambda (a-file) (file-name a-file)) (dir.v3-files a-dir))))

(module+ test
  (check-equal? (ls TS.v3) (list "Text" "Libs" "read!")))

;; Exercise 341
;; Dir.v3 -> Numer
;; the sum of size of all files and dirs in ~dir~
(define (du dir)
  (+ 1
     (foldr (lambda (a-dir r) (+ r (du a-dir))) 0 (dir.v3-dirs dir))
     (foldr (lambda (a-file r) (+ (file-size a-file) r)) 0 (dir.v3-files dir))))

(module+ test
  (check-equal? (du Libs.v3) (+ 8 2 1 19 1 1)))

;; Exercise 342

; A Path is [List-of String].
; interpretation directions into a directory tree

;; Dir.v3 String -> Maybe Path
(define (find a-dir a-file-name)
  (cond
    [(cons? (filter (lambda (a-file) (string=? a-file-name (file-name a-file)))
                    (dir.v3-files a-dir)))
     (list (dir.v3-name a-dir) a-file-name)]
    [else (let ([candidates (filter (lambda (dir-two) (find? dir-two a-file-name)) (dir.v3-dirs a-dir))])
            (cond
              [(empty? candidates) #f]
              [else (cons (dir.v3-name a-dir) (find (first candidates) a-file-name))]))]))

(module+ test
  (check-equal? (find (dir.v3 "one" '() '()) "read!") #f)
  (check-equal? (find Docs.v3 "read!") (list "Docs" "read!"))
  (check-equal? (find Libs.v3 "read!") (list "Libs" "Docs" "read!")))

;; Dir.v3 String -> [Maybe [Listof Path]]
;; a list of path of all target-file in dir-one
(define (find-all dir-one target-file)
  ;; dir.v3 string -> [Maybe [List-of Path]]
  (define (find-all/files file-list target-file)
    (if (cons? (filter (lambda (one-file) (string=? (file-name one-file) target-file)) file-list))
        (list (list target-file))
        #f))
  ;; dir.v3 string -> [Maybe [List-of Path]]
  (define (find-all/dirs dir-list target-file)
    (foldr
     (lambda (maybe-path result)
       (cond
         [(and (boolean? maybe-path) (boolean? result)) #f]
         [(boolean? result) maybe-path]
         [(boolean? maybe-path) result]
         [else (append maybe-path result)]))
     #f
     (map (lambda (sub-dir) (find-all sub-dir target-file)) dir-list)))
  (let ([files-result (find-all/files (dir.v3-files dir-one) target-file)]
        [dirs-result (find-all/dirs (dir.v3-dirs dir-one) target-file)])
    (cond
      [(and (cons? files-result) (cons? dirs-result))
       (append (map (lambda (one-path) (cons (dir.v3-name dir-one) one-path))
                    files-result)
               (map (lambda (one-path) (cons (dir.v3-name dir-one) one-path))
                    dirs-result))]
      [(cons? files-result) (map (lambda (one-path) (cons (dir.v3-name dir-one) one-path)) files-result)]
      [(cons? dirs-result) (map (lambda (one-path) (cons (dir.v3-name dir-one) one-path)) dirs-result)]
      [else #f])))

(module+ test
  (check-equal? (find-all Docs.v3 "read!") (list (list "Docs" "read!")))
  (check-equal? (find-all TS.v3 "read!") (list (list "TS" "read!") (list "TS" "Libs" "Docs" "read!"))))

;; Exercise 343
;; Dir.v3 -> [Listof Path]
;; list all files and dirs under a-dir
(define (ls-R a-dir)
  ;; List-of dirs.v3 -> [List-of Path]
  (define (ls-R/dirs dir-list)
    (match dir-list
      ['() '()]
      [(cons head tail)
       (append (map (lambda (result) (cons (dir.v3-name head) result))
                    (ls-R head))
               (ls-R/dirs tail))]))
    (append (map (lambda (sub) (list sub)) (ls a-dir))
            (ls-R/dirs (dir.v3-dirs a-dir))))

(module+ test
  (check-equal? (ls-R Libs.v3) (list (list "Code")
                                     (list "Docs")
                                     (list "Code" "hang")
                                     (list "Code" "draw")
                                     (list "Docs" "read!"))))

;; Exercise 344
;; Dir.v3 -> [List-of Path]
;; ...
(define (find-all.v2 a-dir target)
   (let ([candidates (filter (lambda (path) (string=? target (last path)))
                             (ls-R a-dir))])
     (cond
       [(empty? candidates) #false]
       [else (map (lambda (candidate) (cons (dir.v3-name a-dir) candidate))
                  candidates)])))