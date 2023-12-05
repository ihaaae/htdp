#lang racket
(require 2htdp/batch-io)
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

;(define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
(define date1 (create-date 1999 12 19 16 12 30))
(define date2 (create-date 2001 12 19 16 12 30))
(define date3 (create-date 2001 12 19 17 12 30))

; An LTracks is one of:
; – '()
; – (cons Track LTracks)
 
;(define-struct track
;  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
(define track1 (create-track "sex machine" "liu" "real mother fucker" 2000 10 date1 10 date2))
(define track2 (create-track "Mommy beach" "liu" "real mother fucker" 2000 13 date1 19 date3))
(define track3 (create-track "virgin mom" "liu" "fake mother fucker" 2000 11 date1 19 date3))
(define track4
  (create-track "different generation and same mom" "liu" "real mother fucker" 2000 12 date1 19 date2))
(define Ltracks1 (list track1 track2))
(define Ltracks2 (list track1 track2 track3 track4))

;Ltracks -> Number
(define (total-time lts)
  (cond
    [(empty? lts) 0]
    [else (+ (track-time (first lts))
             (total-time (rest lts)))]))

;(check-expect (total-time '()) 0)
;(check-expect (total-time Ltracks1) 4000)

;Ltracks -> list-of-strings
(define (select-all-album-titles lts)
  (cond
    [(empty? lts) '()]
    [else
     (cons (track-album (first lts))
           (select-all-album-titles (rest lts)))]))

;(check-expect (select-all-album-titles '()) '())
;(check-expect (select-all-album-titles Ltracks1)
;              (list "real mother fucker" "real mother fucker"))

;list-of-strings [list-of strings] -> list-of-strings
(define (create-set los)
  (cond
    [(empty? los) '()]
    [else
     (if (member? (first los) (create-set (rest los)))
         (create-set (rest los))
         (cons (first los) (create-set (rest los))))]))

;(check-expect (create-set (list "a" "b" "c" "a")) (list "b" "c" "a"))
;(check-expect (create-set '()) '())
;(check-expect (create-set (list "a" "b" "c")) (list "a" "b" "c"))
;(check-expect (create-set (list "a" "b" "a" "b")) (list "a" "b"))

;Ltracks -> list-of-string
(define (select-album-titles/unique lts)
  (cond
    [(empty? lts) '()]
    [else
     (create-set
      (cons (track-album (first lts))
                       (select-all-album-titles (rest lts))))]))

;(check-expect (select-album-titles/unique Ltracks1)
;              (list "real mother fucker"))

;string LTracks -> LTracks
(define (select-album at lts)
  (cond
    [(empty? lts) '()]
    [else
     (if (string=? (track-album (first lts)) at)
         (cons (first lts) (select-album at (rest lts)))
         (select-album at (rest lts)))]))

;(check-expect (select-album "real mother fucker" Ltracks2)
;              (list track1 track2 track4))
;(check-expect (select-album "fake mother fucker" Ltracks2)
;              (list track3))

;string Date LTracks -> LTracks
(define (select-album-date at id lts)
  (cond
    [(empty? lts) '()]
    [else
     (if (and (string=? (track-album (first lts)) at)
               (Date-bigger (track-played (first lts)) id))
          (cons (first lts) (select-album-date at id (rest lts)))
          (select-album-date at id (rest lts)))]))

;(define-struct date [year month day hour minute second])
;Date Date -> boolean
(define (Date-bigger dt1 dt2)
  (cond
    [(three-level-bigger (date-year dt1) (date-month dt1)
                       (date-day dt1) (date-year dt2)
                       (date-month dt2) (date-day dt2))
     #true]
    [(and (= (date-year dt1) (date-year dt2))
          (= (date-minute dt1)  (date-month dt2))
          (= (date-day dt1) (date-day dt2)))
     (cond
       [(three-level-bigger (date-hour dt1) (date-minute dt1)
                          (date-second dt1) (date-hour dt2)
                          (date-minute dt2) (date-second dt2))
        #true]
       [else #false])]
    [else #false]))

;number number number number number number -> boolean
(define (three-level-bigger n1 n2 n3 nn1 nn2 nn3)
  (cond
    [(> n1 nn1) #true]
    [(= n1 nn1)
     (cond
       [(> n2 nn2) #true]
       [(= n2 nn2)
        (cond
          [(> n3 nn3) #true]
          [else #false])]
       [else #false])]
    [else #false]))

;(check-expect (three-level-bigger 3 3 4 3 3 2) #true)
;(check-expect (three-level-bigger 3 3 4 4 3 4) #false)

;(check-expect (Date-bigger date1 date2) #false)
;(check-expect (Date-bigger date3 date2) #true)

;(check-expect (select-album-date "real mother fucker" date2 Ltracks2) (list track2))

; Ltracks [list-of string] -> [list-of Ltracks]
(define (select-albums lts titles)
  (cond
    [(empty? titles) '()]
    [else
     (cons (select-album (first titles) lts)
           (select-albums lts (rest titles)))]))

;(check-member-of
; (select-albums Ltracks2 (select-album-titles/unique Ltracks2))
; (list (list track1 track2 track4) (list track3))
; (list (list track3) (list track1 track2 track4)))

; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
; 
; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date
(define a1 (cons "album" (cons "real mother fucker" '())))
(define a2 (cons "name" (cons "sex machine" '())))
(define a3 (cons "play#" (cons 1 '())))
(define a4 (cons "Total Time" (cons 2000 '())))
(define a5 (cons "Compilation" (cons #true '())))
(define a21 (cons "name" (cons "Mommy beach" '())))
(define a51 (cons "Visible" (cons #true '())))

(define la1 (list a1 a2 a4 a3 a5))
(define la2 (list a1 a21 a4 a3 a51))

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
(define ll1 (list la1 la2))

; string LAssoc Any -> [Assoc or Any]
(define (find-association key la default)
  (cond
    [(empty? la) default]
    [else
     (if (string=? (first (first la)) key)
         (first la)
         (find-association key (rest la) default))]))

;(check-expect (find-association "play#" la1 "not found") a3)
;(check-expect (find-association "artist" la1 "not found") "not found")

;LLists -> Number
(define (total-time/list lls)
  (cond
    [(empty? lls) 0]
    [else
     (if (boolean? (assoc "Total Time" (first lls)))
         (total-time/list (rest lls))
         (+ (first (rest (assoc "Total Time" (first lls))))
            (total-time/list (rest lls))))]))

;(check-expect (total-time/list ll1) 4000)

;LLists -> [list-of string]
(define (boolean-attributes lls)
  (cond
    [(empty? lls) '()]
    [else
     (if (> (length (boolean-attributes/LAssoc (first lls))) 0)
         (create-set (append (boolean-attributes/LAssoc (first lls))
                             (boolean-attributes (rest lls))))
         (boolean-attributes (rest lls)))]))

;(check-expect (boolean-attributes ll1) (list "Compilation" "Visible"))

;LAssoc -> [list-of string]
(define (boolean-attributes/LAssoc las)
  (cond
    [(empty? las) '()]
    [else
     (if (not (boolean? (first (rest (first las)))))
         (boolean-attributes/LAssoc (rest las))
         (cons (first (first las))
               (boolean-attributes/LAssoc (rest las))))]))

;(check-expect (boolean-attributes/LAssoc la1) (list "Compilation"))

;LAssoc -> [Track Boolean]
(define (track-as-struct las)
  (if (and (not (boolean? (find-association "" las #false)))
           (not (boolean? (find-association "" las #false)))
           (not (boolean? (find-association "" las #false)))
           (not (boolean? (find-association "" las #false)))
           (not (boolean? (find-association "" las #false)))
           (not (boolean? (find-association "" las #false)))
           (not (boolean? (find-association "" las #false)))
           (not (boolean? (find-association "" las #false))))
      (create-track ())
      #false))

(define a12 (cons "album" (cons "real mother fucker" '())))
(define a22 (cons "name" (cons "sex machine" '())))
(define a32 (cons "play#" (cons 1 '())))
(define a42 (cons "time" (cons 2000 '())))
(define a52 (cons "track#" (cons 10 '())))
(define a62 (cons "added" (cons date1 '())))
(define a72 (cons "artist" (cons "liu" '())))
(define a82 (cons "played" (cons date2 '())))
(define la11 (list a12 a22 a32 a42 a52 a62 a72 a82))
(define la12 (list a12 a32 a42 a52 a62 a72 a82))

(check-expect (track-as-struct la11) track1)
(check-expect (track-as-struct la12) #false)
