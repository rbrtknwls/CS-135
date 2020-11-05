;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sillystring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 05, Problem 3(A-C)
;; ******************************************


;; ** Structs **

(define-struct silly-string (first middle last))
;; A SillyStringStruct is a (make-silly-string Char SillyStr Char)

;; A SillyStr is one of:
;; * empty
;; * a Char
;; * a SillyStringStruct



;;
;;   Problem 3A
;;



;; (sillify s) Consumes a string (s),
;;   and converts it into its silly string
;;   counter part.
;; Examples:
(check-expect (sillify "Babbage")
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
(check-expect (sillify "Lovelace")
              (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\l)
                 #\a)
                #\c)
               #\e))

;; sillify: Str -> SillyStr
(define (sillify s)
  (cond [(empty? s)                      empty]
        [else (list->sillify (string->list s))]))

        
;; ** Helper Function **
        
;; (list->sillify loc) Consumes a list of
;;   characters (loc) and converts it into
;;   its silly string counter part.
;; Examples:

(check-expect (list->sillify (list #\B #\a #\b
                                   #\b #\a #\g #\e))
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))

(check-expect (list->sillify empty) empty)

;; list->sillify: (listof Char) -> SillyStr
(define (list->sillify loc)
  (cond [(empty? loc) empty]
        [(= (length loc) 1) (first loc)]
        [(= (length loc) 2)
           (make-silly-string (first loc)
                              empty
                              (second loc))]
        [else
           (make-silly-string (first loc)
                              (list->sillify (middle loc empty))
                              (last loc))]))

;; (middle loc) Consumes a list of
;;   characters (loc) and produces the list (nl)
;;   with the first and last case missing
;; Examples:
(check-expect (middle (list #\B #\a #\b #\b #\a #\g #\e) empty)
              (list #\a #\b #\b #\a #\g))

(check-expect (middle (list #\a #\a #\b #\b #\c #\c) empty)
              (list #\a #\b #\b #\c))

;; middle: (listof Char) (list of Char) -> (listof Char)
;;  Requires: Length of loc >=3 
(define (middle loc nl)
   (cond [(= (length loc) 1)             (rest nl)]
         [else
           (middle (rest loc)
                   (append nl (list (first loc))))]))
          

;; (last loc) Consumes a list of
;;   characters (loc) and produces the
;;   last character
;; Examples:
(check-expect (last (list #\B #\a #\b #\b #\a #\g #\e))
              #\e)

(check-expect (last (list #\a #\a #\b #\b #\c #\c))
              #\c)

;; last: (listof Char) -> Char
(define (last loc)
  (cond [(= (length loc) 1) (first loc)]
        [else (last (rest loc))]))

;; Empty Test
(check-expect (sillify empty) empty)

;; 1 Element Test
(check-expect (sillify "1") #\1)
(check-expect (sillify "a") #\a)

;; 2 Element Test
(check-expect (sillify "24") (make-silly-string #\2 empty #\4))
(check-expect (sillify "Aa") (make-silly-string #\A empty #\a))

;; 3 Element Lists
(check-expect (sillify "135") (make-silly-string #\1 #\3 #\5))
(check-expect (sillify "AaA") (make-silly-string #\A #\a #\A))
(check-expect (sillify "ABC") (make-silly-string #\A #\B #\C))

;; General Tests
(check-expect (sillify "CaKe")
              (make-silly-string
               #\C
               (make-silly-string
                 #\a
                 empty
                 #\K)
               #\e))

(check-expect (sillify "Tired")
              (make-silly-string
               #\T
               (make-silly-string
                 #\i
                 #\r
                 #\e)
               #\d))

(check-expect (sillify "123456")
              (make-silly-string
               #\1
               (make-silly-string
                 #\2
                 (make-silly-string
                   #\3
                   empty
                   #\4)
                 #\5)
               #\6))

(check-expect (sillify "FARFAR1")
              (make-silly-string
               #\F
               (make-silly-string
                 #\A
                 (make-silly-string
                   #\R
                   #\F
                   #\A)
                 #\R)
               #\1))



;;
;;   Problem 3B
;;



;; (unsillify ss) Consumes a Silly String (ss),
;;   and converts it into a string
;; Examples:
(check-expect (unsillify 
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
              "Babbage")
(check-expect (unsillify
               (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\l)
                 #\a)
                #\c)
               #\e))
               "Lovelace")
(check-expect (unsillify empty) empty)

;; unsillify: SillyStr -> Str
(define (unsillify ss)
  (cond [(empty? ss)                      empty]
        [else (list->string (sillify->list ss))]))

;; ** Helper Functions **

;; (sillify->list ss) Consumes a silly string (ss),
;;   and converts it a character List
;; Examples:
(check-expect (sillify->list 
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
              (list #\B #\a #\b #\b #\a #\g #\e))
(check-expect (sillify->list
               (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\l)
                 #\a)
                #\c)
               #\e))
               (list #\L #\o #\v #\e #\l #\a #\c #\e))
(check-expect (sillify->list empty) empty)              

;; sillify->list: SillyStr -> (listof Char)
(define (sillify->list ss)
  (cond [(empty? ss) empty]
        [(not (silly-string? ss))
           (list ss)]
        [else 
           (cond [(empty? (silly-string-middle ss))
                    (list (silly-string-first ss)
                          (silly-string-last ss))]
                 [else (append
                         (cons (silly-string-first ss)
                         (sillify->list (silly-string-middle ss)))
                       (list (silly-string-last ss)))])]))


;; Empty Test
(check-expect (sillify empty) empty)

;; 1 Element Test
(check-expect (unsillify #\1) "1")
(check-expect (unsillify #\a) "a")

;; 2 Element Test
(check-expect (unsillify (make-silly-string #\2 empty #\4)) "24")
(check-expect (unsillify (make-silly-string #\A empty #\a)) "Aa")

;; 3 Element Lists
(check-expect (unsillify (make-silly-string #\1 #\3 #\5)) "135")
(check-expect (unsillify (make-silly-string #\A #\a #\A)) "AaA")
(check-expect (unsillify (make-silly-string #\A #\B #\C)) "ABC")

;; General Tests
(check-expect (unsillify 
              (make-silly-string
               #\C
               (make-silly-string
                 #\a
                 empty
                 #\K)
               #\e)) "CaKe")

(check-expect (unsillify
               (make-silly-string
               #\T
               (make-silly-string
                 #\i
                 #\r
                 #\e)
               #\d)) "Tired")

(check-expect (unsillify 
              (make-silly-string
               #\1
               (make-silly-string
                 #\2
                 (make-silly-string
                   #\3
                   empty
                   #\4)
                 #\5)
               #\6)) "123456")

(check-expect (unsillify
              (make-silly-string
               #\F
               (make-silly-string
                 #\A
                 (make-silly-string
                   #\R
                   #\F
                   #\A)
                 #\R)
               #\1)) "FARFAR1")

                    

;;
;;   Problem 3C
;;


;; (palindrome? ss) Consumes a Silly String (ss),
;;   and produces if its a palindrome or not.
;; Examples:
(check-expect (palindrome? 
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
              false)

(check-expect (palindrome?
               (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\e)
                 #\v)
                #\o)
               #\L))
               true)
              
;; unsillify: SillyStr -> Bool
;; Requires: ss is not empy
(define (palindrome? ss)
  (cond [(not (silly-string? ss))              true]
        [else 
           (cond [(empty? (silly-string-middle ss))
                    (char=? (silly-string-first ss)
                            (silly-string-last ss))]
                 [else
                      (and (char=? (silly-string-first ss)
                                   (silly-string-last ss))
                          (palindrome? (silly-string-middle ss)))])]))

;; 1 Element Test
(check-expect (palindrome? #\1) true)
(check-expect (palindrome? #\a) true)

;; 2 Element Test
(check-expect (palindrome? (make-silly-string #\2 empty #\4)) false)
(check-expect (palindrome? (make-silly-string #\A empty #\a)) false)
(check-expect (palindrome? (make-silly-string #\A empty #\A)) true)

;; 3 Element Lists
(check-expect (palindrome? (make-silly-string #\1 #\3 #\5)) false)
(check-expect (palindrome? (make-silly-string #\A #\a #\A)) true)
(check-expect (palindrome? (make-silly-string #\A #\B #\C)) false)

;; General Tests
(check-expect (palindrome? 
              (make-silly-string
               #\C
               (make-silly-string
                 #\a
                 empty
                 #\a)
               #\C)) true)

(check-expect (palindrome?
               (make-silly-string
               #\T
               (make-silly-string
                 #\i
                 #\r
                 #\i)
               #\T)) true)

(check-expect (palindrome? 
              (make-silly-string
               #\1
               (make-silly-string
                 #\2
                 (make-silly-string
                   #\3
                   empty
                   #\4)
                 #\5)
               #\6)) false)

(check-expect (palindrome?
              (make-silly-string
               #\F
               (make-silly-string
                 #\A
                 (make-silly-string
                   #\R
                   #\1
                   #\R)
                 #\A)
               #\F)) true)

