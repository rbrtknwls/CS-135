;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date-bool) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 01, Problem 5 (5 C)
;; ******************************************

;; Constants
(define jan 1) (define feb 2)
(define mar 3) (define apr 4)
(define may 5) (define jun 6)
(define jul 7) (define aug 8)
(define sep 9) (define oct 10)
(define nov 11) (define dec 12)


;; (valid-date? a) produces if the year, date and month
;;    as representated by a is valid
;; Examples:
(check-expect (valid-date? 20021302) false)
(check-expect (valid-date? 11232) false)
(check-expect (valid-date? 19960812) true)

;; valid-date?: Num -> Bool
;; Requires:
;;   length of a > 4
(define (valid-date? a)
  (and (<= (get-month a) 12) (not (withinskip? a))
       (day-in-boundry (get-month a) (get-year a) (get-day a))))
    
;; Tests:

(check-expect (valid-date? 17520910) false)
(check-expect (valid-date? 17520930) true)
(check-expect (valid-date? 11111) true)
(check-expect (valid-date? 20200230) false)
(check-expect (valid-date? 28888) false)
(check-expect (valid-date? 19930812) true)
(check-expect (valid-date? 18881517) false)
(check-expect (valid-date? 20060423) true)
(check-expect (valid-date? 20070101) true)
(check-expect (valid-date? 20080229) true)
(check-expect (valid-date? 20010229) false)
(check-expect (valid-date? 20010228) true)
(check-expect (valid-date? 2000712) true)

;; *** Helper Functions ***


;; (leap-year? a) Checks if the natural number a
;;   (which represents a year) is a leap year
;; Examples:
(check-expect (leap-year? 1996) true)
(check-expect (leap-year? 1904) true)
(check-expect (leap-year? 1900) false)

;; leap-year: Nat -> Bool
;; Require:
;;   a > 0
(define (leap-year? a)
  (or (divis? a 400) (and
      (not (divis? a 100)) (divis? a 4))))


;; (divis? a b) Check if the natural number
;;    a is divisible by the natural number b
;; Examples:
(check-expect (divis? 4 2) true)
(check-expect (divis? 20 5) true)
(check-expect (divis? 20 3) false)
(check-expect (divis? 1 400) false)

;; divis?: Nat Nat -> Bool
;; Require:
;;   a > 0
(define (divis? a b)
    (= a (* (quotient a b) b)))


;; (get-day a) produces the day from the natural
;;    number a by taking the 2 most digits
;; Examples:
(check-expect (get-day 20020455) 55)
(check-expect (get-day 20020400) 00)

;; get-day: Num -> Num
;; Requires:
;;   length of a > 4
(define (get-day a)
  (floor (- a (*
     (quotient a 100) 100))))


;; (get-month a) produces the month from the natural
;;    number a by taking the digits from the
;;    hundreds and thousands column
;; Examples:
(check-expect (get-month 19960321) 3)
(check-expect (get-month 20021302) 13)
(check-expect (get-month 200305) 3)

;; get-month: Num -> Num
;; Requires:
;;   length of a > 4
(define (get-month a )
  (floor (/ (- a (*
     (quotient a 10000) 10000)) 100)))


;; (get-year a) produces the year from the natural
;;    number a by taking the digits from every
;;    number to the left thousands column
;; Examples:
(check-expect (get-year 2005730) 200)
(check-expect (get-year 11111) 1)

;; get-year: Num -> Num
;; Requires:
;;   length of a > 4
(define (get-year a)
  (floor (/ a 10000)))


;; (day-in-boundry a b c) using the year which is b,
;;    the month which is a, and c which is the
;;    day will produce if the current day is 
;;    inside the bounds of the month
;; Examples:
(check-expect (day-in-boundry sep 1999 31) false)
(check-expect (day-in-boundry feb 2020 29) true)
(check-expect (day-in-boundry feb 2021 28) true)
(check-expect (day-in-boundry apr 2020 31) false)
(check-expect (day-in-boundry aug 2021 31) true)

;; day-in-boundry: Num Num Num -> Bool
;; Requires:
;;   b <= 12
(define (day-in-boundry a b c)
   (or (and (<= c 31) (or (= a jan) (= a mar) (= a may)
        (= a jul) (= a aug) (= a oct) (= a dec)))
       (and (<= c 30) (or (= a apr) (= a jun)
        (= a sep) (= a nov)))
       (and (<= c 29) (leap-year? b))
       (<= c 28)))


;; (withinskip? a) checks if the natural number
;;    a is within the time skip that happened
;;    in 1753 september 3 - 13 and produces
;;    the result
;; Examples:
(check-expect (withinskip? 17520913) true)
(check-expect (withinskip? 17520903) true)
(check-expect (withinskip? 17520907) true)
(check-expect (withinskip? 17520914) false)
(check-expect (withinskip? 20913) false)

;; withinskip?: Num -> Bool
;; Requires:
;;   length of a > 4 
(define (withinskip? a)
   (and (= (get-year a) 1752) (= (get-month a) sep)
           (and (<= (get-day a) 13) (>= (get-day a) 3))))



               




    

