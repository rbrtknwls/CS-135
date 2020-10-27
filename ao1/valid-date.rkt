;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 01, Problem 5 (A B)
;; ******************************************


;;
;; Problem A
;; 

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
  (cond
    [(divis? a 400) true]
    [(divis? a 100) false]
    [(divis? a 4)   true]
    [else           false]))

;; Tests:

(check-expect (leap-year? 1600) true)
(check-expect (leap-year? 1601) false)
(check-expect (leap-year? 1800) false)
(check-expect (leap-year? 1700) false)
(check-expect (leap-year? 1533) false)
(check-expect (leap-year? 2000) true)
(check-expect (leap-year? 2020) true)
(check-expect (leap-year? 1) false)
(check-expect (leap-year? 4) true)


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


;;
;; Problem B
;;


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
  (cond
    [(< dec (get-month a))   false]
    [(< (day-max (get-month a) (get-year a))
         (get-day a))        false]
    [(withinskip? a)         false]
    [else                    true]))
    
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

;; (getday a) produces the day from the natural
;;    number a by taking the 2 most digits
;; Examples:
(check-expect (get-day 20020455) 55)
(check-expect (get-day 20020400) 00)

;; getday: Num -> Num
;; Requires:
;;   length of a > 4
(define (get-day a)
  (floor (- a (*
     (quotient a 100) 100))))


;; (getmonth a) produces the month from the natural
;;    number a by taking the digits from the
;;    hundreds and thousands column
;; Examples:
(check-expect (get-month 19960321) 3)
(check-expect (get-month 20021302) 13)
(check-expect (get-month 200305) 3)

;; getmonth: Num -> Num
;; Requires:
;;   length of a > 4
(define (get-month a )
  (floor (/ (- a (*
     (quotient a 10000) 10000)) 100)))


;; (getyear a) produces the year from the natural
;;    number a by taking the digits from every
;;    number to the left thousands column
;; Examples:
(check-expect (get-year 2005730) 200)
(check-expect (get-year 11111) 1)

;; getyear: Num -> Num
;; Requires:
;;   length of a > 4
(define (get-year a)
  (floor (/ a 10000)))


;; (day-max a b) using the year which is b
;;    and the month which is a, the function
;;    will produce the max amount of days
;;    that can be in the month
;; Examples:
(check-expect (day-max sep 1999) 30)
(check-expect (day-max feb 2020) 29)
(check-expect (day-max feb 2021) 28)

;; day-max: Num Num -> Num
;; Requires:
;;   length of a <= 12 (december)
(define (day-max a b)
   (cond
     [(or (= a jan) (= a mar) (= a may) (= a jul)
         (= a aug) (= a oct) (= a dec)) 31]
     [(or (= a apr) (= a jun) (= a sep)
          (= a nov))                    30]
     [(leap-year? b)                    29]
     [else                              28]))


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
   (cond
     [(and (= (get-year a) 1752) (= (get-month a) sep)
           (and (<= (get-day a) 13) (>= (get-day a) 3))) true]
     [else                                               false]))


               




    

