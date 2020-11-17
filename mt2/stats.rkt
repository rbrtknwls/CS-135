;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname stats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm 02, Problem 1
;; ******************************************


;; ==================================
;; struct and data definitions For Q7
;; =================================


;; A Resource is (anyof 'fire 'wood 'water)

(define-struct card (type power))
;; A Card is a (make-card Resource Nat)

;; An Outcome is (anyof 'player-1 'player-2 'tie-game)







;; =================================
;;
;; Question 7
;;
;; =================================



;; (median lon) Consumes a List of Numbers (lon) 
;;   and then produces the median of the lon
;; Examples:

(check-expect (median (list 9 32.4 -2 6 28 22 129332)) 22)
(check-expect (median (list 7 827 -23.2 -300)) -8.1)
(check-expect (median (list 7 7 7 7 7 89)) 7)

;; median: (listof Num) -> Num
;; Requires: lon is nonempty
(define (median lon)
  (cond [(empty? lon)                         empty]
        [(= (length lon) 1)             (first lon)]
        [(= (length lon) 2)  (/ (+ (first lon)
                                   (second lon)) 2)]
        [else  (median (rm-maxandmin lon (getmax lon) false
                                         (getmin lon) false))]))
                                                   

        
;; ===  Helper Functions  ===


;; (rm-maxandmin lon max ma min mi) Consumes a List of Numbers (lon) 
;;   (lon), Nums (max, min) and Bools (ma, mi) and then
;;   produces a new list of numbers where the highest and lowest
;;   value is given, ma and mi are used to see if the max or min
;;   is already picked
;; Examples:

(check-expect (rm-maxandmin (list 1 2 3 4 5) 1 false 5 false)
                            (list 2 3 4))
(check-expect (rm-maxandmin (list 7 827 -23.2 -300)
                            827 false -300 false) (list 7 -23.2))
(check-expect (rm-maxandmin (list 7 7 7 7 7 89)
                            7 false 89 false) (list 7 7 7 7))

;; rm-maxandmin: (listof Num) Num Bool Num Bool -> (listof Num)
(define (rm-maxandmin lon max ma min mi)
  (cond [(empty? lon)                        empty]
        [(and (not ma) (= (first lon) max))
                  (rm-maxandmin (rest lon) max true
                                           min mi)]
        [(and (not mi) (= (first lon) min))
                  (rm-maxandmin (rest lon) max ma
                                           min true)]
        [else
                  (cons (first lon)
                        (rm-maxandmin (rest lon) max
                                      ma min mi))]))

;; (getmax lon) Consumes a List of Numbers (lon) and
;;   produces the maximum value of the lon
;; Examples:

(check-expect (getmax (list 1 2 3 4 5)) 5)
(check-expect (getmax (list 7 827 -23.2 -300)) 827)
(check-expect (getmax (list 7 7 7 7 7 89)) 89)

;; getmax: (listof Num) -> Num
(define (getmax lon)
  (cond [(empty? (rest lon))    (first lon)]
        [else     (max (first lon)
                       (getmax (rest lon)))]))


;; (getmin lon) Consumes a List of Numbers (lon) and
;;   produces the minimum value of the lon
;; Examples:

(check-expect (getmin (list 1 2 3 4 5)) 1)
(check-expect (getmin (list 7 827 -23.2 -300)) -300)
(check-expect (getmin (list 7 7 7 7 7 89)) 7)

;; getmin: (listof Num) -> Num
(define (getmin lon)
  (cond [(empty? (rest lon))    (first lon)]
        [else     (min (first lon)
                       (getmin (rest lon)))]))

;; Empty Tests
(check-expect (median empty) empty)

;; General Tests
(check-expect (median (list 1 2)) 1.5)
(check-expect (median (list 1 2 3)) 2)
(check-expect (median (list 1 2 3 4)) 2.5)
(check-expect (median (list 1 2 3 4 5)) 3)
(check-expect (median (list -100 -20 3 20 100)) 3)
(check-expect (median (list 1 2 3 4 5 6 7 8 9 10)) 5.5)
(check-expect (median (list 1 2 3 4 5 6 7 8 9 10
                            11 12 13 14 15 16 17
                            18 19 20)) 10.5)
(check-expect (median (list 1 2 3  -4 5 6 7 8 9 10
                            -11 12 -13 14 15 16 17
                            18 19 -20)) 7.5)