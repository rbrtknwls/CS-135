;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname strictly-alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 08, Problem 3(A-D)
;; ******************************************



;; =================================
;;
;; Question 2A
;;
;; =================================



;; (occurrences index lon) Produces the number of occurrences 
;;    a number (index) has within a given list of numbers (lon)
;;    amounts of 1s.
;; Examples:
(check-expect (occurrences 2  '(1 2 1 2 2 3 1)) 3)
(check-expect (occurrences 1  '(1 1 1)) 3)
(check-expect (occurrences 12 '( 1 2 3 4 5)) 0)

;; occurrences: Num (listof Num) -> Num
(define (occurrences index lon)

  (foldr (lambda (x rorr)
                 (cond [(= x index) (add1 rorr)]
                       [else              rorr]))
         0
         lon))


;; Basic Tests
(check-expect (occurrences 3  '(33 3 33 3 33))       2)
(check-expect (occurrences 5  '(1 2 3 4 5 4 3 2 1))  1)
(check-expect (occurrences 1  '(1 2 3 4 5 4 3 2 1))  2)
(check-expect (occurrences 10 '(1 10 100 1 10 10))   3)
(check-expect (occurrences 0  '(0 0 0 0 0 0 0 0 0))  9)


;; Negitives
(check-expect (occurrences -7  '(7 -7))              1)
(check-expect (occurrences -3  '(-3 4 -3 5 -3 -3))   4)
(check-expect (occurrences -10  '(-10 -20 -10 -10))  3)


;; Real Numbers
(check-expect (occurrences 0.1  '(0.1 1 10 0.1))     2)
(check-expect (occurrences 0.3  '(0.3 0.333 0.33))   1)
(check-expect (occurrences 0.9  '(0.9 0.9 0.9 0.9))  4)

;; Not in List
(check-expect (occurrences 0.4  '(0.3 0.333 0.33))   1)
(check-expect (occurrences 10  '(-10 -20 -10 -10))   3)



;; =================================
;;
;; Question 2B
;;
;; =================================




;; =================================
;;
;; Question 2C
;;
;; =================================




;; =================================
;;
;; Question 2D
;;
;; =================================









