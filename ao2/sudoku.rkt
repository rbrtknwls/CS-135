;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 02, Problem 6
;; ******************************************


;; Constants
(define SumOfOneToNine 45) ; Found by adding 1 through 9
(define MulOfOneToNine 362880) ; Found by multiplying 1 through 9

;; Test Cases

(define test1 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
              (cons 9 (cons 8 (cons 7 (cons 6 empty))))))))))
(define test2 (cons 2 (cons 2 (cons 3 (cons 4 (cons 5
              (cons 9 (cons 8 (cons 7 (cons 6 empty))))))))))
(define test3 (cons 45 (cons 0 (cons 0 (cons 0 (cons 0
              (cons 0 (cons 0 (cons 0 (cons 0 empty))))))))))
(define test4 (cons 9 (cons 8 (cons 7 (cons 6 (cons 5
              (cons 4 (cons 3 (cons 2 (cons 1 empty))))))))))
(define test5 (cons 1 (cons 3 (cons 5 (cons 7 (cons 9
              (cons 2 (cons 4 (cons 6 (cons 8 empty))))))))))
(define test6 (cons -1 (cons 2 (cons 3 (cons 4 (cons 5
              (cons 9 (cons -8 (cons 7 (cons 6 empty))))))))))
(define test7 empty)

;; (sudoku-valid? lon) Produces if a 
;;   list of numbers (lon) contains the set
;;   of numbers 1-9 inclusive
;; Examples:
(check-expect (sudoku-valid? test1) true)
(check-expect (sudoku-valid? test2) false)

;; sudoku-valid?: (listof Num) -> Bool
(define (sudoku-valid? lon)
   (and (= (sumoflon lon) SumOfOneToNine)
        (= (multoflon lon) MulOfOneToNine)))

;; Tests
(check-expect (sudoku-valid? test3) false)
(check-expect (sudoku-valid? test4) true)
(check-expect (sudoku-valid? test5) true)
(check-expect (sudoku-valid? test6) false)
(check-expect (sudoku-valid? test7) false)

  
;;***** Helper Functions *****


;; (sumoflon lon) given a list of numbers
;;  described by the variable lon, finds
;;  the sum of the list
;; Example:
(check-expect (sumoflon test1) 45)
(check-expect (sumoflon test2) 46)
(check-expect (sumoflon test3) 45)

;; sumoflon (listof Num) -> Num
(define (sumoflon lon)
  (cond [(empty? lon)        0]
        [else (+ (first lon) (sumoflon (rest lon)))]))


;; (multoflon lon) given a list of numbers
;;  described by the variable lon, finds
;;  the multiplication of each element
;; Example:
(check-expect (multoflon test1) 362880)
(check-expect (multoflon test2) 725760)
(check-expect (multoflon test3) 0)

;; multoflon (listof Num) -> Num
(define (multoflon lon)
  (cond [(empty? lon)        1]
        [else (* (first lon) (multoflon (rest lon)))]))
