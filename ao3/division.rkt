;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname division) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 03, Problem 5
;; ******************************************

;; ** Test Cases **

;; General Test Cases
(define test1var1 20)   (define test1var2 5)
(define solution1
        (cons 4 (cons 0 empty)))

(define test2var1 20)   (define test2var2 4)
(define solution2
          (cons 5 (cons 0 empty)))

(define test3var1 39)   (define test3var2 20)
(define solution3
          (cons 1 (cons 19 empty)))

(define test4var1 30)   (define test4var2 1)
(define solution4
          (cons 30 (cons 0 empty)))

(define test5var1 15)   (define test5var2 7)
(define solution5
          (cons 2 (cons 1 empty)))

(define test6var1 27)   (define test6var2 2)
(define solution6
          (cons 13 (cons 1 empty)))

(define test7var1 100)  (define test7var2 21)
(define solution7
          (cons 4 (cons 16 empty)))

(define test8var1 77)   (define test8var2 21)
(define solution8
          (cons 3 (cons 14 empty)))

(define test9var1 74)   (define test9var2 4)
(define solution9
            (cons 18 (cons 2 empty)))

(define test10var1 12)  (define test10var2 5)
(define solution10
            (cons 2 (cons 2 empty)))

(define test11var1 1)   (define test11var2 1)
(define solution11
            (cons 1 (cons 0 empty)))

(define test12var1 774) (define test12var2 10)
(define solution12
            (cons 77 (cons 4 empty)))

(define test13var1 20)  (define test13var2 3)
(define solution13
            (cons 6 (cons 2 empty)))

(define test14var1 15)  (define test14var2 10)
(define solution14
            (cons 1 (cons 5 empty)))

;; Numa < Numb Test Cases
(define test15var1 1)   (define test15var2 2)
(define solution15
            (cons 0 (cons 1 empty)))

(define test16var1 499) (define test16var2 500)
(define solution16
            (cons 0 (cons 499 empty)))

(define test17var1 13)  (define test17var2 200)
(define solution17
            (cons 0 (cons 13 empty)))

(define test18var1 10)  (define test18var2 20)
(define solution18
            (cons 0 (cons 10 empty)))

;; Zero Test Cases
(define test19var1 0)   (define test19var2 15)
(define solution19
            (cons 0 (cons 0 empty)))

(define test20var1 0)   (define test20var2 1)
(define solution20
            (cons 0 (cons 0 empty)))


;;
;;   Problem 5
;;


;; (divide numa numb) Produces the list of the quotient and
;;   the remainder when numa is divided by numb

;; Examples:
(check-expect (divide 17 5)
              (cons 3 (cons 2 empty)))
(check-expect (divide 12 6)
              (cons 2 (cons 0 empty)))

;; divide: Nat Int -> (list Nat Nat)
;; require:
;;         numb > 0
(define (divide numa numb)
  (cons (find-quotient numa numb)
        (cons (- numa (* (find-quotient numa numb) numb)) empty)))

;; Basic Tests
(check-expect (divide test1var1 test1var2) solution1)
(check-expect (divide test2var1 test2var2) solution2)
(check-expect (divide test3var1 test3var2) solution3)
(check-expect (divide test4var1 test4var2) solution4)
(check-expect (divide test5var1 test5var2) solution5)
(check-expect (divide test6var1 test6var2) solution6)
(check-expect (divide test7var1 test7var2) solution7)
(check-expect (divide test8var1 test8var2) solution8)
(check-expect (divide test9var1 test9var2) solution9)
(check-expect (divide test10var1 test10var2) solution10)
(check-expect (divide test11var1 test11var2) solution11)
(check-expect (divide test12var1 test12var2) solution12)
(check-expect (divide test13var1 test13var2) solution13)
(check-expect (divide test14var1 test14var2) solution14)
  
;; Larger Divisor Test
(check-expect (divide test15var1 test15var2) solution15)
(check-expect (divide test16var1 test16var2) solution16)
(check-expect (divide test17var1 test17var2) solution17)
(check-expect (divide test18var1 test18var2) solution18)

;; Zero Tests
(check-expect (divide test19var1 test19var2) solution19)
(check-expect (divide test20var1 test20var2) solution20)


;; ** Helper Functions **

;; (find-quotient numa numb) Produces the quotient between 
;;   a number (numa) and a number that divides it (numb)

;; Examples
(check-expect (find-quotient 10 3) 3)
(check-expect (find-quotient 2 3) 0)

;; find-quotient: Nat Nat -> Nat
(define (find-quotient numa numb)
  (cond [(< numa numb) 0]
        [else (add1 (find-quotient (- numa numb) numb))]))





