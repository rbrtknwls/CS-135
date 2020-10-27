;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname alternate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 03, Problem 4
;; ******************************************

;; ** Test Cases **

;; General Test Cases
(define test1 20)
(define solution1 2)

(define test2 1)
(define solution2 0)

(define test3 10)
(define solution3 1)

(define test4 7)
(define solution4 0)

(define test5 14)
(define solution5 1)

(define test6 2)
(define solution6 1)

(define test7 4)
(define solution7 2)

(define test8 8)
(define solution8 3)

(define test9 16)
(define solution9 4)

(define test10 32)
(define solution10 5)

;; Zero Test Cases
(define test11 0)
(define solution11 0)

(define test12 -0)
(define solution12 0)

;; Extra Test Cases

(define test13 3)
(define solution13 0)

(define test14 4)
(define solution14 2)

(define test15 2)
(define solution15 1)

(define test16 200)
(define solution16 3)

(define test17 64)
(define solution17 6)

(define test18 17)
(define solution18 0)

(define test19 34)
(define solution19 1)

(define test20 9192)
(define solution20 3)


;;
;;   Problem 4A
;;

;; Natural Numbers (Nat) are of the form:
;; 0
;; (* Nat 2)
;; (* (add1 Nat) 2)

;; alt-nat-template: Nat -> Any
(define (alt-nat-template num)
  (cond [(zero? num) ...]
        [else (... [(even? num) (alt-nat-template (/ num 2))]
                   [(odd? num)  (alt-nat-template (/ (sub1 num) 2))]
                   [(...)    (...)])]))

;;
;;   Problem 4B
;;


;; (powers-of-2 num) Produces the amount of times a natural
;;   number (num) can be divided by 2 before reaching an odd
;;   number or zero

;; Examples:
(check-expect (powers-of-2 12) 2)
(check-expect (powers-of-2 0) 0)

;; powers-of-2: Nat -> Nat
(define (powers-of-2 num)
  (cond [(zero? num) 0]
        [else  (cond
                 [(even? num) (+ 1 (powers-of-2 (/ num 2)))]
                 [else                                0])]))

;; Basic Tests
(check-expect (powers-of-2 test1) solution1)
(check-expect (powers-of-2 test2) solution2)
(check-expect (powers-of-2 test3) solution3)
(check-expect (powers-of-2 test4) solution4)
(check-expect (powers-of-2 test5) solution5)
(check-expect (powers-of-2 test6) solution6)
(check-expect (powers-of-2 test7) solution7)
(check-expect (powers-of-2 test8) solution8)
(check-expect (powers-of-2 test9) solution9)
(check-expect (powers-of-2 test10) solution10)

;; Zero Tests
(check-expect (powers-of-2 test11) solution11)
(check-expect (powers-of-2 test12) solution12)

;; Extra Tests
(check-expect (powers-of-2 test13) solution13)
(check-expect (powers-of-2 test14) solution14)
(check-expect (powers-of-2 test15) solution15)
(check-expect (powers-of-2 test16) solution16)
(check-expect (powers-of-2 test17) solution17)
(check-expect (powers-of-2 test18) solution18)
(check-expect (powers-of-2 test19) solution19)
(check-expect (powers-of-2 test20) solution20)




