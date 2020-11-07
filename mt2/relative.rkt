;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm 02, Problem 3
;; ******************************************


;; -------Testing Struct------------
;; A Question is a list of Ints
;; A Answer is a list of Ints

(define-struct test (q a))
;; A Test is a (make-test Question Answer)
;; ---------------------------------


;; =================================
;; Testing Suite
;; =================================

;; ======= "Empty" Tests ===========

(define test1 (make-test empty
                         empty))
(define test2 (make-test '(0)
                         '(0)))

;; ======= Negative Tests ==========

(define test3 (make-test '(2 -4 6 -8 10 -12)
                         '(2 -2 4 -4 6 -6)))
(define test4 (make-test '(-1 -2 -3 -4 -5 -6) 
                         '(-1 -3 -6 -10 -15 -21)))
(define test5 (make-test '(-1 -2  3  4 -5 -6 100) 
                         '(-1 -3 0 4 -1 -7 93)))

;; ======= Positive Tests ==========

(define test6 (make-test '(1 1 2 3)
                         '(1 2 4 7)))
(define test7 (make-test '(2 4 6 10 20 60) 
                         '(2 6 12 22 42 102)))
(define test8 (make-test '(1 0 1 0 1 0 1 0)
                         '(1 1 2 2 3 3 4 4)))
(define test9 (make-test '(10 20 30 40 50) 
                         '(10 30 60 100 150)))
(define test10 (make-test '(9 90 900 9000 90000) 
                          '(9 99 999 9999 99999)))


;; =================================
;;
;; Question 3
;;
;; =================================



;; (relative->absolute loi) Consumes a list of integers (loi) 
;;   and produces a list of values that results from
;;  adding these values one at a time
;; Examples:

(check-expect (relative->absolute '(3 -2 4)) '(3 1 5))
(check-expect (relative->absolute empty) empty)

;; relative->absolute: (listof Int) -> (listof Int)
(define (relative->absolute loi)
  (cond [(empty? loi)  empty]
        [else (find-abso loi 0)]))


;; ===  Helper Functions  ===


;; (find-abso loi start) Consumes a list of integers (loi) 
;;   and a starting value (start) to produces a list of values
;;   which is the value of adding each element of loi to the start
;; Examples:

(check-expect (find-abso '(3 -2 4) 0) '(3 1 5))
(check-expect (find-abso '(2 -1) 0) '(2 1))

;; find-abso: (listof Int) Int -> (listof Int)
(define (find-abso loi start)
  (cond [(empty? loi)  empty]
        [else
         (cons (+ start (first loi))
               (find-abso (rest loi)
                          (+ start (first loi))))]))
;; Empty Tests 
(check-expect (relative->absolute (test-q test1)) (test-a test1))
(check-expect (relative->absolute (test-q test2)) (test-a test2))

;; Negative Tests
(check-expect (relative->absolute (test-q test3)) (test-a test3))
(check-expect (relative->absolute (test-q test4)) (test-a test4))
(check-expect (relative->absolute (test-q test5)) (test-a test5))

;; Positive Tests

(check-expect (relative->absolute (test-q test6)) (test-a test6))
(check-expect (relative->absolute (test-q test7)) (test-a test7))
(check-expect (relative->absolute (test-q test8)) (test-a test8))
(check-expect (relative->absolute (test-q test9)) (test-a test9))
(check-expect (relative->absolute (test-q test10)) (test-a test10))

              







