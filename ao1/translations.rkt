;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname translations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 01, Problem 2 (A,B)
;; ******************************************

;; Universal Constant
(define tolerance 0.01); Tolerance for check-within


;;
;;   Problem 2A
;;


;; (volume r) produces the volume of a sphere
;;   using r as the radius
;; Examples:
(check-within (volume 3) 113.09 tolerance)
(check-within (volume 1) 4.18 tolerance)

;; volume: Num -> Num
;;    Requires:
;;    r >= 0
(define (volume r)
  (* (/ 4 3) pi (expt r 3)))

;; Tests:
(check-within (volume 0) 0 tolerance)
(check-within (volume 5.5) 696.91 tolerance)
(check-within (volume 0.5) 0.52 tolerance)
(check-within (volume 20) 33510.32 tolerance)
(check-within (volume 7) 1436.75 tolerance)


;;
;;   Problem 2B
;;


(define phi (/ (+ 1 (sqrt 5)) 2)); Golden ratio

;; (fib n) produces the nth number of the
;;    Fibonacci number sequence
;; Example:
(check-within (fib 1) 1 tolerance)
(check-within (fib 3) 2 tolerance)

;; fib: Nat -> Nat
;;    Requires:
;;    n > 0
(define (fib n)
   (/ (- (expt phi n) (expt (* phi -1) (* n -1)))
      (- (* 2 phi) 1)))

;; Tests:
(check-within (fib 2) 1 tolerance)
(check-within (fib 5) 5 tolerance)
(check-within (fib 7) 13 tolerance)
(check-within (fib 8) 21 tolerance)