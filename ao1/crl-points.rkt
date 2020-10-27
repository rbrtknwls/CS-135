;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname crl-points) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 01, Problem 4
;; ******************************************

;; Constants
(define min-vi 0.95); 5% of points are lost for a minor violation
(define maj-vi 0.75); 25% of points are lost for a major violation
(define max-vi 0); 100% of points are lost for a disqualification

;; (crl-points a b c d) produces the amount of points 
;;   that a person with a first places, b second places
;;   c third places, and a status of d will have
;; Examples:
(check-expect (crl-points 0 2 4 'minor-violation) 90)
(check-expect (crl-points 2 4 6 'disqualified) 0)
(check-expect (crl-points 2 5 10 'good-standing) 315)

;; sequence-type: Nat Nat Nat Sym -> Nat
(define (crl-points a b c d)
  (cond
    [(symbol=? d 'good-standing)
        (floor (point_total a b c))]
    [(symbol=? d 'minor-violation)
        (floor (* (point_total a b c) min-vi))]
    [(symbol=? d 'major-violation)
        (floor (* (point_total a b c) maj-vi))]
    [else
        (floor (* (point_total a b c) max-vi))]))
    

;; Tests:
(check-expect (crl-points 2 0 0 'minor-violation) 95)
(check-expect (crl-points 0 2 0 'major-violation) 30)
(check-expect (crl-points 0 0 2 'good-standing) 20)
(check-expect (crl-points 0 2 2 'disqualified) 0)
(check-expect (crl-points 0 2 3 'disqualified) 0)
(check-expect (crl-points 3 1 0 'good-standing) 170)
(check-expect (crl-points 2 7 0 'minor-violation) 242)
(check-expect (crl-points 2 7 0 'major-violation) 191)

;;***** Helper Functions *****

;; (point_total a b c) counts the points that a first places,
;;    b second places and c third places give
;; Example:
(check-expect (point_total 0 0 0) 0)
(check-expect (point_total 1 2 3) 135)
(check-expect (point_total 0 0 1) 10)
(check-expect (point_total 0 2 4) 95)

;; isgeometric: Nat Nat Nat -> Nat
(define (point_total a b c)
  (cond
    [(>= (+ a b c) 5) 
      (+ (* 50 a) (* 20 b) (* 10 c) 15)]
    [else
      (+ (* 50 a) (* 20 b) (* 10 c))]))

