;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 02, Problem 3
;; ******************************************

;; Constants
(define selfc-weight 0.1) ; Weight of Self Check Marks
(define assign-weight 0.60) ; Weight of Assigment Marks
(define mt1-weight 0.07) ; Weight of Midterm 1 Mark
(define mt2-weight 0.07) ; Weight of Midterm 2 Mark
(define fe-weight 0.16) ; Weight of Final Exam Mark
(define failtresh 0.5) ; Lowest Bound for Passing
(define fail-ub 0.46) ; Upper Mark Bound in case of Failure

;; (cs135-grade self-check assign mt1 mt2 final)
;;    Produces the final grade one will have, either
;;    being the sum of self-check, assign, mt1, mt2, final
;;    or the lowest between the 46 and the sum if
;;    the final exam (final) + midterm 1-2 (mt1,mt2) or the
;;    assigmnents (assgn) grades are lest then 50 inclusive
;; Examples:
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0.95) 68.3)
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0) 46)
(check-expect (cs135-grade 1 0 0 0 0) 10)

;; cs135-grade: (listof Num) -> Num
;; Require:
;;    0 <= self-check <= 1
;;    0 <= assign <= 1
;;    0 <= mt1 <= 1
;;    0 <= mt2 <= 1
;;    0 <= final <= 1

(define (cs135-grade self-check assign mt1 mt2 final)
  (cond [(< (exam-mark mt1 mt2 final) failtresh)    (* 100 (min
             (normal_calc self-check assign mt1 mt2 final) fail-ub))]
        [(< assign failtresh)                       (* 100 (min
             (normal_calc self-check assign mt1 mt2 final) fail-ub))]
        [else                                 (* 100
             (normal_calc self-check assign mt1 mt2 final) )]))

;; Tests
(check-expect (cs135-grade 1 1 1 1 1) 100)
(check-expect (cs135-grade 0.9 0.4 1 1 1) 46)
(check-expect (cs135-grade 0.9 1 1 1 0.4) 89.4)
(check-expect (cs135-grade 0.6 1 0.5 0.5 0.4) 46)
(check-expect (cs135-grade 0.5 1 0.5 0.2 0.4) 46)
(check-expect (cs135-grade 0.2 0.7 0.2 0.8 0.6) 60.6)
(check-expect (cs135-grade 0 0 0 0 0) 0)
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0.90) 68.3)

;; Singular Mark Tests:
(check-expect (cs135-grade 1 0 0 0 0) 10)
(check-expect (cs135-grade 0 1 0 0 0) 46)
(check-expect (cs135-grade 0 0 1 0 0) 7)
(check-expect (cs135-grade 0 0 0 1 0) 7)
(check-expect (cs135-grade 0 0 0 0 1) 16)

;;***** Helper Functions *****


;; (exam-mark mt1 mt2 exam) Produces the exam total by
;;   taking the sum of midterm 1 (mt1) midterm 2 (mt2)
;;   and the final exam (exam) and combine this sum
;;   with different weights
;; Example:
(check-expect (exam-mark 0.5 0.5 0.5) 0.5)
(check-expect (exam-mark 1 1 0.4) 0.68)
(check-expect (exam-mark 0 0 0.9) 0.48)
(check-expect (exam-mark 1 1 1) 1)
(check-expect (exam-mark 0 0 0) 0)

;; exam-mark: Num Num Num -> Num
(define (exam-mark mt1 mt2 exam)
  (/ (+ (* mt1 mt1-weight) (* mt2 mt2-weight) (* exam fe-weight))
     (+ mt1-weight mt2-weight fe-weight)))


;; (normal_calc sc asn mt1 mt2 exam) Produces the mark total by
;;   taking the sum of midterm 1 (mt1),  midterm 2 (mt2)
;;   and the final exam (exam), self check (sc), assignements (asn)
;;   and combine this sum with different weights
;; Example:
(check-expect (normal_calc 0.5 0.5 0.5 0.5 0.5) 0.5)
(check-expect (normal_calc 0.4 0.5 0.6 0.7 0.8) 0.559)
(check-expect (normal_calc 0.4 0.4 0.6 0.7 0.8) 0.499)
(check-expect (normal_calc 1 1 1 1 1) 1)
(check-expect (normal_calc 0 0 0 0 0) 0)

;; normal_calc: Num Num Num Num Num -> Num
(define (normal_calc sc asn mt1 mt2 exam)
  (/ (+ (* sc selfc-weight) (* asn assign-weight)
        (* mt1 mt1-weight) (* mt2 mt2-weight) (* exam fe-weight)) 
     (+ selfc-weight assign-weight mt1-weight mt2-weight fe-weight)))

