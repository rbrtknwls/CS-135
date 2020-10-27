;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname extremes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 02, Problem 2(A-C)
;; ******************************************

;;
;;   Problem 2A
;;

;; (smallest lon) produces the lowest number out of
;;    a list-of-numbers (lon), by comparing each
;;    element to the one in front, starting at the end
;; Examples:
(check-expect (smallest (cons 5 empty)) 5)
(check-expect (smallest (cons 5 (cons 2 empty))) 2)

;; smallest: (listof Num) -> Num
(define (smallest lon)
  (cond [(empty? (rest lon)) (first lon)]
        [(cons? lon)         (min (first lon)
                                  (smallest (rest lon)))]))

;; Tests
(check-expect (smallest (cons -5 (cons 2 (cons 10.5 empty)))) -5)
(check-expect (smallest (cons 0 (cons 0 empty))) 0)
(check-expect (smallest (cons 33 (cons -20 (cons -19 empty)))) -20)
(check-expect (smallest (cons 912 (cons 999 empty))) 912)
(check-expect (smallest (cons -1.5 (cons 2.2 empty))) -1.5)
(check-expect (smallest (cons 5.52 (cons 4 empty))) 4)

;;
;;   Problem 2B
;;

;; (largest lon) produces the highest number out of
;;    a list-of-numbers (lon), by comparing each
;;    element to the one in front, starting at the end
;; Examples:
(check-expect (largest (cons 5 empty)) 5)
(check-expect (largest (cons 5 (cons 2 empty))) 5)

;; largest: (listof Num) -> Num
(define (largest lon)
  (cond [(empty? (rest lon)) (first lon)]
        [(cons? lon)         (max (first lon)
                                  (largest (rest lon)))]))

;; Tests
(check-expect (largest (cons -5 (cons 2 (cons 10.5 empty)))) 10.5)
(check-expect (largest (cons 0 (cons 0 empty))) 0)
(check-expect (largest (cons 2001 (cons 2000 empty))) 2001)
(check-expect (largest (cons 0.95 (cons 0.21 empty))) 0.95)
(check-expect (largest (cons -1.5 (cons 2.2 empty))) 2.2)
(check-expect (largest (cons 5.52 (cons 4 empty))) 5.52)

;;
;;   Problem 2C
;;

;; (max-diff lon) produces the largest difference out of
;;    a list-of-numbers (lon), by subracting the
;;    maximum and minimum values.
;; Examples:
(check-expect (max-diff (cons 5 empty)) 0)
(check-expect (max-diff (cons 5 (cons 2 empty))) 3)

;; max-diff: (listof Num) -> Num
(define (max-diff lon)
  (cond [(empty? (rest lon)) 0]
        [(cons? lon)         (- (largest lon)
                             (smallest lon))]))

;; Tests
(check-expect (max-diff (cons -5 (cons 2 (cons 10.5 empty)))) 15.5)
(check-expect (max-diff (cons 0 (cons 0 empty))) 0)
(check-expect (max-diff (cons 2001 (cons 2000 empty))) 1)
(check-expect (max-diff (cons 0.95 (cons 0.21 empty))) 0.74)
(check-expect (max-diff (cons -1.5 (cons 2.2 empty))) 3.7)
(check-expect (max-diff (cons 5.52 (cons 4 empty))) 1.52)
(check-expect (max-diff (cons 5.52 empty)) 0)