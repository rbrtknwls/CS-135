;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname shortans) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Final Exams, Problem 3(a-c)
;; ******************************************


;; =================================
;;
;; Problem (3a) [make-it-so]
;;
;; =================================

(define (make-it-so x k)
  (foldr (lambda (x k)
           (cond [(odd? x) k]
                 [else (cons (* 2 x) k)]))
         empty
         (map (lambda (x) (+ x k)) x)))

;; (make-it-so2 l_num addit) given a list of numbers (l_num) and
;;  an additive (addit), will add each element by the additive
;;  and then find the sum of all the even numbers multiplied by two

;; make-it-so2: (listof Num) Num -> (listof Num)
(define (make-it-so2 l_num addit) 
  (cond [(empty? l_num) empty]
        [(even? (+ (first l_num) addit))
                (cons (* (+ (first l_num) addit) 2)
                      (make-it-so2 (rest l_num) addit))]
        [else         (make-it-so2 (rest l_num) addit)]))
        
                                            


;; =================================
;; Testing Suite
;; =================================


;; === General Tests ===
(check-expect (make-it-so  '(1) 1) '(4))
(check-expect (make-it-so2 '(1) 1) '(4))
(check-expect (make-it-so  '(1 2 4) 1) '(4))
(check-expect (make-it-so2 '(1 2 4) 1) '(4))
(check-expect (make-it-so  '(1 3 5) 1) '(4 8 12))
(check-expect (make-it-so2 '(1 3 5) 1) '(4 8 12))
(check-expect (make-it-so  '(1 3 5) 3) '(8 12 16))
(check-expect (make-it-so2 '(1 3 5) 3) '(8 12 16))
(check-expect (make-it-so  '(1 3 5) 2) '())
(check-expect (make-it-so2 '(1 3 5) 2) '())
(check-expect (make-it-so  '(0 0) 10) '(20 20))
(check-expect (make-it-so2 '(0 0) 10) '(20 20))



;; =================================
;;
;; Problem (3b) [fun]
;;
;; =================================



;; The contract tells us that fun must return a Num, however note
;;  that when lst is empty fun will return a:
;;
;;                          empty
;;
;; This is a problem as empty is not a Num  and thus fun will violate
;;  the contract.

                                             
;; =================================
;;
;; Problem (3c) [xnode]b 
;;
;; =================================


(define-struct xnode (label children))
;; an exam tree node (XNode) is a (make-xnode Num (listof XTree))
;; an exam tree (XTree) is one of:
;; * a XNode or
;; * a Num.

;; Fill in the contract for g here
;; g: XTree -> Num

;(define (g t)
;  (cond [(number? t) t]
;        [else
;         (h (xnode-label t)
;            (foldr
;             + 0 (map g
;                      (xnode-children t))))]))

;; Fill in the contract for h here
;; h: Num Num -> Num

