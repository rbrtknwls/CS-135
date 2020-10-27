;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname soft) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm-1, Problem 5 (A-C)
;; ******************************************

;; ** Constants **
(define tol 0.01)

;; ** Test Cases **

;; Single Element Tests
(define test1  1.0)
(define sout1a 2.72)

(define norm1  2)
(define sout11b 1.36)
(define sout12b -1)
(define sout13b -1)

(define sout11c 1)
(define sout12c -1)
(define sout13c -1)


(define test2  2.5)
(define sout2a 12.18)

(define norm2  7)
(define sout21b 1.74)
(define sout22b -1)
(define sout23b -1)

(define sout21c 1)
(define sout22c -1)
(define sout23c -1)


;; Single List Element Tests
(define test3  (cons -2 empty))
(define sout3a 0.14)

(define norm3  0.2)
(define sout31b 0.67)
(define sout32b -1)
(define sout33b -1)

(define sout31c 1)
(define sout32c -1)
(define sout33c -1)

  
;; Two List Element Tests
(define test4  (cons 3 (cons -2 empty)))
(define sout4a 20.22)

(define norm4  5)
(define sout41b 4.02)
(define sout42b 0.03)
(define sout43b -1)

(define sout41c 0.99)
(define sout42c 0.01)
(define sout43c -1)


(define test5  (cons 7 (cons 8 empty)))
(define sout5a 4077.59)

(define norm5  -2)
(define sout51b -548.31)
(define sout52b -1490.48)
(define sout53b -1)

(define sout51c 0.27)
(define sout52c 0.73)
(define sout53c -1)

  
;; Three List Element Tests
(define test6  (cons 0.1 (cons 0.2
               (cons 0.3 empty))))
(define sout6a 3.67)

(define norm6  1)
(define sout61b 1.11)
(define sout62b 1.22)
(define sout63b 1.35)

(define sout61c 0.3)
(define sout62c 0.33)
(define sout63c 0.37)


(define test7  (cons -0.1 (cons -0.2
               (cons -0.3 empty))))
(define sout7a 2.46)

(define norm7  1)
(define sout71b 0.90)
(define sout72b 0.82)
(define sout73b 0.74)

(define sout71c 0.37)
(define sout72c 0.33)
(define sout73c 0.30)


;; Four List Element Tests
(define test8  (cons 1 (cons 2 (cons 3.2
               (cons 4 empty)))))
(define sout8a 89.24)

(define norm8  2.7)
(define sout81b 1)
(define sout82b 2.73)
(define sout83b 9.09)

(define sout81c 0.03)
(define sout82c 0.08)
(define sout83c 0.27)


(define test9  (cons 2 (cons 3.7 (cons 5.2
               (cons 8 empty)))))
(define sout9a 3210.07)

(define norm9  0.3)
(define sout91b 24.63)
(define sout92b 134.82)
(define sout93b 604.24)

(define sout91c 0)
(define sout92c 0.01)
(define sout93c 0.06)

;; Five List Element Tests
(define test10 (cons 1.1 (cons -1.2 (cons 1.3
               (cons -1.4 (cons 1.5 empty))))))
(define sout10a 11.70)

(define norm10  7.3)
(define sout101b 0.41)
(define sout102b 0.04)
(define sout103b 0.51)

(define sout101c 0.26)
(define sout102c 0.02)
(define sout103c 0.31)


;;
;;   Problem 5A
;;

;; (compute-norm lst) Produces the sum of when
;;   exp is applied to each element of a list (lst)

;; Examples:
(define example0 (cons 1.0 (cons 2.0 (cons -1.0 (cons 3.0 empty)))))
(check-within (compute-norm example0) 30.56 tol)
(define example1 (cons 0 (cons 7.5 empty)))
(check-within (compute-norm example1) 1809.04 tol)

;; compute-norm: (listof Num) -> Num
;;   Requires: lst is non empty
(define (compute-norm lst)
  (cond [(empty? lst)   0]
        [(cons? lst) (+ (exp (first lst))
                        (compute-norm (rest lst)))]
        [else        (exp lst)]))

;; Single Element Tests
(check-within (compute-norm test1) sout1a tol)
(check-within (compute-norm test2) sout2a tol)

;; Single List Element Tests
(check-within (compute-norm test3) sout3a tol)

;; Two List Element Tests
(check-within (compute-norm test4) sout4a tol)
(check-within (compute-norm test5) sout5a tol)

;; Three List Element Tests
(check-within (compute-norm test6) sout6a tol)
(check-within (compute-norm test7) sout7a tol)

;; Four List Element Tests
(check-within (compute-norm test8) sout8a tol)
(check-within (compute-norm test9) sout9a tol)

;; Five List Element Tests
(check-within (compute-norm test10) sout10a tol)


;;
;;   Problem 5B
;;


;; (apply-norm norm lst) Produces a new list
;;  from a given list of numbers (lst) in which each
;;  element is taken to the expontent of e and
;;  divided by a number (norm)

;; Examples:
(check-within (first       (apply-norm 30.56 example0)) 0.09 tol)
(check-within (first (rest (apply-norm 30.56 example0))) 0.24 tol)
(check-within (first       (apply-norm 2 example1)) 0.5 tol)
(check-within (first (rest (apply-norm 2 example1))) 904.02 tol)

;; apply-norm: Num (listof Num) -> Num
;;   Requires: lst is non empty
;;             norm != 0
(define (apply-norm norm lst)
  (cond [(empty? lst)   empty]
        [(cons? lst) (cons (/ (exp (first lst)) norm)
                        (apply-norm norm (rest lst)))]
        [else        (/ (exp lst) norm)]))


;; Single Element Tests
(check-within (apply-norm norm1 test1) sout11b tol)
(check-within (apply-norm norm2 test2) sout21b tol)

;; Single List Element Tests
(check-within (first (apply-norm norm3 test3)) sout31b tol)

;; Two List Element Tests
(check-within (first (apply-norm norm4 test4)) sout41b tol)
(check-within (first (rest (apply-norm norm4 test4))) sout42b tol)
(check-within (first (apply-norm norm5 test5)) sout51b tol)
(check-within (first (rest (apply-norm norm5 test5))) sout52b tol)

;; Three List Element Tests
(check-within (first (apply-norm norm6 test6)) sout61b tol)
(check-within (first (rest (apply-norm norm6 test6))) sout62b tol)
(check-within (first (rest
                     (rest (apply-norm norm6 test6)))) sout63b tol)
(check-within (first (apply-norm norm7 test7)) sout71b tol)
(check-within (first (rest (apply-norm norm7 test7))) sout72b tol)
(check-within (first (rest
                     (rest (apply-norm norm7 test7)))) sout73b tol)

;; Four List Element Tests
(check-within (first (apply-norm norm8 test8)) sout81b tol)
(check-within (first (rest (apply-norm norm8 test8))) sout82b tol)
(check-within (first (rest
                     (rest (apply-norm norm8 test8)))) sout83b tol)
(check-within (first (apply-norm norm9 test9)) sout91b tol)
(check-within (first (rest (apply-norm norm9 test9))) sout92b tol)
(check-within (first (rest
                     (rest (apply-norm norm9 test9)))) sout93b tol)

;; Five List Element Tests
(check-within (first (apply-norm norm10 test10)) sout101b tol)
(check-within (first (rest (apply-norm norm10 test10))) sout102b tol)
(check-within (first (rest
                     (rest (apply-norm norm10 test10)))) sout103b tol)


;;
;;   Problem 5C
;;


;; (softmax lst) Produces a new list in which each element
;;  is taken to the exponent e and then divided by the sum of every
;;  element in the list taken to the e as well

;; Examples:
(check-within (first (softmax       example0))   0.09 tol)
(check-within (first (rest (softmax example0)))  0.24 tol)
(check-within (first (rest
                     (rest (softmax example0)))) 0.01 tol)
(check-within (first (softmax       example1))   0 tol)
(check-within (first (rest (softmax example1)))  1 tol)

;; softmax: (listof Num) -> (listof Num)
;;   Requires: lst is non empty
(define (softmax lst)
  (cond [(cons? lst) (softmaxstor (compute-norm lst) lst)]
        [else                                1]))

;; ** Helper function **

;; (softmaxstor norm nlst) Produces a new list in which each element
;;  is taken to the exponent e and then divided by the sum of every
;;  element in the list taken to the e as well found from (norm)

;; Examples: 
(check-within (first (softmaxstor
                           (compute-norm example1) example1))   0 tol)
(check-within (first (rest (softmaxstor
                           (compute-norm example1) example1)))  1 tol)
;; softmaxstor: norm (listof Num) -> (listof Num)
;;   Requires: lst is non empty
;;             norm != 0
(define (softmaxstor norm lst)
  (cond [(empty? lst)                      empty]
        [else  (cons (/ (exp (first lst)) norm)
                     (softmaxstor norm (rest lst)))]))


;; Single Element Tests
(check-within (softmax test1) sout11c tol)
(check-within (softmax test2) sout21c tol)

;; Single List Element Tests
(check-within (first (softmax test3)) sout31c tol)

;; Two List Element Tests
(check-within (first (softmax test4)) sout41c tol)
(check-within (first (rest (softmax test4))) sout42c tol)
(check-within (first (softmax test5)) sout51c tol)
(check-within (first (rest (softmax test5))) sout52c tol)

;; Three List Element Tests
(check-within (first (softmax test6)) sout61c tol)
(check-within (first (rest (softmax test6))) sout62c tol)
(check-within (first (rest
                     (rest (softmax test6)))) sout63c tol)
(check-within (first (softmax test7)) sout71c tol)
(check-within (first (rest (softmax test7))) sout72c tol)
(check-within (first (rest
                     (rest (softmax test7)))) sout73c tol)

;; Four List Element Tests
(check-within (first (softmax test8)) sout81c tol)
(check-within (first (rest (softmax test8))) sout82c tol)
(check-within (first (rest
                     (rest (softmax test8)))) sout83c tol)
(check-within (first (softmax test9)) sout91c tol)
(check-within (first (rest (softmax test9))) sout92c tol)
(check-within (first (rest
                     (rest (softmax test9)))) sout93c tol)

;; Five List Element Tests
(check-within (first (softmax test10)) sout101c tol)
(check-within (first (rest (softmax test10))) sout102c tol)
(check-within (first (rest
                     (rest (softmax test10)))) sout103c tol)

                

               