;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 04, Problem 2 (A-C)
;; ******************************************

;; * Constants *
(define tol 0.01)

;; ** A-B Test Cases **

;; Boundry Tests
(define test1 empty)
(define Asoul1 0)
(define Bsoul1 0)

(define test2 (list 0 empty))
(define Asoul2 0)
(define Bsoul2 (list 0 0))

(define test3 (list empty 12))
(define Asoul3 0)
(define Bsoul3 (list 0 0))

;; Tests with Zero

(define test4 (list 0 0))
(define Asoul4 0)
(define Bsoul4 (list 0 0))

(define test5 (list 0))
(define Asoul5 0)
(define Bsoul5 (list 0))

(define test6 (list 17 0))
(define Asoul6 17)
(define Bsoul6 (list 1 0))

;; Negitive Tests

(define test7 (list -1 -2))
(define Asoul7 2.24)
(define Bsoul7 (list -0.45 -0.89))

(define test8 (list -25 2))
(define Asoul8 25.08)
(define Bsoul8 (list -0.99 0.07))

(define test9 (list 1 -7))
(define Asoul9 7.07)
(define Bsoul9 (list 0.14 -0.99))

(define test10 (list -0.2 -7))
(define Asoul10 7.01)
(define Bsoul10 (list -0.03 -0.99))

;; Decimal Tests
  
(define test11 (list 3.5 2.7))
(define Asoul11 4.42)
(define Bsoul11 (list 0.79 0.61))

(define test12 (list 0 3.2))
(define Asoul12 3.2)
(define Bsoul12 (list 0 1))

(define test13 (list 1.5 0))
(define Asoul13 1.5)
(define Bsoul13 (list 1 0))

(define test14 (list 5.7 1.2))
(define Asoul14 5.82)
(define Bsoul14 (list 0.98 0.21))

;; General Tests
  
(define test15 (list 22 31))
(define Asoul15 38.01)
(define Bsoul15 (list 0.58 0.82))

(define test16 (list 2 2))
(define Asoul16 2.82)
(define Bsoul16 (list 0.71 0.71))

(define test17 (list 7 7))
(define Asoul17 9.89)
(define Bsoul17 (list 0.71 0.71))

(define test18 (list 12 3))
(define Asoul18 12.37)
(define Bsoul18 (list 0.97 0.24))

(define test19 (list 2.4 4))
(define Asoul19 4.66)
(define Bsoul19 (list 0.52 0.86))

(define test20 (list 1.5 1.5))
(define Asoul20 2.12)
(define Bsoul20 (list 0.71 0.71))

;; ** C Test Cases **

;; Boundry Tests
(define C1test1 empty)
(define C2test1 empty)
(define Csoul1 0)

(define C1test2 empty)
(define C2test2 (list 23 1))
(define Csoul2 0)

(define C1test3 (list 1.5 1.5))
(define C2test3 empty)
(define Csoul3 0)

;; Tests with Zero

(define C1test4 (list 0))
(define C2test4 (list 0 0))
(define Csoul4 0)

(define C1test5 (list 0 ))
(define C2test5 (list 0 0))
(define Csoul5 0)

(define C1test6 (list 0 0))
(define C2test6 (list 0 0))
(define Csoul6 0)

;; Multiple Element Tests (3D Vectors?)

(define C1test7  (list 1 2 2))
(define C2test7 (list 2 10 11))
(define Csoul7 0.98)

(define C1test8 (list 2 6 9))
(define C2test8 (list 6 6 7))
(define Csoul8 0.92)

(define C1test9 (list 1 12 12))
(define C2test9 (list 8 9 12))
(define Csoul9 0.90)

(define C1test10 (list 3 16 24))
(define C2test10 (list 11 12 24))
(define Csoul10 0.95)

;; Decimal Tests
  
(define C1test11 (list 0.2 0.4))
(define C2test11 (list 0.3 0.6))
(define Csoul11 1)

(define C1test12 (list 0.77 0.2))
(define C2test12 (list 0.81 0.234))
(define Csoul12 1)

(define C1test13 (list 0.5 2.5))
(define C2test13 (list 5 25))
(define Csoul13 1)

(define C1test14 (list 3.7 21))
(define C2test14 (list 0.1 0.9))
(define Csoul14 0.99)

;; General Tests
  
(define C1test15 (list 16 63))
(define C2test15 (list 33 56))
(define Csoul15 0.96)

(define C1test16 (list 13 84))
(define C2test16 (list 36 77))
(define Csoul16 0.96)

(define C1test17 (list 0 17))
(define C2test17 (list 0 12))
(define Csoul17 1)

(define C1test18 (list 1 0))
(define C2test18 (list 20 99))
(define Csoul18 0.2)

(define C1test19 (list 96 247))
(define C2test19 (list 231 0))
(define Csoul19 0.36)

(define C1test20 (list 21 220))
(define C2test20 (list 140 171))
(define Csoul20 0.83)


;;
;;   Problem 2A
;;


;; (euclidean-norm lon) Produces the sum of
;;   the square of two vectors contained in a
;;   list of numbers (lon) and then taken to
;;   the square root
;; Examples:
(check-within (euclidean-norm (list 3 4)) 5 tol)
(check-within (euclidean-norm (list 5 12)) 13 tol)

;; euclidean-norm: (listof Num) -> Num
;;   Requires:  lon to not be empty
(define (euclidean-norm lon)
  (cond [(empty? lon)                       0]
        [else   (sqrt (squa-sum-of-list lon))]))


;; ** Helper Functions **

;; (squa-sum-of-list lon) Given a list of numbers
;;  (lon), will find the square sum of all the elements
;; Examples:
(check-expect (squa-sum-of-list (list 2 3)) 13)
(check-expect (squa-sum-of-list (list 1 0)) 1)

;; squa-sum-of-list: (listof Num) -> Num
(define (squa-sum-of-list lon)
  (cond [(empty? lon)                            0]
        [(empty? (first lon))                    0]
        [else    (+ (expt (first lon) 2)
                    (squa-sum-of-list (rest lon)))]))

;; Boundry Tests
(check-within (euclidean-norm test1) Asoul1 tol)
(check-within (euclidean-norm test2) Asoul2 tol)
(check-within (euclidean-norm test3) Asoul3 tol)

;; Tests with Zero
(check-within (euclidean-norm test4) Asoul4 tol)
(check-within (euclidean-norm test5) Asoul5 tol)
(check-within (euclidean-norm test6) Asoul6 tol)

;; Negitive Tests
(check-within (euclidean-norm test7) Asoul7 tol)
(check-within (euclidean-norm test8) Asoul8 tol)
(check-within (euclidean-norm test9) Asoul9 tol)
(check-within (euclidean-norm test10) Asoul10 tol)

;; Decimal Tests
(check-within (euclidean-norm test11) Asoul11 tol)
(check-within (euclidean-norm test12) Asoul12 tol)
(check-within (euclidean-norm test13) Asoul13 tol)
(check-within (euclidean-norm test14) Asoul14 tol)

;; General Tests
(check-within (euclidean-norm test15) Asoul15 tol)
(check-within (euclidean-norm test16) Asoul16 tol)
(check-within (euclidean-norm test17) Asoul17 tol)
(check-within (euclidean-norm test18) Asoul18 tol)
(check-within (euclidean-norm test19) Asoul19 tol)
(check-within (euclidean-norm test20) Asoul20 tol)


;;
;;   Problem 2B
;;



;; (unit-vector lon) Divides each element of a 
;;  list by their euclidean-norm and returns the
;;  new list
;; Examples:
(check-within (unit-vector (list 3 4)) (list 0.6 0.8) tol)
(check-within (unit-vector (list 5 12)) (list 0.38 0.92) tol)

;; unit-vector: (listof Num) -> (listof Num)
;;   Requires:  lon to not be empty
;;              and lon is not only 0
(define (unit-vector lon)
   (cond [(empty? lon) 0]
         [else (unit-vector-given-norm lon
                     (euclidean-norm lon))]))

;; ** Helper Functions **

;; (unit-vector-given-norm lon norm) Divides each element of a 
;;  list by their euclidean-norm (given norm) and returns the
;;  new list
;; Examples:
(check-within (unit-vector-given-norm (list 3 4) 5)
                                      (list 0.6 0.8) tol)
(check-within (unit-vector-given-norm (list 5 12) 13)
                                      (list 0.38 0.92) tol)

;; unit-vector-given-norm: (listof Num) Num -> Num
;;   Requires:  lon to not be empty
;;              and norm to not be 0
(define (unit-vector-given-norm lon norm)
   (cond [(empty? lon)                        empty]
         [(= norm 0) (cons 0 (unit-vector-given-norm
                              (rest lon) norm))]
         [else    (cons (/ (first lon) norm) (unit-vector-given-norm
                              (rest lon) norm))]))

;; Boundry Tests
(check-within (unit-vector test1) Bsoul1 tol)
(check-within (unit-vector test2) Bsoul2 tol)
(check-within (unit-vector test3) Bsoul3 tol)

;; Tests with Zero
(check-within (unit-vector test4) Bsoul4 tol)
(check-within (unit-vector test5) Bsoul5 tol)
(check-within (unit-vector test6) Bsoul6 tol)

;; Negitive Tests
(check-within (unit-vector test7) Bsoul7 tol)
(check-within (unit-vector test8) Bsoul8 tol)
(check-within (unit-vector test9) Bsoul9 tol)
(check-within (unit-vector test10) Bsoul10 tol)

;; Decimal Tests
(check-within (unit-vector test11) Bsoul11 tol)
(check-within (unit-vector test12) Bsoul12 tol)
(check-within (unit-vector test13) Bsoul13 tol)
(check-within (unit-vector test14) Bsoul14 tol)

;; General Tests
(check-within (unit-vector test15) Bsoul15 tol)
(check-within (unit-vector test16) Bsoul16 tol)
(check-within (unit-vector test17) Bsoul17 tol)
(check-within (unit-vector test18) Bsoul18 tol)
(check-within (unit-vector test19) Bsoul19 tol)
(check-within (unit-vector test20) Bsoul20 tol)


;;
;;   Problem 2C
;;


;; (cos-between vectora vectorb) Finds the cosine
;;   between angle a and b which is the
;;   dot product of the unit vector representation
;;   of vectora and vectorb
;; Examples:
(check-within (cos-between (list 3 4) (list 0 6)) 0.8   tol)
(check-within (cos-between (list 5 12) (list 3 4)) 0.97 tol)
(check-within (cos-between (list 5 12 5) (list 3 4)) 0 tol)

;; cos-between: (listof Num) (listof Num) -> Num
;;   Requires:  vectora and vectorb are 
;;              equal length and non empty
(define (cos-between vectora vectorb)
   (cond [(empty? vectora) 0]
         [(empty? vectorb) 0]
         [(not (= (length vectora) (length vectorb))) 0]
         [else (sum-of-normalized
                          vectora (euclidean-norm vectora)
                          vectorb (euclidean-norm vectorb))]))

;; ** Helper Functions **

;; (sum-of-normalized: veca norma vecb normb) Produces the
;;  sum of the multiple of every element (veca)
;;  normalized (norma) around its unit vector with the normalized
;;  verision (normb) of each element (vecb) of the same index.
;; Examples:
(check-within (sum-of-normalized (list 3 4) 5
                                 (list 0 6) 6) 0.8  tol)
(check-within (sum-of-normalized (list 5 12) 13
                                 (list 3 4) 5) 0.97 tol) 

;; sum-of-normalized: (listof Num) Num (listof Num) Num -> Num
;;   Requires:  norma and normb not zero
(define (sum-of-normalized lona norma lonb normb)
   (cond [(empty? lona)                                 0]
         [(or (= 0 norma) (= 0 normb))
                    (sum-of-normalized (rest lona) norma
                                       (rest lonb) normb)]
         [else   (+ (* (/ (first lona) norma)
                       (/ (first lonb) normb))
                    (sum-of-normalized (rest lona) norma
                                       (rest lonb) normb))]))
       
;; Boundry Tests
(check-within (cos-between C1test1 C2test1) Csoul1 tol)
(check-within (cos-between C1test2 C2test2) Csoul2 tol)
(check-within (cos-between C1test3 C2test3) Csoul3 tol)

;; Tests with Zero
(check-within (cos-between C1test4 C2test4) Csoul4 tol)
(check-within (cos-between C1test5 C2test5) Csoul5 tol)
(check-within (cos-between C1test6 C2test6) Csoul6 tol)

;; Multiple Element Tests
(check-within (cos-between C1test7 C2test7) Csoul7 tol)
(check-within (cos-between C1test8 C2test8) Csoul8 tol)
(check-within (cos-between C1test9 C2test9) Csoul9 tol)
(check-within (cos-between C1test10 C2test10) Csoul10 tol)

;; Decimal Tests
(check-within (cos-between C1test11 C2test11) Csoul11 tol)
(check-within (cos-between C1test12 C2test12) Csoul12 tol)
(check-within (cos-between C1test13 C2test13) Csoul13 tol)
(check-within (cos-between C1test14 C2test14) Csoul14 tol)

;; General Tests
(check-within (cos-between C1test15 C2test15) Csoul15 tol)
(check-within (cos-between C1test16 C2test16) Csoul16 tol)
(check-within (cos-between C1test17 C2test17) Csoul17 tol)
(check-within (cos-between C1test18 C2test18) Csoul18 tol)
(check-within (cos-between C1test19 C2test19) Csoul19 tol)
(check-within (cos-between C1test20 C2test20) Csoul20 tol)

