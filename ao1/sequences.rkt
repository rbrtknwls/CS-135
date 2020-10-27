;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 01, Problem 3
;; ******************************************


;; (sequence-type a b c d) produces the type of a sequence
;;   that the numbers a, b, c and d form
;; Examples:
(check-expect (sequence-type 1 1 1 1) 'both)
(check-expect (sequence-type 2 4 6 8) 'arithmetic)
(check-expect (sequence-type -2.5 5 -10 20) 'geometric)

;; sequence-type: Num Num Num Num -> Sym
(define (sequence-type a b c d)
  (cond
    [(and (isgeometric a b c d) (isarithmetic a b c d))
                             'both]
    [(isgeometric a b c d)   'geometric]
    [(isarithmetic a b c d)   'arithmetic]
    [else                    'neither]))

;; Tests:
(check-expect (sequence-type 0 0 0 1) 'neither)
(check-expect (sequence-type 0 0 0 0) 'both)
(check-expect (sequence-type -7.5 22.5 -67.5 202.5) 'geometric)
(check-expect (sequence-type 6 0 0 0) 'geometric)
(check-expect (sequence-type 0 1 2 3) 'arithmetic)
(check-expect (sequence-type 0 5 20 25) 'neither)
(check-expect (sequence-type 20 15 10 5) 'arithmetic)

;;***** Helper Functions *****

;; (isgeometric) checks if the sequence a, b, c and d
;;    forms a geometric sequence
;; Example:
(check-expect (isgeometric 0 0 0 0) true)
(check-expect (isgeometric 1 2 3 4) false)
(check-expect (isgeometric 0 0 1 2) false)
(check-expect (isgeometric 16 8 4 2) true)
(check-expect (isgeometric 2 0 0 0) true)

;; isgeometric: Num Num Num Num -> Bool
(define (isgeometric a b c d)
  (cond
    [(and (= b 0) (= c 0) (= d 0)) true]
    [(or (= a 0) (= b 0) (= c 0) (= d 0))  false]
    [(= (/ b a) (/ c b) (/ d c))           true]
    [else                                  false]))


;; (isarithmetic) checks if the sequence a, b, c and d
;;    forms a arithmetic sequence
;; Example:
(check-expect (isarithmetic 0 0 0 0) true)
(check-expect (isarithmetic 1 2 3 4) true)
(check-expect (isarithmetic 0 0 1 2) false)
(check-expect (isarithmetic 16 8 4 2) false)

;; isarithmetic Num Num Num Num -> Bool
(define (isarithmetic a b c d)
  (cond
    [(and (= a 0) (= b 0) (= c 0) (= d 0)) true]
    [(= (- b a) (- c b) (- d c))           true]
    [else                                  false]))



