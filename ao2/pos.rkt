;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 02, Problem 4
;; ******************************************

;; Constants
(define test1
        (cons 2.36 (cons 37.21 (cons 8.59 empty))))
(define test2
        (cons 0 (cons 0 (cons 0 empty))))
(define test3
        empty)
(define test4
        (cons 25 (cons 5 empty)))
(define test5
        (cons 0.2 empty))
(define test6
        (cons 0.1 (cons 0.2 (cons 0.3
        (cons 0.4 (cons 0.5 (cons 0.6 empty)))))))

;;
;;   Problem 4A
;;


;; (change-due costs paid) Produces the change
;;    that will be needed by subtracting the amount
;;    paid (paid) by the sum of the list of costs (costs)
;; Examples:
(check-expect (change-due test1 50) 1.84)
(check-expect (change-due test2 50) 50)
(check-expect (change-due test2 0) 0)

;; change-due: (listof Num) Num -> Num
;; Require:
;;    paid >= 0
;;    costs >= 0
(define (change-due costs paid)
  (cond [(empty? costs)   paid]
        [(cons? costs)    (- (change-due (rest costs) paid)
                          (first costs))]))

;; Tests
(check-expect (change-due test3 50) 50)
(check-expect (change-due test3 0) 0)
(check-expect (change-due test4 35) 5)
(check-expect (change-due test4 30) 0)
(check-expect (change-due test5 1) 0.8)
(check-expect (change-due test5 0.2) 0)
(check-expect (change-due test6 3) 0.9)
(check-expect (change-due test6 2.1) 0)
(check-expect (change-due test6 2.5) 0.4)


;;
;;   Problem 4B
;;


;; (paid-enough? costs paid) Produces the result
;;    of if the amount paid (paid) is greater 
;;    then the sum of the list of costs (costs)
;; Examples:
(check-expect (paid-enough? test1 40) false)
(check-expect (paid-enough? test2 50) true)
(check-expect (paid-enough? test2 0) true)

;; paid-enough?: (listof Num) Num -> Bool
;; Require:
;;    paid >= 0
;;    costs >= 0
(define (paid-enough? costs paid)
  (>= (change-due costs paid) 0))

;; Tests
(check-expect (paid-enough? test3 30) true)
(check-expect (paid-enough? test3 0) true)
(check-expect (paid-enough? test4 35) true)
(check-expect (paid-enough? test4 20) false)
(check-expect (paid-enough? test5 1) true)
(check-expect (paid-enough? test5 0.2) true)
(check-expect (paid-enough? test6 2) false)
(check-expect (paid-enough? test6 1) false)


;;
;;   Problem 4C
;;


;; (free-item costs paid) Produces the first
;;    cost that you need to nullify in order for
;;    the amount paid (paid) to be greater then
;;    the list of costs (costs)
;; Examples:
(check-expect (free-item test1 40) 37.21)
(check-expect (free-item test2 0) 0)

;; free-item: (listof Num) Num -> Num
;; Require:
;;    paid >= 0
;;    costs >= 0
(define (free-item costs paid)
  (cond [(or (empty? costs) (not (cons? costs)))
                                              0]
        [(>= (first costs) (* -1 (change-due costs paid)))
                                   (first costs)]
        [else                      (free-item (rest costs) paid)]))

;; Tests

(check-expect (free-item test3 0) 0)
(check-expect (free-item test4 25) 25)
(check-expect (free-item test4 11) 25)
(check-expect (free-item test5 0.1) 0.2)
(check-expect (free-item test5 0.05) 0.2)
(check-expect (free-item test6 2.0) 0.1)
(check-expect (free-item test6 1.9) 0.2)



