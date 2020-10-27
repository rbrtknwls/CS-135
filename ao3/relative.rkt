;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 03, Problem 3
;; ******************************************

;; ** Test Cases **

;; General Test Cases
(define test1
        (cons 1 (cons 1 empty)))
(define solution1 (cons 0 empty))

(define test2
        (cons 30 (cons 20 empty)))
(define solution2 (cons -10 empty))

(define test3
        (cons 5 (cons 10 empty)))
(define solution3 (cons 5 empty))

(define test4
        (cons 1 (cons 100 empty)))
(define solution4 (cons 99 empty))

(define test5
        (cons 1 (cons 2 (cons 3 empty))))
(define solution5
        (cons 1 (cons 1 empty)))

(define test6
        (cons 3 (cons 2 (cons 1 empty))))
(define solution6
        (cons -1 (cons -1 empty)))

(define test7
        (cons 10 (cons 15 (cons 13 empty))))
(define solution7
        (cons 5 (cons -2 empty)))

(define test8
        (cons 0 (cons 50 (cons 25 empty))))
(define solution8
        (cons 50 (cons -25 empty)))

(define test9
        (cons 15 (cons 30
        (cons 0 (cons 15 empty)))))
(define solution9
        (cons 15 (cons -30 (cons 15 empty))))

(define test10
        (cons 2 (cons 3 (cons 5
        (cons 8 (cons 13 empty))))))
(define solution10
        (cons 1 (cons 2
        (cons 3 (cons 5 empty)))))

;; Empty Test Cases
(define test11 empty)
(define solution11 empty)

(define test12
        (cons 1 empty))
(define solution12 empty)

(define test13
        (cons empty empty))
(define solution13 empty)

;; Negitive Case Test Cases
(define test14
        (cons 0 (cons -1 (cons 0 empty))))
(define solution14
        (cons -1 (cons 1 empty)))

(define test15
        (cons -20 (cons 30
        (cons 0 (cons -30 empty)))))
(define solution15
        (cons 50 (cons -30 (cons -30 empty))))

(define test16
        (cons -1 (cons -1 (cons 0
        (cons -20 (cons -20 empty))))))
(define solution16
        (cons 0 (cons 1
        (cons -20 (cons 0 empty)))))

(define test17
        (cons 0 (cons 0 (cons -100
        (cons 0 (cons 0 empty))))))
(define solution17
        (cons 0 (cons -100
        (cons 100 (cons 0 empty)))))


;; Natural Number Case Test Cases
(define test18
        (cons 0.1 (cons 0.34 (cons 0
        (cons 2.2 (cons 2.3 empty))))))
(define solution18
        (cons 0.24 (cons -0.34
        (cons 2.2 (cons 0.1 empty)))))

(define test19
        (cons 0.1 (cons 0.22 (cons 0.33
        (cons 0.44 (cons 0.55 empty))))))
(define solution19
        (cons 0.12 (cons 0.11
        (cons 0.11 (cons 0.11 empty)))))

(define test20
        (cons -0.1 (cons 0 (cons 0.1
        (cons 0 (cons -0.1 empty))))))
(define solution20
        (cons 0.1 (cons 0.1
        (cons -0.1 (cons -0.1 empty)))))



;;
;;   Problem 3
;;


;; (differences lon) Produces a new list from a list 
;;   of numbers (lon) which contains the differences between
;;   each number

;; Examples:
(check-expect (differences (cons 4 (cons 7
                           (cons 1 empty))))
              (cons 3 (cons -6 empty)))
(check-expect (differences (cons 1 (cons 2 empty)))
              (cons 1 empty))

;; differences (listof Num) -> (listof Num)
(define (differences lon)
  (cond [(empty? lon) empty]
        [(empty? (rest lon)) empty]
        [(cons? lon) (cons (- (first (rest lon)) (first lon))
                           (differences (rest lon)))]))


;; Basic Tests
(check-expect (differences test1) solution1)
(check-expect (differences test2) solution2)
(check-expect (differences test3) solution3)
(check-expect (differences test4) solution4)
(check-expect (differences test5) solution5)
(check-expect (differences test6) solution6)
(check-expect (differences test7) solution7)
(check-expect (differences test8) solution8)
(check-expect (differences test9) solution9)
(check-expect (differences test10) solution10)

;; Empty List Tests
(check-expect (differences test11) solution11)
(check-expect (differences test12) solution12)
(check-expect (differences test13) solution13)

;; Negitive/Zero Tests
(check-expect (differences test14) solution14)
(check-expect (differences test15) solution15)
(check-expect (differences test16) solution16)
(check-expect (differences test17) solution17)

;; Natural Number Tests
(check-expect (differences test18) solution18)
(check-expect (differences test19) solution19)
(check-expect (differences test20) solution20)




