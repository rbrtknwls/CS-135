;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname factors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 03, Problem 6(A-C)
;; ******************************************

;; ** Constants **
(define basecase 1)

;; ** Test Cases **

;; 6A Test Cases (General Tests)
(define 6Atest1 16)
(define 6Asolution1
        (cons 1 (cons 2 (cons 4
        (cons 8 empty)))))

(define 6Atest2 14)
(define 6Asolution2 
        (cons 1 (cons 2 (cons 7
         empty))))

(define 6Atest3 63)
(define 6Asolution3
        (cons 1 (cons 3 (cons 7
        (cons 9 (cons 21 empty))))))

(define 6Atest4 50)
(define 6Asolution4
        (cons 1 (cons 2 (cons 5
        (cons 10 (cons 25 empty))))))

(define 6Atest5 30)
(define 6Asolution5
        (cons 1 (cons 2 (cons 3
        (cons 5 (cons 6 (cons 10
        (cons 15 empty))))))))

;; 6A Test Case (1 or 0)
(define 6Atest6 0)
(define 6Asolution6
        empty)

(define 6Atest7 1)
(define 6Asolution7
        empty)

;; 6A Test Case (Prime Numbers)
(define 6Atest8 2)
(define 6Asolution8
        (cons 1 empty))

(define 6Atest9 7)
(define 6Asolution9
        (cons 1 empty))

(define 6Atest10 13)
(define 6Asolution10
        (cons 1 empty))


;; 6B Test Cases (Composite Tests)
(define 6Btest1 30)
(define 6Bsolution1 false)

(define 6Btest2 20)
(define 6Bsolution2 false)

(define 6Btest3 4)
(define 6Bsolution3 false)

(define 6Btest4 33)
(define 6Bsolution4 false)

(define 6Btest5 14)
(define 6Bsolution5 false)

;; 6B Test Case (0 or 1 Tests)
(define 6Btest6 0)
(define 6Bsolution6 false)

(define 6Btest7 1)
(define 6Bsolution7 false)

;; 6B Test Case (Prime Tests)
(define 6Btest8 23)
(define 6Bsolution8 true)

(define 6Btest9 97)
(define 6Bsolution9 true)

(define 6Btest10 3)
(define 6Bsolution10 true)


;; 6C Test Cases (Composite Tests)
(define 6Ctest1 30)
(define 6Csolution1 true)

(define 6Ctest2 20)
(define 6Csolution2 true)

(define 6Ctest3 4)
(define 6Csolution3 true)

(define 6Ctest4 33)
(define 6Csolution4 true)

(define 6Ctest5 14)
(define 6Csolution5 true)

;; 6C Test Case (0 or 1 Tests)
(define 6Ctest6 0)
(define 6Csolution6 false)

(define 6Ctest7 1)
(define 6Csolution7 false)

;; 6C Test Case (Prime Tests)
(define 6Ctest8 23)
(define 6Csolution8 false)

(define 6Ctest9 97)
(define 6Csolution9 false)

(define 6Ctest10 3)
(define 6Csolution10 false)


;;
;;   Problem 1A
;;


;; (all-factors num) Produces a the list of natural (nat) factors
;;   of a given number (num), that divide evenly into that number
;; Examples:
(check-expect
 (all-factors 30)
  (cons 1 (cons 2 (cons 3 (cons 5
  (cons 6 (cons 10 (cons 15 empty))))))))
(check-expect
 (all-factors 10)
  (cons 1 (cons 2 (cons 5 empty))))

;; all-factors: Nat -> (listof Nat)
(define (all-factors num)
  (cond [(or (= num 1) (= num 0)) empty]
        [else (factor num basecase)]))

;; Basic Tests
(check-expect (all-factors 6Atest1)  6Asolution1)
(check-expect (all-factors 6Atest2)  6Asolution2)
(check-expect (all-factors 6Atest3)  6Asolution3)
(check-expect (all-factors 6Atest4)  6Asolution4)
(check-expect (all-factors 6Atest5)  6Asolution5)

;; Zero/One Test
(check-expect (all-factors 6Atest6)  6Asolution6)
(check-expect (all-factors 6Atest7)  6Asolution7)

;; Prime Tests
(check-expect (all-factors 6Atest8)  6Asolution8)
(check-expect (all-factors 6Atest9)  6Asolution9)
(check-expect (all-factors 6Atest10) 6Asolution10)

;; ** Helper Functions **

;; (factor num index) Checks if a value less the number
;;   being checked (num) is divisible by that number (index)
;;   if so it is added to a list

;; Examples
(check-expect (factor 10 1)
              (cons 1 (cons 2 (cons 5 empty))))
(check-expect (factor 2 1)
              (cons 1 empty))

;; factor: Nat Nat -> (listof Nat)
(define (factor num index)
  (cond [(<= num index) empty]
        [(= (remainder num index) 0)
            (cons index (factor num (add1 index)))]
        [else           (factor num (add1 index))]))


;;
;;   Problem 1B
;;


;; (is-prime? num) Produces the result of if a natural number
;;   (num) is prime or not.
;; Examples:
(check-expect (is-prime? 30) false)
(check-expect (is-prime? 7) true)

;; is-prime?: Nat -> Bool
(define (is-prime? num)
  (= (length (all-factors num)) 1))

;; Basic Tests
(check-expect (is-prime? 6Btest1)  6Bsolution1)
(check-expect (is-prime? 6Btest2)  6Bsolution2)
(check-expect (is-prime? 6Btest3)  6Bsolution3)
(check-expect (is-prime? 6Btest4)  6Bsolution4)
(check-expect (is-prime? 6Btest5)  6Bsolution5)

;; Zero/One Test
(check-expect (is-prime? 6Btest6)  6Bsolution6)
(check-expect (is-prime? 6Btest7)  6Bsolution7)

;; Prime Tests
(check-expect (is-prime? 6Btest8)  6Bsolution8)
(check-expect (is-prime? 6Btest9)  6Bsolution9)
(check-expect (is-prime? 6Btest10) 6Bsolution10)

;;
;;   Problem 1C
;;


;; (is-composite? num) Produces the result of if a natural number
;;   (num) is composite or not
;; Examples:
(check-expect (is-composite? 30) true)
(check-expect (is-composite? 7) false)

;; is-composite?: Nat -> Bool
(define (is-composite? num)
  (> (length (all-factors num)) 1))

;; Basic Tests
(check-expect (is-composite? 6Ctest1)  6Csolution1)
(check-expect (is-composite? 6Ctest2)  6Csolution2)
(check-expect (is-composite? 6Ctest3)  6Csolution3)
(check-expect (is-composite? 6Ctest4)  6Csolution4)
(check-expect (is-composite? 6Ctest5)  6Csolution5)

;; Zero/One Test
(check-expect (is-composite? 6Ctest6)  6Csolution6)
(check-expect (is-composite? 6Ctest7)  6Csolution7)

;; Prime Tests
(check-expect (is-composite? 6Ctest8)  6Csolution8)
(check-expect (is-composite? 6Ctest9)  6Csolution9)
(check-expect (is-composite? 6Ctest10) 6Csolution10)



