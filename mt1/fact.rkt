;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fact) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm-1, Problem 2
;; ******************************************

;; ** Test Cases **

;; Empty List Test Cases
(define test1 empty)
(define solt1 empty)

(define test2 (cons empty empty))
(define solt2 (cons empty empty))

(define test3 (cons empty (cons 3 empty)))
(define solt3 (cons empty (cons 6 empty)))

(define test4 (cons 8     (cons 3 
              (cons empty (cons 7 empty)))))
(define solt4 (cons 40320 (cons 6 
              (cons empty (cons 5040 empty)))))

(define test5 (cons empty (cons empty
              (cons empty (cons 10 empty)))))
(define solt5 (cons empty (cons empty
              (cons empty (cons 3628800 empty)))))

;; One Element Test Cases
(define test6 0)
(define solt6 1)

(define test7 1)
(define solt7 1)

(define test8 2)
(define solt8 2)
  
;; One Element Test List Cases
(define test9 (cons 0 empty))
(define solt9 (cons 1 empty))

(define test10 (cons 1 empty))
(define solt10 (cons 1 empty))

(define test11 (cons 30 empty))
(define solt11 (cons 265252859812191058636308480000000 empty))
  
;; Two Element List Test Cases
(define test12 (cons 5 (cons 6 empty)))
(define solt12 (cons 120 (cons 720 empty)))

(define test13 (cons 9 (cons 10 empty)))
(define solt13 (cons 362880 (cons 3628800 empty)))

(define test14 (cons 20 (cons 25 empty)))
(define solt14 (cons 2432902008176640000
               (cons 15511210043330985984000000 empty)))

;; Zero as First Element Test Cases
(define test15 (cons 0 (cons 16 empty)))
(define solt15 (cons 1 (cons 20922789888000 empty)))

(define test16 (cons 0 (cons 12 empty)))
(define solt16 (cons 1 (cons 479001600 empty)))

;; 3-4 Element List Test Cases
(define test17 (cons 13  (cons 14 
              (cons 15 empty))))
(define solt17 (cons 6227020800 (cons 87178291200 
              (cons 1307674368000 empty))))

(define test18 (cons 4 (cons 6 
              (cons 8 (cons 10 empty)))))
(define solt18 (cons 24 (cons 720
              (cons 40320 (cons 3628800 empty)))))

;; 5 Element List Test Cases
(define test19 (cons 3 (cons 5 (cons 7
              (cons 9 (cons 11 empty))))))
(define solt19 (cons 6 (cons 120 (cons 5040
              (cons 362880 (cons 39916800 empty))))))

(define test20 (cons 16 (cons 2 (cons 3
              (cons 19 (cons 0 empty))))))
(define solt20 (cons 20922789888000 (cons 2 (cons 6
              (cons 121645100408832000 (cons 1 empty))))))


;;
;;   Problem 2
;;


;; (factorialize lon) Produces a list of factorial
;;   numbers from a list of numbers (lon)
;; Examples:
(check-expect (factorialize (cons 1 (cons 2
                            (cons 4 (cons 3 empty)))))
              (cons 1 (cons 2 (cons 24 (cons 6 empty)))))
(check-expect (factorialize (cons 5 (cons 4
                            (cons 3 (cons 2 empty)))))
              (cons 120 (cons 24 (cons 6 (cons 2 empty)))))

;; factorialize: (listof Nat) -> (listof Nat)
;;   Requires:   each element of lon >= 0
(define (factorialize lon)
  (cond [(empty? lon)                           empty]
        [(cons? lon)    (cons (nat_factor (first lon))
                           (factorialize (rest lon)))]
        [else                      (nat_factor lon)]))

;; Empty List Test 
(check-expect (factorialize test1) solt1)
(check-expect (factorialize test2) solt2)
(check-expect (factorialize test3) solt3)
(check-expect (factorialize test4) solt4)
(check-expect (factorialize test5) solt5)

;; One Element Test
(check-expect (factorialize test6) solt6)
(check-expect (factorialize test7) solt7)
(check-expect (factorialize test8) solt8)

;; One Element List Test
(check-expect (factorialize test9) solt9)
(check-expect (factorialize test10) solt10)
(check-expect (factorialize test11) solt11)
  
;; Two Element List Test 
(check-expect (factorialize test12) solt12)
(check-expect (factorialize test13) solt13)
(check-expect (factorialize test14) solt14)
  
;; Zero as First Element Test 
(check-expect (factorialize test15) solt15)
(check-expect (factorialize test16) solt16)

;; 3-4 Element List Test 
(check-expect (factorialize test17) solt17)
(check-expect (factorialize test18) solt18)

;; 5 Element List Test
(check-expect (factorialize test19) solt19)
(check-expect (factorialize test20) solt20)


;; ** Helper Functions **

;; (nat_factor natnum) Given a natural number (natnum), will produce
;;   the factorial of that number
(check-expect (nat_factor 2) 2)
(check-expect (nat_factor 0) 1)
(check-expect (nat_factor empty) empty)

;; nat_factor: Nat -> Nat
;;   Requires: natnum >= 0
(define (nat_factor natnum)
  (cond [(empty? natnum) empty]
        [(= natnum 0) 1]
        [else (* natnum
                (nat_factor (- natnum 1)))]))
                           

                                             

