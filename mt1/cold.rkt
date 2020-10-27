;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cold) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm-1, Problem 6
;; ******************************************

;; ** Test Cases **

;; Edge Cases
(define test1 empty)
(define solt1 'okay)

(define test2 (cons 7.9 empty))
(define solt2 'sad)

(define test3 (cons 8 empty))
(define solt3 'okay)

(define test4 (cons 20 empty))
(define solt4 'okay)

(define test5 (cons 20.1 empty))
(define solt5 'happy)

;; 'Sad Cases
(define test6 (cons 13  (cons 7.99999 
              (cons 15 empty))))
(define solt6 'sad)

(define test7 (cons 4 (cons 6 
              (cons 8 (cons 10 empty)))))
(define solt7 'sad)

(define test8 (cons -4 (cons 0 
              (cons 3.22 (cons 12 empty)))))
(define solt8 'sad)

(define test9 (cons 3.4 (cons 7.7 (cons 7
              (cons 19.102 (cons -11 empty))))))
(define solt9 'sad)

(define test10 (cons 13 (cons 15 (cons 7
              (cons 9.001 (cons 11.21 empty))))))
(define solt10 'sad)

;; 'Happy and 'Sad Cases
(define test11 (cons 2.1  (cons 50.1 
              (cons 30 empty))))
(define solt11 'sad)

(define test12 (cons 4.4 (cons 44 
              (cons 7.7 (cons 77 empty)))))
(define solt12 'sad)

(define test13 (cons 22 (cons 0 
              (cons 36 (cons 17 empty)))))
(define solt13 'sad)

(define test14 (cons -1 (cons -2 (cons -3
              (cons 21 (cons -4 empty))))))
(define solt14 'sad)

(define test15 (cons 13 (cons 15 (cons 7
              (cons 29 (cons 17 empty))))))
(define solt15 'sad)

;; 'Happy

(define test16 (cons 10  (cons 50.1 
              (cons 30 empty))))
(define solt16 'happy)

(define test17 (cons 17.2 (cons 46 
              (cons 9.3 (cons 100 empty)))))
(define solt17 'happy)

(define test18 (cons 22.2 (cons 10 
              (cons 18 (cons 17 empty)))))
(define solt18 'happy)

(define test19 (cons 19 (cons 18 (cons 17
              (cons 21 (cons 16 empty))))))
(define solt19 'happy)

(define test20 (cons 13 (cons 11 (cons 12
              (cons 29 (cons 13 empty))))))
(define solt20 'happy)

;; 'Okay Cases
(define test21 (cons 10  (cons 18 
              (cons 17 empty))))
(define solt21 'okay)

(define test22 (cons 17.2 (cons 12.1 
              (cons 9.3 (cons 13.1 empty)))))
(define solt22 'okay)

(define test23 (cons 18.2 (cons 10 
              (cons 18 (cons 17 empty)))))
(define solt23 'okay)

(define test24 (cons 19 (cons 9 (cons 17
              (cons 9 (cons 16 empty))))))
(define solt24 'okay)

(define test25 (cons 13 (cons 8.001 (cons 12
              (cons 19.999 (cons 13 empty))))))
(define solt25 'okay)


;;
;;   Problem 6
;;

;; (weather lst) Produces if the weather is 'happy, 'sad
;;   'okay, depending on the list of numbers

;; Examples:
(define example0 (cons 15.0 (cons 22.0
                 (cons -273.15 (cons 20.0 empty)))))
(check-expect (weather example0) 'sad)
(define example1 (cons 8.0 (cons 12.0
                 (cons 35.0 (cons 9.0 empty)))))
(check-expect (weather example1) 'happy)

;; weather: (listof Num) -> Symb
(define (weather lst)
   (cond [(lessthen8 lst)      'sad]
         [(greaterthen20 lst) 'happy]
         [else                'okay]))

;; ** Helper function **

;; (lessthen8 lst) Produces if any element in list
;;   is less then 8
;; Examples:

(check-expect (lessthen8 example0)  true)
(check-expect (lessthen8 example1) false)

;; lessthen8: (listof Num) -> Bool
(define (lessthen8 lst)
  (cond [(empty? lst)          false]
        [(< (first lst) 8)      true]
        [else (lessthen8 (rest lst))]))


;; (greaterthen20 lst) Produces if any element in list
;;   is greater then 20
;; Examples:

(check-expect (greaterthen20 example0) true)
(check-expect (greaterthen20 example1) true)

;; greaterthen20: (listof Num) -> Bool
(define (greaterthen20 lst)
  (cond [(empty? lst)              false]
        [(> (first lst) 20)         true]
        [else (greaterthen20 (rest lst))]))


;; Edge Case Tests
(check-expect (weather test1) solt1)
(check-expect (weather test2) solt2)
(check-expect (weather test3) solt3)
(check-expect (weather test4) solt4)
(check-expect (weather test5) solt5)

;; Sad Case Tests
(check-expect (weather test6) solt6)
(check-expect (weather test7) solt7)
(check-expect (weather test8) solt8)
(check-expect (weather test9) solt9)
(check-expect (weather test10) solt10)

;; Sad and Happy Case Tests
(check-expect (weather test11) solt11)
(check-expect (weather test12) solt12)
(check-expect (weather test13) solt13)
(check-expect (weather test14) solt14)
(check-expect (weather test15) solt15)

;; Happy Case Tests
(check-expect (weather test16) solt16)
(check-expect (weather test17) solt17)
(check-expect (weather test18) solt18)
(check-expect (weather test19) solt19)
(check-expect (weather test20) solt20)

;; Okay Case Tests
(check-expect (weather test21) solt21)
(check-expect (weather test22) solt22)
(check-expect (weather test23) solt23)
(check-expect (weather test24) solt24)
(check-expect (weather test25) solt25)


