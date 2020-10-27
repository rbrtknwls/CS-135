;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wild) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm-1, Problem 4
;; ******************************************

;; ** Test Cases **

;; Empty List Test Cases
(define test1 empty)
(define solt1 empty)

(define test2 (cons empty empty))
(define solt2 empty)

(define test3 (cons empty (cons 'emu empty)))
(define solt3 empty)

(define test4 (cons 'emu  (cons 'emu
              (cons empty (cons 'penguin empty)))))
(define solt4 empty)

(define test5 (cons empty (cons empty
              (cons empty (cons 'emu empty)))))
(define solt5 empty)

;; 1-2 Non Null Test Cases
(define test6 (cons 'emu (cons 'emu
              (cons 'hello (cons 'emu empty)))))
(define solt6 (cons 'hello empty))

(define test7 (cons 'I (cons 'penguin
              (cons 'am (cons 'emu empty)))))
(define solt7 (cons 'I (cons 'am empty)))

(define test8 (cons 'penguin1 (cons 'emu1
              (cons 'penguin (cons 'emu empty)))))
(define solt8 (cons 'penguin1 (cons 'emu1 empty)))

(define test9 (cons 'emu (cons 'emu
              (cons 'emu (cons 'cars empty)))))
(define solt9 (cons 'cars empty))

(define test10 (cons 'emu (cons 'hi empty)))
(define solt10 (cons 'hi empty))

;; Full List / 1 Emu or Penguin Test Cases
(define test11 (cons 'hi! empty))
(define solt11 (cons 'hi! empty))

(define test12 (cons '1a empty))
(define solt12 (cons '1a empty))

(define test13 (cons 'i (cons 'emu
               (cons 'am (cons 'robbie empty)))))
(define solt13 (cons 'i (cons 'am
               (cons 'robbie empty))))

(define test14 (cons 'no (cons 'emus
               (cons 'or (cons 'penguins empty)))))
(define solt14 (cons 'no (cons 'emus
               (cons 'or (cons 'penguins empty)))))

(define test15 (cons 'some (cons 'emus
               (cons 'or (cons 'penguin empty)))))
(define solt15 (cons 'some (cons 'emus
               (cons 'or empty))))

(define test16 (cons 'some (cons 'emu
               (cons 'or (cons 'penguins empty)))))
(define solt16 (cons 'some (cons 'or
               (cons 'penguins empty))))

(define test17 (cons 'no (cons 'emu
               (cons 'or (cons 'penguin empty)))))
(define solt17 (cons 'no (cons 'or empty)))

(define test18 (cons 'huh (cons 'a
               (cons 'normal (cons 'list empty)))))
(define solt18 (cons 'huh (cons 'a
               (cons 'normal (cons 'list empty)))))

(define test19 (cons 'more (cons 'lists
               (cons 'withno (cons 'abnormalities empty)))))
(define solt19 (cons 'more (cons 'lists
               (cons 'withno (cons 'abnormalities empty)))))

(define test20 (cons 'i (cons 'like
               (cons 'emu empty))))
(define solt20 (cons 'i (cons 'like empty)))

;;
;;   Problem 4
;;

;; (waterloo-wildlife los) Produces a currated list of symbols
;;   from a exsitings list of symbols (los), by removing
;;   any 'emu or 'penguin
;; Examples:
(check-expect (waterloo-wildlife (cons 'car (cons 'emu
                                 (cons 'tree empty))))
              (cons 'car (cons 'tree empty)))
(check-expect (waterloo-wildlife (cons 'penguin (cons 'hello
                            (cons 'itstrue (cons 'emu empty)))))
              (cons 'hello (cons 'itstrue empty)))

;; waterloo-wildlife: (listof Symb) -> (listof Symb)
(define (waterloo-wildlife los)
  (cond [(empty? los)                                         empty]
        [(empty? (first los))        (waterloo-wildlife (rest los))]
        [(or (symbol=? 'emu (first los))
             (symbol=? 'penguin (first los)))
                                     (waterloo-wildlife (rest los))]
        [else   (cons (first los) (waterloo-wildlife (rest los)))]))

;; Empty List Test 
(check-expect (waterloo-wildlife test1) solt1)
(check-expect (waterloo-wildlife test2) solt2)
(check-expect (waterloo-wildlife test3) solt3)
(check-expect (waterloo-wildlife test4) solt4)
(check-expect (waterloo-wildlife test5) solt5)

;; 1-2 Non Null Element Tests
(check-expect (waterloo-wildlife test6) solt6)
(check-expect (waterloo-wildlife test7) solt7)
(check-expect (waterloo-wildlife test8) solt8)
(check-expect (waterloo-wildlife test9) solt9)
(check-expect (waterloo-wildlife test10) solt10)

;; Almost Full List Tests
(check-expect (waterloo-wildlife test11) solt11)
(check-expect (waterloo-wildlife test12) solt12)
(check-expect (waterloo-wildlife test13) solt13)
(check-expect (waterloo-wildlife test14) solt14)
(check-expect (waterloo-wildlife test15) solt15)
(check-expect (waterloo-wildlife test16) solt16)
(check-expect (waterloo-wildlife test17) solt17)
(check-expect (waterloo-wildlife test18) solt18)
(check-expect (waterloo-wildlife test19) solt19)
(check-expect (waterloo-wildlife test20) solt20)