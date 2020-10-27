;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vowels) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 03, Problem 2
;; ******************************************

;; ** Constants **
(define acharcode #\a) (define echarcode #\e)
(define icharcode #\i) (define ocharcode #\o)
(define ucharcode #\u)


;; ** Test Cases **

;; General Test Cases
(define test1 (cons "pineapple" empty))
(define solution1 4)

(define test2 (cons "pizza" empty))
(define solution2 2)

(define test3
        (cons "pineaple" (cons "pizza" empty)))
(define solution3 6)

(define test4
        (cons "i" (cons "am" empty)))
(define solution4 2)

(define test5
        (cons "i" (cons "love" (cons "coding" empty))))
(define solution5 5)

(define test6
        (cons "abc" (cons "def" (cons "ghi" empty))))
(define solution6 3)

(define test7
        (cons "lots" (cons "of" (cons "vowels" empty))))
(define solution7 4)

(define test8
        (cons "aaa" (cons "eee" (cons "ooo" empty))))
(define solution8 9)

(define test9
        (cons "Peoples" (cons "Peoples"
        (cons "Peoples" (cons "Peoples" empty)))))
(define solution9 12)

(define test10
        (cons "Name" (cons "Name" (cons "Name"
        (cons "Name" (cons "Name" empty))))))
(define solution10 10)

;; Empty Test Cases
(define test11 empty)
(define solution11 0)

(define test12
        (cons empty empty))
(define solution12 0)

;; Upper Case Test Cases
(define test13 (cons "HEllO I Am RObbIE" empty))
(define solution13 0)

(define test14 (cons "Hello i am Robbie" empty))
(define solution14 7)

(define test15
        (cons "Hello" (cons "I"
        (cons "am" (cons "robbie" empty)))))
(define solution15 6)

(define test16
        (cons "ABC." (cons "abc"
        (cons "Abc" empty))))
(define solution16 1)

;; Special Character Test Cases

(define test17
        (cons "very'nice'to" (cons "meetcha"
        (cons "runonsentance..." empty))))
(define solution17 12)

(define test18
        (cons "hello-" (cons "hi"
        (cons "hey" empty))))
(define solution18 4)

(define test19
        (cons "1234" (cons "5678"
        (cons "i123" (cons "456e" empty)))))
(define solution19 2)

(define test20
        (cons "1-" (cons "2'" (cons "3|"
        (cons "4#" (cons "531131" empty))))))
(define solution20 0)


;;
;;   Problem 2
;;


;; (total-vowels los) Produces the total number of
;;   lower case vowels (E.G. a,e,i,o,u) from a list
;;   of strings (los)
;; Examples:
(check-expect (total-vowels (cons "test" (cons "look" empty))) 3)
(check-expect (total-vowels (cons "hi" (cons "how are you" empty))) 6)

;; total-vowels: (listof Str) -> Int
(define (total-vowels los)
  (cond [(empty? los) 0]
        [(empty? (first los)) 0]
        [(cons? los) (+ (vowels-in-word (string->list (first los)))
                        (total-vowels   (rest los)))]))

;; Basic Tests
(check-expect (total-vowels test1) solution1)
(check-expect (total-vowels test2) solution2)
(check-expect (total-vowels test3) solution3)
(check-expect (total-vowels test4) solution4)
(check-expect (total-vowels test5) solution5)
(check-expect (total-vowels test6) solution6)
(check-expect (total-vowels test7) solution7)
(check-expect (total-vowels test8) solution8)
(check-expect (total-vowels test9) solution9)
(check-expect (total-vowels test10) solution10)

;; Empty List Tests
(check-expect (total-vowels test11) solution11)
(check-expect (total-vowels test12) solution12)

;; Upper Case Tests
(check-expect (total-vowels test13) solution13)
(check-expect (total-vowels test14) solution14)
(check-expect (total-vowels test15) solution15)
(check-expect (total-vowels test16) solution16)

;; Unqiue Character Tests
(check-expect (total-vowels test17) solution17)
(check-expect (total-vowels test18) solution18)
(check-expect (total-vowels test19) solution19)
(check-expect (total-vowels test20) solution20)



;; ** Helper Functions **

;; (vowels-in-word word) Produces the number of vowels
;;   in a given word (word), which is expressed as a list of
;;   characters
;; Examples
(check-expect (vowels-in-word (string->list "This is word")) 3)
(check-expect (vowels-in-word (string->list "cysts")) 0)
(check-expect (vowels-in-word (string->list "aeiou")) 5)

;; vowels-in-word: (listof Char) -> Int
(define (vowels-in-word word)
  (cond [(empty? word) 0]
        [(cons?  word)
                (cond [(char=? (first word) acharcode)
                       (add1 (vowels-in-word (rest word)))]
                      [(char=? (first word) echarcode)
                       (add1 (vowels-in-word (rest word)))]
                      [(char=? (first word) icharcode)
                       (add1 (vowels-in-word (rest word)))]
                      [(char=? (first word) ocharcode)
                       (add1 (vowels-in-word (rest word)))]
                      [(char=? (first word) ucharcode)
                       (add1 (vowels-in-word (rest word)))]
                      [else  (vowels-in-word (rest word))])]))