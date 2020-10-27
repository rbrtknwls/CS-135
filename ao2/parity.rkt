;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname parity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 02, Problem 5
;; ******************************************


;; (parity bstring) Produces the if the given string
;;    (bstring) contains an even amount of 1s
;; Examples:
(check-expect (parity "1110") 'odd)
(check-expect (parity "Hello123") 'odd)
(check-expect (parity "00110011") 'even)

;; parity: Str -> Bool
(define (parity bstring)
  (cond [(= (remainder (numones (string->list bstring)) 2) 0)
                                 'even]
        [else                    'odd]))

;; Basic Tests
(check-expect (parity "") 'even)
(check-expect (parity "11011011") 'even)
(check-expect (parity "1") 'odd)
(check-expect (parity "2") 'even)
(check-expect (parity "11") 'even)
(check-expect (parity "1") 'odd)
(check-expect (parity "2") 'even)
(check-expect (parity "11") 'even)


;; Letters and Numbers Mixed
(check-expect (parity "1Hihowareyou2") 'odd)
(check-expect (parity "Hi211") 'even)
(check-expect (parity "01100111Hi") 'odd)
(check-expect (parity "") 'even)
(check-expect (parity "hi") 'even)



;;***** Helper Functions *****


;; (numones bstring) Produces the number of 1s that
;;   can be found in the string bstring 
;; Example:
(check-expect (numones (string->list "")) 0)
(check-expect (numones (string->list "Hello")) 0)
(check-expect (numones (string->list "123451")) 2)
(check-expect (numones (string->list "00001")) 1)
(check-expect (numones (string->list "101011")) 4)
(check-expect (numones (string->list "000")) 0)

;; numones: (listof Char) -> Num
(define (numones bstring)
  (cond [(empty? bstring)        0]
        [(char=? (first bstring) #\1)
                                 (+ 1 (numones (rest bstring)))]
        [else                    (numones (rest bstring))]))

                        