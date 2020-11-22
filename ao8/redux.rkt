;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname redux) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 08, Problem 2
;; ******************************************



;; =================================
;;
;; Question 2A
;;
;; =================================


;; (parity bstring) Produces the if the given string
;;    (bstring) contains an even amount of 1s
;; Examples:
;(check-expect (parity "1110") 'odd)
;(check-expect (parity "Hello123") 'odd)
(check-expect (parity "00110011") 'even)

;; parity: Str -> Bool
;; Requires: Each charactor is either #\1 or #\0
(define (parity bstring)
  (local [(define clist (string->list bstring))]
    (cond [(even? (foldr (lambda (x rorr)
                            (cond [(char=? x #\1) (+ 1 rorr)]
                                  [else rorr]))
                         0
                         clist)) 'even]
          [else 'odd])))


;; Basic Tests
;(check-expect (parity "") 'even)
;(check-expect (parity "11011011") 'even)
;(check-expect (parity "1") 'odd)
;(check-expect (parity "2") 'even)
;(check-expect (parity "11") 'even)
;(check-expect (parity "1") 'odd)
;(check-expect (parity "2") 'even)
;(check-expect (parity "11") 'even)


;; Letters and Numbers Mixed
;(check-expect (parity "1Hihowareyou2") 'odd)
;(check-expect (parity "Hi211") 'even)
;(check-expect (parity "01100111Hi") 'odd)
;(check-expect (parity "") 'even)
;(check-expect (parity "hi") 'even)



