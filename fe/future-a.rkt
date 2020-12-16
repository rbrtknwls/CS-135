;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname future-a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Final Exam, Problem 2a
;; ******************************************

;; =================================
;; [Testing Defs]
;; =================================

(define packet1 '(a b c d e f g h))
(define packet2 '(a b a b e f e f))
(define packet3 '(a b c d a b c d))

(define packet4 '(hello I am me))
(define packet5 '(Hello I Am Me))
(define packet6 '(hello I am Me))

(define packet7 '(hey!))
(define packet8 '(hey.))

(define packet9 '(abcd))
(define packet10 '(dbca))

;; =================================
;;
;; Problem 2a [hamming-distance]
;;
;; =================================

;; (hamming-distance list1 list2) Produces at how many places
;;  two lists (list1, list2) of equal lengths have different
;;  elements. Note that this is done without recursion.
;; Examples:

(define test1 '(t a g a a g t t t))
(define test1b '(t a g a a g g t t))
(check-expect (hamming-distance test1 test1b) 1)

(define test2 '(b a b a b a a a b a a a))
(define test2b '(b a b a b b a a b a b b))
(check-expect (hamming-distance test2 test2b) 3)

; hamming-distance: (listof Sym) (listof Sym) -> Nat
;; Requires both lists are of equal size
(define (hamming-distance list1 list2)
  (foldl (lambda (l1 l2 rorr) (cond [(symbol=? l1 l2)       rorr]
                                    [else            (add1 rorr)]))
         0 list1 list2))
                                            


;; =================================
;; Testing Suite
;; =================================

;; === Empty Tests ===
(check-expect (hamming-distance empty empty) 0)

;; === No Change Tests ===
(check-expect (hamming-distance packet1 packet1) 0)
(check-expect (hamming-distance packet5 packet5) 0)
(check-expect (hamming-distance packet8 packet8) 0)
(check-expect (hamming-distance packet9 packet9) 0)

;; === General Tests ===
(check-expect (hamming-distance packet1 packet2) 4)
(check-expect (hamming-distance packet2 packet3) 6)
(check-expect (hamming-distance packet1 packet3) 4)

(check-expect (hamming-distance packet4 packet5) 3)
(check-expect (hamming-distance packet5 packet6) 2)
(check-expect (hamming-distance packet4 packet6) 1)

(check-expect (hamming-distance packet7 packet8) 1)

(check-expect (hamming-distance packet10 packet9) 1)



