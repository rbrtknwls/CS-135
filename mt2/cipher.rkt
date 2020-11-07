;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cipher) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm 02, Problem 5
;; ******************************************


;; ==================================
;; struct and data definitions For Q5
;; =================================


;; A Substitution Cipher (SCipher) is a (listof (list Char Char))
;; Requires: The first Char in each pair (the key) is unique

(define-struct test (q sc a))
;; A Test is a (make-testa Str SCipher Str)

;; =================================
;; Testing Suite 5
;; =================================

(define vowel-shift '((#\a #\e) (#\e #\i) (#\i #\o)
                      (#\o #\u) (#\u #\y) (#\y #\a)))

(define a-to-b '((#\a #\b)))

;; ======= "Empty" Tests ===========

(define test1 (make-test empty
                         empty
                         ""))
(define test2 (make-test "Hi is the cypher working?"
                         empty
                         "Hi is the cypher working?"))
(define test3 (make-test empty
                         "Nope... sorry"
                         ""))

;; ======= No Cyph Tests ==========

(define test4 (make-test "I"
                         empty
                         "I"))
(define test5 (make-test "Am robbie its (list nice) to 12313131 -1"
                         empty
                         "Am robbie its (list nice) to 12313131 -1"))

;; ======= General Tests ==========

(define test6 (make-test "1 + 2 = 4"
                         '((#\1 #\2) (#\+ #\*) (#\2 #\2))
                         "2 * 2 = 4"))
(define test7 (make-test "This is a speeling mistake"
                         '((#\h #\n) (#\e #\l))
                         "Tnis is a spllling mistakl"))
(define test8 (make-test "This is a speeling mistake"
                         '((#\h #\l) (#\e #\l) (#\a #\l))
                         "Tlis is l spllling mistlkl"))
(define test9 (make-test "happy"
                         '((#\h #\s) (#\p #\d) (#\y #\d))
                         "saddd"))
(define test10 (make-test "Hi!!! :)"
                         '((#\i #\1) (#\) #\())
                         "H1!!! :("))



;; =================================
;;
;; Question 5
;;
;; =================================



;; (subsitute mess ciph) Consumes a Str (mess) and a SCipher 
;;   (ciph), and produces a new string where each character is
;;   replaced by the cipher
;; Examples:

(check-expect (substitute "I, plain text!9" vowel-shift)
                          "I, pleon tixt!9")

;; subsitute: Str SCipher -> Str
(define (substitute mess ciph)
  (cond [(or (empty? mess) (string=? mess "")) ""]
        [else
              (string-append (replace-val (first (string->list mess))
                                          ciph)
                             (substitute (list->string (rest
                                         (string->list mess)))
                                         ciph))]))


;; ===  Helper Functions  ===


;; (replace-val cha ciph) Consumes a Char (cha) and a SCipher 
;;   (ciph), and if the char is in the ciph it will replace it
;;   with the corrispodning value
;; Examples:

(check-expect (replace-val #\a '((#\b #\c) (#\a #\b))) "b")
(check-expect (replace-val #\c '((#\b #\c) (#\a #\b))) "c")


;; replace-val: Char SCipher -> Str
(define (replace-val cha ciph)
  (cond [(empty? ciph)         (list->string (list cha))]
        [(char=? (first (first ciph))
                 cha)
                    (list->string (list
                                  (second (first ciph))))]
        [else               (replace-val cha (rest ciph))]))

;; Empty Tests
(check-expect (substitute (test-q test1)
                          (test-sc test1)) (test-a test1))
(check-expect (substitute (test-q test2)
                          (test-sc test2)) (test-a test2))
(check-expect (substitute (test-q test3)
                          (test-sc test3)) (test-a test3))

;; No Cypher Tests
(check-expect (substitute (test-q test4)
                          (test-sc test4)) (test-a test4))
(check-expect (substitute (test-q test5)
                          (test-sc test5)) (test-a test5))

;; General Tests
(check-expect (substitute (test-q test6)
                          (test-sc test6)) (test-a test6))
(check-expect (substitute (test-q test7)
                          (test-sc test7)) (test-a test7))
(check-expect (substitute (test-q test8)
                          (test-sc test8)) (test-a test8))
(check-expect (substitute (test-q test9)
                          (test-sc test9)) (test-a test9))
(check-expect (substitute (test-q test10)
                          (test-sc test10)) (test-a test10))
