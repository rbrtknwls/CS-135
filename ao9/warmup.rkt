;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 09, Problem 1 (A-B)
;; ******************************************



;; =================================
;;
;; Question 1A
;;
;; =================================



;; (flip-case los) Given a list of strings (los) will produce
;;  a new list of strings where each value (not including the
;;  first two) will be mutated in some fashion
;; Examples:
(check-expect
       (flip-case '("Mr" "Goose!")) '("Mr" "Goose!"))
(check-expect
       (flip-case '("OnLiNE" "ClAsSEs" "ArE" "sOo" "mUcH" "FuN!"))
           '("OnLiNE" "ClAsSEs" "are" "SOO" "MUCH" "fun!"))
(check-expect
       (flip-case '("Mr")) '("Mr"))

;; flip-case: (listof Str) -> (listof Str)
(define (flip-case los)
  (cond [(< (length los) 3)
                           los]
        [else
         (append (list (first los)
                       (second los))
                 (mutate los))]))


;; =================================
;; Helper Functions
;; =================================


;; (mutate los) Given a list of strings (los) will check
;;  the first 2 values and then mutate the third depending
;;  if the first 2 values are even or odd
;; Examples:
(check-expect
       (mutate '("hi" "I" "Am")) '("am"))
(check-expect
       (mutate '("OnLiNE" "ClAsSEs" "ArE" "sOo" "mUcH" "FuN!"))
           '("are" "SOO" "MUCH" "fun!"))

;; mutate: (listof Str) -> (listof Str)
;; Requires: los to have a length greater then 2
(define (mutate los)
  (local [

        ;; (divs2 str1 str2) given two strings (str1,str2)
        ;;  will produce if the sum of the characters of
        ;;  the two strings are divisible by 2

        ;; divs2: Str Str -> Bool
        (define (divs2 str1 str2)
          (even? (+ (string-length str1)
                 (string-length str2))))]

    
     (cond  [(< (length los) 3) empty]
            [(divs2 (first los) (second los))
             (cons (string-upcase (third los))
                   (mutate (rest los)))]
            [else
             (cons (string-downcase (third los))
                   (mutate (rest los)))])))


;; =================================
;; Testing Suite
;; =================================


;; === Length Less Than 2 ===

(check-expect
   (flip-case empty) empty)
(check-expect
   (flip-case '("AAA")) '("AAA"))
(check-expect
   (flip-case '("aaa" "bbb")) '("aaa" "bbb"))

;; === General Tests ===

(check-expect
   (flip-case '("WOW what a" "niCe" "Day"))
              '("WOW what a" "niCe" "DAY"))
(check-expect
   (flip-case '("WiLL" "NoT" "ChAnGe"))
              '("WiLL" "NoT" "change"))
(check-expect
   (flip-case '("A" "b" "C" "dd"))
              '("A" "b" "C" "DD"))
(check-expect
   (flip-case '("one" "Two" "Three" "FoUr" "Fivvee"))
              '("one" "Two" "THREE" "FOUR" "fivvee"))
(check-expect
   (flip-case '("one" "Two" "oN" "T" "One" "TwO"))
              '("one" "Two" "ON" "t" "one" "TWO"))
(check-expect
   (flip-case '("a" "Ab" "aBc" "aBcd" "aBcde" "abcdEf" "abcDefg"))
              '("a" "Ab" "abc" "abcd" "abcde" "abcdef" "abcdefg"))
(check-expect
   (flip-case '("a" "ab" "aBc" "aBc" "aB" "a" "AAAA" "aaaa" "aa" "o" "long" "test" "case"))
              '("a" "ab" "abc" "abc" "AB" "a" "aaaa" "aaaa" "AA" "O" "long" "test" "CASE"))


;; =================================
;;
;; Question 5B
;;
;; =================================

;; (function-go-round fn-list data-list) Produces a new list in which
;;  each element of data-list is changed by the element at data-list
;;  mod fn-list of fn-list.
;; Examples:
(check-expect
   (function-go-round (list string-length
                            string-upcase
                            (lambda (x) (string-append x "!!")))
                       '("joy" "anger" "disgust" "sadness" "fear"))
    '(3 "ANGER" "disgust!!" 7 "FEAR"))
(check-expect
    (function-go-round (list even?
                             odd?
                             add1
                             (lambda (x) (> 3 x)) even?) '(8 9 2 3))
    (list true true 3 false))

;; function-go-round: (listof (Any -> Any)) (listof Any) -> (listof Any)
;; Requires: fn-list is non empty
(define (function-go-round fn-list data-list)
  (local [

        ;; (get-val indx fnlist) Produces the element at the corrisponding
        ;;  index (index) of a list (fnlist)
        ;;  the two strings are divisible by 2

        ;; get-val: Nat (listof (X -> Y)) -> (X -> Y)
        (define (divs2 indx fnlist)
          (cond [(= indx 0)              (first fnlist)]
                [else (divs2 (sub1 indx) (rest fnlist))]))]
    
    (map (lambda (x y) (x y))
         (build-list (length data-list)
                     (lambda (x) (divs2 (remainder x (length fn-list))
                                        fn-list)))
         data-list)))


;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===

(check-expect
    (function-go-round (list even?) empty)
    empty)
(check-expect
    (function-go-round (list empty?) '(()))
    '(#true))
(check-expect
    (function-go-round (list empty?) '(() ()))
    '(#true #true))

;; === Integer Tests ===

(check-expect
    (function-go-round (list add1) '(1 2 3 4 5 1 2 3 4 5))
    '(2 3 4 5 6 2 3 4 5 6))
(check-expect
    (function-go-round (list add1 sub1) '(1 2 3 4 5 1 2 3 4 5))
    '(2 1 4 3 6 0 3 2 5 4))
(check-expect
    (function-go-round (list even? sub1 odd?) '(20 12 10 -30 -40 7 5 13 -17))
    '(#true 11 #false #true -41 #true #false 12 #true))
(check-expect
    (function-go-round (list (lambda (x) (= x 1))
                             (lambda (x) (= (remainder x 5) 0))
                             (lambda (x) (> x 9)))
                       '(20 12 10 -30 -40 7 5 13 -17))
    '(#false #false #true #false #true #false #false #false #false))

;; === Boolean Tests ===

(check-expect
    (function-go-round (list (lambda (x) (not x))
                             (lambda (x) x ))
                       '(#true #false #true #false))
    '(#false #false #false #false))
(check-expect
    (function-go-round (list (lambda (x) (not x))
                             (lambda (x)  x))
                       '(#true #true #false #false))
    '(#false #true #true #false))

;; === String Tests ===

(check-expect
    (function-go-round (list (lambda (x) (string-append x "!!!"))
                             (lambda (x) (string-length x))
                             (lambda (x) (string? x)))
                       '("A" "B" "C" "D" "E" "F"))
    '("A!!!" 1 #true "D!!!" 1 #true))
(check-expect
    (function-go-round (list (lambda (x) (string-append x "!!!")))
                       '("A" "B" "C" "D" "E" "F"))
    '("A!!!" "B!!!" "C!!!" "D!!!" "E!!!" "F!!!"))
(check-expect
    (function-go-round (list (lambda (x) (string-length x)))
                       '("A" "B" "C" "D" "E" "F"))
    '(1 1 1 1 1 1))
(check-expect
    (function-go-round (list (lambda (x) (length x)))
                       '(()))
    '(0))