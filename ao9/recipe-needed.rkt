;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recipe-needed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 09, Problem 5 (A-C)
;; ******************************************



;; =================================
;;
;; Question 1A
;;
;; =================================



;; (m1 a) Given a string (a) will produce if the string is a
;;  palindrome or not, by comparing the string to its reversed
;;  counterpart
;; Examples:
(check-expect
       (m1 "AbcdcbA") true)
(check-expect
       (m1 "AA") true)
(check-expect
       (m1 "AB") false)

;; m1: Str -> Bool
;; Requires: A to be non empty
(define (m1 a)
  (local

    ;; (b) given a string will convert it into a string
    ;; list

    ;; b: (listof Char)
    [(define b (string->list a))]
    (foldr
     (lambda (x y z)
       (and (char=? x y) z))
     true
     b
     (foldl cons empty b))))



;; =================================
;; Testing Suite
;; =================================


;; === Single Element Tests ===

(check-expect
       (m1 "A") true)
(check-expect
       (m1 "B") true)
(check-expect
       (m1 "C") true)

;; === General Tests ===

(check-expect
       (m1 "ABA") true)
(check-expect
       (m1 "CB") false)
(check-expect
       (m1 "Cbc") false)
(check-expect
       (m1 "ABCBA") true)
(check-expect
       (m1 "ABAAA") false)



;; =================================
;;
;; Question 5B
;;
;; =================================



;; (m2 a) Takes in a list of strings (a) and produces a predicate
;;  which will take in a string and produce if that string's
;;  compontents exist within an index of the list of strings (a).
;;  However if the string itself is inside the list its not counted
;; Examples:
(check-expect
       ((m2 '("gi" "hi" "bi")) "ih") true)
(check-expect
       ((m2 '("gi" "hi" "bi")) "hi") false)
(check-expect
       ((m2 '("gi" "hi" "bi")) "gbi") false)

;; m2: (listof Str) -> (Str -> Bool)
(define (m2 a)
  (lambda (s)
    (local [

      ;; (r x) given a string (x) will create a list where
      ;;  each Char in the string is in alphabeltical order

      ;; r: Str -> (listof Str)
      (define (r x) (quicksort (string->list x) char<?))

      ;; (u) Takes a list of strings from the header (a)
      ;; and then produces a list of that string and its
      ;; sorted elements

      ;; u: (listof (list Str (listof Char))
       (define u (map (lambda (x) (list x (r x))) a))
      ;; (t) Takes a list of things known as x, and will
      ;; return the sisze of the list of characters

      ;; t: (listof Any) -> Nat
       (define (t x) (foldr (lambda (y z) (add1 z)) 0 x))]
      
      (foldr
       (lambda (x y)
         (cond
           [(string=? s (first x)) y]
           [(not (= (t (r s)) (t (second x)))) y]
           [else
            (or y (foldr
                   (lambda (x y)
                     (cond
                       [(and x y) y]
                       [else false]))
                   true
                   (map (lambda (a b) (char=? a b))
                        (r s) (second x))))]))
       false
       u))))



;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===

(check-expect
       ((m2 '("gi" "hi" "bi")) "") false)
(check-expect
       ((m2 '("")) "") false)

;; === General Tests ===

(check-expect
       ((m2 '("heoy"))
             "heyo") true)
(check-expect
       ((m2 '("heoy" "heou"))
             "heou") false)
(check-expect
       ((m2 '("heoy" "heou" "be" "saf" "af"))
             "sad") false)
(check-expect
       ((m2 '("bad" "mad" "cad" "lad"))
             "bda") true)
(check-expect
       ((m2 '("aaaa" "aaabaa" "cacasas" "sasfa"))
             "cacassa") true)



;; =================================
;;
;; Question 5C
;;
;; =================================



;; (m3 a b) Given a predicate, will return what it will do
;;  when applied to the difference between the smallest and largest
;;  element in a list of any (b).
;; Examples:
(check-expect
       (m3 (lambda (x) (= x 8)) '(1 9)) true)
(check-expect
       (m3 (lambda (x) (= x 8)) '(1 aahhaha 9 zjzxjxz)) true)
(check-expect
       (m3 (lambda (x) (= x 81)) '(1 aahhaha 9 zjzxjxz)) false)

;; m3: (Num Num -> Any) (listof Any) -> Any
;; Requires: b to be nonempty
(define (m3 a b)
   (local[
          
     ;; (c) Given a list will return
     ;;  all the integer parts of b.

     ;; c: (listof Int)
          
      (define c (filter integer? b))

     ;; (d e f) Given a list (e) and a function (f) will return
     ;;  the value in the list that satisfies
     ;;  the condition, in our case either the smallest
     ;;  or lowest value

     ;; d: (listof Int) (Int Int -> Bool) -> Int
      (define (d e f)
        (foldl (lambda (x y)
                 (cond
                   [(f x y) x]
                   [else y]))
               (first e)
               e))]
        
     (a (- (d c >) (d c <)))))



;; =================================
;; Testing Suite
;; =================================


;; === Bool Tests ===

(check-expect
       (m3 (lambda (x) (= x 81)) '(1 2 3 4 5 6 7 8 82)) true)
(check-expect
       (m3 (lambda (x) (>= x 81)) '(2 213 14 19)) true)
(check-expect
       (m3 (lambda (x) (< x 81)) '(91 2 314 51 2)) false)
(check-expect
       (m3 (lambda (x) (odd? x)) '(-1 -23 4 5 -1 2202)) true)
(check-expect
       (m3 (lambda (x) (even? x)) '(-2029 282 21 4)) false)

;; === Int Tests ===

(check-expect
       (m3 (lambda (x) (sub1 x)) '(1)) -1)
(check-expect
       (m3 (lambda (x) (add1 x)) '(100 90 42 1 -12 -3 -100)) 201)
(check-expect
       (m3 (lambda (x) (sub1 x)) '(9999)) -1)

;; === Str Tests ===

(check-expect
       (m3 (lambda (x) (string-length (number->string x)))
           '(83 4 12 31 41 51)) 2)
(check-expect
       (m3 (lambda (x) (string-append (number->string x) "!!"))
           '(-5 1 2 3 4 5 6 7 8 10)) "15!!")

