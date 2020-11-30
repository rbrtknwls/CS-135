;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htmlagain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 09, Problem 3 (A-B)
;; ******************************************



;; ==================================
;; struct and data definitions For Q3
;; =================================

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))



;; =================================
;;
;; Question 3A
;;
;; =================================


;; (tokenize hstring) Given a string will produce a list
;; of all the elements (opening, closing and strings)
;; Examples:
(check-expect (tokenize "<p><h1>Heading</h1>Text</p>")
              '("<p>" "<h1>" "Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize "a b   c") '("a b   c"))
(check-expect (tokenize "") empty)

;; tokenize: Str -> (listof Str)
(define (tokenize hstring)
  (clist->nlist (string->list hstring)))


;; =================================
;; Helper Functions
;; =================================


;; (clist->nlist loc) Given a list of characters (los) will 
;;   produce a list of all the elements (opening, closing and strings)
;; Examples:
(check-expect (clist->nlist (string->list "<p></p>"))
              '("<p>" "</p>"))

;; clist->nlist: (listof Char) -> (listof Str)
(define (clist->nlist loc)
     (cond  [(empty? loc) empty]
            [else (cons (list->string (curr-list loc))
                        (clist->nlist (rest-list loc)))]))


;; (curr-list loc) Produces a list of all the values in a list form
;;   up to the first end bracket or to the end of the last
;; Examples:
(check-expect (curr-list (string->list "<p></p>"))
              (string->list "<p>"))
(check-expect (curr-list (string->list "a b c d"))
              (string->list "a b c d"))

;; curr-list: (listof Char) -> (listof Char)
(define (curr-list loc)
     (cond  [(empty? loc)                     empty]
            [(char=? (first loc) #\>)  (cons (first loc)
                                               empty)]
            [(char=? (first loc) #\<)  (cons (first loc)
                                               empty)]
            [else (cons (first loc)
                        (curr-list (rest loc)))]))


;; (rest-list loc) Produces a list of all the values in a list form
;;   after to the first end brackets
;; Examples:
(check-expect (rest-list (string->list "<p></p>"))
              (string->list "<p>"))
(check-expect (rest-list (string->list "a b c d"))
              empty)

;; rest-list: (listof Char) -> (listof Char)
(define (rest-list loc)
     (cond  [(empty? loc)                  empty]
            [(char=? (first loc) #\>) (rest loc)]
            [(char=? (first loc) #\<) (rest loc)]
            [else         (rest-list (rest loc))]))


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
;; Question 3B
;;
;; =================================


