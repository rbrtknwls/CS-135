;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matches) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 08, Problem 4
;; ******************************************


;; (matches-func? func lop) Given a function (func) and a list
;;  of pairs (lop) will produce if for each pair, the second
;;  element is the result of applying the function to the
;;  first element
;; Examples:
(check-expect (matches-func? sqr '((5 25) (2 4) (10 100))) true)
(check-expect (matches-func? add1 '((1 2) (3 5) (10 15))) false)
(check-expect (matches-func? not '((#false #true)
                                   (#true #false))) true)

;; matches-func?: (Any -> Any) (listof (list Any Any)) -> Bool
(define (matches-func? func lop)
  (foldr (lambda (x rorr)
                 (and (equal? (func (first x)) (second x))
                      rorr))
         true
         lop))


;; Number Tests
(check-expect (matches-func? sqrt '((36 6) (9 3) (225 15))) true)
(check-expect (matches-func? sqrt '((64 8) (81 9) (4 3))) false)
(check-expect (matches-func? sub1 '((4 3) (-4 -5) (3 2))) true)
(check-expect (matches-func? sub1 '((25 24) (-7 -6) (1 1))) false)
(check-expect (matches-func? sub1 '()) true)

;; String Tests
(check-expect (matches-func? string=?
                             '(("hi" "hey") ("i" "I"))) false)
(check-expect (matches-func? string=?
                             '(("hi" "hi") ("i" "i"))) false)
(check-expect (matches-func? string?
                             '(("hey" #false)(hey #false))) false)
(check-expect (matches-func? string?
                             '(("hi" #true) (1 #false)
                               (234 #false) (#true #false))) true)

;; Bool Tests
(check-expect (matches-func? boolean? '(('hi #false))) true)
(check-expect (matches-func? boolean? '((#true #false))) false)
(check-expect (matches-func? not '((#true #false)(#false #true)))
                             true)
(check-expect (matches-func? not '((#false #true) (#false #true)))
                             true)

;; List tests
(check-expect (matches-func? length '(((2 5 3 1 2 1) 6))) true)
(check-expect (matches-func? length '(((2 5 3 1 2 1) 5))) false)



