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
(check-expect (tokenize "Heading</h1>Text</p>")
              '("Heading" "</h1>" "Text" "</p>"))
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
            [else (cons (list->string (cons (first loc)
                                            (curr-list (rest loc))))
                        (clist->nlist (rest-list (rest loc))))]))


;; (curr-list loc) Produces a list of all the values in a list form
;;   up to the first end bracket or to the end of the last
;; Examples:
(check-expect (curr-list (string->list "p></p>"))
              (string->list "p>"))
(check-expect (curr-list (string->list "a b c d"))
              (string->list "a b c d"))

;; curr-list: (listof Char) -> (listof Char)
(define (curr-list loc)
     (cond  [(empty? loc)                     empty]
            [(char=? (first loc) #\>)  (cons (first loc)
                                               empty)]
            [(char=? (first loc) #\<)  empty]
            [else (cons (first loc)
                        (curr-list (rest loc)))]))


;; (rest-list loc) Produces a list of all the values in a list form
;;   after to the first end brackets
;; Examples:
(check-expect (rest-list (string->list "p></p>"))
              (string->list "</p>"))
(check-expect (rest-list (string->list "a b c d"))
              empty)

;; rest-list: (listof Char) -> (listof Char)
(define (rest-list loc)
     (cond  [(empty? loc)                  empty]
            [(char=? (first loc) #\>) (rest loc)]
            [(char=? (first loc) #\<) loc]
            [else         (rest-list (rest loc))]))


;; =================================
;; Testing Suite
;; =================================


;; === Empty String ===
(check-expect (tokenize "") empty)
(check-expect (tokenize "empty") '("empty"))
(check-expect (tokenize "<empty>") '("<empty>"))
(check-expect (tokenize "<empty></empty>") '("<empty>" "</empty>"))

;; === General Tests ===

(check-expect (tokenize "<a><b><c>hihi</c></b></a>")
              '("<a>" "<b>" "<c>" "hihi" "</c>" "</b>" "</a>"))
(check-expect (tokenize "<a><b>hi</b><c>hi</c></a>")
              '("<a>" "<b>" "hi" "</b>" "<c>" "hi" "</c>" "</a>"))
(check-expect (tokenize "<a><b><c></c><d></d></b>1234</a>")
              '("<a>" "<b>" "<c>" "</c>" "<d>" "</d>" "</b>"
                      "1234" "</a>"))
(check-expect (tokenize "<1></1>")
              '("<1>" "</1>"))
(check-expect (tokenize "<1></1><2></2><3></3>")
              '("<1>" "</1>" "<2>" "</2>" "<3>" "</3>"))
              '("<1>" "</1>"))
(check-expect (tokenize "<hi>hi</hi>")
              '("<hi>" "hi" "</hi>"))
(check-expect (tokenize "<hi>hi</hi>")
              '("<hi>" "hi" "</hi>"))


;; =================================
;;
;; Question 3B
;;
;; =================================


