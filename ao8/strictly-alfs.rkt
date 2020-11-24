;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname strictly-alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 08, Problem 3(A-D)
;; ******************************************



;; =================================
;;
;; Question 3A
;;
;; =================================



;; (occurrences index lon) Produces the number of occurrences 
;;    a number (index) has within a given list of numbers (lon)
;;    amounts of 1s.
;; Examples:
(check-expect (occurrences 2  '(1 2 1 2 2 3 1)) 3)
(check-expect (occurrences 1  '(1 1 1)) 3)
(check-expect (occurrences 12 '( 1 2 3 4 5)) 0)

;; occurrences: Num (listof Num) -> Num
(define (occurrences index lon)
  (foldr (lambda (x rorr)
                 (cond [(= x index) (add1 rorr)]
                       [else              rorr]))
         0
         lon))


;; Basic Tests
(check-expect (occurrences 3  '(33 3 33 3 33))       2)
(check-expect (occurrences 5  '(1 2 3 4 5 4 3 2 1))  1)
(check-expect (occurrences 1  '(1 2 3 4 5 4 3 2 1))  2)
(check-expect (occurrences 10 '(1 10 100 1 10 10))   3)
(check-expect (occurrences 0  '(0 0 0 0 0 0 0 0 0))  9)


;; Negatives
(check-expect (occurrences -7  '(7 -7))              1)
(check-expect (occurrences -3  '(-3 4 -3 5 -3 -3))   4)
(check-expect (occurrences -10  '(-10 -20 -10 -10))  3)


;; Real Numbers
(check-expect (occurrences 0.1  '(0.1 1 10 0.1))     2)
(check-expect (occurrences 0.3  '(0.3 0.333 0.33))   1)
(check-expect (occurrences 0.9  '(0.9 0.9 0.9 0.9))  4)

;; Not in List
(check-expect (occurrences 0.4  '(0.3 0.333 0.33))   0)
(check-expect (occurrences 10  empty)   0)



;; =================================
;;
;; Question 3B
;;
;; =================================



;; (zip list1 list2) Given two lists (list1 list 2) of
;; equal size will produce a new list where each element is
;; the (ith element of list1, ith element of list 2).
;; Examples:
(check-expect (zip '(1 2 3) '(a b c)) '((1 a)(2 b)(3 c)))
(check-expect (zip '(h h) '(i i))     '((h i)(h i)))
(check-expect (zip '(noth) '(()))     '((noth ())))

;; zip: (listof X) (listof X) -> (listof (list X X))
;; Requires: list1 and list2 to be the same size
(define (zip list1 list2)
  (map (lambda (x r) (list x r))
       list1 list2))


;; Basic Tests
(check-expect (zip '(a a) '(b b))     '((a b)(a b)))
(check-expect (zip '(1 -1) '(-1 1))   '((1 -1)(-1 1)))
(check-expect (zip '(heyo) '(!))      '((heyo !)))
(check-expect (zip '(1 2 3) '(4 5 6)) '((1 4)(2 5)(3 6)))
(check-expect (zip '(8) '((2 4 6 8))) '((8 (2 4 6 8))))


;; Empty Tests
(check-expect (zip empty empty) '())
(check-expect (zip '(()) '(a))  '((() a)))
(check-expect (zip '(a) '(()))  '((a ())))


;; Weird Types
(check-expect (zip '((a b) a) '(b (b c))) '(((a b) b)(a (b c))))
(check-expect (zip '(hey !) '(! !))    '((hey !)(! !)))
(check-expect (zip '(-1 1 2) '(1 1 2)) '((-1 1)(1 1)(2 2)))
(check-expect (zip '(0.1 -0.2) '(1 2)) '((0.1 1)(-0.2 2)))
(check-expect (zip '(() ()) '(empt emp)) '((() empt)(() emp)))


 
;; =================================
;;
;; Question 3C
;;
;; =================================

 

;; (unzip listg) Given a list (listg) will produce a new
;; list where each of the elements (each contain two values)
;; a split into their own lists.
;; Examples:
(check-expect (unzip '((h i)(h i)))      '((h h)(i i)))
(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))
(check-expect (unzip '())                '(()()))

;; unzip: (listof (list X X)) -> (list (listof X) (listof X))
(define (unzip listg)
  (list (map (lambda (x) (first x)) listg)
        (map (lambda (x) (second x)) listg)))


;; Basic Tests
(check-expect (unzip '((a a) (b b)))      '((a b)(a b)))
(check-expect (unzip '((1 -1)(-1 1)))     '((1 -1) (-1 1)))  
(check-expect (unzip '((heyo !)))         '((heyo) (!)))      
(check-expect (unzip '((1 4)(2 5)(3 6)))  '((1 2 3) (4 5 6)))
(check-expect (unzip '((8 (2 4 6 8))))    '((8) ((2 4 6 8))))


;; Empty Tests
(check-expect (unzip '((() ()))) '((()) (())))
(check-expect (unzip '((() a)))  '((()) (a)))
(check-expect (unzip '((a ())))  '((a) (())))


;; Weird Types
(check-expect (unzip '(((a b) b)(a (b c)))) '(((a b) a) (b (b c))))
(check-expect (unzip '((hey !)(! !)))       '((hey !) (! !)))  
(check-expect (unzip '((-1 1)(1 1)(2 2)))   '((-1 1 2) (1 1 2)))
(check-expect (unzip '((0.1 1)(-0.2 2)))    '((0.1 -0.2) (1 2)))
(check-expect (unzip '((() empt)(() emp)))  '((() ()) (empt emp)))



;; =================================
;;
;; Question 3D
;;
;; =================================



;; (subsequence listg starting ending) Given a list (listg) will
;; produce a new list with a starting index (starting) and an
;; ending index (ending)
;; Examples:
(check-expect (subsequence '(a b c d e f g) 1 4) '(b c d))
(check-expect (subsequence '(a b c d e f g) 1 1) '())
(check-expect (subsequence '(a b c d) 0 400) '(a b c d))

;; subsequence: (listof X) Num Num -> (listof X) 
(define (subsequence listg starting ending)
  (foldr (lambda (x y rorr)
                     (cond [(= x 0) (cons y rorr)]
                           [else             rorr]))
         empty
         (build-list (length listg)
                     (lambda (x)
                             (cond [(>= x ending)   -1]
                                   [(< x starting) -1]
                                   [else             0])))
         listg))



;; Basic Tests
(check-expect (subsequence '(a b c d e f g) 5 4) '())
(check-expect (subsequence '(a b c) 0 3) '(a b c))
(check-expect (subsequence '(1 2 3 4 5 6) 0 400) '(1 2 3 4 5 6))
(check-expect (subsequence '(abc () () 3) 3 4) '(3))
(check-expect (subsequence '(0.1 0.3 0.5 6) 0 4) '(0.1 0.3 0.5 6))


;; Empty Tests
(check-expect (subsequence '(0.1 0.3 0.5 6) 5 7) '())
(check-expect (subsequence '() 0 2) '())
(check-expect (subsequence '() 5 4) '())


;; Weird Types
(check-expect (subsequence '((a b c) (b c d) e) 0 2)
                           '((a b c) (b c d)))
(check-expect (subsequence '(0.412 241 -1 -1 -3) 0 3) '(0.412 241 -1))
(check-expect (subsequence '(1 2 3 4 5 6) 0 400) '(1 2 3 4 5 6))
(check-expect (subsequence '(abc () () 3) 3 4) '(3))
(check-expect (subsequence '(0.1 0.3 0.5 6) 0 4) '(0.1 0.3 0.5 6))







