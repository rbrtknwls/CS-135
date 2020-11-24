;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super-foldr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 08, Problem 5 (A-C)
;; ******************************************



;; =================================
;;
;; Question 5A
;;
;; =================================



;; (super-filter pred base loe) Given a nested list of elements (loe)
;;  will apply foldr using a predicate (pred) and a base
;;  index (base) to get a value across the nested list
;; Examples:
(check-expect (super-foldr + 0 '(1 (5 5 (1 3)) (10 2) 2)) 29)
(check-expect (super-foldr - 0 '(1 (5 5 (1 3)) (10 2) 2)) 9)
(check-expect (super-foldr + 0 '(5 (5 5 (5 5)) (5 5) 5)) 40)


;; super-filter: (X -> X) X (nested-listof X) -> X
(define (super-foldr pred base lop)
    (foldr (lambda (x rorr)
             (cond [(list? x)
                    (pred (super-foldr pred base x)
                          rorr)]
                   [else (pred x rorr)]))
           base
           lop))

  
    
;; Number Tests
(check-expect (super-foldr + 0  '((((((1 1))))))) 2)
(check-expect (super-foldr + 0  '( 1 ( 2 ( 3 ( 4 ( 5 )))))) 15)
(check-expect (super-foldr + 0  '( 1.3 ( -2 ) (( 3 ( 2.7 ) )))) 5)
(check-expect (super-foldr + 0  '(-1 ( -2) (( -3( -2.7))) -4)) -12.7)
(check-expect (super-foldr * 10 '(1 (( 1 ( 3 ))) 1)) 30000)
(check-expect (super-foldr + 3 '((((3))))) 15)

;; Empty Tests
(check-expect (super-foldr / 10 '()) 10)
(check-expect (super-foldr / 5 '((()))) 0.2)
(check-expect (super-foldr string-append "a" '((()))) "aaa")

;; String Tests
(check-expect (super-foldr (lambda (val sofar)
                   (cond [(string? val)
                          (+ (string-length val) sofar)]
                         [(number? val) (+ val sofar)]))
              0
             '("pancho" "lefty" ("my" "proud" "mountains")
                        ("tecumseh" "valley")))
              41)
(check-expect (super-foldr string-append "!!!" '("hinicetomeetyou"))
              "hinicetomeetyou!!!")
(check-expect (super-foldr string-append "!!!"
              '("hi"("nice"("to")"meet")"you"))
              "hiniceto!!!meet!!!you!!!")
(check-expect (super-foldr string-append "|"
              '(((((((("hi")))))))))
              "hi||||||||")
(check-expect (super-foldr string-append "|"
              '((("heyo")((("gi")((("hi"))))))))
              "heyo|gi|hi|||||||")



;; =================================
;;
;; Question 5B
;;
;; =================================



;; (magnitudes nl) Given a nested list of numbers (nl)
;;  that will get the sum of all the absolute values
;;  neted in the list.
;; Examples:
(check-expect (magnitudes '(1 (-5 -5 (1 -3)) (-10 2) 2)) 29)
(check-expect (magnitudes '(1 (-5 -5 (1 -3)) (-10 2))) 27)
(check-expect (magnitudes '(1 (-5 -5 (1 -3)))) 15)

;; magnitudes: (nested-listof Num) -> Num
(define (magnitudes nl)
  (super-foldr (lambda (x rorr)
                 (+ (abs x) (abs rorr)))
               0 nl))
    
;; Empty Tests
(check-expect (magnitudes '(()()()(()())()((())))) 0)
(check-expect (magnitudes '()) 0)

;; General Tests
(check-expect (magnitudes '(1 2 3 4 5 6)) 21)
(check-expect (magnitudes '(1 -5 -4 5 6)) 21)
(check-expect (magnitudes '((1 2 (-3 4 (5) (-6))))) 21)
(check-expect (magnitudes '((-1 -2 (3 -4 (5) (6))))) 21)
(check-expect (magnitudes '((-5 (-1.23) ((-7.31)) 10) 10)) 33.54)
(check-expect (magnitudes '((-5 (-1.23) ((-7.31)) -10) -10)) 33.54)
(check-expect (magnitudes '((((((((((-2))))))))))) 2)
(check-expect (magnitudes '(-1(-1(-1(-1(-2)))))) 6)



;; =================================
;;
;; Question 5C
;;
;; =================================



;; (super-filter pred? lst) Given a predicate (pred?), will filter
;;  through any depth of list (lis) and get rid of data
;;  that does not satisfy the predicate
;; Examples:
(check-expect (super-filter odd?
   (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
   (list 1 (list (list 3) 5 (list 7 9)) 11))

;; super-filter: (X -> Bool) (nested-listof X) -> (listof X)
(define (super-filter pred? lst)
    (super-foldr (lambda (x rorr)
                     (cond [(list? x) (append (list x) rorr)]
                           [(pred? x) (cons x rorr)]
                           [else rorr]))
                 empty
                 lst))

;; Empty Test
(check-expect (super-filter even? empty) empty)

;; General Tests (Numbers)
(check-expect (super-filter even? '(1 2 3 4 5 6)) '(2 4 6))
(check-expect (super-filter even?
                            '((1 2) (3 4) 5 6)) '((2) (4) 6))
(check-expect (super-filter odd?
                            '((((1))) 2 ((3)))) '((((1))) ((3))))
(check-expect (super-filter odd?
                            '(1 2 (3 4 (5 6)) 1 1)) '(1 (3 (5)) 1 1))
(check-expect (super-filter odd?
                            '(1 ((1)) (1) 2 1)) '(1 ((1)) (1) 1))

;; General Tests (Strings)
(check-expect (super-filter string? '("aa" a b)) '("aa"))
(check-expect (super-filter string? '((hi) "(hi)" ("hi")))
                                    '(()"(hi)" ("hi")))

;; General Tests (Symbols)
(check-expect (super-filter symbol? '("aa" a b)) '(a b))
(check-expect (super-filter symbol? '((hi) "(hi)" ("hi")))
                                    '((hi)()))