;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 07, Problem 1(A-D)
;; ******************************************



;; =================================
;;
;; Question 1A
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
  (cond [(empty? lst)                         empty]
        [(list?  lst)
        (cond   [(list? (first lst))
                 (append (cons (super-filter pred? (first lst))
                               (super-filter pred? (rest lst))))]
                       
                [else (append (super-filter pred? (first lst)) 
                              (super-filter pred? (rest lst)))])]
        [(pred? lst)         (cons lst empty)]
                                   
        [else                  empty]))

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


;; =================================
;;
;; Question 1B
;;
;; =================================



;; (ruthless lst) Given a list of symbols (lst) will use
;;  super-filter to remove all 'ruth elements that are
;;  within the nested list
;; Examples:
(check-expect
(ruthless
 (list 'rabbit
       (list 'apple 'pluto
             (list 'ruth 'blue)
             'ruth)
       'hello))
 (list 'rabbit
       (list 'apple 'pluto
              (list 'blue))
       'hello))

;; ruthless: (nested-listof Sym) -> (listof Sym)
(define (ruthless lst)
  
  (local[
         
    ;; (isnotruth? symb) Checks to see if a symbol (symb) is equal to
    ;; ruth, if so returns false (as to remove it from filter)
    ;; isruth?: Sym -> Bool
         (define (isnotruth? symb)
            (not (symbol=? symb 'ruth)))]
    
   (super-filter isnotruth? lst)))


;; Empty Test
(check-expect (ruthless empty) empty)
(check-expect (ruthless '(empty)) '(empty))

;; No Change Tests
(check-expect (ruthless '(a b c d (a b c d)))
                        '(a b c d (a b c d)))
(check-expect (ruthless '(no _ruth_ here!!))
                        '(no _ruth_ here!!))

;; Only Change Tests
(check-expect (ruthless '(ruth ruth ruth))
                        '())
(check-expect (ruthless '(ruth (ruth) ruth))
                        '(()))

;; General Tests 
(check-expect (ruthless '(I am (ruth ruthless)))
                        '(I am (ruthless)))
(check-expect (ruthless '(ruth ruth ((RUTH) ru1h)))
                        '(((RUTH) ru1h)))
(check-expect (ruthless '((ruthruth) ruth (ruth ruuth)))
                        '((ruthruth) (ruuth)))
(check-expect (ruthless '((ruth ruth) _ruth (ruth ruth)))
                        '(() _ruth ()))



;; =================================
;;
;; Question 1C
;;
;; =================================



;; (supersize pred? lst) Given a nested list (lst) keeps
;;  will use super-filter to remove all naturals less then
;;  a given number (n)

;; Examples:
(check-expect (supersize 2
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
  (list (list 2 (list 2 4) 6 (list 8)) 10 12))

;; supersize: Num (nested-listof Nat) -> (listof Nat)
(define (supersize n lst)
  
  (local[
         
    ;; (isnotless? n cpson) Checks to see if a number (cpson)
    ;; is less then n and will return false if so
    ;; isnotless?: Num Num -> Bool
         (define (isnotless? cpson)
            (not (and (number? cpson) (> n cpson))))]
    
   (super-filter isnotless? lst)))


;; Empty Test
(check-expect (supersize 1 empty) empty)
(check-expect (supersize 1 '(empty)) '(empty))

;; No Change Tests
(check-expect (supersize 1 '(2 3 4 5 (2 3 4 5)))
                            '(2 3 4 5 (2 3 4 5)))
(check-expect (supersize 6 '(6 6 6 6 6 6 6 7 6))
                            '(6 6 6 6 6 6 6 7 6))

;; Only Change Tests
(check-expect (supersize 6 '(2 3 4 5 (2 3 4 5)))
                            '(()))
(check-expect (supersize 8 '(6 6 6 6 6 6 6 7 6))
                            '())
;; General Tests 
(check-expect (supersize 3 '((1 2) 3 (4 5) ((3 1) 2) 1))
                            '(() 3 (4 5) ((3))))
(check-expect (supersize 2 '(((1 2 3) 2 3) (((1) 2 (1) 4))))
                            '(((2 3) 2 3) ((() 2 () 4))))
(check-expect (supersize 7 '(6 6 6 6 6 6 6 7 6))
                            '(7))
(check-expect (supersize 6 '(((78 5 6) 2 (8) ((6) 5 (1) 4))))
                            '(((78 6) (8) ((6) ()))))


;; =================================
;;
;; Question 1D
;;
;; =================================



;; (super-keeper pred? lst) Given a nested list (lst) will
;;  remove all the elments that satisfy the condition and keep
;;  those that dont

;; Examples:
(check-expect (super-keeper odd?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
  (list (list 2 (list 2 4) 6 (list 8)) 10 12))


;; super-keeper: (X -> Bool) (nested-listof X) -> (listof X)
(define (super-keeper pred? lst)
  
  (local[
         
    ;; (npred? value) Checks to see if a value is
    ;; correct by the predicate, returns false
    ;; isnotless?: X -> Bool
         (define (npred? value)
            (not (pred? value)))]
    
   (super-filter npred? lst)))


;; Empty Test
(check-expect (super-keeper even? empty) empty)

;; General Tests (Numbers)
(check-expect (super-keeper even? '(1 2 3 4 5 6)) '(1 3 5))
(check-expect (super-keeper even?
                            '((1 2) (3 4) 5 6)) '((1) (3) 5))
(check-expect (super-keeper odd?
                            '((((1))) 2 ((3)))) '(((())) 2 (())))
(check-expect (super-keeper odd?
                            '(1 2 (3 4 (5 6)) 1 1)) '(2 (4 (6))))
(check-expect (super-keeper odd?
                            '(1 ((1)) (1) 2 1)) '((())()2))

;; General Tests (Strings)
(check-expect (super-keeper string? '("aa" a b)) '(a b))
(check-expect (super-keeper string? '((hi) "(hi)" ("hi")))
                                    '((hi) ()))

;; General Tests (Symbols)
(check-expect (super-keeper symbol? '("aa" a b)) '("aa"))
(check-expect (super-keeper symbol? '((hi) "(hi)" ("hi")))
                                    '(() "(hi)" ("hi")))