;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname redux) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 08, Problem 2(A-D)
;; ******************************************



;; =================================
;;
;; Question 2A
;;
;; =================================



;; (parity bstring) Produces the if the given string
;;    (bstring) contains an even amount of 1s or odd
;;    amounts of 1s.
;; Examples:
(check-expect (parity "1110") 'odd)
(check-expect (parity "Hello123") 'odd)
(check-expect (parity "00110011") 'even)

;; parity: Str -> Bool
;; Requires: Each charactor is either #\1 or #\0
(define (parity bstring)
    (cond [(even? (foldr (lambda (x rorr)
                            (cond [(char=? x #\1) (+ 1 rorr)]
                                  [else rorr]))
                         0
                         (string->list clist))) 'even]
          [else 'odd]))


;; Basic Tests
(check-expect (parity "") 'even)
(check-expect (parity "11011011") 'even)
(check-expect (parity "1") 'odd)
(check-expect (parity "2") 'even)
(check-expect (parity "11") 'even)
(check-expect (parity "1") 'odd)
(check-expect (parity "2") 'even)
(check-expect (parity "11") 'even)


;; Letters and Numbers Mixed
(check-expect (parity "1Hihowareyou2") 'odd)
(check-expect (parity "Hi211") 'even)
(check-expect (parity "01100111Hi") 'odd)
(check-expect (parity "") 'even)
(check-expect (parity "hi") 'even)



;; =================================
;;
;; Question 2B
;;
;; =================================



;; (replace-word indexstr replacstr strlist) Produces a
;;   new list in which all occurrences in a string list
;;   (strlist) of a string (indexstr) are replaced by
;;   a different string (replacstr)
;; Examples:
(check-expect
 (replace-word "exam" "assessment"
               (cons "content"
                     (cons "exam"
                           (cons "assignment" empty))))
 (cons "content" (cons "assessment" (cons "assignment" empty))))
(check-expect
 (replace-word "oranges" "apples"
               (cons "oranges"
                     (cons "orang"
                           (cons "apples" empty))))
 (cons "apples" (cons "orang" (cons "apples" empty))))

;; replace-word: Str Str (listof Str) -> (listof Str)
(define (replace-word indexstr replacstr strlist)
  (map (lambda (x) (cond [(string=? x indexstr) replacstr]
                         [else                          x]))
       strlist))

;; General Tests
(check-expect (replace-word "apple" "orange" '("apples"))
              '("apples"))
(check-expect (replace-word "pen" "pencil" '("pens" "pens"))
              '("pens" "pens"))
(check-expect (replace-word "Jack" "Jill" '("Jack" "Jack Jack"))
              '("Jill" "Jack Jack"))
(check-expect (replace-word "abc" "efg" '("abcde" "abcd" "abc"))
              '("abcde" "abcd" "efg"))
(check-expect (replace-word "hello." "Hello!!"
                           '("hello." "Hello?" "hello"))
              '("Hello!!" "Hello?" "hello"))

;; Empty List Test
(check-expect (replace-word "Calc" "No" empty)
              empty)

;; Multiple Replacements / Zero Replacements
(check-expect (replace-word "Name" "Robbie" '("Name?"))
              '("Name?"))
(check-expect (replace-word "Name" "Robbie" '("Name"))
              '("Robbie"))
(check-expect (replace-word "Name" "Robbie"
                            '("What" "is" "your" "name?"))
              '("What" "is" "your" "name?"))
(check-expect (replace-word "Name" "Robbie"
                            '("Name" "Name" "Name" "Name"))
              '("Robbie" "Robbie" "Robbie" "Robbie"))



;; =================================
;;
;; Question 2C
;;
;; =================================



;; (all-factors num) Produces a the list of natural (nat) factors
;;   of a given number (num), that divide evenly into that number
;; Examples:
(check-expect
 (all-factors 30)
  (cons 1 (cons 2 (cons 3 (cons 5
  (cons 6 (cons 10 (cons 15 empty))))))))
(check-expect
 (all-factors 10)
  (cons 1 (cons 2 (cons 5 empty))))

;; all-factors: Nat -> (listof Nat)
(define (all-factors num)
  (foldr (lambda (x rorr)
                (cond [(empty? x) rorr]
                      [else   (append (list x) rorr)]))
         empty
         (build-list num (lambda (x)
                    (cond [(= x 0)             empty]
                          [(= (remainder num x) 0) x]
                          [else                empty])))))

;; Basic Tests
(check-expect (all-factors 16)  (list 1 2 4 8))
(check-expect (all-factors 14)  (list 1 2 7))
(check-expect (all-factors 63)  (list 1 3 7 9 21))
(check-expect (all-factors 50)  (list 1 2 5 10 25))
(check-expect (all-factors 30)  (list 1 2 3 5 6 10 15))
(check-expect (all-factors 18)  (list 1 2 3 6 9))

;; Zero/One Test
(check-expect (all-factors 0)  empty)
(check-expect (all-factors 1)  empty)

;; Prime Tests
(check-expect (all-factors 2)  (list 1))
(check-expect (all-factors 13) (list 1))



;; =================================
;;
;; Question 2D
;;
;; =================================


;; ==========
; Test Cases
;; ==========

(define test1 (cons 5 empty))
(define test2 (cons 4 (cons 6 empty)))
(define test3 (cons 1 (cons 3 (cons 5 empty))))

(define test4 (cons 2 (cons 2 (cons 4 empty))))
(define test5 empty)
(define test6 (cons 0 (cons 0 (cons 6 empty))))
(define test7 (cons 1 (cons 2 (cons 3 empty))))
(define test8 (cons 3 (cons 2 (cons 1 empty))))

(define test9 (cons 0 (cons 0 empty)))
(define test10 (cons 2 (cons 2 empty)))
(define test11 (cons -2 (cons -2 (cons -2 empty))))

(define test12 (cons -1 (cons 2 (cons 3 empty))))
(define test13 (cons -3 (cons 5 (cons 1 empty))))
(define test14 (cons -1 (cons -5 (cons -3 empty))))
(define test15 (cons -3 (cons 6 (cons -3 empty))))


;; (mean-relative lon) Produces a new list
;;   based on the list of numbers (lon) that
;;   tells if each element is below above
;;   or at the mean
;; Examples:
(check-expect (mean-relative test1) (cons 'mean empty))
(check-expect (mean-relative test2)
              (cons 'below-mean (cons 'above-mean empty)))
(check-expect (mean-relative test3) (cons 'below-mean
              (cons 'mean (cons 'above-mean empty))))

;; mean-relative: (listof Num) -> (List of Sym)
(define (mean-relative lon)

 (cond [(empty? lon) empty]
       [else
        
        
             (local [
               ;; (mean lon) Produces the mean of a given list
               ;;  of numbers (lon)
               ;; mean: (listof Num -> Num)
               (define (mean lon (/ (foldr + 0 lon) (length lon)))]
    
              (map (lambda (x) (cond [(= x mean)       'mean]
                                     [(> x mean) 'above-mean]
                                     [else       'below-mean]))
                   lon))]))
  


;; Basic Tests
(check-expect (mean-relative test4) (cons 'below-mean
                 (cons 'below-mean (cons 'above-mean empty))))
(check-expect (mean-relative test6) (cons 'below-mean
                 (cons 'below-mean (cons 'above-mean empty))))
(check-expect (mean-relative test7) (cons 'below-mean
              (cons 'mean (cons 'above-mean empty))))
(check-expect (mean-relative test8) (cons 'above-mean
              (cons 'mean (cons 'below-mean empty))))

;;Empty List Test:
(check-expect (mean-relative test5) empty)

;;Equal Lists Tests:
(check-expect (mean-relative test9)
              (cons 'mean (cons 'mean empty)))
(check-expect (mean-relative test10)
              (cons 'mean (cons 'mean empty)))
(check-expect (mean-relative test11) (cons 'mean
              (cons 'mean (cons 'mean empty))))

;;Negitive Element List Tests:
(check-expect (mean-relative test12) (cons 'below-mean
              (cons 'above-mean (cons 'above-mean empty))))
(check-expect (mean-relative test13) (cons 'below-mean
              (cons 'above-mean (cons 'mean empty))))
(check-expect (mean-relative test14) (cons 'above-mean
              (cons 'below-mean (cons 'mean empty))))
(check-expect (mean-relative test15) (cons 'below-mean
              (cons 'above-mean (cons 'below-mean empty))))







