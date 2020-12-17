;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname general) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Final Exam, Problem 6
;; ******************************************



;; =================================
;;
;; [Global Struct Defns Defns]
;;
;; =================================


;; an (alistof X Y) is a (listof (list X Y))
;; Requires: each X is unique (no duplicates)


;; =================================
;;
;; Problem (6) [alist-combine]
;;
;; =================================



;; (alis-combine alist1 alist2 combine) Consumes the two 
;;   associated lists (alist1 alist2) and a function
;;   to combine the two, and produces a new 
;;   new association list.
;; Examples:

(check-expect (alist-combine '((a 1) (b 2)) '((c 1) (a 3)) +)
                             '((a 4) (b 2) (c 1)))
(check-expect (alist-combine '((1 1) (2 2)) empty +)
                             '((1 1) (2 2)))
(check-expect (alist-combine '((1 "One") (3 "Three") (2 "Two"))
                             '((2 "Deux") (1 "Un") (3 "Trois"))
                             (lambda (s1 s2)
                                (cond [(< (string-length s1)
                                          (string-length s2))
                                            s1]
                                      [else s2])))
                             '((1 "Un") (3 "Trois") (2 "Two")))

;; alist-combine:
;;           (listof (alist X Any))
;;           (listof (alist X Any))) -> (listof (alist X Any))
;;                   (X Any -> Any) 
(define (alist-combine alist1 alist2 combine)
  (cond [(and (empty? alist1) (empty? alist2)) empty]
        [(empty? alist1)
                 (alist-combine alist2 alist1 combine)]
        [(empty? alist2)
                 (cons (first alist1)
                       (alist-combine (rest alist1) alist2 combine))]
        [else
          (local [

                  ;; (secondmatch) Produces the matching key pair from
                  ;;   the second alist. This is the opposite of the
                  ;;   other function

                  ;; secondmatch: Any
                  (define secondmatch 
                      (foldr (lambda (x rorr)
                                (cond [(equal? (first (first alist1))
                                                           (first x))
                                                          (second x)]
                                      [else                   rorr]))
                             empty
                             alist2))

                  ;; (secondnomatch) Produces the second alist without
                  ;;   the key pair from the second alist. This is the
                  ;;   the opposite of the other function

                  ;; secondmatch: (listof (alist X Any))
                  (define secondnomatch 
                      (foldr (lambda (x rorr)
                                (cond [(equal? (first (first alist1))
                                                           (first x))
                                                                rorr]
                                      [else          (cons x rorr)]))
                             empty
                             alist2))]


            
            (cond [(empty? secondmatch)
                    (cons
                       (first alist1)
                       (alist-combine (rest alist1) alist2 combine))]
                  [else
                    (cons
                       (list (first (first alist1))
                             (combine (second (first alist1))
                                      secondmatch))
                       (alist-combine (rest alist1)
                                      secondnomatch combine))]))]))
                             
                     
                               

                
        
                                  
           
        
                                            
;; =================================
;; Testing Suite
;; =================================

;; === Empty Tests ===
(check-expect (alist-combine empty empty equal?)
                             empty)

;; === One List Tests ===
(check-expect (alist-combine '((a b) (c "d") (1 1)) empty +)
                             '((a b) (c "d") (1 1)))
(check-expect (alist-combine empty '((2 2) (3 3) (1 1)) +)
                             '((2 2) (3 3) (1 1)))
(check-expect (alist-combine '((7 b)) empty -)
                             '((7 b)))
(check-expect (alist-combine empty '((i 2) (3 i) (d 1)) /)
                             '((i 2) (3 i) (d 1)))
(check-expect (alist-combine '((7 b) (e 5)) empty *)
                             '((7 b) (e 5)))

;; === Non Repeating List Tests ===
(check-expect (alist-combine '(((a a) 2)) '((b b 2)) equal?)
                             '(((a a) 2) (b b 2)))
(check-expect (alist-combine '((1 2)) '((3 2)) /)
                             '((1 2) (3 2)))
(check-expect (alist-combine '((1 2) (a b)) '((3 2)) +)
                             '((1 2) (a b) (3 2)))
(check-expect (alist-combine '((1 2) (a b)) '((3 2) (d e)) -)
                             '((1 2) (a b) (3 2) (d e)))
(check-expect (alist-combine '((1 2) (a b) (e f)) '((3 2) (d e)) *)
                             '((1 2) (a b) (e f) (3 2) (d e)))

;; === General List Tests ===
(check-expect (alist-combine '(((a a) 2)) '(((a a) 2)) +)
                             '(((a a) 4)))
(check-expect (alist-combine '(((3 5) 5)) '(((3 5) 2)) -)
                             '(((3 5) 3)))
(check-expect (alist-combine '((1 5) (2 4)) '((1 3)) *)
                             '((1 15) (2 4)))
(check-expect (alist-combine '((1 2) (a b)) '((3 2) (d e)) -)
                             '((1 2) (a b) (3 2) (d e)))
(check-expect (alist-combine '((1 2) (a b) (e f)) '((3 2) (d e)) *)
                             '((1 2) (a b) (e f) (3 2) (d e)))

;; === Extreme Comparison Tests ===
(check-expect (alist-combine '((3 3) (a b)) '((3 2) (a b)) equal?)
                             '((3 #false) (a #true)))
(check-expect (alist-combine '((3 c) (a b)) '((3 d) (a b)) symbol=?)
                             '((3 #false) (a #true)))
(check-expect (alist-combine '((1 "One") (3 "Three") (2 "Two"))
                             '((2 "Deux") (1 "Un") (3 "Trois"))
                             (lambda (s1 s2)
                                (cond [(> (string-length s1)
                                          (string-length s2))
                                            s1]
                                      [else s2])))
                             '((1 "One") (3 "Trois") (2 "Deux")))
(check-expect (alist-combine '((1 "a") (3 "bb") (2 "dd"))
                             '((2 "ccc") (1 "b") (3 "ee"))
                             (lambda (s1 s2)
                                (cond [(= (string-length s1)
                                          (string-length s2))
                                            s1]
                                      [else s2])))
                             '((1 "a") (3 "bb") (2 "ccc")))
(check-expect (alist-combine '((1 5) (3 -72) (2 10))
                             '((2 -2) (1 -10) (3 80))
                             (lambda (s1 s2)
                                (cond [(negative?
                                        (+ s1 s2))
                                            -1]
                                      [else 1])))
                             '((1 -1) (3 1) (2 1)))






