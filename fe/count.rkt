;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname count) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Final Exam, Problem 5
;; ******************************************



;; =================================
;;
;; [Global Struct Defns Defns]
;;
;; =================================


;; A (nested-listof X) is one of:
;; * empty
;; * (cons X (nested-listof X))
;; * (cons (nested-listof X) (nested listof X))



;; =================================
;;
;; Problem (5) [count-starting-with]
;;
;; =================================



;; (count-starting-with chr los) Consumes a list of strings (los)
;;   and a char (chr) and returns how many lists - both nested and
;;   non nested - contains this char as their first character
;; Examples:

(check-expect (count-starting-with #\9 empty)                  0)
(check-expect (count-starting-with #\9 '("9" "99"))            1)
(check-expect (count-starting-with #\a '("alpha" ("able")
                                        ("gamma" ("aaa"))))    3)
;; count-starting-with: Char (nested-listof Str) -> Nat
(define (count-starting-with chr los)
  (cond [(empty? los) 0]
        [else
         (local [

           ;; (getlocamount los) Given a nested list (los) will
           ;;   produce 1 if the values at the very top of the list
           ;;   have the same first character, and 0 if not.

           ;; getlocamount: Nat
           (define (getlocamount los)
            (cond [(> 1
                     (foldr (lambda (x rorr)
                          (cond [(and (string? x)
                                (char=? (first (string->list x)) chr))
                                                           (+ 1 rorr)]
                                [else                          rorr]))
                             0 los))                                0]
                  [else                                          1]))]
           
          (+ (getlocamount los)
             (foldr (lambda (x rorr)
                      (cond [(list? x) (+ (count-starting-with chr x)
                                                               rorr)]
                            [else                              rorr]))
                    0 los)))]))
                               

                
        
                                  
           
        
                                            
;; =================================
;; Testing Suite
;; =================================

;; === Empty Tests ===
(check-expect (count-starting-with #\a empty) 0)
(check-expect (count-starting-with #\a '("b")) 0)
(check-expect (count-starting-with #\a '("b" "c" "d")) 0)
(check-expect (count-starting-with #\a '(("b" "c") "d")) 0)
(check-expect (count-starting-with #\a '(("b" "c") "d" "e")) 0)

;; === General Tests ===
(check-expect (count-starting-with #\a '("a" "c" "d")) 1)
(check-expect (count-starting-with #\a '(("a" "c") "a")) 2)
(check-expect (count-starting-with #\a '(("a" "a") "d" "e")) 1)
(check-expect (count-starting-with #\a '(("a" ("a")) "d" "e")) 2)
(check-expect (count-starting-with #\a '(("a" ("a")) "a" "a")) 3)

(check-expect (count-starting-with #\b '((((((("be")))))))) 1)
(check-expect (count-starting-with #\b '(((((((("ban" "ba"))))))))) 1)
(check-expect (count-starting-with #\b '("barf" "bark" ("ball"))) 2)
(check-expect (count-starting-with #\b '((("ba") ("ba")) "ba")) 3)
(check-expect (count-starting-with #\b '("bz" ("be" "bu" "bi")
                                        (("bz") "be"))) 4)






