;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname brackets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 09, Problem 2 (A-B)
;; ******************************************


;; (balanced? bstring) Given a bracket string (bstring), will produce
;;  if the brackets end up closing in an appropriate way (AKA no
;;  overlap of order)
;; Examples:
(check-expect (balanced? "(<)>") false)
(check-expect (balanced? "((<>[])<>)[]") true)
(check-expect (balanced? "(<>[])") true)

;; balanced?: Str -> Bool
(define (balanced? bstring)

  (cond [(empty? bstring) true]
        [else 
          (local [
            ;; (compare-brack blist bstack) Given a string of
            ;;  brackets, compares if for each element in b string
            ;;  its corrisponding value will exists within the bstack

            ;; compare-brack: (listof Str) (listof Str) -> Bool
            
                  (define (compare-brack blist bstack)
                    (cond [(and (empty? blist) (empty? bstack)) true]
                          [(empty? blist)                      false]
                          [(and (empty? bstack)
                                (or (char=? (first blist) #\))
                                    (char=? (first blist) #\>)
                                    (char=? (first blist) #\])))
                                                               false]
                          [(char=? (first blist) #\))
                           (and (char=? (first bstack) #\()
                                (compare-brack (rest blist)
                                               (rest bstack)))]
                          [(char=? (first blist) #\>)
                           (and (char=? (first bstack) #\<)
                                (compare-brack (rest blist)
                                               (rest bstack)))]
                          [(char=? (first blist) #\])
                           (and (char=? (first bstack) #\[)
                                (compare-brack (rest blist)
                                               (rest bstack)))]
                          [else
                           (compare-brack
                            (rest blist)
                            (append (list (first blist))
                                    bstack))]))]
            
                (compare-brack (string->list bstring)
                               empty))]))


;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===
(check-expect (balanced? empty) true)


;; === General Tests ===
(check-expect (balanced? "[][][][][][]") true)
(check-expect (balanced? "[[][][][][][]<]>") false)
(check-expect (balanced? "[") false)
(check-expect (balanced? "<[]>") true)
(check-expect (balanced? "<[()][()()]>[]") true)
(check-expect (balanced? "<[[]]()>") true)
(check-expect (balanced? "<[[]()(<>)]>") true)
(check-expect (balanced? "([]<>)") true)
(check-expect (balanced? ">>>><<<<") false)
(check-expect (balanced? "))[]()") false)
(check-expect (balanced? "][)[") false)


