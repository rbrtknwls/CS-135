;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname binary) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Final Exam, Problem 4
;; ******************************************



;; =================================
;;
;; [Global Struct Defns Defns]
;;
;; =================================



(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; Requires: key > every key in left BST
;;  key < every key in right BST

;; A Binary Search Tree (BST) is one of
;; * empty
;; * Node



;; =================================
;;
;; Problem (2bi) [sorted-list->bst]
;;
;; =================================



;; (sorted-list->bst lon) Consumes a list of sorted nums (lon) and
;;   produces a balanced binary search tree from those numbers, also
;;   assumes there is no duplicates
;; Examples:

(check-expect (sorted-list->bst empty) empty)

(check-expect (sorted-list->bst '(1 2 3 4 5))
              (make-node 3
                         (make-node 2
                                    (make-node 1 empty empty)
                                    empty)
                         (make-node 5
                                    (make-node 4 empty empty)
                                    empty)))

;; sorted-list->bst: (listof Num) -> BST
(define (sorted-list->bst lon)
  (cond [(empty? lon) empty]
        [else
         (local [

                ;; (split compare current currlist frontlist)
                ;;  Produces a list containing three smaller num
                ;;  lists such that the orignal list is split
                ;;  right through the middle

                ;; split: Nat Nat                 (list (listof Nat)
                ;;       (listof Nat)       -->         (listof Nat)
                ;;       (listof Nat)                   (listof Nat))
                
                (define (split compare current currlist frontlist)
                    (cond [(= current compare)
                             (append (list frontlist)
                                     (list (first currlist))
                                     (list (rest currlist)))]
                           [else (split compare
                                   (add1 current)
                                   (rest currlist)
                                   (append frontlist
                                        (list (first currlist))))]))

                ;; (currsplit) instantiate split for the current list 
                ;;  of numbers, used for efficency.

                ;; currsplit: (list (listof Nat)
                ;;                  (listof Nat) (listof Nat))
                (define currsplit (split (floor (/ (length lon) 2))
                                         0 lon empty))]

           (make-node (second currsplit)
                      (sorted-list->bst (first currsplit))
                      (sorted-list->bst (third currsplit))))]))
           
        
                                            
;; =================================
;; Testing Suite
;; =================================

;; === Empty Tests ===
(check-expect (sorted-list->bst empty) empty)

;; === Odd Tests ===
(check-expect (sorted-list->bst '(1)) (make-node 1 empty empty))
(check-expect (sorted-list->bst '(1 3 5))
              (make-node 3
                        (make-node 1 empty empty)
                        (make-node 5 empty empty)))
(check-expect (sorted-list->bst '(1 3 5 7 9))
              (make-node 5
                        (make-node 3
                                   (make-node 1 empty empty)
                                   empty)
                        (make-node 9
                                   (make-node 7 empty empty)
                                   empty)))
(check-expect (sorted-list->bst '(1 3 5 7 9 11 13))
              (make-node 7
                        (make-node 3
                                   (make-node 1 empty empty)
                                   (make-node 5 empty empty))
                        (make-node 11
                                   (make-node 9 empty empty)
                                   (make-node 13 empty empty))))
(check-expect (sorted-list->bst '(2 4 6 8 10 12 14))
              (make-node 8
                        (make-node 4
                                   (make-node 2 empty empty)
                                   (make-node 6 empty empty))
                        (make-node 12
                                   (make-node 10 empty empty)
                                   (make-node 14 empty empty))))

;; === Even Tests ===
(check-expect (sorted-list->bst '(2 4))
              (make-node 4
                        (make-node 2 empty empty)
                        empty))
(check-expect (sorted-list->bst '(2 4 6 8))
              (make-node 6
                        (make-node 4
                                   (make-node 2 empty empty)
                                   empty)
                        (make-node 8 empty empty)))
(check-expect (sorted-list->bst '(2 4 6 8 10 12))
              (make-node 8
                        (make-node 4
                                   (make-node 2 empty empty)
                                   (make-node 6 empty empty))
                        (make-node 12
                                   (make-node 10 empty empty)
                                   empty)))
(check-expect (sorted-list->bst '(2 4 6 8 10 12 14 16))
              (make-node 10
                        (make-node 6
                                   (make-node 4
                                             (make-node 2 empty empty)
                                              empty)
                                   (make-node 8 empty empty))
                        (make-node 14
                                   (make-node 12 empty empty)
                                   (make-node 16 empty empty))))
(check-expect (sorted-list->bst '(2 4 6 8 10 12 14 16 18 20))
              (make-node 12
                        (make-node 6
                                   (make-node 4
                                          (make-node 2 empty empty)
                                           empty)
                                   (make-node 10
                                          (make-node 8 empty empty)
                                          empty))
                        (make-node 18
                                   (make-node 16
                                          (make-node 14 empty empty)
                                          empty)
                                   (make-node 20 empty empty))))
(check-expect (sorted-list->bst '(2 4 6 8 10 12 14 16 18 20 22))
              (make-node 12
                        (make-node 6
                                   (make-node 4
                                          (make-node 2 empty empty)
                                           empty)
                                   (make-node 10
                                          (make-node 8 empty empty)
                                          empty))
                        (make-node 18
                                   (make-node 16
                                          (make-node 14 empty empty)
                                          empty)
                                   (make-node 22
                                          (make-node 20 empty empty)
                                          empty))))
(check-expect (sorted-list->bst '(1 2 3 4 5 6 7 8 9 10 11))
              (make-node 6
                        (make-node 3
                                   (make-node 2
                                          (make-node 1 empty empty)
                                           empty)
                                   (make-node 5
                                          (make-node 4 empty empty)
                                          empty))
                        (make-node 9
                                   (make-node 8
                                          (make-node 7 empty empty)
                                          empty)
                                   (make-node 11
                                          (make-node 10 empty empty)
                                          empty))))




