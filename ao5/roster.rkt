;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname roster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 06, Problem 1(A-D)
;; ******************************************

(require "cs135-trace.rkt")

;; ==================================
;; struct and data definitions For Q1
;; =================================

;; A StudentID is a Nat with at most 8 digits
;;                  (i.e. 0 <= id <= 99999999)

;; A Grade is one of:
;; * false
;; * Nat
;;   Requires: Nat is between 0 and 100 (inclusive)

(define-struct student (id name grade))
;; A Student is a (make-student StudentID Str Grade)


(define-struct rnode (student left right))
;; A Roster Node (RN) is a (make-rnode Student Roster Roster)
;; Requires: all students in the left subtree have
;;                                    an ID < student's ID
;;           all students in the right subtree have
;;                                    an ID > student's ID

;; A Roster is one of 
;; * empty
;; * RN


;; =================================
;; constants used in Q1 examples
;; =================================

(define beth (make-student 12345678 "Beth" 96))
(define jenny (make-student 08675309 "Jenny" 81))
(define john1 (make-student 48975311 "John" 95))
(define jenny/new (make-student 08675309 "Jen" 81))
(define john2 (make-student 20488192 "John" false))

(define sample-roster
  (make-rnode beth ; root
              (make-rnode jenny empty empty)   ; left child
              (make-rnode john1 empty empty))) ; right child

(define sample-roster-2
  (make-rnode beth 
              (make-rnode jenny/new empty empty)
              (make-rnode john1
                          (make-rnode john2 empty empty)
                          empty)))

;; More Tests

(define ta (make-student 50000000 "a" 99))
(define tb (make-student 40000000 "b" 88))
(define tc (make-student 30000000 "c" 93))
(define td (make-student 35000000 "d" 92))
(define te (make-student 10000000 "e" 85))
(define tf (make-student 37500000 "f" 81))
(define tg (make-student 70000000 "g" 87))
(define th (make-student 80000000 "h" false))
(define ti (make-student 90000000 "i" false))
(define tj (make-student 90090000 "j" false))

(define test1
  (make-rnode ta 
        (make-rnode tc
              (make-rnode tb empty empty)
              (make-rnode td
                    (make-rnode te empty empty)
                    (make-rnode tf empty empty)))
        (make-rnode th
              (make-rnode tg empty empty)
              (make-rnode ti
                    empty
                    (make-rnode tj empty empty)))))

(define test2
  (make-rnode ta 
        (make-rnode tc
              empty
              (make-rnode td empty empty))
        (make-rnode th
              empty
              (make-rnode ti empty empty))))


;; =================================
;;
;; Question 1A
;;
;; =================================


;; (find-student StuID Rost) Consumes a StuID (StudentID)
;;   and a Rost (Roster), and produces the student that
;;   has that id that matches or produces false.
;; Examples:

(check-expect (find-student 12345678 sample-roster) beth)
(check-expect (find-student 87654321 sample-roster) false)

;; find-student: StudentID Interval -> (anyof false Str)
(define/trace (find-student StuID Rost)
   (cond [(empty? Rost)                        false]
         [(and (empty? (rnode-left Rost))
                (empty? (rnode-right Rost)))   false]
         [(= (student-id (rnode-student Rost)) StuID)
                                (rnode-student Rost)]
         [(< (student-id (rnode-student Rost)) StuID)
              (find-student StuID (rnode-left Rost))]
         [else
             (find-student StuID (rnode-right Rost))]))
;; Tests Where The Element DNE

(check-expect (find-student 10000000 empty) false)
(check-expect (find-student 0 sample-roster) false)
(check-expect (find-student 55 sample-roster-2) false)
(check-expect (find-student 37000 test1) false)
(check-expect (find-student 90000 test2) false)

;; General Tests

(check-expect (find-student 12345678 sample-roster-2) beth)
(check-expect (find-student 08675309 sample-roster-2) jenny)
(check-expect (find-student 48975311 sample-roster-2) john1)
(check-expect (find-student 08675309 sample-roster-2) jenny/new)
(check-expect (find-student 20488192 sample-roster-2) john2)
(check-expect (find-student 50000000 test2) ta)
(check-expect (find-student 70000000 test2) tg)
(check-expect (find-student 40000000 test2) tb)
(check-expect (find-student 37500000 test2) tf)
(check-expect (find-student 90090000 test2) tj)
             
   


