;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname future-a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Final Exams, Problem 2a
;; ******************************************


;; (hamming-distance list1 list2) Produces at how many places
;;  two lists (list1, list2) of equal lengths have different
;;  elements. Note that this is done without recursion.
;; Examples:

(

;; infect: Town Nat -> Horde
(define (infect town zombies)
  (cond [(empty? town) empty]
        [else 
          (append (list (list (first (first town))
                              zombies))
                  (infect (rest town) zombies))]))
                                            


;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===
(check-expect (infect fourOfour 0)
              empty)
(check-expect (infect fourOfour 10)
              empty)

;; === Small Scale Tests ===

(check-expect (infect uwp_home 101)
              (list (list 0 101)))
(check-expect (infect uwp_home 4815162342)
              (list (list 0 4815162342)))

(check-expect (infect g_washon 1)
              (list (list 0 1) (list 1 1)))
(check-expect (infect g_washon 2)
              (list (list 0 2) (list 1 2)))

(check-expect (infect huisclos 3411)
              (list (list 0 3411) (list 1 3411) (list 2 3411)))
(check-expect (infect huisclos 3412)
              (list (list 0 3412) (list 1 3412) (list 2 3412)))

;; === Large Scale Tests ===

(check-expect (infect waterloo 9999)
              (list (list 0 9999) (list 1 9999) (list 2 9999)
                    (list 3 9999) (list 4 9999) (list 5 9999)))
(check-expect (infect waterloo 10)
              (list (list 0 10) (list 1 10) (list 2 10)
                    (list 3 10) (list 4 10) (list 5 10)))

(check-expect (infect repdiamo 09)
              (list (list 0 09) (list 1 09) (list 2 09)
                    (list 3 09) (list 4 09) (list 5 09)))
(check-expect (infect repdiamo 9721)
              (list (list 0 9721) (list 1 9721) (list 2 9721)
                    (list 3 9721) (list 4 9721) (list 5 9721)))

(check-expect (infect findiamo 7)
              (list (list 0 7) (list 1 7) (list 2 7) (list 3 7)
                    (list 4 7) (list 5 7) (list 6 7)))
(check-expect (infect findiamo 1)
              (list (list 0 1) (list 1 1) (list 2 1) (list 3 1)
                    (list 4 1) (list 5 1) (list 6 1)))


