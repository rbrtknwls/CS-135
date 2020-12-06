;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname brackets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 10, Problem 1-7
;; ******************************************

;; =================================
;;
;; [Global Tesing Defns]
;;
;; =================================


;; ========= Struct Defns ==========


;; A Location is a Nat
;; A Town is a (listof (list Location (listof Location)))
;; Requires: Town represents a valid graph as defined in Module 16

;; A Horde is a (listof (list Location Nat))


;; ========== Test Defns ===========


(define waterloo '((0 (1 2 3)) ;; MALL
                   (1 (2 3))   ;; PARK
                   (2 (0 4))   ;; MARKET
                   (3 (1))     ;; WLU
                   (4 (5))     ;; UW yayyy
                   (5 (3))))   ;; UPtown

(define repdiamo '((0 (1 2))   ;; Cong 1
                   (1 (3))     ;; Route 1a
                   (2 (3))     ;; Route 1b
                   (3 (4 5))   ;; Cong 2
                   (4 (0))     ;; Route 2a
                   (5 (0))))   ;; Route 2b

(define findiamo '((0 (1 2))   ;; Cong 1
                   (1 (3))     ;; Route 1a
                   (2 (3))     ;; Route 1b
                   (3 (4 5))   ;; Cong 2
                   (4 (6))     ;; Route 2a
                   (5 (6))     ;; Route 2b
                   (6 ())))    ;; Cong 3 (Terminus)


;; Metaphorical Tests

(define fourOfour '())   ;; Empty Town

(define uwp_home '((0 ())))    ;; Just my Room

(define g_washon '((0 (1))     ;; Reference to Grover Washington
                   (1 (0))))   ;; famous "Just the two of us"
  
(define huisclos '((0 (1 2))   ;; Joseph Garcin
                   (1 (0))     ;; Inez Serrano
                   (2 (0))))   ;; Estelle Rigault   




;; =================================
;;
;; Problem 1 [Infect]
;;
;; =================================



;; (infect town zombies) Produces the starting number of zombies
;;  in a graph, given the graph (town) and the the number of zombies
;;  (zombies)
;; Examples:
(check-expect (infect waterloo 1000)
              (list (list 0 1000) (list 1 1000) (list 2 1000)
                    (list 3 1000) (list 4 1000) (list 5 1000)))

(check-expect (infect waterloo 0)
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))

(check-expect (infect fourOfour 500)
              empty)

;; infect: Town Int -> Horde
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
(check-expect (infect huisclos -3411)
              (list (list 0 -3411) (list 1 -3411) (list 2 -3411)))

;; === Large Scale Tests ===

(check-expect (infect waterloo 9999)
              (list (list 0 9999) (list 1 9999) (list 2 9999)
                    (list 3 9999) (list 4 9999) (list 5 9999)))
(check-expect (infect waterloo 10)
              (list (list 0 10) (list 1 10) (list 2 10)
                    (list 3 10) (list 4 10) (list 5 10)))

(check-expect (infect repdiamo 0.9)
              (list (list 0 0.9) (list 1 0.9) (list 2 0.9)
                    (list 3 0.9) (list 4 0.9) (list 5 0.9)))
(check-expect (infect repdiamo 9721)
              (list (list 0 9721) (list 1 9721) (list 2 9721)
                    (list 3 9721) (list 4 9721) (list 5 9721)))

(check-expect (infect findiamo 7)
              (list (list 0 7) (list 1 7) (list 2 7) (list 3 7)
                    (list 4 7) (list 5 7) (list 6 7)))
(check-expect (infect findiamo -1)
              (list (list 0 -1) (list 1 -1) (list 2 -1) (list 3 -1)
                    (list 4 -1) (list 5 -1) (list 6 -1)))


