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

(define sinkrate 0.05)

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



;; =================================
;;
;; Problem 2 [Sink]
;;
;; =================================



;; (sink horde) Produces the result of one night and the amount of 
;;  zombies that sunk into the ground from a given horde (horde)
;; Examples:
(check-expect (sink (infect waterloo 1000))
          (list 300 (list (list 0 950) (list 1 950) (list 2 950)
                          (list 3 950) (list 4 950) (list 5 950))))

(check-expect (sink (infect waterloo 0))
          (list 0 (list (list 0 0) (list 1 0) (list 2 0)
                        (list 3 0) (list 4 0) (list 5 0))))

(check-expect (sink (infect fourOfour 500))
              empty)

;; sink: Horde -> (list Nat Horde)
(define (sink horde)
  
  (local [
          
     ;; (get-sunk zombies) produces 5% of the zombie population
     ;;  (zombies) rounded using the built in method

     ;; get-sunk: Nat -> Nat
     (define (get-sunk zombies)
          (round (* sinkrate zombies)))

     ;; (get-sunk-count horde) produces the total amount of
     ;;  sunk zombies from a horde, using the get-sunk

     ;; get-sunk: Horde -> Nat
     (define (get-sunk-count horde)
          (cond [(empty? horde)                       0]
                [else (+ (get-sunk (second (first horde)))
                         (get-sunk-count (rest horde)))]))

     ;; (update-loc horde) Updates each location by getting
     ;;  rid of the 5% of zombies that died at each location

     ;; update-loc: Horde -> Horde
     (define (update-loc horde)
          (cond [(empty? horde)                   empty]
                [else (append (list (list
                               (first (first horde))
                               (- (second (first horde))
                                  (get-sunk (second (first horde))))))
                              (update-loc (rest horde)))]))]
                     
    (cond [(empty? horde)           empty]
          [else (list (get-sunk-count horde)
                      (update-loc horde))])))
           
                                            


;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===
(check-expect (sink (infect fourOfour 0))
              empty)
(check-expect (sink (infect fourOfour 10))
              empty)

;; === Small Scale Tests ===

(check-expect (sink (infect uwp_home 101))
      (list 5 (list (list 0 96))))
(check-expect (sink (infect uwp_home 4815162342))
      (list 240758117 (list (list 0 4574404225))))

(check-expect (sink (infect g_washon 1))
      (list 0 (list (list 0 1) (list 1 1))))
(check-expect (sink (infect g_washon 20))
      (list 2 (list (list 0 19) (list 1 19))))

(check-expect (sink (infect huisclos 3411))
      (list 513 (list (list 0 3240) (list 1 3240) (list 2 3240))))
(check-expect (sink (infect huisclos 3412))
      (list 513 (list (list 0 3241) (list 1 3241) (list 2 3241))))

;; === Large Scale Tests ===

(check-expect (sink (infect waterloo 9999))
          (list 3000 (list (list 0 9499) (list 1 9499) (list 2 9499)
                          (list 3 9499) (list 4 9499) (list 5 9499))))
(check-expect (sink (infect waterloo 10))
          (list 0 (list (list 0 10) (list 1 10) (list 2 10)
                        (list 3 10) (list 4 10) (list 5 10))))

(check-expect (sink (infect repdiamo 21))
          (list 6 (list (list 0 20) (list 1 20) (list 2 20)
                          (list 3 20) (list 4 20) (list 5 20))))
(check-expect (sink (infect repdiamo 9721))
          (list 2916 (list (list 0 9235) (list 1 9235) (list 2 9235)
                          (list 3 9235) (list 4 9235) (list 5 9235))))

(check-expect (sink (infect findiamo 7))
          (list 0 (list (list 0 7) (list 1 7) (list 2 7) (list 3 7)
                    (list 4 7) (list 5 7) (list 6 7))))
(check-expect (sink (infect findiamo 2))
          (list 0 (list (list 0 2) (list 1 2) (list 2 2) (list 3 2)
                    (list 4 2) (list 5 2) (list 6 2))))



;; =================================
;;
;; Problem 3 [apportion]
;;
;; =================================



;; (apportion zombies n) Produces a number of splits (n) of a zombie 
;;  number such that theses numbers add up to the total (zombies) and
;;  the difference between any two numbers is less then 1
;; Examples:
(check-expect (apportion 100 3) (list 34 33 33))
(check-expect (apportion 1 3) (list 1 0 0))
(check-expect (apportion 2 1) (list 2))


;; apportion: Nat Nat -> (listof Nat)
(define (apportion zombies n)
  
  (local [

     ;; (apportion zombie_total index) Given the total amount
     ;;  of zombies and the indexs avaible will split the zombies
     ;;  into equal groups.

     ;; apportion: Nat Nat -> (listof Nat)     
     (define (get-divis zombie_total index)
          (cond [(zero? index) empty]
                [(= (remainder zombie_total n) 0)
                    (append (list (/ zombie_total n))
                            (get-divis zombie_total (sub1 index)))]
                [else
                    (append (list (+ (floor (/ zombie_total n)) 1))
                            (get-divis (sub1 zombie_total)
                                       (sub1 index)))]))]
                     
    (get-divis zombies n)))
           
                                            


;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===
(check-expect (apportion 10 0) empty)
(check-expect (apportion 200 0) empty)

;; === General Tests ===

(check-expect (apportion 2 1) '(2))
(check-expect (apportion 5 1) '(5))

(check-expect (apportion 2 2) '(1 1))
(check-expect (apportion 5 2) '(3 2))

(check-expect (apportion 2 3) '(1 1 0))
(check-expect (apportion 5 3) '(2 2 1))

;; === Large Scale Tests ===

(check-expect (apportion 50 3) '(17 17 16))
(check-expect (apportion 100 3) '(34 33 33))

(check-expect (apportion 50 4) '(13 13 12 12))
(check-expect (apportion 100 4) '(25 25 25 25))

(check-expect (apportion 77 5) '(16 16 15 15 15))
(check-expect (apportion 91 6) '(16 15 15 15 15 15))


