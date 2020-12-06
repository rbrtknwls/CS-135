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


(define waterloo '((0 (1 2 3)) ;; MALL 1/2
                   (1 (2 3))   ;; PARK 1/3 + 1
                   (2 (0 4))   ;; MARKET 1/2 + 1/2
                   (3 (1))     ;; WLU 1/2 + 1/3 + 1
                   (4 (5))     ;; UW yayyy 1/2
                   (5 (3))))   ;; UPtown 1

(define repdiamo '((0 (1 2))   ;; Cong 1 1 + 1
                   (1 (3))     ;; Route 1a 1/2
                   (2 (3))     ;; Route 1b 1/2
                   (3 (4 5))   ;; Cong 2 1 + 1
                   (4 (0))     ;; Route 2a 1/2
                   (5 (0))))   ;; Route 2b 1/2

(define findiamo '((0 (1 2))   ;; Cong 1 0
                   (1 (3))     ;; Route 1a 1/2
                   (2 (3))     ;; Route 1b 1/2
                   (3 (4 5))   ;; Cong 2 1 + 1
                   (4 (6))     ;; Route 2a 1/2
                   (5 (6))     ;; Route 2b 1/2
                   (6 (6))))    ;; Cong 3 (Terminus) 1+1


;; Metaphorical Tests

(define fourOfour '())   ;; Empty Town

(define uwp_home '((0 (0))))    ;; Just my Room 1

(define g_washon '((0 (1))     ;; Reference to Grover Washington 1
                   (1 (0))))   ;; famous "Just the two of us" 1
  
(define huisclos '((0 (1 2))   ;; Joseph Garcin 1 + 1
                   (1 (0))     ;; Inez Serrano 1/2
                   (2 (0))))   ;; Estelle Rigault 1/2




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

     ;; (get-divis zombie_total index) Given the total amount
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

(check-expect (apportion 50 3) '(17 17 16))
(check-expect (apportion 100 3) '(34 33 33))

(check-expect (apportion 50 4) '(13 13 12 12))
(check-expect (apportion 100 4) '(25 25 25 25))

(check-expect (apportion 77 5) '(16 16 15 15 15))
(check-expect (apportion 91 6) '(16 15 15 15 15 15))



;; =================================
;;
;; ADDITIONAL TESTS
;;
;; =================================


(define waterloo_h1 (second (sink (infect waterloo 1000))))
(define waterloo_h2 (second (sink (infect waterloo 105))))

(define repdiamo_h1 (second (sink (infect repdiamo 100))))
(define repdiamo_h2 (second (sink (infect repdiamo 21))))

(define findiamo_h1 (second (sink (infect findiamo 561))))
(define findiamo_h2 (second (sink (infect findiamo 1))))

(define fourOfour_h1 (sink (infect fourOfour 5125125)))
(define fourOfour_h2 (sink (infect fourOfour 3141)))

(define uwp_home_h1 (second (sink (infect uwp_home 889))))
(define uwp_home_h2 (second (sink (infect uwp_home 3))))

(define g_washon_h1 (second (sink (infect g_washon 777))))
(define g_washon_h2 (second (sink (infect g_washon 555))))

(define huisclos_h1 (second (sink (infect huisclos 321))))
(define huisclos_h2 (second (sink (infect huisclos 999))))


;; =================================
;;
;; Problem 4 [shamble]
;;
;; =================================



;; (shamble town horde) Produces the horde of zombies splitting up 
;;  and moving to each corrisponding mapped location in the town.
;; Examples:

(check-expect (shamble waterloo waterloo_h1)
              (list (list 1 1267) (list 2 792) (list 3 1741)
                    (list 0 475) (list 4 475) (list 5 950)))
(check-expect (shamble waterloo waterloo_h2)
              (list (list 1 134) (list 2 83) (list 3 183)
                    (list 0 50) (list 4 50) (list 5 100)))
(check-expect (shamble fourOfour fourOfour_h1)
              empty)


;; shamble: Town Horde -> Horde
(define (shamble town horde)                     
  (merge-sort
   (append (foldr (lambda (x y rorr)
                    (append (map (lambda (x y) (list x y))
                                 (second y)
                                 (apportion (second x)
                                            (length (second y))))
                            rorr))
                  empty horde town)
           (map (lambda (x) (list (first x) 0)) town))))
           
                                            
;; =================================
;; Helper Functions
;; =================================

;; (merge-sort results) Given a horde with repeating elements
;;  creates a set of the horde with each elements sum of
;;  zombies
;; Examples:

(check-expect (merge-sort (list (list 0 1) (list 0 2)))
              (list (list 0 3)))
(check-expect (merge-sort (list (list 0 1) (list 1 2) (list 1 5)))
              (list (list 0 1) (list 1 7)))


;; merge-sort: Horde -> Horde
(define (merge-sort results)
  (cond [(empty? results) empty]
        [else
         
         ;; (singleresult) Gets the result of a single
         ;;  town being complied using merge-sort, this
         ;;  is used so it only runs once

         ;; singleresult: (list Horde Horde)
         (local [(define singleresult
                   (merge (first results)
                          (rest results)
                          empty))]
                          
           (append (list (first singleresult))
                   (merge-sort (second singleresult))))]))


;; (merge curr_town compare_town unused_town) Given a town (curr_town)
;;  will take in a list of town (compare_town) and if one of the towns
;;  is Location it will add its zombie to curr_town else it will be 
;;  added to unused_town
;; Examples:

(check-expect (merge (list 0 1) (list (list 0 1) (list 0 2)) empty)
              (list (list 0 4) empty))
(check-expect (merge (list 0 1)
                          (list (list 0 5) (list 1 2) (list 1 5))
                          empty)
              (list (list 0 6) (list (list 1 2) (list 1 5))))


;; merge-sort: Horde Horde Horde -> (list Horde Horde)
(define (merge curr_town compare_town unused_town)
  (cond [(empty? compare_town) (list curr_town unused_town)]
        [(= (first curr_town) (first (first compare_town)))
         (merge (list (first curr_town)
                      (+ (second curr_town)
                         (second (first compare_town))))
                (rest compare_town)
                unused_town)]
        [else
         (merge curr_town
                (rest compare_town)
                (append unused_town
                        (list (first compare_town))))]))


;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===
(check-expect (shamble fourOfour fourOfour_h1)
              empty)
(check-expect (shamble fourOfour fourOfour_h2)
              empty)

;; === Small Scale Tests ===

(check-expect (shamble uwp_home uwp_home_h1)
              (list (list 0 845)))
(check-expect (shamble uwp_home uwp_home_h2)
              (list (list 0 3)))

(check-expect (shamble g_washon g_washon_h1)
              (list (list 1 738) (list 0 738)))
(check-expect (shamble g_washon g_washon_h2)
              (list (list 1 527) (list 0 527)))

(check-expect (shamble huisclos huisclos_h1)
              (list (list 1 153) (list 2 152) (list 0 610)))
(check-expect (shamble huisclos huisclos_h2)
              (list (list 1 475) (list 2 474) (list 0 1898)))

;; === Large Scale Tests ===

(check-expect (shamble waterloo waterloo_h1)
              (list (list 1 1267) (list 2 792) (list 3 1741)
                    (list 0 475) (list 4 475) (list 5 950)))
(check-expect (shamble waterloo waterloo_h2)
              (list (list 1 134) (list 2 83) (list 3 183)
                    (list 0 50) (list 4 50) (list 5 100)))

(check-expect (shamble repdiamo repdiamo_h1)
              (list (list 1 48) (list 2 47) (list 3 190)
                    (list 4 48) (list 5 47) (list 0 190)))
(check-expect (shamble repdiamo repdiamo_h2)
              (list (list 1 10) (list 2 10) (list 3 40)
                    (list 4 10) (list 5 10) (list 0 40)))

(check-expect (shamble findiamo findiamo_h1)
              (list (list 1 267) (list 2 266) (list 3 1066)
                    (list 4 267) (list 5 266) (list 6 1599)
                    (list 0 0)))
(check-expect (shamble findiamo findiamo_h2)
              (list (list 1 1) (list 2 0) (list 3 2)
                    (list 4 1) (list 5 0) (list 6 3) (list 0 0)))



;; =================================
;;
;; Problem 5 [rise]
;;
;; =================================



;; (rise zombies horde) Produces a new horde where each element 
;;  in the origonal horde is added to by some division of the
;;  zombies
;; Examples:

(check-expect (rise 300 waterloo_h1)
              (list (list 0 1000) (list 1 1000) (list 2 1000)
                    (list 3 1000) (list 4 1000) (list 5 1000)))
(check-expect (rise 300 (shamble waterloo waterloo_h1))
              (list (list 1 1317) (list 2 842) (list 3 1791)
                    (list 0 525) (list 4 525) (list 5 1000)))
(check-expect (rise 20 (shamble fourOfour fourOfour_h1))
              empty)

;; rise: Nat Horde -> Horde
(define (rise zombies horde)                     
  (merge-sort
   (append horde
           (map (lambda (x y) (list (first x) y))
                horde
                (apportion zombies (length horde))))))
           
                                           


;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===
(check-expect (rise 300 fourOfour_h1)
              empty)
(check-expect (rise 0 fourOfour_h2)
              empty)

;; === Small Scale Tests ===

(check-expect (rise 4 uwp_home_h1)
              (list (list 0 849)))
(check-expect (rise 0 uwp_home_h2)
              (list (list 0 3)))

(check-expect (rise 354 g_washon_h1)
              (list (list 0 915) (list 1 915)))
(check-expect (rise 3 g_washon_h2)
              (list (list 0 529) (list 1 528)))

(check-expect (rise 8 huisclos_h1)
              (list (list 0 308) (list 1 308) (list 2 307)))
(check-expect (rise 2 huisclos_h2)
              (list (list 0 950) (list 1 950) (list 2 949)))

;; === Large Scale Tests ===

(check-expect (rise 600 waterloo_h1)
              (list (list 0 1050) (list 1 1050) (list 2 1050)
                    (list 3 1050) (list 4 1050) (list 5 1050)))
(check-expect (rise 700 waterloo_h2)
              (list (list 0 217) (list 1 217) (list 2 217)
                    (list 3 217) (list 4 216) (list 5 216)))

(check-expect (rise 800 repdiamo_h1)
              (list (list 0 229) (list 1 229) (list 2 228)
                    (list 3 228) (list 4 228) (list 5 228)))
(check-expect (rise 900 repdiamo_h2)
              (list (list 0 170) (list 1 170) (list 2 170)
                    (list 3 170) (list 4 170) (list 5 170)))

(check-expect (rise 10 findiamo_h1)
              (list (list 0 535) (list 1 535) (list 2 535)
                    (list 3 534) (list 4 534) (list 5 534)
                    (list 6 534)))
(check-expect (rise 14 findiamo_h2)
              (list (list 0 3) (list 1 3) (list 2 3)
                    (list 3 3) (list 4 3) (list 5 3) (list 6 3)))



;; =================================
;;
;; Problem 6 [night]
;;
;; =================================



;; (night town horde) Produces the change in zombies in
;; a given town and horde given a night passing
;; Examples:

(check-expect (night waterloo (infect waterloo 1000))
              (list (list 1 1317) (list 2 842) (list 3 1791)
                    (list 0 525) (list 4 525) (list 5 1000)))
(check-expect (night waterloo waterloo_h1)
              (list (list 1 1251) (list 2 800) (list 3 1701)
                    (list 0 499) (list 4 499) (list 5 950)))
(check-expect (night fourOfour fourOfour_h1)
              empty)

;; night: Town Horde -> Horde
(define (night town horde)
  (local [(define sunk (sink horde))]
    (cond [(empty? sunk) empty]
          [else (rise (first sunk) (shamble town (second sunk)))])))
  
           
                                           


;; =================================
;; Testing Suite
;; =================================


;; === Empty Tests ===
(check-expect (night fourOfour fourOfour_h1)
              empty)
(check-expect (night fourOfour fourOfour_h2)
              empty)

;; === Small Scale Tests ===

(check-expect (night uwp_home uwp_home_h1)
              (list (list 0 845)))
(check-expect (night uwp_home uwp_home_h2)
              (list (list 0 3)))

(check-expect (night g_washon g_washon_h1)
              (list (list 1 738) (list 0 738)))
(check-expect (night g_washon g_washon_h2)
              (list (list 1 527) (list 0 527)))

(check-expect (night huisclos huisclos_h1)
              (list (list 1 160) (list 2 160) (list 0 595)))
(check-expect (night huisclos huisclos_h2)
              (list (list 1 498) (list 2 498) (list 0 1851)))

;; === Large Scale Tests ===

(check-expect (night waterloo waterloo_h1)
              (list (list 1 1251) (list 2 800) (list 3 1701)
                    (list 0 499) (list 4 499) (list 5 950)))
(check-expect (night waterloo waterloo_h2)
              (list (list 1 132) (list 2 85) (list 3 178)
                    (list 0 53) (list 4 52) (list 5 100)))

(check-expect (night repdiamo repdiamo_h1)
              (list (list 1 50) (list 2 50) (list 3 185)
                    (list 4 50) (list 5 50) (list 0 190)))
(check-expect (night repdiamo repdiamo_h2)
              (list (list 1 11) (list 2 10) (list 3 39)
                    (list 4 11) (list 5 10) (list 0 39)))

(check-expect (night findiamo findiamo_h1)
              (list (list 1 280) (list 2 280) (list 3 1039)
                    (list 4 280) (list 5 280) (list 6 1545)
                    (list 0 27)))
(check-expect (night findiamo findiamo_h2)
              (list (list 1 1) (list 2 0) (list 3 2)
                    (list 4 1) (list 5 0) (list 6 3) (list 0 0)))
