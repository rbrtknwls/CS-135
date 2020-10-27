;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname duck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm-1, Problem 3 (A-C)
;; ******************************************

;; ** Test Cases **

;; 3A Test Cases
(define 1Atest1 50)
(define 1Asoul1 'small)

(define 1Atest2 499)
(define 1Asoul2 'small)

(define 1Atest3 500)
(define 1Asoul3 'medium)

(define 1Atest4 501)
(define 1Asoul4 'medium)

(define 1Atest5 1000)
(define 1Asoul5 'medium)

(define 1Atest6 5000)
(define 1Asoul6 'medium)

(define 1Atest7 9999)
(define 1Asoul7 'medium)

(define 1Atest8 10000)
(define 1Asoul8 'medium)

(define 1Atest9 10001)
(define 1Asoul9 'large)

(define 1Atest10 27773)
(define 1Asoul10 'large)


;;
;;   Problem 3A
;;

;; (animal-size weight) Produces the weight class
;;   of an animal based on its natural number weight
;; Examples:
(check-expect (animal-size 500) 'medium)
(check-expect (animal-size 200000) 'large)

;; animal-size: Nat -> Symb
;;   Requires: weight >= 0
(define (animal-size weight)
  (cond [(> weight 10000) 'large]
        [(< weight 500)   'small]
        [else             'medium]))

;; General Tests 
(check-expect (animal-size 1Atest1) 1Asoul1)
(check-expect (animal-size 1Atest2) 1Asoul2)
(check-expect (animal-size 1Atest3) 1Asoul3)
(check-expect (animal-size 1Atest4) 1Asoul4)
(check-expect (animal-size 1Atest5) 1Asoul5)
(check-expect (animal-size 1Atest6) 1Asoul6)
(check-expect (animal-size 1Atest7) 1Asoul7)
(check-expect (animal-size 1Atest8) 1Asoul8)
(check-expect (animal-size 1Atest9) 1Asoul9)
(check-expect (animal-size 1Atest10) 1Asoul10)

;;
;;   Problem 3B
;;

;; (goose? swims? flies? angry? weight) Produces
;;  if an animal that does or does not swim (swims?),
;;  fly (flies), is angry (angry?) and has a weight
;;  (weight) fits in the classification of a goose.

;; Examples:
(check-expect (goose? true true true 500) false)
(check-expect (goose? true true true 200000) true)

;; goose?: Bool Bool Bool Nat -> Bool
;;   Requires: weight >= 0
(define (goose? swims? flies? angry? weight)
  (and swims? flies? angry?
       (symbol=? (animal-size weight) 'large)))
;; Weight Dependant
(check-expect (goose? true true true 2) false)
(check-expect (goose? true true true 10001) true)
(check-expect (goose? true true true 10000) false)
(check-expect (goose? true true true 30000) true)
(check-expect (goose? true true true 501) false)

;; Swim? Dependant
(check-expect (goose? true true true 30000) true)
(check-expect (goose? false true true 30000) false)

;; Flies? Dependant
(check-expect (goose? true true true 30000) true)
(check-expect (goose? true false true 30000) false)

;; Angry? Dependant
(check-expect (goose? true true true 30000) true)
(check-expect (goose? true true false 30000) false)


;;
;;   Problem 3C
;;

;; (what-animal swims? flies? angry? weight) Produces
;;  the name of animal that does or does not swim (swims?),
;;  fly (flies), is angry (angry?) and has a weight
;;  (weight).

;; Examples:
(check-expect (what-animal true true true 10001) 'goose)
(check-expect (what-animal true true true 10000) 'gull)

;; what-animal: Bool Bool Bool Nat -> Symb
;;   Requires: weight >= 0
(define (what-animal swims? flies? angry? weight)
  (cond [swims?
         
         ;; Animals That Swim
         ;;(Duck, Goose, Gull, Pengiun)
         (cond [flies?
                
                ;; Animals That Fly/Swim
                ;; (Duck, Goose, Gull)
                (cond [angry?
                       
                       ;; Animals That Fly/Swim/Angry
                       ;; (Goose, Gull)
                       (cond [(symbol=? (animal-size weight)
                                        'large)      'goose]
                             [else                   'gull])]
                      
                       ;; Animals that Fly/Swim/Not Angry
                       ;; (Duck)
                      [else                          'duck])]
               
               ;; Animals That Fly/Not Swim
               ;;  (Penguin)
               [else                                 'penguin])]
         
        [else
         
         ;; Animals That Dont Swim
         ;;(Crow, Sparrow, Squirrel, Emu)
         (cond [flies?
                
                ;; Animals That Dont Swim/But Fly
                ;;(Crow, Sparrow)
                (cond [(symbol=? (animal-size weight)
                                 'small)             'sparrow]
                      [else                          'crow])]
  
               [else

                ;; Animals That Dont Swim or Fly
                ;;(Squirrel, Emu)
                (cond [(symbol=? (animal-size weight)
                                 'small)             'squirrel]
                      [else                          'emu])])]))

;; Tests! (Every Combination Possible)

;; T, T, T
(check-expect (what-animal true true true 499) 'gull)
(check-expect (what-animal true true true 5000) 'gull)
(check-expect (what-animal true true true 10001) 'goose)

;; T, T, F
(check-expect (what-animal true true false 499) 'duck)
(check-expect (what-animal true true false 5000) 'duck)
(check-expect (what-animal true true false 10001) 'duck)

;; T, F, F
(check-expect (what-animal true false false 499) 'penguin)
(check-expect (what-animal true false false 5000) 'penguin)
(check-expect (what-animal true false false 10001) 'penguin)

;; F, T, T
(check-expect (what-animal false true true 499) 'sparrow)
(check-expect (what-animal false true true 5000) 'crow)
(check-expect (what-animal false true true 10001) 'crow)

;; F, T, F
(check-expect (what-animal false true false 499) 'sparrow)
(check-expect (what-animal false true false 5000) 'crow)
(check-expect (what-animal false true false 10001) 'crow)

;; F, F, F
(check-expect (what-animal false false false 499) 'squirrel)
(check-expect (what-animal false false false 5000) 'emu)
(check-expect (what-animal false false false 10001) 'emu)

                

               