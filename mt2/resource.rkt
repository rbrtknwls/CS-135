;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname resource) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm 02, Problem 6(A-D)
;; ******************************************


;; ==================================
;; struct and data definitions For Q6
;; =================================


;; A Resource is (anyof 'fire 'wood 'water)

(define-struct card (type power))
;; A Card is a (make-card Resource Nat)

;; An Outcome is (anyof 'player-1 'player-2 'tie-game)


;; =================================
;; Testing Suite 6
;; =================================


;; ======= General Tests ==========
(define testcards
  (list (make-card 'fire 10)
        (make-card 'wood 23)
        (make-card 'water 3)
        (make-card 'fire 20)))

(define testcards1
  (list (make-card 'fire 10)))

(define testcards2
  (list (make-card 'fire 10)
        (make-card 'water 23)))

(define testcards3
  (list (make-card 'fire 4)
        (make-card 'fire 23)))

(define testcards4
  (list (make-card 'fire 1)
        (make-card 'fire 2)
        (make-card 'fire 3)))

(define testcards5
  (list (make-card 'wood 1)
        (make-card 'fire 2)
        (make-card 'water 3)))


;; =================================
;;
;; Question 6A
;;
;; =================================



;; (beats? res1 res2) Consumes 2 resources (res1 res2) 
;;   and produces if the first resourcse beats the second
;; Examples:

(check-expect (beats? 'fire 'water) false)
(check-expect (beats? 'wood 'water) true)

;; beats?: Resource Resource -> Bool
;; Requires: res1 is not the same as res2
;;           and both are non empty
(define (beats? res1 res2)
  (cond [(symbol=? 'fire res1)  (symbol=? 'wood res2)]
        [(symbol=? 'water res1) (symbol=? 'fire res2)]
        [else                  (symbol=? 'water res2)]))


;; General Tests
(check-expect (beats? 'water 'fire) true)
(check-expect (beats? 'water 'wood) false)
(check-expect (beats? 'wood 'water) true)
(check-expect (beats? 'wood 'fire)  false)
(check-expect (beats? 'fire 'wood)  true)
(check-expect (beats? 'fire 'water) false)



;; =================================
;;
;; Question 6B
;;
;; =================================



;; (winner card1 card2) Consumes 2 cards (card1 card2),
;;   and produces the outcome of the two cards card1 card2
;; Examples:

(check-expect (winner (make-card 'wood 8)
                      (make-card 'fire 1)) 'player-2)
(check-expect (winner (make-card 'wood 8)
                      (make-card 'wood 3)) 'player-1)

;; winner: Card Card -> Outcome
(define (winner card1 card2)
  (cond [(and (symbol=? 'fire (card-type card1))
              (symbol=? 'wood (card-type card2)))
                                        'player-1]
        [(and (symbol=? 'water (card-type card1))
              (symbol=? 'fire (card-type card2)))
                                        'player-1]
        [(and (symbol=? 'wood (card-type card1))
              (symbol=? 'water (card-type card2)))
                                        'player-1]
        [(and (symbol=? (card-type card1) (card-type card2))
              (>      (card-power card1) (card-power card2)))
                                        'player-1]
        [(and (symbol=? (card-type card1) (card-type card2))
              (=      (card-power card1) (card-power card2)))
                                        'tie-game]
        [else                           'player-2]))


;; General Tests
(check-expect (winner  (make-card 'water 2)
                       (make-card 'fire 2))  'player-1)
(check-expect (winner  (make-card 'water 2)
                       (make-card 'wood 2))  'player-2)
(check-expect (winner  (make-card 'water 2)
                       (make-card 'water 1)) 'player-1)
(check-expect (winner  (make-card 'water 2)
                       (make-card 'water 2)) 'tie-game)
(check-expect (winner  (make-card 'water 2)
                       (make-card 'water 3)) 'player-2)

(check-expect (winner  (make-card 'wood 20)
                       (make-card 'water 20)) 'player-1)
(check-expect (winner  (make-card 'wood 20)
                       (make-card 'fire 20))  'player-2)
(check-expect (winner  (make-card 'fire 20)
                       (make-card 'fire 5))   'player-1)
(check-expect (winner  (make-card 'fire 20)
                       (make-card 'fire 20))  'tie-game)
(check-expect (winner  (make-card 'fire 20) 
                       (make-card 'fire 60))  'player-2)

(check-expect (winner  (make-card 'fire 4)
                       (make-card 'wood 4))  'player-1)
(check-expect (winner  (make-card 'fire 4)
                       (make-card 'water 4)) 'player-2)
(check-expect (winner  (make-card 'fire 4)
                       (make-card 'fire 0))  'player-1)
(check-expect (winner  (make-card 'fire 4)
                       (make-card 'fire 4))  'tie-game)
(check-expect (winner  (make-card 'fire 4) 
                       (make-card 'fire 9))  'player-2)



;; =================================
;;
;; Question 6C
;;
;; =================================



;; (winnertable loc) Consumes a list of Cards (loc) 
;;   and produces a 2d table which represents who wins
;;   in each match up against each other
;; Examples:

(check-expect (winner-table empty) empty)
(check-expect (winner-table testcards) (list
                 (list 'tie-game 'player-1 'player-2 'player-2)
                 (list 'player-2 'tie-game 'player-1 'player-2)
                 (list 'player-1 'player-2 'tie-game 'player-1)
                 (list 'player-1 'player-1 'player-2 'tie-game)))

;; winnertable: (listof Card) -> (listof (listof Outcome))
(define (winner-table loc)
  (cond [(empty? loc)         empty]
        [else
               (winner-2d loc loc)]))


;; ===  Helper Functions  ===


;; (winner-2d loc1 loc2) Consumes 2 lists of Cards (loc) 
;;   produces a 2d table which represents who wins
;;   in each match up against each other
;; Examples:

(check-expect (winner-2d testcards testcards) (list
                 (list 'tie-game 'player-1 'player-2 'player-2)
                 (list 'player-2 'tie-game 'player-1 'player-2)
                 (list 'player-1 'player-2 'tie-game 'player-1)
                 (list 'player-1 'player-1 'player-2 'tie-game)))

;; winner-2d: (listof Card) (listof Card) -> (listof (listof Outcome))
(define (winner-2d loc1 loc2)
  (cond [(empty? loc2)         empty]
        [else
              (cons (winner-1d loc1 (first loc2))
                    (winner-2d loc1 (rest loc2)))]))


;; (winner-1d loc card) Consumes a lists of Card (loc) 
;;   and a Card (card) produces a list which shows the
;;   match up of all the cards
;; Examples:

(check-expect (winner-1d testcards (first testcards))
                 (list 'tie-game 'player-1 'player-2 'player-2))

;; winner-1d: (listof Card) Card -> (listof Outcome)
(define (winner-1d loc card)
  (cond [(empty? loc)         empty]
        [else
              (cons (winner    card (first loc))
                    (winner-1d (rest loc) card))]))
;; Empty Tests
(check-expect (winner-table empty) empty)

;; General Tests
(check-expect (winner-table testcards1)
              (list (list 'tie-game)))
(check-expect (winner-table testcards2)
              (list (list 'tie-game 'player-2)
                    (list 'player-1 'tie-game)))
(check-expect (winner-table testcards3)
              (list (list 'tie-game 'player-2)
                    (list 'player-1 'tie-game)))
(check-expect (winner-table testcards4)
              (list (list 'tie-game 'player-2 'player-2)
                    (list 'player-1 'tie-game 'player-2)
                    (list 'player-1 'player-1 'tie-game)))
(check-expect (winner-table testcards5)
              (list (list 'tie-game 'player-2 'player-1)
                    (list 'player-1 'tie-game 'player-2)
                    (list 'player-2 'player-1 'tie-game)))



;; =================================
;;
;; Question 6D
;;
;; =================================



;; (cheat loc) Consumes a list of Cards (loc) 
;;   and produces a new list of cards where if
;;   a card has a power below or equal to 5
;;   its power is increased by 2
;; Examples:

(check-expect (cheat empty) empty)
(check-expect (cheat testcards) (list
                 (make-card 'fire 10)
                 (make-card 'wood 23)
                 (make-card 'water 5)
                 (make-card 'fire 20)))

;; cheat: (listof Card) -> (listof Card)
(define (cheat loc)
  (cond [(empty? loc)         empty]
        [(<= (card-power (first loc)) 5)
             (cons (make-card (card-type (first loc))
                              (+ 2 (card-power (first loc))))
                   (cheat (rest loc)))]
        [else (cons (first loc)
                    (cheat (rest loc)))]))
;; Empty Tests
(check-expect (cheat empty) empty)

;; General Tests
(check-expect (cheat testcards1)
              (list (make-card 'fire 10)))
(check-expect (cheat testcards2)
              (list (make-card 'fire 10)
                    (make-card 'water 23)))
(check-expect (cheat testcards3)
              (list (make-card 'fire 6)
                    (make-card 'fire 23)))
(check-expect (cheat testcards4)
              (list (make-card 'fire 3)
                    (make-card 'fire 4)
                    (make-card 'fire 5)))
(check-expect (cheat testcards5)
              (list (make-card 'wood 3)
                    (make-card 'fire 4)
                    (make-card 'water 5)))

