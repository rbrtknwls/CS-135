;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname groceries) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 05, Problem 2(A-F)
;; ******************************************

;; ** Structs **


(define-struct grocery (dept name cost mass))
;; A Grocery is a (make-grocery Str Str Num Num)
;; Requires: cost >= 0, mass > 0.

;; A Store is a (listof Grocery)
;; Requires: no two items have both the same dept and same name.


(define-struct interval (lo hi))
;; An Interval is a (make-interval (anyof 'dontcare Num)
;;                                 (anyof 'dontcare Num))

;; A StrPatt is a (anyof Str 'dontcare)


(define-struct query (dept name cost mass))
;; A GroceryQuery is a
;;   (make-query StrPatt StrPatt Interval Interval)


;; ** Constants **

(define try-n-save
  (list (make-grocery "produce" "apple" 2.49 600)
        (make-grocery "seed" "rice" 0.95 1000)
        (make-grocery "dairy" "milk" 3.99 4000)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "potato" 2.99 5000)
        (make-grocery "chips" "potato" 1.99 250)
        (make-grocery "chips" "corn" 1.99 275)
        (make-grocery "seed" "wheat" 0.49 500)
        (make-grocery "produce" "banana" 0.69 450)
        (make-grocery "dairy" "cheese" 6.49 900)
        (make-grocery "chips" "banana" 1.99 50)
        (make-grocery "produce" "peach" 3.99 400)
        (make-grocery "seed" "lentil" 2.99 800)
        (make-grocery "produce" "corn" 0.99 100)
        (make-grocery "seed" "corn" 4.99 850)
        (make-grocery "dairy" "kefir" 5.99 1000)))

(define kwik-e-mart
  (list (make-grocery "seed" "rice" 0.38 400)
        (make-grocery "can" "corn" 4.00 400)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "apple" 2.99 400)
        (make-grocery "can" "creamed eels" 2.19 350)
        (make-grocery "produce" "pineapple" 3.17 250)))


;;
;;   Problem 2A
;;


(define student-shop
  (list (make-grocery "can" "pineapple" 1.2 25.5)
        (make-grocery "can" "apples" 7 900)
        (make-grocery "can" "mango" 0 230)
        (make-grocery "can" "lemon" 3.81 325)
        (make-grocery "bakery" "pizza" 2 1.25)
        (make-grocery "bakery" "bread" 1.99 7.25)
        (make-grocery "bakery" "pastery" 3.99 25)
        (make-grocery "bakery" "egg roll" 2.5 400)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "seed" "edamame" 1.97 540.25)
        (make-grocery "seed" "pea" 2.19 400)
        (make-grocery "seed" "oats" 3.27 500)))


;;
;;   Problem 2B
;;


;; (in-interval? canidate int) Consumes a canidate 
;;   and checks to see if it is bounded within the given
;;  interval (int)
;; Examples:
(check-expect (in-interval? 42
              (make-interval 'dontcare 'dontcare)) true)
(check-expect (in-interval? 34
              (make-interval 35 'dontcare)) false)
(check-expect (in-interval? 34
              (make-interval 'dontcare 35)) true)

;; in-interval?: Num Interval -> Bool
(define (in-interval? canidate int)
  (cond [(and (symbol? (interval-hi int))
              (symbol? (interval-lo int)))
                                                    true]
        [(symbol? (interval-hi int))
         
           (cond [(>= canidate (interval-lo int))
                                                    true]
                 [else                             false])]
                 
        [(symbol? (interval-lo int))
              
           (cond [(<= canidate (interval-hi int))
                                                    true]
                 [else                             false])]
                 
        [(and (>= canidate (interval-lo int))
              (<= canidate (interval-hi int)))
                                                    true]
        [else                                      false]))

;; Edge Tests
(check-expect (in-interval? 9
              (make-interval 9 'dontcare)) true)
(check-expect (in-interval? -2
              (make-interval -2 'dontcare)) true)
(check-expect (in-interval? 9
              (make-interval 'dontcare 9)) true)
(check-expect (in-interval? -2
              (make-interval 'dontcare -2)) true)

;; Bounded One Way Tests
(check-expect (in-interval? 21
              (make-interval 'dontcare 20)) false)
(check-expect (in-interval? 21
              (make-interval 20 'dontcare)) true)
(check-expect (in-interval? 0.21
              (make-interval 0.31 'dontcare)) false)
(check-expect (in-interval? -28
              (make-interval 'dontcare -29)) false)

;; Bounded Both Ways Tests
(check-expect (in-interval? 4
              (make-interval 3 5)) true)
(check-expect (in-interval? 3
              (make-interval 4 5)) false)
(check-expect (in-interval? 0.6
              (make-interval 0.5 0.7)) true)


;;
;;   Problem 2C
;;


;; (find-matches stre qury) Consumes a store (stre) and a 
;;   groceryquery (qury) and produces a new Store
;;   where each grocery is within the given the qeury
;; Examples:
(check-expect
  (find-matches try-n-save (make-query "seed" 'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)))
                     (list
                           (make-grocery "seed" "rice" 0.95 1000)
                           (make-grocery "seed" "pinto" 2.49 500)
                           (make-grocery "seed" "wheat" 0.49 500)
                           (make-grocery "seed" "lentil" 2.99 800)
                           (make-grocery "seed" "corn" 4.99 850)))

(check-expect
  (find-matches try-n-save (make-query 'dontcare "corn"
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)))
                    (list
                           (make-grocery "chips" "corn" 1.99 275)
                           (make-grocery "produce" "corn" 0.99 100)
                           (make-grocery "seed" "corn" 4.99 850)))

;; find-matches: Store GroceryQuery -> Store
(define (find-matches stre qury)
  (cond
         [(empty? stre)                                    empty]
         [(and
           
              ;; Department Check
              (or (symbol?  (query-dept qury))
                  (string=? (grocery-dept (first stre))
                            (query-dept qury)))
                   
              
              ;; Name Check
              (or (symbol?  (query-name qury))
                  (string=? (grocery-name (first stre))
                            (query-name qury)))
                   

              ;; Cost Check
              (in-interval? (grocery-cost (first stre))
                            (query-cost qury))

              ;; Mass Check
              (in-interval? (grocery-mass (first stre))
                            (query-mass qury)))
 
                            (cons (first stre)
                                  (find-matches (rest stre) qury))]
        [else                     (find-matches  (rest stre) qury)]))

;; Empty Tests
                 
(check-expect
  (find-matches student-shop (make-query 'dontcare "corn"
                              (make-interval 'dontcare 'dontcare)
                              (make-interval 'dontcare 'dontcare)))
                              empty)

(check-expect
  (find-matches try-n-save  (make-query 'dontcare "corn"
                             (make-interval 5 'dontcare)
                             (make-interval 'dontcare 'dontcare)))
                             empty)

(check-expect
  (find-matches try-n-save  (make-query "seed" 'dontcare
                             (make-interval 'dontcare 0.94)
                             (make-interval 600 'dontcare)))
                             empty)

;; General Tests

(check-expect
  (find-matches try-n-save  (make-query 'dontcare 'dontcare
                             (make-interval 'dontcare 'dontcare)
                             (make-interval 0 50)))
                (list (make-grocery "chips" "banana" 1.99 50)))

(check-expect
  (find-matches student-shop  (make-query 'dontcare 'dontcare
                             (make-interval 'dontcare 'dontcare)
                             (make-interval 0 50)))
                (list (make-grocery "can" "pineapple" 1.2 25.5)
                      (make-grocery "bakery" "pizza" 2 1.25)
                      (make-grocery "bakery" "bread" 1.99 7.25)
                      (make-grocery "bakery" "pastery" 3.99 25)))

(check-expect
  (find-matches try-n-save  (make-query "seed" 'dontcare
                             (make-interval 'dontcare 3.0)
                             (make-interval 600 'dontcare)))
                (list (make-grocery "seed" "rice" 0.95 1000)
                      (make-grocery "seed" "lentil" 2.99 800)))

(check-expect
  (find-matches try-n-save  (make-query "seed" 'dontcare
                             (make-interval 'dontcare 5.0)
                             (make-interval 600 'dontcare)))
                (list (make-grocery "seed" "rice" 0.95 1000)
                      (make-grocery "seed" "lentil" 2.99 800)
                      (make-grocery "seed" "corn" 4.99 850)))

(check-expect
  (find-matches kwik-e-mart  (make-query 'dontcare 'dontcare
                             (make-interval 3 5.0)
                             (make-interval 'dontcare 'dontcare)))
                (list (make-grocery "can" "corn" 4 400)
                      (make-grocery "produce" "pineapple" 3.17 250)))

(check-expect
  (find-matches kwik-e-mart  (make-query 'dontcare 'dontcare
                             (make-interval 0 5.0)
                             (make-interval 'dontcare 'dontcare)))
                (list (make-grocery "seed" "rice" 0.38 400)
                      (make-grocery "can" "corn" 4 400)
                      (make-grocery "seed" "pinto" 2.49 500)
                      (make-grocery "produce" "apple" 2.99 400)
                      (make-grocery "can" "creamed eels" 2.19 350)
                      (make-grocery "produce" "pineapple" 3.17 250)))

(check-expect
  (find-matches student-shop  (make-query 'dontcare 'dontcare
                             (make-interval 'dontcare 'dontcare)
                             (make-interval 0 50)))
                (list (make-grocery "can" "pineapple" 1.2 25.5)
                      (make-grocery "bakery" "pizza" 2 1.25)
                      (make-grocery "bakery" "bread" 1.99 7.25)
                      (make-grocery "bakery" "pastery" 3.99 25)))


;;
;;   Problem 2D
;;


;; (sort-dept-name stre) Consumes a store (stre) and produces
;;  a new store in which every grocery is sorted
;; Examples:
(check-expect (sort-dept-name try-n-save)
              (list
                    (make-grocery "chips" "banana" 1.99 50)
                    (make-grocery "chips" "corn" 1.99 275)
                    (make-grocery "chips" "potato" 1.99 250)
                    (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "dairy" "kefir" 5.99 1000)
                    (make-grocery "dairy" "milk" 3.99 4000)
                    (make-grocery "produce" "apple" 2.49 600)
                    (make-grocery "produce" "banana" 0.69 450)
                    (make-grocery "produce" "corn" 0.99 100)
                    (make-grocery "produce" "peach" 3.99 400)
                    (make-grocery "produce" "potato" 2.99 5000)
                    (make-grocery "seed" "corn" 4.99 850)
                    (make-grocery "seed" "lentil" 2.99 800)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "seed" "rice" 0.95 1000)
                    (make-grocery "seed" "wheat" 0.49 500)))

;; find-matches: Store -> Store
(define (sort-dept-name stre)
   (sort-deptstres stre empty))

;; ** Helper Functions **
                     
;; (sort-deptstres stre nstre) Consumes a store (stre) uses
;;  accululative recursion to produce a sorted list

;; Examples:
(check-expect (sort-deptstres try-n-save empty)
              (list (make-grocery "chips" "banana" 1.99 50)
                    (make-grocery "chips" "corn" 1.99 275)
                    (make-grocery "chips" "potato" 1.99 250)
                    (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "dairy" "kefir" 5.99 1000)
                    (make-grocery "dairy" "milk" 3.99 4000)
                    (make-grocery "produce" "apple" 2.49 600)
                    (make-grocery "produce" "banana" 0.69 450)
                    (make-grocery "produce" "corn" 0.99 100)
                    (make-grocery "produce" "peach" 3.99 400)
                    (make-grocery "produce" "potato" 2.99 5000)
                    (make-grocery "seed" "corn" 4.99 850)
                    (make-grocery "seed" "lentil" 2.99 800)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "seed" "rice" 0.95 1000)
                    (make-grocery "seed" "wheat" 0.49 500)))

;; sort-deptstres: Store Store -> Store
(define (sort-deptstres stre nstre)
   (cond [(or (empty? stre) (empty? (first stre)))          nstre]
         [else
           (sort-deptstres (rest stre)
                           (insert-elm-nstre nstre (first stre)))]))

;; (insert-elm-nstre stre elem) Consumes a sorted store (stre)
;;   and a an grocery (elem) and makes a new sorted store.
;;   This is a Insertion Sort!
;; Examples:
(check-expect (insert-elm-nstre
              (list (make-grocery "chips" "banana" 1.99 50)
                    (make-grocery "chips" "potato" 1.99 250))
                    (make-grocery "chips" "corn" 1.99 275))
                    
              (list (make-grocery "chips" "banana" 1.99 50)
                    (make-grocery "chips" "corn" 1.99 275)
                    (make-grocery "chips" "potato" 1.99 250)))

              
(check-expect (insert-elm-nstre
              (list (make-grocery "seed" "corn" 4.99 850)
                    (make-grocery "seed" "lentil" 2.99 800)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "seed" "rice" 0.95 1000))
                    (make-grocery "seed" "wheat" 0.49 500))
                    
              (list (make-grocery "seed" "corn" 4.99 850)
                    (make-grocery "seed" "lentil" 2.99 800)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "seed" "rice" 0.95 1000)
                    (make-grocery "seed" "wheat" 0.49 500)))
                    

;; insert-elm-nstre: Store Grocery -> Store
;; Requires:  stre is sorted
(define (insert-elm-nstre stre elem)
   (cond [(empty? stre)             (cons elem empty)]
         [(string>? (grocery-dept (first stre))
                     (grocery-dept elem))
                                      (cons elem stre)]
         [(string=? (grocery-dept (first stre))
                    (grocery-dept elem))       
            (cond [(string>=? (grocery-name (first stre))
                              (grocery-name elem))
                                      (cons elem stre)]
                  
                  [else (cons (first stre)
                              (insert-elm-nstre (rest stre)
                                                elem))])]
         
         [else
                 (cons (first stre) (insert-elm-nstre
                                    (rest stre) elem))]))

;; Empty Store Tests
(check-expect (sort-dept-name empty) empty)

(check-expect (sort-dept-name (list empty empty empty)) empty)

(check-expect (sort-dept-name (find-matches kwik-e-mart 
                              (make-query "AA" 'dontcare
                              (make-interval 'dontcare 'dontcare)
                              (make-interval 'dontcare 'dontcare))))
              empty)
             
;; Priority Tests

(check-expect (sort-dept-name
              (list (make-grocery "a" "a" 100 100)
                    (make-grocery "a" "a" 1 1)))
              (list (make-grocery "a" "a" 1 1)
                    (make-grocery "a" "a" 100 100)))

(check-expect (sort-dept-name
              (list (make-grocery "a" "b" 100 100)
                    (make-grocery "a" "a" 1 1)))
              (list (make-grocery "a" "a" 1 1)
                    (make-grocery "a" "b" 100 100)))
               
;; General Store Tests
(check-expect (sort-dept-name try-n-save)
              (list (make-grocery "chips" "banana" 1.99 50)
                    (make-grocery "chips" "corn" 1.99 275)
                    (make-grocery "chips" "potato" 1.99 250)
                    (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "dairy" "kefir" 5.99 1000)
                    (make-grocery "dairy" "milk" 3.99 4000)
                    (make-grocery "produce" "apple" 2.49 600)
                    (make-grocery "produce" "banana" 0.69 450)
                    (make-grocery "produce" "corn" 0.99 100)
                    (make-grocery "produce" "peach" 3.99 400)
                    (make-grocery "produce" "potato" 2.99 5000)
                    (make-grocery "seed" "corn" 4.99 850)
                    (make-grocery "seed" "lentil" 2.99 800)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "seed" "rice" 0.95 1000)
                    (make-grocery "seed" "wheat" 0.49 500)))

(check-expect (sort-dept-name kwik-e-mart)
              (list (make-grocery "can" "corn" 4 400)
                    (make-grocery "can" "creamed eels" 2.19 350)
                    (make-grocery "produce" "apple" 2.99 400)
                    (make-grocery "produce" "pineapple" 3.17 250)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "seed" "rice" 0.38 400)))

(check-expect (sort-dept-name student-shop)
              (list (make-grocery "bakery" "bread" 1.99 7.25)
                    (make-grocery "bakery" "egg roll" 2.5 400)
                    (make-grocery "bakery" "pastery" 3.99 25)
                    (make-grocery "bakery" "pizza" 2 1.25)
                    (make-grocery "can" "apples" 7 900)
                    (make-grocery "can" "lemon" 3.81 325)
                    (make-grocery "can" "mango" 0 230)
                    (make-grocery "can" "pineapple" 1.2 25.5)
                    (make-grocery "seed" "edamame" 1.97 540.25)
                    (make-grocery "seed" "oats" 3.27 500)
                    (make-grocery "seed" "pea" 2.19 400)
                    (make-grocery "seed" "pinto" 2.49 500)))


;;
;;   Problem 2E
;;

                                  
;; (overlap stre1 stre2) Consumes 2 stores (stre1 stre2)
;;  and produces a new store with groceries that are in
;;  both (only picks the cheaper or equally cheap but less
;;  massive one)
;; Examples:
(check-expect (overlap kwik-e-mart try-n-save)
              (list (make-grocery "produce" "apple" 2.49 600)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "seed" "rice" 0.38 400))) 


;; overlap: Store Store -> Store
(define (overlap stre1 stre2)
  (sorted-overlap (sort-dept-name stre1)
                  (sort-dept-name stre2)))


;; ** Helper Functions **


;; (sorted-overlap stre1 stre2) Consumes 2 stores (stre1 stre2)
;;  which are sorted and produces a new store with groceries
;;  that are in both (only picks the cheaper or equally cheap
;;  but less massive one)

;; Examples:
(check-expect (sorted-overlap (sort-dept-name kwik-e-mart)
                              (sort-dept-name try-n-save))
              (list (make-grocery "produce" "apple" 2.49 600)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "seed" "rice" 0.38 400)))

(check-expect (sorted-overlap (sort-dept-name student-shop)
                              (sort-dept-name try-n-save))
              (list (make-grocery "seed" "pinto" 2.49 500)))

(check-expect (sorted-overlap (sort-dept-name student-shop)
                              (sort-dept-name kwik-e-mart))
              (list (make-grocery "seed" "pinto" 2.49 500)))


;; sorted-overlap: Store Store -> Store

(define (sorted-overlap stre1 stre2)
   (cond [(or (empty? stre1) (empty? stre2))
                                                     empty]
         
         [(string>? (grocery-dept (first stre1))
                    (grocery-dept (first stre2)))
                       (sorted-overlap stre1 (rest stre2))]
         [(string<? (grocery-dept (first stre1))
                    (grocery-dept (first stre2)))
                       (sorted-overlap (rest stre1) stre2)]
         
         [(string>? (grocery-name (first stre1))
                    (grocery-name (first stre2)))
                       (sorted-overlap stre1 (rest stre2))]
         [(string<? (grocery-name (first stre1))
                    (grocery-name (first stre2)))
                       (sorted-overlap (rest stre1) stre2)]

         [(< (/ (grocery-cost (first stre1))
               (grocery-mass (first stre1)))
            (/ (grocery-cost (first stre2))
               (grocery-mass (first stre2))))
                 (cons (first stre1) (sorted-overlap (rest stre1)
                                            (rest stre2)))]
         [(> (/ (grocery-cost (first stre1))
               (grocery-mass (first stre1)))
            (/ (grocery-cost (first stre2))
               (grocery-mass (first stre2))))
                 (cons (first stre2) (sorted-overlap (rest stre1)
                                            (rest stre2)))]
         
         [(< (grocery-cost (first stre1))
             (grocery-cost (first stre2)))
                      (cons (first stre1) (sorted-overlap (rest stre1)
                                            (rest stre2)))]
         [else
                      (cons (first stre2) (sorted-overlap (rest stre1)
                                            (rest stre2)))]))

;; Empty List Tests

(check-expect (overlap empty empty) empty)

(check-expect (overlap (list empty empty empty empty)
                       (list empty)) empty)

;; Equal Cost Per Gram
(check-expect (overlap (list (make-grocery "can" "apple" 4 400))
                       (list (make-grocery "can" "apple" 8 800)))
                       (list (make-grocery "can" "apple" 4 400)))

(check-expect (overlap (list (make-grocery "can" "apple" 10 2000))
                       (list (make-grocery "can" "apple" 1 200)))
                       (list (make-grocery "can" "apple" 1 200)))
;; Mass Dependant Tests
(check-expect (overlap (list (make-grocery "can" "corn" 4 400))
                       (list (make-grocery "can" "corn" 4 300)))
                       (list (make-grocery "can" "corn" 4 400)))

(check-expect (overlap (list (make-grocery "can" "corn" 4 300))
                       (list (make-grocery "can" "corn" 4 400)))
                       (list (make-grocery "can" "corn" 4 400)))

(check-expect (overlap (list (make-grocery "can" "corn" 4 300)
                             (make-grocery "pizza" "cola" 2.99 25)
                             (make-grocery "can" "nut" 8 2))
                       (list (make-grocery "can" "corn" 4 400)
                             (make-grocery "water" "water" 0.75 400)
                             (make-grocery "can" "nut" 8 1)))
                       (list (make-grocery "can" "corn" 4 400)
                             (make-grocery "can" "nut" 8 2)))

;; General Tests
(check-expect (overlap (list (make-grocery "a" "1" 0.1 9)
                             (make-grocery "a" "2" 0.2 8)
                             (make-grocery "a" "3" 0.3 7)
                             (make-grocery "a" "4" 0.4 6)
                             (make-grocery "a" "5" 0.5 5)
                             (make-grocery "a" "6" 0.6 4)
                             (make-grocery "a" "7" 0.7 3))
                       (list (make-grocery "a" "1" 1 9)
                             (make-grocery "a" "2" 0.2 0.8)
                             (make-grocery "a" "3" 3 7)
                             (make-grocery "a" "4" 0.4 0.6)
                             (make-grocery "a" "5" 5 5)
                             (make-grocery "a" "6" 0.6 0.4)
                             (make-grocery "a" "7" 7 3)))
              
                       (list (make-grocery "a" "1" 0.1 9)
                             (make-grocery "a" "2" 0.2 8)
                             (make-grocery "a" "3" 0.3 7)
                             (make-grocery "a" "4" 0.4 6)
                             (make-grocery "a" "5" 0.5 5)
                             (make-grocery "a" "6" 0.6 4)
                             (make-grocery "a" "7" 0.7 3)))

(check-expect (overlap (list (make-grocery "a" "1" 0.1 9)
                             (make-grocery "b" "2" 0.2 8)
                             (make-grocery "a" "3" 0.3 7)
                             (make-grocery "b" "4" 0.4 6)
                             (make-grocery "a" "5" 0.5 5)
                             (make-grocery "b" "6" 0.6 4)
                             (make-grocery "a" "7" 0.7 3))
                       (list (make-grocery "c" "1" 1 9)
                             (make-grocery "a" "2" 0.2 0.8)
                             (make-grocery "a" "3" 3 7)
                             (make-grocery "c" "4" 0.4 0.6)
                             (make-grocery "a" "5" 5 5)
                             (make-grocery "a" "6" 0.6 0.4)
                             (make-grocery "c" "7" 7 3)))
              
                       (list (make-grocery "a" "3" 0.3 7)
                             (make-grocery "a" "5" 0.5 5)))

(check-expect (overlap (list (make-grocery "a" "1" 0.1 0.9)
                             (make-grocery "a" "2" 0.2 8))

                       (list (make-grocery "a" "1" 1 9)
                             (make-grocery "a" "2" 0.2 0.8)))
                       (list (make-grocery "a" "1" 0.1 0.9)
                             (make-grocery "a" "2" 0.2 8)))


;;
;;   Problem 2F
;;


;; (scale-prices stre groqury ratio) Consumes a store (stre)
;;  and a grocery querty index (groqury) and produces a new
;;  store in which each grocery is multiplied by a ratio
;; Examples:
(check-expect (scale-prices kwik-e-mart
               (make-query "can" 'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 1.10)
              (list (make-grocery "seed" "rice" 0.38 400)
                    (make-grocery "can" "corn" 4.40 400)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "produce" "apple" 2.99 400)
                    (make-grocery "can" "creamed eels" 2.41 350)
                    (make-grocery "produce" "pineapple" 3.17 250)))


;; scale-prices: Store GroceryQuery Num -> Store
;; Requires: ratio >= 0
(define (scale-prices stre groqury ratio)
        (cond [(empty? stre)                                empty]
              [(within-stre? (find-matches stre groqury)
                             (first stre))
                 (cons (change-prices (first stre) ratio)
                       (scale-prices (rest stre)  groqury ratio))]
              [else
                 (cons (first stre) (scale-prices (rest stre)
                                                  groqury ratio))]))
              

                            
;; ** Helper Functions **


;; (change-prices gro ratio) Consumes a grocery (gro) and
;;  a number (ratio), and produces a new grocery in which the price
;;  is multipled by the number
;; Examples:

(check-expect (change-prices (make-grocery "can" "corn" 4.00 400) 1.1)
              (make-grocery "can" "corn" 4.40 400))

(check-expect (change-prices (make-grocery "gam" "e" 2 2) 0)
              (make-grocery "gam" "e" 0 2))
 
;; change-prices: Grocery Num -> Grocery
(define (change-prices gro ratio)
   (make-grocery (grocery-dept gro) (grocery-name gro)
                 (/ (round (* (* (grocery-cost gro) ratio) 100)) 100)
                 (grocery-mass gro)))

  
;; (within-stre? stre canidate) Consumes a Store (stre) and a
;;  Grocery (canidate), produces if the canidate is contained
;;  within the store.
;; Examples:

(check-expect (within-stre? (list (make-grocery "can" "corn" 4.40 400)
                                  (make-grocery "can" "creamed eels"
                                                            4.40 400))
                                  (make-grocery "can" "creamed eels"
                                                            4.40 400))
              true)

;; within-stre?: Store Grocery -> Bool
(define (within-stre? stre canidate)
   (cond [(empty? stre)                false]
         [(and (string=? (grocery-dept (first stre))
                         (grocery-dept canidate))
               (string=? (grocery-name (first stre))
                         (grocery-name canidate))
               (=        (grocery-cost (first stre))
                         (grocery-cost canidate))
               (=        (grocery-mass (first stre))
                         (grocery-mass canidate)))
                                        true]
         [else (within-stre? (rest stre)
                                   canidate)]))
;; Empty List Test
(check-expect (scale-prices empty
                 (make-query "can" 'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0)
              empty)
     


;; No Changes Tests
(check-expect (scale-prices kwik-e-mart
               (make-query 'dontcare 'dontcare
                           (make-interval 200 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0)
              kwik-e-mart)

(check-expect (scale-prices try-n-save
               (make-query "abc" 'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 7)
               try-n-save)
(check-expect (scale-prices empty
               (make-query "abc" 'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 1.10)
              empty)

;; General Tests
(check-expect (scale-prices kwik-e-mart
                     (make-query 'dontcare 'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0.5)
              (list (make-grocery "seed" "rice" 0.19 400)
                    (make-grocery "can" "corn" 2 400)
                    (make-grocery "seed" "pinto" 1.24 500)
                    (make-grocery "produce" "apple" 1.5 400)
                    (make-grocery "can" "creamed eels" 1.1 350)
                    (make-grocery "produce" "pineapple" 1.58 250)))

(check-expect (scale-prices try-n-save
                     (make-query 'dontcare'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0.9)
               (list (make-grocery "produce" "apple" 2.24 600)
                     (make-grocery "seed" "rice" 0.86 1000)
                     (make-grocery "dairy" "milk" 3.59 4000)
                     (make-grocery "seed" "pinto" 2.24 500)
                     (make-grocery "produce" "potato" 2.69 5000)
                     (make-grocery "chips" "potato" 1.79 250)
                     (make-grocery "chips" "corn" 1.79 275)
                     (make-grocery "seed" "wheat" 0.44 500)
                     (make-grocery "produce" "banana" 0.62 450)
                     (make-grocery "dairy" "cheese" 5.84 900)
                     (make-grocery "chips" "banana" 1.79 50)
                     (make-grocery "produce" "peach" 3.59 400)
                     (make-grocery "seed" "lentil" 2.69 800)
                     (make-grocery "produce" "corn" 0.89 100)
                     (make-grocery "seed" "corn" 4.49 850)
                     (make-grocery "dairy" "kefir" 5.39 1000))) 
                
(check-expect (scale-prices student-shop
                    (make-query 'dontcare 'dontcare
                            (make-interval 'dontcare 'dontcare)
                            (make-interval 'dontcare 'dontcare)) 1.77)
              (list (make-grocery "can" "pineapple" 2.12 25.5)
                    (make-grocery "can" "apples" 12.39 900)
                    (make-grocery "can" "mango" 0 230)
                    (make-grocery "can" "lemon" 6.74 325)
                    (make-grocery "bakery" "pizza" 3.54 1.25)
                    (make-grocery "bakery" "bread" 3.52 7.25)
                    (make-grocery "bakery" "pastery" 7.06 25)
                    (make-grocery "bakery" "egg roll" 4.42 400)
                    (make-grocery "seed" "pinto" 4.41 500)
                    (make-grocery "seed" "edamame" 3.49 540.25)
                    (make-grocery "seed" "pea" 3.88 400)
                    (make-grocery "seed" "oats" 5.79 500)))
                  
               
                 
