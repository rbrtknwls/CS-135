;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 09, Problem 4 (A-B)
;; ******************************************

;; (fewest-coins target denom) Given a target value and a list
;;  of possible denominations (denom) will produce the minimum
;;  number of denominations in order to create that list
;; Examples:

(check-expect (fewest-coins 45 '(1 5 10 25 100 200)) '(25 10 10))
(check-expect (fewest-coins 0 '(1 5 10 25 100 200)) empty)
(check-expect (fewest-coins 30 '(1 2 3 5)) '(5 5 5 5 5 5))


;; fewest-coins: Nat (listof Nat) -> (listof Nat)
;; Requires: Denom is non empty (contains at least '(1))
(define (fewest-coins target denom)
  (cond [(< target 1) empty]
        [else
             (replace-multiples target
                                denom '(1) empty)]))


;; =================================
;; Helper Functions
;; =================================


;; (make-factor fac pfac) Given a current factor (fac) -or a
;;  denomination, and a list of previous factors (pfac) will
;;  produce that number expressed as the previous factors
;; Examples:

(check-expect (make-factor 45 '(50 20 5 2 1)) '(20 20 5))
(check-expect (make-factor 5 '(2 1)) '(2 2 1))
(check-expect (make-factor 17 '(10 7 3 1)) '(10 7))

;; make-factor: Nat (listof Nat) -> (listof Nat)
(define (make-factor fac pfac)
     (cond [(empty? pfac) empty]
           [(>= (- fac (first pfac)) 0)
               (cons (first pfac)
                     (make-factor (- fac
                                (first pfac))
                             pfac))]
           [else
                (make-factor fac
                        (rest pfac))]))

;; (make-gcd fac pfac) Given a current factor (fac) -or a
;;  denomination, and a list of previous factors (pfac) will
;;  produce that number expressed as the first number it
;;  is divisble by
;; Examples:

(check-expect (make-gcd 100 '(50 20 5 2 1)) '(50 50))
(check-expect (make-gcd 16 '(8 4 2 1)) '(8 8))
(check-expect (make-gcd 2 '(10 7 1)) '(1 1))

;; make-gcd: Nat (listof Nat) -> (anyof (listof Nat) Sym)
(define (make-gcd fac pfac)
     (cond [(empty? pfac) 'a]
           [(= (remainder fac (first pfac)) 0)
               (build-list (/ fac
                              (first pfac))
                          (lambda (x) (first pfac)))]
           [else
                (make-gcd fac
                        (rest pfac))]))


;; (euclid-gcd n m) Given two Nats (n, m) will produce the  
;;  gcd of the two values using genitive recursion
;; Examples:

(check-expect (euclid-gcd 45 5) 5)
(check-expect (euclid-gcd 7 5) 1)
(check-expect (euclid-gcd 200 100) 100)

;; make-factor: Nat Nat -> Nat
(define (euclid-gcd n m)
        (cond [(zero? m) n]
              [else (euclid-gcd m (remainder n m))]))


;; (contains mainlist checklist listw) Given a list to check through 
;;  (mainlist) and a list to check for (checklist), will produce a
;;  new list (list w) of all the values from checklist removed from
;;  mainlist, or if all the values were not nullifed "a"
;; Examples:

(check-expect (contains '(1 1 1 1) '(1 1) empty) '(1 1))
(check-expect (contains '(1 1) '(1 1 1 1) empty) "a")
(check-expect (contains '(7 5 5 5 2 2 1) '(7 5 1) empty) '(5 5 2 2))

;; contains: (listof Nat) (listof Nat)
;;                        (listof Nat) -> (anyof (listof Nat) Sym)
(define (contains mainlist checklist listw)
        (cond [(empty? checklist) (append listw   mainlist)]
              [(empty? mainlist)                      "a"]
              [(< (first mainlist) (first checklist)) "a"]
              [(= (first mainlist) (first checklist))
                                  (contains (rest mainlist)
                                           (rest checklist)
                                                     listw)]
              [else    (contains (rest mainlist)
                                checklist
                                    (append listw
                                          (list (first mainlist))))]))

;; (replace-multiples target cur_denom prev_denom changelist) Given a
;;  target amount of money the current denomination (cur_denom) and
;;  the previous denominations (prev_denom) and the changelist, will
;;  produce the optimal configeration of change
;; Examples:

(check-expect (replace-multiples 16 '(1 2 3 4) '(1) empty)
                                                   '(4 4 4 4))

;; replace-multiples: Nat (listof Nat) (listof Nat)
;;                                     (listof Nat) -> (listof Nat)
(define (replace-multiples target cur_denom prev_denom changelist)
  
     (cond [(empty? cur_denom)  changelist]
           [else 
               (local [

                       ;; (nlist) will produce the changelist
                       ;; with the new factor included (took out
                       ;; past factors so sum is still at target
                       ;; or will return 'a

                       ;; minirep: (anyof (listof Nat) Sym)
                       (define nlist
                            (contains changelist
                                      (make-factor (first cur_denom)
                                                   prev_denom)
                                      empty))
                       
                       ;; (gcd) gets the gcd of the current 
                       ;; denomination and the past demonmination

                       ;; gcd: Nat
                       (define gcd (euclid-gcd (first cur_denom)
                                               (first prev_denom)))

                       ;; (divis-rep) represents the current denom
                       ;;  by the previous denoms it is divisble
                       ;;  by

                       ;; divis-rep: (anyof (lisof Nat) Sym)
                       (define divis-rep
                             (contains changelist
                                      (make-gcd (first cur_denom)
                                                   prev_denom)
                                      empty))
                       
                       ;; (co-prime-nlist) Finds the smallest multiple
                       ;;  of the current denomination and the past
                       ;;  denomination and converts the past
                       ;;  demonation into the present

                       ;; co-prime-nlist: (anyof (listof Nat) Sym)
                       (define co-prime-nlist
                             (contains changelist
                                      (build-list (/ (first cur_denom)
                                                    gcd)
                                                 (lambda (x)
                                                  (first prev_denom)))
                                      empty))]
               (cond
                 
                     ;; Starting Case (current denomination is 0)
                 
                     [(= (first cur_denom) 1)
                         (replace-multiples
                            target
                            (rest cur_denom)
                            (list (first cur_denom))
                            (build-list target (lambda (x) 1)))]

                     ;; Enough Of previous denomination merge into
                     ;; new denomination
                     
                     [(list? nlist)
                      (replace-multiples target cur_denom prev_denom
                                         (append (list
                                                  (first cur_denom))
                                                 nlist))]
                     
                     ;; Enough Of previous denomination merge into
                     ;; gcd of old denomination
                     
                     [(list? co-prime-nlist)
                      (replace-multiples
                          target cur_denom prev_denom
                          (append (build-list
                                    (/ (first prev_denom)
                                       gcd)
                                    (lambda (x) 
                                    (first cur_denom)))
                                   co-prime-nlist))]

                     ;; Check if current list can sum up to make
                     ;;  current denominatoin

                     [(list? divis-rep)
                      (replace-multiples target cur_denom prev_denom
                                  (append (list (first cur_denom))
                                          divis-rep))]
 

                     ;; Out of possible combinations, move to next
                     ;; denomination
                     
                     [else
                      (replace-multiples
                         target (rest cur_denom)
                        (append (list (first cur_denom))
                                prev_denom)
                        changelist)]))]))
;; =================================
;; Testing Suite
;; =================================


;; === Two Element Tests ===
(check-expect (fewest-coins 300 '(1 300))
              '(300))
(check-expect (fewest-coins 300 '(1 100))
              '(100 100 100))
(check-expect (fewest-coins 25 '(1 20))
              '(20 1 1 1 1 1))


;; === Multiple Denoms Tests ===
(check-expect (fewest-coins 12 '(1 2 3 4))
              '(4 4 4))
(check-expect (fewest-coins 12 '(1 2 3 4 8))
              '(8 4))
(check-expect (fewest-coins 64 '(1 2 3 4 7 8 13 16 32))
              '(32 32))
(check-expect (fewest-coins 27 '(1 9 19))
              '(9 9 9))
(check-expect (fewest-coins 32 '(1 2 4 8 9))
              '(8 8 8 8))




;; === General Denoms Tests ===
(check-expect (fewest-coins 300 '(1 5 10 25 100 200))
              '(200 100))
(check-expect (fewest-coins 12 '(1 3 4))
              '(4 4 4))
(check-expect (fewest-coins 8 '(1 3 4))
              '(4 4))
(check-expect (fewest-coins 8 '(1 4 7))
              '(4 4))
(check-expect (fewest-coins 25 '(1 25))
              '(25))
(check-expect (fewest-coins 1 '(1))
              '(1))
