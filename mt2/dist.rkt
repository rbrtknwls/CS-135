;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname dist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Midterm 02, Problem 4 (A-B)
;; ******************************************


;; ==================================
;; struct and data definitions For Q4
;; =================================

;; A Channel is a (listof (list Str (listof Sym)))

;; A Lists is a list of Symbol
;; A Answer is a Num
;; A Names is a Str

(define-struct testa (l1 l2 a))
;; A 4A-Test is a (make-testa Lists Lists Answer)

(define-struct testb (c l n))
;; A 4A-Test is a (make-testb Channel Lists Names)

;; A HamPair is a (list Str (listof Sym))
    
;; A HamList is a (listof HamPair)
;; Requires: every (listof Sym) in the HamList has the 
;;   same length. Each Str in each HamPair is distinct.


;; =================================
;; Testing Suite 4A
;; =================================

(define packet1 '(a b c d e f g h))
(define packet2 '(a b a b e f e f))
(define packet3 '(a b c d a b c d))

(define packet4 '(hello I am me))
(define packet5 '(Hello I Am Me))
(define packet6 '(hello I am Me))

(define packet7 '(hey))
(define packet8 '(hey.))


;; ======= "Empty" Tests ===========

(define test1 (make-testa empty
                          empty
                          0))

;; ======= General Tests ==========

(define test2 (make-testa packet1
                          packet1
                          0))
(define test3 (make-testa packet1
                          packet2
                          4))
(define test4 (make-testa packet2
                          packet3
                          6))
(define test5 (make-testa packet1
                          packet3
                          4))

(define test6 (make-testa packet4
                          packet4
                          0))
(define test7 (make-testa packet4
                          packet5
                          3))
(define test8 (make-testa packet5
                          packet6
                          2))
(define test9 (make-testa packet4
                          packet6
                          1))

(define test10 (make-testa packet7
                           packet7
                           0))
(define test11 (make-testa packet7
                           packet8
                           1))

;; =================================
;; Testing Suite 4B
;; =================================

(define channel1 (list
     (list "channel1" '(b a b a b a a a b a a a))
     (list "channel2" '(b a b a b b a a b b b b))
     (list "channel3" '(b b b b b b b a b a b b))))

(define channel2 (list
     (list "channel1" '(a a a a))
     (list "channel2" '(a a b b))
     (list "channel3" '(b b a a))
     (list "channel4" '(b b b b))))

;; ======= "Empty" Tests ===========

(define test12 (make-testb empty
                           empty
                           empty))
(define test13 (make-testb '(a b c)
                           empty
                           empty))
(define test14 (make-testb empty
                           '(a b c)
                           empty))

;; ======= General Tests ==========

(define test15 (make-testb channel1
                           '(b a b a a a b b a b b a)
                           (list "channel1" "channel2")))
(define test16 (make-testb channel1
                           '(b b b a b b a a b a b b)
                           (list "channel2" "channel3")))
(define test17 (make-testb channel1
                           '(b a b a b a a a b a a b)
                           (list "channel1")))

(define test18 (make-testb channel2
                           '(b a b a)
                           (list "channel1" "channel2"
                                 "channel3" "channel4")))
(define test19 (make-testb channel2
                           '(a b a b)
                           (list "channel1" "channel2"
                                 "channel3" "channel4")))
(define test20 (make-testb channel2
                           '(b b a b)
                           (list "channel3" "channel4")))

;; =================================
;;
;; Question 4A
;;
;; =================================



;; (hamming-distance los1 los2) Consumes 2 lists of symbols
;;   (los1, los2) and produces the number of positions that
;;   the two symbols differ at
;; Examples:

(check-expect (hamming-distance '(t a g a a g t t t)
                                '(t a g a a g g t t)) 1)
(check-expect (hamming-distance '(b a b a b a a a b a a a)
                                '(b a b a b b a a b a b b)) 3)

;; hamming-distance: (listof Sym) (listof Sym) -> Nat
;; Requires: los1 and los2 to
;;           be the same size
(define (hamming-distance los1 los2)
  (cond [(or (empty? los1) (empty? los2))                   0]
        [(symbol=? (first los1) (first los2))
                    (hamming-distance (rest los1) (rest los2))]
        [else
              (+ 1 (hamming-distance (rest los1) (rest los2)))]))

;; Empty Tests
(check-expect (hamming-distance (testa-l1 test1)
                                (testa-l2 test1)) (testa-a test1))

;; General Tests
(check-expect (hamming-distance (testa-l1 test2)
                                (testa-l2 test2)) (testa-a test2))
(check-expect (hamming-distance (testa-l1 test3)
                                (testa-l2 test3)) (testa-a test3))
(check-expect (hamming-distance (testa-l1 test4)
                                (testa-l2 test4)) (testa-a test4))
(check-expect (hamming-distance (testa-l1 test5)
                                (testa-l2 test5)) (testa-a test5))
(check-expect (hamming-distance (testa-l1 test6)
                                (testa-l2 test6)) (testa-a test6))
(check-expect (hamming-distance (testa-l1 test7)
                                (testa-l2 test7)) (testa-a test7))
(check-expect (hamming-distance (testa-l1 test8)
                                (testa-l2 test8)) (testa-a test8))
(check-expect (hamming-distance (testa-l1 test9)
                                (testa-l2 test9)) (testa-a test9))
(check-expect (hamming-distance (testa-l1 test10)
                                (testa-l2 test10)) (testa-a test10))
(check-expect (hamming-distance (testa-l1 test11)
                                (testa-l2 test11)) (testa-a test11))



;; =================================
;;
;; Question 4B
;;
;; =================================



;; (min-hamming los hamlist) Consumes a lists of symbols
;;   (los) and a HamList (hamlist) and produces a string(s)
;;   corrisponding to the Hampair with the lowest hamming-distance
;; Examples:

(check-expect (min-hamming '(b a b a b a a a b a a a)
                           channel1) '("channel1"))
(check-expect (min-hamming '(b a b a b b a a b a a b)
                           channel1) '("channel1" "channel2"))

;; min-hamming: (listof Sym) HamList -> (listof Str)
;; Requires: los and to be same size as
;;           each element of Hamlist (hamlist is non empty)
(define (min-hamming los hamlist)
  (cond [(empty? hamlist)               empty]
        [(empty? los)                   empty]
        [else
             (get-hpair los
                        hamlist
                        (getmin los hamlist))]))

  
;; ===  Helper Functions  ===

  
;; (getmin los hamlist) Given a los and a hamlist
;;   finds the value of the smallest hamlist
;; Examples:

(check-expect (getmin '(b a b a b a a a b a a a)
                      channel1) 0)
(check-expect (getmin '(b a b a b b a a b a a a)
                      channel1) 1)
                      

;; getmin: (listof Sym) HamList -> Nat
;; Requires: los and to be same size as
;;           each element of Hamlist (hamlist is non empty)
(define (getmin los hamlist)
   (cond [(empty? (rest hamlist))
                  (hamming-distance los
                                   (second (first hamlist)))]
         [else
                  (min (hamming-distance los
                                         (second (first hamlist)))
                       (getmin los (rest hamlist)))]))


;; (get-hpair los hamlist index) Given a Hamlist (hamlist) and an
;;   index and a los, will produce all the list of all the hampairs
;;   with the same hamming-distance as the index
;; Examples:

(check-expect (get-hpair '(b a b a b a a a b a a a)
                     channel1 0) (list "channel1"))
(check-expect (get-hpair '(b a b a b b a a b a a a)
                      channel1 1) (list "channel1"))
                      

;; get-hpair: (listof Sym) HamList Nat -> (listof Str)
;; Requires: los and to be same size as
;;           each element of Hamlist (hamlist is non empty)
(define (get-hpair los hamlist index)
   (cond [(empty? hamlist)                             empty]
         [(= (hamming-distance los (second (first hamlist)))
             index)
             (cons (first (first hamlist))
                   (get-hpair los (rest hamlist) index))]
         [else (get-hpair los (rest hamlist) index)]))
   

;; Empty Tests
(check-expect (min-hamming (testb-c test12)
                           (testb-l test12)) (testb-n test12))
(check-expect (min-hamming (testb-c test13)
                           (testb-l test13)) (testb-n test13))
(check-expect (min-hamming (testb-c test14)
                           (testb-l test14)) (testb-n test14))

;; General Tests
(check-expect (min-hamming (testb-l test15)
                           (testb-c test15)) (testb-n test15))
(check-expect (min-hamming (testb-l test16)
                           (testb-c test16)) (testb-n test16))
(check-expect (min-hamming (testb-l test17)
                           (testb-c test17)) (testb-n test17))
(check-expect (min-hamming (testb-l test18)
                           (testb-c test18)) (testb-n test18))
(check-expect (min-hamming (testb-l test19)
                           (testb-c test19)) (testb-n test19))
(check-expect (min-hamming (testb-l test20)
                           (testb-c test20)) (testb-n test20))
