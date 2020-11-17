;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "animals.rkt")

;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 07, Problem 2(A-I)
;; ******************************************


;; ==================================
;; struct and data definitions For Q6
;; =================================


;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)




;; =================================
;; Testing Suite 6
;; =================================


(define seen
  (list
    (list 'squirrel 'small 'angry)
    (list 'goose 'large 'swims 'flies 'angry)
    (list 'goose 'large 'swims 'flies 'angry)
    (list 'crow 'medium 'flies 'angry)))

;; =================================
;;
;; Question 2A
;;
;; =================================



;; (collect-attributes examples) Given a list of examples 
;;   and then produces a list of attributes where each
;;   of the attributes only occurs once
;; Examples:

(check-expect (collect-attributes seen)
(list 'large 'swims 'flies 'medium 'angry 'small))

;; collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes examples)

    (local[
         
    ;; (createset los set) Given a list of symbols (los) creates
    ;; a symbol set, that only contains unique values
    ;; createset: (listof Sym) (listof Sym) -> (listof Sym)
         (define (createset los set)
           (cond [(empty? los) set]
                 [(isin? (first los) set)
                         (createset (rest los) set)]
                 [else
                         (createset (rest los)
                                    (append (list (first los))
                                            set))]))

    ;; (isin? symbol set) checks to see if a symbol is contained
    ;; within a symbol set
    ;; isin?: Sym (listof Sym) -> Bool
         (define (isin? symbol set)
           (cond [(empty? set)                 false]
                 [(symbol=? symbol (first set)) true]
                 [else     (isin? symbol (rest set))]))]
  
   (cond [(empty? examples)           empty]
         [(list? examples)
              (createset (append (rest (first examples))
                                 (collect-attributes (rest examples)))
                         empty)])))


;; Empty Test
(check-expect (collect-attributes empty) empty)

;; All The Same Tests
(check-expect (collect-attributes '(( a a a a a a a a a)
                                    ( b a a a a a a a a)
                                    ( c a a a a a a a a)
                                    ( d a a a a a a a a)))
                                  '(a))
(check-expect (collect-attributes '(( a a a a a a a a a)
                                    ( b )
                                    ( c )
                                    ( d )))
                                  '(a))

;; Only Change Tests
(check-expect (collect-attributes '(( a a)
                                    ( b b c)
                                    ( c d e f)
                                    ( d g h i j)))
                                  '(b c g h i j f e d a))
(check-expect (collect-attributes '(( a aa bb cc dd ee)
                                    ( b aa bb dd ee cc)))
                                  '(ee dd cc bb aa))



;; =================================
;;
;; Question 2B
;;
;; =================================



;; (split-examples examples symb) We split a list of examples
;;   (examples) by symbol, to create to different lists
;;   which either contain to 
;; Examples:

(check-expect (split-examples seen 'goose) ; splitting on a label
       (list
        (list
         (list 'goose 'large 'swims 'flies 'angry)
         (list 'goose 'large 'swims 'flies 'angry))
        (list
         (list 'squirrel 'small 'angry)
         (list 'crow 'medium 'flies 'angry))))

(check-expect (split-examples seen 'small) ; splitting on an attribute
      (list
        (list
         (list 'squirrel 'small 'angry))
        (list
         (list 'goose 'large 'swims 'flies 'angry)
         (list 'goose 'large 'swims 'flies 'angry)
         (list 'crow 'medium 'flies 'angry))))


;; split-examples: (listof Example) Sym -> (list (listof Example)
;;                                               (listof Example))
(define (split-examples examples symb)

    (local[

    ;; (arrsort symbol loe samp nl le) Sorts a element list into
    ;; two lists, (nl) for examples without the symbol (le) in
    ;; terms of atributes. samp stores data
           
    ;; arrsort: Sym (listof Sym) (listof Sym)   (list (listof Sym)
    ;;                           (listof Sym) ->      (listof Sym))
           
         (define (arrsort symb loe samp nl le)
           (cond [(empty? loe) (list le nl)]
                 [(empty? (first loe))
                          (arrsort symb (rest loe) (rest samp)
                                     (append nl (list (first samp)))
                                     le)]
                 [(symbol=? symb (first (first loe)))
                          (arrsort symb (rest loe) (rest samp)
                                      nl
                                     (append le (list (first samp))))]
                 [else (arrsort symb
                                (append (list (rest (first loe)))
                                        (rest loe))
                                samp
                                nl
                                le)]))
           

    ;; (labsort symbol loe nl le) Sorts a list into two lists,
    ;; (nl) for examples without the symbol (le) in terms of labels
    ;; with the symbol in them
           
    ;; labsort: Sym (listof Sym) (listof Sym)   (list (listof Sym)
    ;;                           (listof Sym) ->      (listof Sym))
           
         (define (labsort symb loe nl le)
           (cond [(empty? loe)  (list le nl)]
                 [(symbol=? symb (first (first loe))) 
                         (labsort symb (rest loe) nl
                                           (append le
                                                (list (first loe))))]
                 [else
                         (labsort symb (rest loe)
                                           (append nl
                                                (list (first loe)))
                                                    le)]))
         

    ;; (islab? sym example) checks to see if a symbol is contained
    ;; within a the example as a label
    ;; islab? Sym (listof Sym) -> Bool
         (define (islab? symbol example)
           (cond [(empty? example)                         false]
                 [(symbol=? symbol (first (first example))) true]
                 [else     (islab? symbol (rest example))]))]

      
  
   (cond [(islab? symb examples) (labsort symb examples empty empty)]
         [else        (arrsort symb examples examples empty empty)])))
;; Empty Test
(check-expect (split-examples empty "a") '(()()))

;; General Tests
(check-expect (split-examples '(( c a a a a a a a a)
                                ( c a a a a a a a a)
                                ( c a a a a a a a a)
                                ( c a a a a a a a a)) 'c)
                                '((( c a a a a a a a a)
                                   ( c a a a a a a a a)
                                   ( c a a a a a a a a)
                                   ( c a a a a a a a a))
                                  ()))
(check-expect (split-examples '(( c b)
                                ( c b)
                                ( c e)
                                ( c e)) 'b)
                                '((( c b)
                                   ( c b))
                                  (( c e)
                                   ( c e))))

(check-expect (split-examples '(( a a)
                                ( b b))'c)
                              '(()
                                ( (a a)
                                   (b b))))

;; =================================
;;
;; Question 2C
;;
;; =================================



;; (histogram examples) Given a list of examples (examples)
;;   will create a histogram with the relative intensities
;;   of each attribute 
;; Examples:

(check-expect (histogram seen)
(list (list 'flies 3) (list 'swims 2) (list 'angry 4) (list 'medium 1)
      (list 'large 2) (list 'small 1)))




;; histogram: (listof Examples) -> Histogram
(define (histogram examples)

    (local[

    ;; (hist loa curr currupto) Given a loa will create
    ;; a histogram of each attribute even if they dont exist
    ;; yet in the histogram (curr)
           
    ;; hist: (listof Sym) ->  (listof (list Sym Nat))
           
         (define (hist loa curr currupto)
           (cond [(empty? loa)  curr]
                 [(empty? curr)
                      (hist (rest loa)
                            (append currupto
                                    (list (list (first loa) 1)))
                             empty)]
                 [(symbol=? (first (first curr)) (first loa))
                      (hist (rest loa)
                            (append currupto
                                  (list (list (first (first curr))
                                        (add1 (second (first curr)))))
                                  (rest curr))
                            empty)]

                 [else
                      (hist loa
                            (rest curr)
                            (append (list (first curr))
                                     currupto))]))]
           

      
   (cond [(empty? examples) empty]
         [else    (hist (rest (first examples))
                        (histogram (rest examples))
                        empty)])))



;; Empty Test
(check-expect (histogram empty) empty)

;; General Tests
(check-expect (histogram '(( c a a a a a a a a)
                           ( c a a a a a a a a)
                           ( c a a a a a a a a)
                           ( c a a a a a a a a)))
                           '((a 32)))

(check-expect (histogram '(( c b)
                           ( c b)
                           ( c e)
                           ( c e)))
                          '((e 2) (b 2)))

(check-expect (histogram '(( a a)
                           ( b b)))
                          '((b 1) (a 1)))


;; =================================
;;
;; Question 2D
;;
;; =================================



;; (augment-histogram histogram attributes total) Given a histogram
;;   a list of attributes and the total count of attributes will
;;   produce a augmented histogram
;; Examples:

(check-expect
   (augment-histogram
       (list (list 'a 100) (list 'c 50))
       (list 'a 'b 'c)
       200)
    (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect
   (augment-histogram empty (list 'x 'y) 10)
   (list (list 'x 0 10) (list 'y 0 10)))



;; augment-histogram: Histogram (listof Sym) Nat -> AH
;; Requires: Each element of attributes to be unique
(define (augment-histogram histogram attributes total)

    (local[

    ;; (aug-hist hist curr atr) Given a histogram will create
    ;; a augmented histogram for each elemnent (curr), keeps
    ;; atr as a back up
           
    ;; aug-hist: Histogram Histogram (listof Sym) -> AH
        
         (define (aug-hist hist curr currupto)
           (cond [(empty? curr) empty]
                 [(empty? hist)
                       (append (list (list (first curr) 0 total))
                               (aug-hist currupto
                                         (rest curr)
                                         currupto))]
                 [(symbol=? (first (first hist)) (first curr))
                       (append (list (list (first (first hist))
                                           (second (first hist))
                                           (- total (second
                                                    (first hist)))))
                               (aug-hist currupto
                                         (rest curr)
                                         currupto))]
                 [else
                      (aug-hist (rest hist) curr currupto)]))]
           

      
   (cond [(empty? attributes) empty]
         [else   (aug-hist histogram attributes histogram)])))



;; Empty Test
(check-expect (augment-histogram empty empty 0) empty)

;; General Tests
(check-expect (augment-histogram '((a 32)) '(a) 32)
              '((a 32 0)))

(check-expect (augment-histogram '((e 2) (b 2)) '(e b) 4)
              '((e 2 2)(b 2 2)))

(check-expect (augment-histogram '((b 1) (a 1)) '(a b) 4)
              '((a 1 3) (b 1 3)))


;; =================================
;;
;; Question 2E
;;
;; =================================



;; (entropy positive-counts negative-counts) Given a augmented
;;   histogram of the positive and negitive counts of an attribute
;;   will compute the entropy
;; Examples:

(check-within (entropy (list 'large 126 59)
                       (list 'large 146 669))
              #i0.5663948489858 0.001)
(check-within (entropy (list 'small 17 168)
                       (list 'small 454 361))
              #i0.5825593868115 0.001)
(check-within (entropy (list 'a 0 100)
                       (list 'b 100 0))
              0.0 0.001)
(check-within (entropy (list 'a 0 100)
                       (list 'b 0 100))
              1 0.001)




;; entropy: (list AH AH) -> Num
(define (entropy positive-counts negative-counts)

    (local[
           (define a (second positive-counts))
           (define b (second negative-counts))
           (define c (third positive-counts))
           (define d (third negative-counts))

    ;; (pfunc num1 num2) Given two numbers uses the
    ;;  big P function to create a num as expressed in the
    ;;  assigment
           
    ;; pfunc: Num Num -> Num
    ;; Requires: Num1 and Num2 to be greater/equal to 0
        
         (define (pfunc num1 num2)
           (cond [(> (+ num1 num2) 0)
                     (/ num1 (+ num1 num2))]
                 [else 0.5]))


    ;; (efunc p) Given a p value from the pfunc, uses the
    ;;  big e function to create a num as expressed in the
    ;;  assigment
           
    ;; efunc: Num -> Num
    ;; Requires: p to be between 0 and 1 inclusive
          (define (efunc p)
           (cond [(= p 0) 0]
                 [else (* (* -1 p) (log p 2))]))]
                     
           

      
   (+ (* (pfunc (+ a b) (+ c d)) (+ (efunc (pfunc a b))
                                    (efunc (pfunc b a))))
      (* (pfunc (+ c d) (+ a b)) (+ (efunc (pfunc c d))
                                    (efunc (pfunc d c)))))))





;; =================================
;;
;; Question 2F
;;
;; =================================



;; (entropy-attributes positive negative) Given a list of
;;   two augmented trees will compare the entropy between
;;   the terms and produce a list of attribute entropy pairs
;; Examples:

(check-within (entropy-attributes
     (list
       (list 'large 126 59) (list 'angry 161 24)
       (list 'small 17 168) (list 'flies 170 15)
       (list 'swims 162 23) (list 'medium 42 143))
      (list
       (list 'large 146 669) (list 'angry 469 346)
       (list 'small 454 361) (list 'flies 615 200)
       (list 'swims 365 450) (list 'medium 215 600)))
    (list
     (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
     (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
     (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))
     0.001)


;; entropy: (listof (list AH AH)) -> EAL
(define (entropy-attributes positive negative)
   (cond [(empty? positive) empty]
         [else (append (list (list
                         (first (first positive))
                         (entropy (first positive) (first negative))))
                       (entropy-attributes (rest positive)
                                           (rest negative)))]))

;; =================================
;;
;; Question 2G
;;
;; =================================



;; (best-attribute entropies) Given a list of attribute/
;;   entropy pairs will produce the attribute with the smallest
;;   entropy
;; Examples:

(check-expect (best-attribute
 (list
  (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
  (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
  (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
 'large)



;; best-attribute: EAL -> Sym
(define (best-attribute entropies)
    (local[
    ;; (findmin losn store) Given a list of numbers and symbol
    ;;  pairs will find the minimum by storeing a num/sym pair and
    ;;  comparing it to the rest
           
    ;; findmin: (listof (Sym Num) (Sym Num) -> Sym
    ;; Requires: Num1 and Num2 to be greater/equal to 0
        
         (define (findmin losn store)
           (cond [(empty? losn) (first store)]
                 [(<= (second (first losn)) (second store))
                     (findmin (rest losn) (first losn))]
                 [else
                     (findmin (rest losn) store)]))]
      
   (cond [(empty? entropies) empty]
         [else (findmin entropies (first entropies))])))


;; Empty Test
(check-expect (best-attribute empty) empty)

;; General Tests
(check-expect (best-attribute '((a 32) (b 3) (c 2) (d 0)))
              'd)

(check-expect (best-attribute '((esaf 0.001) (fsfsab 0.00111)
                                (fas 0.00121) (fas2 1)))
              'esaf)

(check-expect (best-attribute '((b 1)))
              'b)


;; =================================
;;
;; Question 2H
;;
;; =================================



;; (build-dt examples label) Given a list of examples with a label
;;   will produce a decision tree using all of the previous functions
;; Examples:


(check-expect (build-dt (random-animals 1000) 'goose)
(list 'large (list 'swims (list 'angry true false) false) false))
(check-expect (build-dt (random-animals 1000) 'crow)
(list 'swims
false
(list 'flies
(list 'angry
(list 'medium
true
(list 'large
true
(list 'small true false)))
false)
false)))
(check-expect (build-dt (random-animals 1000) 'emu)
false)





;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
    (local[

    ;; (split-examp examp label) given a list of examples
    ;; will create two lists depending on if the example has
    ;; the label

    ;; split-examples: (listof Example) Sym -> (list (listof Sym)
    ;;                                                (listof Sym)
        (define (split-examp examp label)
                (split-examples examp label))


        
    ;; (positive-at splitfunction) Get the positive examples
    ;; positive-at: ((listof Example)-> (list (listof Sym) -> (listof
    ;;                          Sym           (listof Sym))     Sym)
        (define positive-at (first (split-examp examples label)))
        
    ;; (negative-at splitfunction) Get the negative examples
    ;; negative-at: ((listof Example)-> (list (listof Sym) -> (listof
    ;;                          Sym           (listof Sym))     Sym)
        (define negative-at (second (split-examp examples label)))]


      
   (cond [(empty? positive-at) false]
         [(empty? negative-at) true]
         [(empty? (collect-attributes examples))
          (> (length positive-at)
             (length negative-at))]
         [else 


         (local[


        ;; (attributes examp) given a list of examples will create
    ;; a list of all the attributes

    ;; attributes: (listof Example) -> (listof Sym)
        (define (attributes examp)
                (collect-attributes examp))
   

        
    ;; (get-countatr examp) given a example will produce the
    ;; total count of attributes

    ;; get-countatr: (listof Example) -> Num 
        (define (get-countatr examp)
            (cond [(empty? examp) 0]
                  [else (+ (- (length (first examp)) 1)
                           (get-countatr (rest examp)))]))

     ;; (get-rootatt pos-atr neg-atr) given a list of positive and 
    ;;  negative attributes will create a the root atribute

    ;; get-rootatt: (listof Example) (listof Example) -> Sym 
        (define (get-rootatt pos-atr neg-atr)
          (cond [(not (empty? (attributes pos-atr)))
            (best-attribute
             (entropy-attributes
                   (augment-histogram (histogram pos-atr)
                                      (attributes pos-atr)
                                      (get-countatr pos-atr))
                   (augment-histogram (histogram neg-atr)
                                      (attributes pos-atr)
                                      (get-countatr neg-atr))))]
                [else 'n]))



        
    ;; (rootatt) Get the rootattribute
    ;; root-att ((listof Example) (listof Example) -> Sym) -> Sym
     (define rootatt (get-rootatt positive-at negative-at))


    ;; (sortbyroot splitfunction) Get the sorted by root attribute
    ;; sortbyroot:
    ;;   ((listof Example)-> (list (listof Sym) -> (list (listof Sym)
    ;;                Sym          (listof Sym))         (listof Sym))
     (define sortbyroot (split-examples examples rootatt))


    ;; (removeatr examp atr) Given a list of examples will remove
    ;; all instances of the attribute (atr) from the list
    ;; removeatr: (listof Sym) -> (listof Sym)
     
     (define (removeatr examp atr)
          (cond [(empty? examp) empty]
                [(list? (first examp))
                    (append (list (removeatr
                                     (first examp) atr))
                            (removeatr (rest examp) atr))]
                [(symbol=? (first examp) atr)
                            (removeatr (rest examp) atr)]
                [else (cons (first examp)
                            (removeatr (rest examp) atr))]))

     
    ;; (lbranch splitfunction) Get the sorted by has root attribute
    ;; lbranch: (listof Sym) -> (listof Sym)      
     (define lbranch (removeatr (first sortbyroot) rootatt))

    ;; (rbranch splitfunction) Get the sorted by not root attribute
    ;; rbranch: (list (listof Sym) (listof Sym)) -> (listof Sym)  
     (define rbranch (second sortbyroot))]



           
         (cond [(equal? (build-dt lbranch label)
                  (build-dt rbranch label))
                  (build-dt rbranch label)]
                [else
             (list rootatt
                  (build-dt lbranch label)
                  (build-dt rbranch label))]))])))

             
         


;; =================================
;;
;; Question 2i
;;
;; =================================

;; (train-classifier examples label) Given a list of examples and
;;  a label will try to produce a predicate to identify a if a
;;  example will classify as that symbol

;; train-classifier: Example Sym -> (Example -> Bool)
(define (train-classifier examples label)
    (local[
          (define  (check-wiathin dt canidate)
             (cond [(empty? canidate) false]
                   [(symbol=? dt (first canidate)) true]
                   [else (check-wiathin dt (rest canidate))]))
             
           (define (compare dt canidate)
             (cond [(boolean? dt) dt]
                   [(empty? canidate) (third dt)]
                   [(check-wiathin (first dt) canidate)
                           (compare (second dt) (rest canidate))]
                   [else
                           (compare (third dt) (rest canidate))]))]
    (compare (build-dt examples label) (first examples))))




(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)



