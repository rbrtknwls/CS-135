;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Starter Code
;; ***************************************************
;;

(require "templates.rkt")

;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point
;;    are less than the respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       
;; ** Constants **

(define tol 0.01)
(define threshhold 0.1)

;;
;;   Problem 3A(i-iv)
;;



;; ** Tests for 3Ai **
(define 3AItest1 (list 0.5 5))
(define 3AIsoul1 (list 0.5 5))

(define 3AItest2 (list -4 7))
(define 3AIsoul2 (list -4 7))

(define 3AItest3 (list -23 3.6))
(define 3AIsoul3 (list -23 3.6))

(define 3AItest4 (cons 5 (cons empty empty)))
(define 3AIsoul4 (list 5 empty))

(define 3AItest5 empty)
(define 3AIsoul5 (list empty empty))


;; ** Tests for 3Aii **
(define 3A2test1 (list (list 0.5 1) (list 1.5 2)
                       (list 2.5 3) (list 3.5 4)))
(define 3A2vars1 (list 0.5 0))
(define 3A2soul1 (list (list 1 1) (list 2 2)
                       (list 3 3) (list 4 4)))

(define 3A2test2 (list (list 26 -2) (list 23 -55)
                       (list 17 -12)))
(define 3A2vars2 (list -10 10))
(define 3A2soul2 (list (list 16 8) (list 13 -45)
                       (list 7 -2) ))

(define 3A2test3 (list (list -1 -1) (list 0 0)
                       (list 1 1)))
(define 3A2vars3 (list 3.6 2.1))
(define 3A2soul3 (list (list 2.6 1.1) (list 3.6 2.1)
                       (list 4.6 3.1) ))

(define 3A2test4 (list (list 2 3) (list 5 8)
                       (list 13 21) (list 34 55)))
(define 3A2vars4 (list -1 -7))
(define 3A2soul4 (list (list 1 -4) (list 4 1)
                       (list 12 14) (list 33 48)))

(define 3A2test5 empty)
(define 3A2vars5 (list 6 5.6))
(define 3A2soul5 empty)


;; ** Tests for 3Aiii **
(define 3A3test1 (list (list 0.5 1) (list 1.5 2)
                       (list 2.5 3) (list 3.5 4)))
(define 3A3vars1 (list 0.5 0))
(define 3A3soul1 (list (list 0.25 0) (list 0.75 0)
                       (list 1.25 0) (list 1.75 0)))

(define 3A3test2 (list (list 26 -2) (list 23 -55)
                       (list 17 -12)))
(define 3A3vars2 (list -10 10))
(define 3A3soul2 (list (list -260 -20) (list -230 -550)
                       (list -170 -120) ))

(define 3A3test3 (list (list -1 -1) (list 0 0)
                       (list 1 1)))
(define 3A3vars3 (list 3.6 2.1))
(define 3A3soul3 (list (list -3.6 -2.1) (list 0 0)
                       (list 3.6 2.1) ))

(define 3A3test4 (list (list 2 3) (list 5 8)
                       (list 13 21) (list 34 55)))
(define 3A3vars4 (list -1 -7))
(define 3A3soul4 (list (list -2 -21) (list -5 -56)
                       (list -13 -147) (list -34 -385)))

(define 3A3test5 empty)
(define 3A3vars5 (list 6 5.6))
(define 3A3soul5 empty)

;; ** Tests for 3Aiv **
(define 3A4test1 (list (list 0.5 1) (list 1.5 2)
                       (list 2.5 3) (list 3.5 4)))
(define 3A4soul1 (list (list 0.5 1) (list 3.5 4)))

(define 3A4test2 (list (list 26 -2) (list 23 -55)
                       (list 17 -12)))
(define 3A4soul2 (list (list 17 -55) (list 26 -2)))

(define 3A4test3 (list (list -1 -1) (list 0 0)
                       (list 1 1)))
(define 3A4soul3 (list (list -1 -1) (list 1 1)))

(define 3A4test4 (list (list 2 3) (list 5 8)
                       (list 13 21) (list 34 55)))
(define 3A4soul4 (list (list 2 3) (list 34 55)))

(define 3A4test5 empty)
(define 3A4soul5 empty)


;; ** 3ai **


;; (get-x point) Given a point, produces the
;;  x value, or the first value in the list
;; Examples:
(check-expect (get-x (list 1 2)) 1)
(check-expect (get-x (list 34 0.2)) 34)

;; get-x: Point -> Num
;; requires: point is non-empty
(define (get-x point)
  (cond [(empty? point)   empty]
        [else     (first point)]))

;; Tests
(check-expect (get-x 3AItest1) (first 3AIsoul1))
(check-expect (get-x 3AItest2) (first 3AIsoul2))
(check-expect (get-x 3AItest3) (first 3AIsoul3))
(check-expect (get-x 3AItest4) (first 3AIsoul4))
(check-expect (get-x 3AItest5) (first 3AIsoul5))

  
;; (get-y point) Given a point, produces the
;;  y value, or the second value in the list
;; Examples:
(check-expect (get-y (list 1 2)) 2)
(check-expect (get-y (list 34 0.2)) 0.2)

;; get-y: Point -> Num
;; requires: point is non-empty
(define (get-y point)
  (cond [(empty? point)            empty]
        [(empty? (second point))   empty]
        [else             (second point)]))

;; Tests
(check-expect (get-y 3AItest1) (second 3AIsoul1))
(check-expect (get-y 3AItest2) (second 3AIsoul2))
(check-expect (get-y 3AItest3) (second 3AIsoul3))
(check-expect (get-y 3AItest4) (second 3AIsoul4))
(check-expect (get-y 3AItest5) (second 3AIsoul5))


;; ** 3Aii **


;; (translate-gesture gesture x-offset y-offset)
;;  Produces a new gesture from a given gesture
;;  (gesture) where each point's x is added to by
;;  the x offset and each point's y is added to by
;;  the y offset
;; Examples:
(check-expect (translate-gesture (list 1 2) 2 3) (list 3 5))
(check-expect (translate-gesture (list (list 1 1)
                                       (list 0.1 0.1)) 5 7)
                                     (list (list 6 8)
                                           (list 5.1 7.1)))

;; translate-gesture: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
(define (translate-gesture gesture x-offset y-offset)
  (cond [(empty? gesture)        empty]
        [(not (cons? (first gesture))) 
                     (list (+ (get-x gesture)
                              x-offset)
                           (+ (get-y gesture)
                              y-offset))]
        [else  (cons (list (+ (get-x (first gesture))
                              x-offset)
                           (+ (get-y (first gesture))
                              y-offset))
                     (translate-gesture (rest gesture)
                              x-offset y-offset))]))

;; Tests
(check-expect (translate-gesture 3A2test1 (first 3A2vars1)
                                          (second 3A2vars1))
              3A2soul1)
(check-expect (translate-gesture 3A2test2 (first 3A2vars2)
                                          (second 3A2vars2))
              3A2soul2)
(check-expect (translate-gesture 3A2test3 (first 3A2vars3)
                                          (second 3A2vars3))
              3A2soul3)
(check-expect (translate-gesture 3A2test4 (first 3A2vars4)
                                          (second 3A2vars4))
              3A2soul4)
(check-expect (translate-gesture 3A2test5 (first 3A2vars5)
                                          (second 3A2vars5))
              3A2soul5)


;; ** 3Aiii **


;; (scale-gesture gesture x-scale y-scale)
;;  Produces a new gesture from a given gesture
;;  (gesture) where each point's x is multiplied by
;;  the x-scale and each point's y is multiplied by
;;  the y-scale
;; Examples:
(check-expect (scale-gesture (list 1 2) 2 3) (list 2 6))
(check-expect (scale-gesture (list (list 1 1)
                                       (list 0.1 0.1)) 5 7)
                                     (list (list 5 7)
                                           (list 0.5 0.7)))

;; scale-gesture: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0 
(define (scale-gesture gesture x-scale y-scale)
  (cond [(empty? gesture)        empty]
        [(not (cons? (first gesture))) 
                     (list (* (get-x gesture)
                              x-scale)
                           (* (get-y gesture)
                              y-scale))]
        [else  (cons (list (* (get-x (first gesture))
                              x-scale)
                           (* (get-y (first gesture))
                              y-scale))
                     (scale-gesture (rest gesture)
                              x-scale y-scale))]))

;; Tests
(check-expect (scale-gesture 3A3test1 (first 3A3vars1)
                                          (second 3A3vars1))
              3A3soul1)
(check-expect (scale-gesture 3A3test2 (first 3A3vars2)
                                          (second 3A3vars2))
              3A3soul2)
(check-expect (scale-gesture 3A3test3 (first 3A3vars3)
                                          (second 3A3vars3))
              3A3soul3)
(check-expect (scale-gesture 3A3test4 (first 3A3vars4)
                                          (second 3A3vars4))
              3A3soul4)
(check-expect (scale-gesture 3A3test5 (first 3A3vars5)
                                          (second 3A3vars5))
              3A3soul5)


;; ** 3Aiv **

;; (get-b-box gesture) Produces a new BoundingBox
;;  from a given gesture (gesture)
;; Examples:
(check-expect (get-b-box (list (list 1 2)))
              (list (list 1 2) (list 1 2)))
(check-expect (get-b-box (list (list 1 1) (list 3 4)
                               (list 2 3)))
                         (list (list 1 1)(list 3 4)))

;; get-b-box: Gesture -> BB
;; requires: gesture is non-empty
(define (get-b-box gesture)
  (cond [(empty? gesture)        empty]
        [else
            (list (list (min-list gesture 'x)
                        (min-list gesture 'y))
                  (list (max-list gesture 'x)
                        (max-list gesture 'y)))]))

                    
;; Helper Function

                            
;; (max-list lop xory) Produces the max
;;   x or y value (xory) given a list of
;;   points (lop)
;; Examples:
(check-expect (max-list (list (list 1 2)) 'x) 1)
(check-expect (max-list (list (list 1 1) (list 3 4)
                               (list 2 3)) 'y) 4)
(check-expect (max-list empty 'y) 0)

;; max-list: Gesture Sym -> Num
;;  Requires: xory to be 'x or 'y
;;            lop is non-empty
(define (max-list lop xory)
    (cond [(empty? lop)                    0]
          [(symbol=? xory 'x)
            (cond [(= (length lop) 1)
                         (first (first lop))]
                  [else
                    (max (first (first lop))
                         (max-list (rest lop) xory))])]
          [else
            (cond [(= (length lop) 1)
                         (second (first lop))]
                  [else
                    (max (second (first lop))
                         (max-list (rest lop) xory))])]))


;; (min-list lop xory) Produces the min
;;   x or y value (xory) given a list of
;;   points (lop)
;; Examples:
(check-expect (min-list (list (list 1 2)) 'y) 2)
(check-expect (min-list (list (list 1 1) (list 3 4)
                               (list 2 3)) 'x) 1)
(check-expect (min-list empty 'x) 0)

;; min-list: Gesture Sym -> Num
;; requires: lop is non-empty
;;  Requires: xory to be 'x or 'y
(define (min-list lop xory)
    (cond [(empty? lop)                   0]        
          [(symbol=? xory 'x)
            (cond [(= (length lop) 1)
                         (first (first lop))]
                  [else
                    (min (first (first lop))
                         (min-list (rest lop) xory))])]
          [else
            (cond [(= (length lop) 1)
                         (second (first lop))]
                  [else
                    (min (second (first lop))
                         (min-list (rest lop) xory))])]))
                            
;; Tests
(check-expect (get-b-box 3A4test1) 3A4soul1)
(check-expect (get-b-box 3A4test2) 3A4soul2)
(check-expect (get-b-box 3A4test3) 3A4soul3)
(check-expect (get-b-box 3A4test4) 3A4soul4)
(check-expect (get-b-box 3A4test5) 3A4soul5)



;;
;;   Problem 3B(i-ii)
;;



;; ** Tests for 3Bi **
(define 3BItest1 (list (list 0.5 5)))
(define 3BIsoul1 0)

(define 3BItest2 (list (list 0 7) (list 7 0)))
(define 3BIsoul2 9.899)

(define 3BItest3 (list (list 1 2) (list 3 4)))
(define 3BIsoul3 2.83)

(define 3BItest4 (list (list 0.1 2) (list 3 0.4)))
(define 3BIsoul4 3.31)

(define 3BItest5 empty)
(define 3BIsoul5 0)

(define 3BItest6 (list (list 0.1 0.2) (list 0.3 0.4)
                       (list 0.5 0.6)))
(define 3BIsoul6 0.57)

(define 3BItest7 (list (list 12 4) (list 31 100)
                       (list 32 31)))
(define 3BIsoul7 166.87)

(define 3BItest8 (list (list 6.1 0) (list 2.4 1)
                       (list 0.01 3) (list 12 8)))
(define 3BIsoul8 19.94)

(define 3BItest9 (list (list 1.2 3.6) (list 3.1 7.7)
                       (list 1.2 36) (list 5.5 2.9)))
(define 3BIsoul9 66.26)

(define 3BItest10 (list (list 0 1) (list 2 3)
                       (list 4 5) (list 6 7)
                       (list 8 9)))
(define 3BIsoul10 11.31)


;; ** Tests for 3Bii **
(define temp1 (list (list 0.5 1) (list 1.5 2)
                    (list 2.5 3) (list 3.5 4)
                    (list 4.5 5) (list 5.5 6)))
(define temp2 (list (list 0 1) (list 2 3)
                    (list 4 5) (list 6 7)
                    (list 8 9)))
(define edge (list (list 1 2)))
  

(define 3B2test1 (list 0 0))
(define 3B2soul1 (list (list 1 2) (list 1 2)))

(define 3B2test2 (list 0 1 2))
(define 3B2soul2 (list (list 0.5 1) (list 1.5 2)
                 (list 2.5 3)))

(define 3B2test3 (list 0 4 5))
(define 3B2soul3 (list (list 0.5 1) (list 4.5 5)
                 (list 5.5 6)))

(define 3B2test4 (list 2 3 3 5))
(define 3B2soul4 (list (list 2.5 3) (list 3.5 4)
                 (list 3.5 4) (list 5.5 6)))

(define 3B2test5 (list 0 4))
(define 3B2soul5 (list (list 0 1) (list 8 9)))

(define 3B2test6 (list 1))
(define 3B2soul6 (list (list 2 3)))

(define 3B2test7 (list 4))
(define 3B2soul7 (list (list 8 9)))

(define 3B2test8 (list 1 2))
(define 3B2soul8 (list (list 2 3) (list 4 5)))

(define 3B2test9 (list 0 3))
(define 3B2soul9 (list (list 0 1) (list 6 7)))

(define 3B2test10 (list 0 0 2 2 4 4))
(define 3B2soul10 (list (list 0 1) (list 0 1)
                  (list 4 5) (list 4 5)
                  (list 8 9) (list 8 9)))


;; ** 3Bi **


;; (gesture-length gesture) Given a gesture (gesture)
;;   produces the sum of the distances between the x's
;;   index point and the (x-1)'s index point
;; Examples:
(check-within (gesture-length (list (list 1 2))) 0 tol)
(check-within (gesture-length (list (list 1 1) (list 3 4)
                               (list 2 3))) 5.019 tol)
(check-within (gesture-length empty) 0 tol)

;; gesture-length: Gesture -> Num
(define (gesture-length gesture)
  (cond [(empty? gesture)                         0]
        [(= (length gesture) 1)                   0]
        [else   (+ (distance-equa  (first gesture)
                                   (second gesture))
                   (gesture-length (rest gesture)))]))
             
;; Helper Function

;; (distance-equa point1 point2) Given 2 points
;;  (point1 and point2) finds the distance between
;;  them
;; Examples:
(check-within (distance-equa (list 0 0) (list 3 4)) 5 tol)
(check-within (distance-equa (list 1 1) (list 3 4)) 3.61 tol)

;; distance-equa: Point Point -> Num
;; requires: point1 and point2 are non-empty
(define (distance-equa point1 point2)
  (sqrt (+ (expt (- (first  point1) (first  point2)) 2)
           (expt (- (second point1) (second point2)) 2))))

;; Tests
(check-within (gesture-length 3BItest1) 3BIsoul1 tol)
(check-within (gesture-length 3BItest2) 3BIsoul2 tol)
(check-within (gesture-length 3BItest3) 3BIsoul3 tol)
(check-within (gesture-length 3BItest4) 3BIsoul4 tol)
(check-within (gesture-length 3BItest5) 3BIsoul5 tol)
(check-within (gesture-length 3BItest6) 3BIsoul6 tol)
(check-within (gesture-length 3BItest7) 3BIsoul7 tol)
(check-within (gesture-length 3BItest8) 3BIsoul8 tol)
(check-within (gesture-length 3BItest9) 3BIsoul9 tol)
(check-within (gesture-length 3BItest10) 3BIsoul10 tol)

  
;; ** 3Bii **

  
;; (get-points g lon) Given a gesture (g)
;;   and a list of naturals (lon), produces a new list
;;   with all the points at each natural's index
;; Examples:
(check-expect (get-points (list (list 1 2)) (list 0))
                          (list (list 1 2)))
(check-expect (get-points (list (list 1 1) (list 3 4)
                                (list 2 3)) (list 1 2))
                          (list (list 3 4) (list 2 3)))
(check-expect (get-points empty 2) empty)

;; get-points: Gesture (listof Nat) -> Gesture
;; requires: gesture is non-empty,
;;           lon is non-empty and
;;           each element of lon is
;;            1) >= 0
;;            2) Non decreasing
;;            3) < the length of gesture
  
(define (get-points g lon)
  (cond [(empty? g)                  empty]
        [(empty? lon)                      empty]
        [(= (first lon) 0)
          (cons (first g)
                (get-points g        (rest lon)))]
        [else   (get-points (rest g) (1down lon))]))
             
;; Helper Function

;; (1down lon) Given a list of numbers (lon) will
;;  subtract each element by 1 and return the new list
;;  them
;; Examples:
(check-expect (1down (list 3 2 3 2 4 5 6 3))
                     (list 2 1 2 1 3 4 5 2))
(check-expect (1down (list 1 1 3 4 1 1 1 2))
                     (list 0 0 2 3 0 0 0 1))

;; 1down: (listof Num) -> (listof Num)
(define (1down lon)
  (cond [(empty? lon)             empty]
        [else (cons (sub1  (first lon))
                    (1down (rest lon)))]))

;; Tests
(check-expect (get-points edge 3B2test1) 3B2soul1)
(check-expect (get-points temp1 3B2test2) 3B2soul2)
(check-expect (get-points temp1 3B2test3) 3B2soul3)
(check-expect (get-points temp1 3B2test4) 3B2soul4)
(check-expect (get-points temp2 3B2test5) 3B2soul5)
(check-expect (get-points temp2 3B2test6) 3B2soul6)
(check-expect (get-points temp2 3B2test7) 3B2soul7)
(check-expect (get-points temp2 3B2test8) 3B2soul8)
(check-expect (get-points temp2 3B2test9) 3B2soul9)
(check-expect (get-points temp2 3B2test10) 3B2soul10)



;;
;;   Problem 3c(i-v)
;;



;; ** 3Ci**

;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2)
                    (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2)
                                 (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6)
                                (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5)
                    (list 7 7) (list 8 8)))
(check-expect (five-sample  empty) empty)

;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample gesture)
  (cond [(empty? gesture)  empty]
        [else
          (get-points             gesture
                      (get-indexs gesture))]))


;; Helper Function


;; (get-indexs gesture) Given a gesture (gesture) will
;;  find the indexs to split the gesture into 5 points.
;; Examples:
(check-expect (get-indexs (list (list 1 1)))
                     (list 0 0 0 0 0))
(check-expect (get-indexs (list (list 1 1) (list 2 2)))
                     (list 0 0 1 1 1))
(check-expect (get-indexs  empty) empty)
                    
;; get-indexs: gesture -> (listof Num)
(define (get-indexs  gesture)
  (cond [(empty?  gesture)             empty]
        [else
             (list 0 (floor (* 0.25 (length gesture)))
                     (floor (* 0.5 (length gesture)))
                     (floor (* 0.75 (length gesture)))
                     (sub1 (length gesture)))]))
                                          

;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2)
                                 (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3)
                    (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1)
                    (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2)
                                 (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3)
                    (list 4 4) (list 5 5)))


;; ** 3Cii **

;;(move-and-scale gesture x-scale y-scale) moves gesture
;;  to (0, 0) and scales it by (x-scale)x (y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1)
              (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture x-scale y-scale)
    (scale-gesture
          (centeraround0 gesture
               (get-x (first (get-b-box gesture)))
               (get-y (first (get-b-box gesture))))
          x-scale y-scale))
                        
;; Helper Function

;;(centeraround0 gesture boundx boundy) moves gesture
;; (gesture) to (0, 0)
;; Examples:
(check-expect (centeraround0 (list (list 1 1)) 1 1)
              (list (list 0 0)))
(check-expect (centeraround0 (list (list 1 5) (list 3 4)) 1 4)
              (list (list 0 1) (list 2 0)))

;; centeraround0: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
(define (centeraround0 gesture minx miny)
  (cond [(empty? gesture)              empty]
        [else
         (cons (list (- (first  (first gesture))
                         minx)
                     (- (second (first gesture))
                         miny))
               (centeraround0 (rest gesture)
                              minx miny))]))

;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 30 2)
              (list (list 90 6) (list 0 0)))


;; ** 3Ciii **

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to
;; (0,0) and a standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0)
                                       (list 100 100)))
              (list (list 0 0) (list 200 200)) tol)
(check-within (normalize-gesture (list (list 100 0)
                                       (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) tol)
(check-within (normalize-gesture empty) empty tol)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture gesture)
    (cond [(empty? gesture)   empty]
          [(less-minwidth gesture)
             (move-and-scale gesture
                  1
                  (/ norm-size (- (get-y (second
                                         (get-b-box gesture)))
                                  (get-y (first
                                         (get-b-box gesture))))))]
          [(less-minheight gesture)
             (move-and-scale gesture
                  (/ norm-size (- (get-x (second
                                         (get-b-box gesture)))
                                  (get-x (first
                                         (get-b-box gesture)))))
                  1)]
          [else
              (move-and-scale gesture
                  (/ norm-size (- (get-x (second
                                         (get-b-box gesture)))
                                  (get-x (first
                                         (get-b-box gesture)))))
                  (/ norm-size (- (get-y (second
                                         (get-b-box gesture)))
                                  (get-y (first
                                         (get-b-box gesture))))))]))  
                
                  
;; Helper Function
           
;;(less-minheight gesture) produces if the
;; difference between the max and min height
;; of the (gesutre) is less then the min-height
;; Examples:
(check-expect (less-minheight (list (list 80 0)
                                    (list 100 100)))  false)
             
(check-expect (less-minheight (list (list 100 30) (list 100 50)
                                    (list 200 50)))   true)
                          
;; less-minheight: Gesture -> Bool
;; requires: gesture is non-empty
(define (less-minheight gesture)
    (> min-height
       (- (max-list gesture 'y)
          (min-list gesture 'y))))

;;(less-minwidth gesture) produces if the
;; difference between the max and min width
;; of the (gesutre) is less then the min-width
;; Examples:
(check-expect (less-minwidth (list (list 80 0)
                                    (list 100 100)))  true)
             
(check-expect (less-minwidth (list (list 100 30) (list 100 50)
                                    (list 200 50)))  false)
                          
;; less-minwidth: Gesture -> Bool
;; requires: gesture is non-empty
(define (less-minwidth gesture)
    (> min-width
       (- (max-list gesture 'x)
          (min-list gesture 'x))))

;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) tol)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) tol)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) tol)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) tol)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) tol)


;; ** 3Civ **

;;(geometric-5match gesture1 gesture2) produces the
;;  average distance between points in sub-sampled gesture1
;;  and gesture2 after sub-sampling them with 5 points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30)
                     (list 40 40) (list 40 40)))
               16.16 tol)
(check-within (geometric-5match (second (fourth templates))
                                (second (fourth templates))) 0 tol)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are
;;      each not both vertical and horizontal
;;      and gestures are non empty
(define (geometric-5match gesture1 gesture2)
   (/ (diff-normalized (normalize-gesture (five-sample gesture1))
                       (normalize-gesture (five-sample gesture2)))   
      5))

;; Helper Function
           
;;(diff-normalized gesture1 gesture2) produces the total
;;  difference between each gesture (gesture1 and
;;  gesture2) once they are both normalized and scaled
;; Examples:
(check-within (diff-normalized (list (list 1 2)) empty) 0 tol)
(check-within (diff-normalized empty (list (list 1 2))) 0 tol)
(check-within (diff-normalized (list (list 1 2))
                               (list (list 1 2))) 0 tol)
(check-within (diff-normalized (list (list 1 2))
                               (list (list 1 3))) 1 tol)

;; diff-normalized: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are
;;      each not both vertical and horizontal
;;      and gestures are non empty
(define (diff-normalized gesture1 gesture2)
  (cond [(empty? gesture1) 0]
        [(empty? gesture2) 0]
        [else  (+ (gesture-length (list (first gesture1)
                                        (first gesture2)))
                  (diff-normalized (rest gesture1)
                                   (rest gesture2)))]))


  
;; Tests:
(check-within (geometric-5match (second (first templates))
                                (second (second templates)))
                                133.25 tol)
(check-within (geometric-5match (second (third templates))
                                (second (fifth templates)))
                                68.86 tol)
(check-within (geometric-5match (second (seventh templates))
                                (second (seventh templates)))
                                0 tol)


;; 3cv)

;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)


;; five-point-rec: Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec candidate template-library)
  (cond  [(empty? template-library) 'none]
         [(< (geometric-5match candidate
                (second (first template-library))) 0.2)
                (first (first template-library))]
         [else
                (five-point-rec candidate
                                (rest template-library))]))
                            
;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)



;;
;;   Problem 3d(i-iii)
;;


;; 3di)

;;(sub-sample gesture k) produces a sampling of gesture k points
;;  the first, 1/(k-1), 2/(k-1) .... (k-2)/(k-1) and last point.
;; Examples:
(check-within (sub-sample (list (list 1 1) (list 2 2)) 5)
              (list (list 1 1) (list 1 1) (list 2 2)
                    (list 2 2) (list 2 2)) tol)
(check-within (sub-sample empty 3) empty tol)
(check-within (sub-sample (list 1 1) 1) empty tol)

;; sub-sample: Gesture Nat -> Gesture
;; requires: gesture is non-empty
;;           k > 2
(define (sub-sample gesture k)
  (cond [(empty? gesture) empty]
        [(<= k 2)         empty]
        [else (get-points gesture
              (get-nth-indexs gesture k 0))]))

;; Helper Functions:

;;(get-nth-indexs gesture k i) produces a list of numbers
;; which represents a k sampleing of the gesture,
;; which uses an iterater (i) to the list that has
;; the first value, (2:(k-2)) and the last point

(check-within (get-nth-indexs (list (list 1 1) (list 2 2)
                                 (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6)
                                (list 7 7) (list 8 8)) 3 0)
              (list 0 4 7) tol)
(check-within (get-nth-indexs (list (list 1 1) (list 2 2)
                                 (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6)
                                (list 7 7) (list 8 8)) 4 0)
              (list 0 2 5 7) tol)

;; sub-sample: Gesture Nat Nat -> Gesture
;; requires: gesture is non-empty
;;           k > 2, i >= 0
(define (get-nth-indexs gesture k i)
  (cond [(= i (sub1 k))  (cons (sub1 (length
                                gesture)) empty)]
        [(= i 0) (cons 0
                    (get-nth-indexs gesture
                                    k (add1 i)))]
        [else    (cons (floor (* (/ i (sub1 k))
                                 (length gesture)))
                    (get-nth-indexs gesture
                                    k (add1 i)))]))

;; Tests:
(check-within (sub-sample (list (list 1 1) (list 2 2)
                                 (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6)
                                (list 7 7) (list 8 8)) 3)
              (list (list 1 1) (list 5 5) (list 8 8)) tol)
(check-within (sub-sample testa 6)
              (list (list 112 309) (list 164 135) (list 190 80)
                    (list 243 129) (list 308 274) (list 330 323))
                     tol)
(check-within (sub-sample testa 9)
              (list (list 112 309) (list 145 209) (list 171 110)
                    (list 186 86) (list 208 63) (list 246 138)
                    (list 295 239) (list 315 291) (list 330 323))
                     tol)
(check-within (sub-sample testd 7)
              (list (list 107 25.5) (list 112 208.5)
                    (list 113 173.5) (list 131 38.5)
                    (list 261 54.5) (list 248 #i241.5)
                    (list 136 #i267.5)) tol)


;; 3dii)

;;(geometric-match gesture1 gesture2 k) produces the
;;  average distance between points in sub-sampled gesture1
;;  and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30)
                     (list 40 40) (list 40 40)) 5)
               16.16 tol)
(check-within (geometric-match  (second (third templates))
                                (second (fifth templates)) 5)
                                68.86 tol)


;; geometric-match: Gesture Gesture Nat -> Num
;; requires: gesture1 and gesture2 are
;;      each not both vertical and horizontal
;;      and gestures are non empty
;;      k > 2

(define (geometric-match gesture1 gesture2 k)
   (/ (diff-normalized (normalize-gesture (sub-sample gesture1 k))
                       (normalize-gesture (sub-sample gesture2 k)))   
      k))

;; Tests:
(check-within (geometric-match  (second (third templates))
                                (second (third templates)) 7)
                                0 tol)
(check-within (geometric-match  (second (fifth templates))
                                (second (fifth templates)) 12)
                                0 tol)

;; 3diii)

;;(k-point-rec candidate template-library k) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (k-point-rec testd templates 10) 'd)
(check-expect (k-point-rec testk templates 10) 'k)
(check-expect (k-point-rec testa templates 10) 'a)

;; k-point-rec: Gesture TL Nat -> Sym
;; requires: candidate is not both vertical and horizontal
;;           k > 2
(define (k-point-rec candidate template-library k)
  (cond  [(empty? template-library) 'none]
         [(< (geometric-match candidate
                (second (first template-library)) k) threshhold)
                (first (first template-library))]
         [else
                (k-point-rec candidate
                                (rest template-library) k)]))
;; Tests:
(check-expect (k-point-rec (second (fifth templates))
                           templates 10) 'e)
(check-expect (k-point-rec (second (first (rest (rest (rest (rest
                           (rest (rest (rest (rest (rest
                           (rest (rest templates)))))))))))))
                           templates 10) 'l)
(check-expect (k-point-rec (second (first (rest (rest (rest (rest
                           (rest (rest templates))))))))
                           templates 10) 'g)


;;
;;   Problem 4i)
;;


;;(spatial-sub-sample origin k) produces a new
;;  gesture that contains k points, (easier said then done...)
;; Examples:

(define perfect-square(list (list 50 100) (list 250 100)
(list 250 300) (list 50 300) (list 50 100)))

(check-within (spatial-sub-sample perfect-square 4)
(list (list 50 100) (list 250 166.66) (list 116.66 300)
(list 50 100)) tol)
(check-within (spatial-sub-sample perfect-square 5)
              perfect-square tol)

;; spatial-sub-sample: Gesture Nat -> Gesture
(define (spatial-sub-sample origin k)
      (cons (first origin)
            (gen-point-list (first origin)
                            (rest origin)
                            (/ (gesture-length origin) (- k 1))
                            (/ (gesture-length origin) (- k 1)))))
;; Tests:

(check-within (spatial-sub-sample (list (list 0 0) (list 10 20)) 5)
              (list (list 0 0) (list 2.5 5.0) (list 5.0 10.0)
                    (list 7.5 15.0) (list 10.0 20.0)) tol)
(check-within (spatial-sub-sample (list (list 0 0) (list 10 20)) 3)
              (list (list 0 0) (list 5.0 10.0)
              (list #i10.0 #i20.0)) tol)
(check-within (spatial-sub-sample perfect-square 3)
              (list (list 50 100) (list 250 300)
              (list 50 100)) tol)
(check-within (spatial-sub-sample perfect-square 11)
              (list (list 50 100) (list 130 100) (list 210 100)
                    (list 250 140) (list 250 220) (list 250 300)
                    (list 170 300) (list 90 300) (list 50 260)
                    (list 50 180) (list 50 100)) tol)


         

;; *** Helper Functions ***

;;(gen-point-list cur_point origin n i) Checks to see if 
;;  the distance between each current point (cur_point) and the
;;  origonal function (origin) is less then the iterater to create
;;  a list of k points disclusive of t.
;; Examples:

(check-within (gen-point-list (list 0 0) (list (list 0 0)
                                               (list 0 10))
                                               (/ 10 9)
                                               (/ 10 9))
 (list (list 0 1.11) (list 0 2.22)
       (list 0 3.33) (list 0 4.44) (list 0 5.55) (list 0 6.66)
       (list 0 7.77) (list 0 8.88) (list 0 10)) tol)

(check-within (gen-point-list (list 0 0)
            (list (list 0 0) (list 10 10))
            (/ (gesture-length (list (list 0 0) (list 10 10))) 9)
            (/ (gesture-length (list (list 0 0) (list 10 10))) 9))
 (list (list 1.11 1.11) (list 2.22 2.22)
       (list 3.33 3.33) (list 4.44 4.44) (list 5.55 5.55)
       (list 6.66 6.66) (list 7.77 7.77) (list 8.88 8.88)
       (list 10 10)) tol)

;; gen-point-list: Point Gesture Nat Nat  -> Gesture
(define (gen-point-list cur_point origin n i)
  (cond [(empty? origin)   empty]
        [(<=  i (distance-equa cur_point (first origin)))
           (cons (findnewpoint cur_point (first origin) i)
           (gen-point-list (findnewpoint cur_point (first origin) i)
                    origin n n))]
                 
        [else
           (gen-point-list (first origin) (rest origin)
                               n (- i (distance-equa cur_point
                                                 (first origin))))]))

;;(findnewpoint pointa pointb k) Given two points (pointa and pointb)
;;  and a distance (k), generates a new point that is along
;;  the line that bisects both the points
;; Examples:
(check-within (findnewpoint (list 0 0) (list 0 1) 10) (list 0 10) tol)
(check-within (findnewpoint (list 0 0) (list 0 1) 10) (list 0 10) tol)

;; findnewpoint Point Point Num -> Point
(define (findnewpoint pointa pointb k)
   (list
         (+ (first pointa)
         (* (/ (- (first pointb) (first pointa))
               (gesture-length (list pointa pointb))) k))
         (+ (second pointa)
         (* (/ (- (second pointb) (second pointa))
               (gesture-length (list pointa pointb))) k))))

;;
;;   Problem 4ii)
;;

;;(spatial-rec candidate template-library k) produces the symbol in
;;  template-library closest to candidate, after converting the
;;  candiate into a new gesture with k points

;; Examples:
(check-expect (spatial-rec testd templates 30) 'd)
(check-expect (spatial-rec testk templates 20) 'k)
(check-expect (spatial-rec testa templates 10) 'a)

;; spatial-rec: Gesture TL Nat -> Sym
;; requires: candidate is not both vertical and horizontal
;;           k > 2
(define (spatial-rec candidate template-library k)
  (cond  [(empty? template-library) 'none]
         [(< (geometric-match-spatial candidate
                (second (first template-library)) k) threshhold)
                (first (first template-library))]
         [else
                (spatial-rec candidate
                                (rest template-library) k)]))
;; Tests:
(check-expect (spatial-rec (second (fifth templates))
                           templates 10) 'e)
(check-expect (spatial-rec (second (first (rest (rest (rest (rest
                           (rest (rest (rest (rest (rest
                           (rest (rest templates)))))))))))))
                           templates 7) 'l)
(check-expect (spatial-rec (second (first (rest (rest (rest (rest
                           (rest (rest templates))))))))
                           templates 10) 'g)  

;; helpers
;;(geometric-match-spatial gesture1 gesture2 k) produces the
;;  average distance between points in sub-sampled gesture1
;;  and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-match-spatial
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30)
                     (list 40 40) (list 40 40)) 5)
               0 tol)
(check-within (geometric-match-spatial  (second (third templates))
                                (second (fifth templates)) 5)
                                48.56 tol)


;; geometric-match-spatial: Gesture Gesture Nat -> Num
;; requires: gesture1 and gesture2 are
;;      each not both vertical and horizontal
;;      and gestures are non empty
;;      k > 2

(define (geometric-match-spatial gesture1 gesture2 k)
   (/ (diff-normalized (normalize-gesture (spatial-sub-sample
                                           gesture1 k))
                       (normalize-gesture (spatial-sub-sample
                                           gesture2 k)))   
      k))
                               
