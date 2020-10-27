;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mean-relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 02, Problem 6
;; ******************************************

;; Test Cases

(define test1 (cons 5 empty))
(define test2 (cons 4 (cons 6 empty)))
(define test3 (cons 1 (cons 3 (cons 5 empty))))

(define test4 (cons 2 (cons 2 (cons 4 empty))))
(define test5 empty)
(define test6 (cons 0 (cons 0 (cons 6 empty))))
(define test7 (cons 1 (cons 2 (cons 3 empty))))
(define test8 (cons 3 (cons 2 (cons 1 empty))))

(define test9 (cons 0 (cons 0 empty)))
(define test10 (cons 2 (cons 2 empty)))
(define test11 (cons -2 (cons -2 (cons -2 empty))))

(define test12 (cons -1 (cons 2 (cons 3 empty))))
(define test13 (cons -3 (cons 5 (cons 1 empty))))
(define test14 (cons -1 (cons -5 (cons -3 empty))))
(define test15 (cons -3 (cons 6 (cons -3 empty))))


;; (mean-relative lon) Produces a new list
;;   based on the list of numbers (lon) that
;;   tells if each element is below above
;;   or at the mean
;; Examples:
(check-expect (mean-relative test1) (cons 'mean empty))
(check-expect (mean-relative test2)
              (cons 'below-mean (cons 'above-mean empty)))
(check-expect (mean-relative test3) (cons 'below-mean
              (cons 'mean (cons 'above-mean empty))))

;; mean-relative: (listof Num) -> (List of Sym)
(define (mean-relative lon)
  (cond [(empty? lon)
                 empty]
        [else
                (return-mean-relative lon (mean lon (length lon)))]))


;; Basic Tests
(check-expect (mean-relative test4) (cons 'below-mean
                 (cons 'below-mean (cons 'above-mean empty))))
(check-expect (mean-relative test6) (cons 'below-mean
                 (cons 'below-mean (cons 'above-mean empty))))
(check-expect (mean-relative test7) (cons 'below-mean
              (cons 'mean (cons 'above-mean empty))))
(check-expect (mean-relative test8) (cons 'above-mean
              (cons 'mean (cons 'below-mean empty))))

;;Empty List Test:
(check-expect (mean-relative test5) empty)

;;Equal Lists Tests:
(check-expect (mean-relative test9)
              (cons 'mean (cons 'mean empty)))
(check-expect (mean-relative test10)
              (cons 'mean (cons 'mean empty)))
(check-expect (mean-relative test11) (cons 'mean
              (cons 'mean (cons 'mean empty))))

;;Negitive Element List Tests:
(check-expect (mean-relative test12) (cons 'below-mean
              (cons 'above-mean (cons 'above-mean empty))))
(check-expect (mean-relative test13) (cons 'below-mean
              (cons 'above-mean (cons 'mean empty))))
(check-expect (mean-relative test14) (cons 'above-mean
              (cons 'below-mean (cons 'mean empty))))
(check-expect (mean-relative test15) (cons 'below-mean
              (cons 'above-mean (cons 'below-mean empty))))

;;***** Helper Functions *****



;; (mean sum length) Finds the sum of a given
;;   list of numbers (lon) using the length of
;;   the same list (length)
;; Example:
(check-expect (mean test1 (length test1)) 5)
(check-expect (mean test2 (length test2)) 5)
(check-expect (mean test3 (length test3)) 3)

;; mean (listof Num) -> Num
(define (mean lon length)
  (cond [(empty? lon)      0]
        [else          (+ (/ (first lon) length)
                          (mean (rest lon) length))]))


;; (return-mean-relative: lon aver) Produces a new list
;;   based on the list of numbers (lon) that
;;   tells if each element is below above
;;   or at the mean given by the variable (aver)
;; Example:
(check-expect (return-mean-relative (cons 5 empty) 5)
                 (cons 'mean empty))
(check-expect (return-mean-relative (cons 5 (cons 15 empty)) 10)
                 (cons 'below-mean (cons 'above-mean empty)))

;; return-mean-relative: (listof Num) -> (List of Sym)
(define (return-mean-relative lon aver)
  (cond [(empty? lon) empty]
        [(= (first lon) aver)
          (cons 'mean (return-mean-relative (rest lon) aver))]
        [(> (first lon) aver)
          (cons 'above-mean (return-mean-relative (rest lon) aver))]
        [(< (first lon) aver)
          (cons 'below-mean (return-mean-relative (rest lon) aver))]))


                        