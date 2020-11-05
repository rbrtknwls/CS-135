;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname not-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 05, Problem 1 (A-B)
;; ******************************************

;; ** Structs **


(define-struct ls (first rest))
;; A (lsof X) is one of:
;; * 'nothing
;; * (make-ls X (lsof X))


;; ** Test Cases **


;; 1A Tests


;; Empty Test Cases
(define test1 'nothing)
(define soul1 0)

(define test2 (make-ls 'nothing 'nothing))
(define soul2 1)

;; General Test Cases

(define test3 (make-ls 3 (make-ls 'Nothing 'nothing)))
(define soul3 2)

(define test4 (make-ls -1 (make-ls empty 'nothing)))
(define soul4 2)

(define test5 (make-ls -1 (make-ls 'hello
              (make-ls empty 'nothing))))
(define soul5 3)

(define test6 (make-ls empty (make-ls 'hello
              (make-ls empty 'nothing))))
(define soul6 3)

(define test7 (make-ls empty (make-ls 'hello
              (make-ls empty (make-ls empty 'nothing)))))
(define soul7 4)


;; 1B Tests


;; One Number Test Cases
(define test8 (make-ls 0 'nothing))
(define soul8 0)

(define test9 (make-ls 1 'nothing))
(define soul9 1)

;; General Test Cases

(define test10 (make-ls 3 (make-ls 2 'nothing)))
(define soul10 3)

(define test11 (make-ls -1 (make-ls -5 'nothing)))
(define soul11 -1)

(define test12 (make-ls 2.2 (make-ls 3.1
              (make-ls -0 'nothing))))
(define soul12 3.1)

(define test13 (make-ls 0 (make-ls -0
              (make-ls 0.0 'nothing))))
(define soul13 0)

(define test14 (make-ls 3 (make-ls 2
              (make-ls 1.1 (make-ls 4.3 'nothing)))))
(define soul14 4.3)

(define test15 (make-ls -1 (make-ls 0
              (make-ls 1 (make-ls 2 'nothing)))))
(define soul15 2)


;;
;;   Problem 1A
;;


;; (ls-length lsof) Consumes our new list
;;   data-definition (lsof) and produces
;;   the number of elements in it
;; Examples:
(check-expect (ls-length (make-ls "!" (make-ls 'huh
                         (make-ls 42 'nothing)))) 3)
(check-expect (ls-length (make-ls "!"
                         (make-ls 42 'nothing))) 2)


;; ls-length: (lsof Any) -> Int
(define (ls-length lsof)
  (cond [(ls? lsof)
                  (+ 1 (ls-length (ls-rest lsof)))]
        [else
                                                 0]))


;; Empty Test
(check-expect (ls-length test1) soul1)
(check-expect (ls-length test2) soul2)
;; General Test
(check-expect (ls-length test3) soul3)
(check-expect (ls-length test4) soul4)
(check-expect (ls-length test5) soul5)
(check-expect (ls-length test6) soul6)
(check-expect (ls-length test7) soul7)


;;
;;   Problem 1B
;;


;; (ls-max lsof) Consumes our new list
;;   data-definition (lsof) and produces
;;   the max of the elements in it
;; Examples:
(check-expect (ls-max (make-ls 5 (make-ls 9
                      (make-ls 7 'nothing)))) 9)
(check-expect (ls-max (make-ls -1 (make-ls 3
                      (make-ls 3.1 'nothing)))) 3.1)

;; ls-max: (lsof Num) -> Int
;; Requires: (ls-length lsof) >= 1
(define (ls-max lsof)
  (cond [(= (ls-length lsof) 1)
                                            (ls-first lsof)]
        [else
              (max (ls-first lsof) (ls-max (ls-rest lsof)))]))

;; Empty Test
(check-expect (ls-max test8) soul1)
(check-expect (ls-max test9) soul2)
;; General Test
(check-expect (ls-max test10) soul10)
(check-expect (ls-max test11) soul11)
(check-expect (ls-max test12) soul12)
(check-expect (ls-max test13) soul13)
(check-expect (ls-max test14) soul14)