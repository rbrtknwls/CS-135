;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 03, Problem 1(A-C)
;; ******************************************

;; ** Test Cases **

;; 1A Test Cases (General Tests)
(define 1Atest1var1 "apple") (define 1Atest1var2 "orange")
(define 1Atest1     (cons "apple" empty))
(define 1Asolution1 (cons "orange" empty))

(define 1Atest2var1 "pen") (define 1Atest2var2 "pencil")
(define 1Atest2     (cons "pens" empty))
(define 1Asolution2 (cons "pens" empty))

(define 1Atest3var1 "Jack") (define 1Atest3var2 "Jill")
(define 1Atest3
        (cons "Jack" (cons "Jack Jack" empty)))
(define 1Asolution3
        (cons "Jill" (cons "Jack Jack" empty)))

(define 1Atest4var1 "abc") (define 1Atest4var2 "efg")
(define 1Atest4
        (cons "abcde" (cons "abcd" (cons "abc" empty))))
(define 1Asolution4
        (cons "abcde" (cons "abcd" (cons "efg" empty))))

(define 1Atest5var1 "hello.") (define 1Atest5var2 "Hello!!")
(define 1Atest5
        (cons "hello." (cons "Hello?" (cons "hello" empty))))
(define 1Asolution5
        (cons "Hello!!" (cons "Hello?" (cons "hello" empty))))

;; 1A Test Case (Empty Lists)
(define 1Atest6var1 "Calc") (define 1Atest6var2 "No")
(define 1Atest6
        empty)
(define 1Asolution6
        empty)

  
;; 1A Test Case (No Replace/All Replace)
(define 1Atest7var1 "Name") (define 1Atest7var2 "Robbie")
(define 1Atest7
        (cons "Name?" empty))
(define 1Asolution7
        (cons "Name?" empty))

(define 1Atest8var1 "Name") (define 1Atest8var2 "Robbie")
(define 1Atest8
        (cons "Name" empty))
(define 1Asolution8
        (cons "Robbie" empty))

(define 1Atest9var1 "Name") (define 1Atest9var2 "Robbie")
(define 1Atest9
        (cons "What" (cons "is" (cons "your" (cons "name?" empty)))))
(define 1Asolution9
        (cons "What" (cons "is" (cons "your" (cons "name?" empty)))))

(define 1Atest10var1 "Name") (define 1Atest10var2 "Robbie")
(define 1Atest10
        (cons "Name" (cons "Name" (cons "Name"
                                  (cons "Name" empty)))))
(define 1Asolution10
        (cons "Robbie" (cons "Robbie" (cons "Robbie"
                                      (cons "Robbie" empty)))))


;; 1B Test Cases (General Tests)
(define 1Btest1var1 1) (define 1Btest1var2 1)
(define 1Bsolution1 2)

(define 1Btest2var1 100) (define 1Btest2var2 20)
(define 1Bsolution2 120)

(define 1Btest3var1 20) (define 1Btest3var2 100)
(define 1Bsolution3 120)

(define 1Btest4var1 7) (define 1Btest4var2 1)
(define 1Bsolution4 8)

(define 1Btest5var1 2) (define 1Btest5var2 12)
(define 1Bsolution5 14)

;; 1B Test Case (Empty Lists)
(define 1Btest6var1 0) (define 1Btest6var2 1)
(define 1Bsolution6 1)

(define 1Btest7var1 1) (define 1Btest7var2 0)
(define 1Bsolution7 1)

(define 1Btest8var1 0) (define 1Btest8var2 0)
(define 1Bsolution8 0)

(define 1Btest9var1 761) (define 1Btest9var2 0)
(define 1Bsolution9 761)

(define 1Btest10var1 0) (define 1Btest10var2 761)
(define 1Bsolution10 761)


;; 1C Test Cases (General Tests)
(define 1Ctest1var1 1) (define 1Ctest1var2 2)
(define 1Csolution1 2)

(define 1Ctest2var1 100) (define 1Ctest2var2 20)
(define 1Csolution2 2000)

(define 1Ctest3var1 20) (define 1Ctest3var2 100)
(define 1Csolution3 2000)

(define 1Ctest4var1 1) (define 1Ctest4var2 1)
(define 1Csolution4 1)

(define 1Ctest5var1 7) (define 1Ctest5var2 7)
(define 1Csolution5 49)

;; 1C Test Case (Empty Lists)
(define 1Ctest6var1 0) (define 1Ctest6var2 1)
(define 1Csolution6 0)

(define 1Ctest7var1 1) (define 1Ctest7var2 0)
(define 1Csolution7 0)

(define 1Ctest8var1 0) (define 1Ctest8var2 0)
(define 1Csolution8 0)

(define 1Ctest9var1 21) (define 1Ctest9var2 0)
(define 1Csolution9 0)

(define 1Ctest10var1 0) (define 1Ctest10var2 21)
(define 1Csolution10 0)


;;
;;   Problem 1A
;;


;; (replace-word indexstr replacstr strlist) Produces a
;;   new list in which all occurrences in a string list
;;   (strlist) of a string (indexstr) are replaced by
;;   a different string (replacstr)
;; Examples:
(check-expect
 (replace-word "exam" "assessment"
               (cons "content"
                     (cons "exam"
                           (cons "assignment" empty))))
 (cons "content" (cons "assessment" (cons "assignment" empty))))
(check-expect
 (replace-word "oranges" "apples"
               (cons "oranges"
                     (cons "orang"
                           (cons "apples" empty))))
 (cons "apples" (cons "orang" (cons "apples" empty))))

;; replace-word: Str Str (listof Str) -> (listof Str)
(define (replace-word indexstr replacstr strlist)
  (cond [(empty? strlist) empty]
        [(cons? strlist)
           (cond [(string=? (first strlist) indexstr)
                    (cons replacstr
                          (replace-word indexstr replacstr
                          (rest strlist)))]
                 [else
                    (cons (first strlist)
                          (replace-word indexstr replacstr
                          (rest strlist)))])]))

;; Basic Tests
(check-expect (replace-word 1Atest1var1 1Atest1var2 1Atest1)
              1Asolution1)
(check-expect (replace-word 1Atest2var1 1Atest2var2 1Atest2)
              1Asolution2)
(check-expect (replace-word 1Atest3var1 1Atest3var2 1Atest3)
              1Asolution3)
(check-expect (replace-word 1Atest4var1 1Atest4var2 1Atest4)
              1Asolution4)
(check-expect (replace-word 1Atest5var1 1Atest5var2 1Atest5)
              1Asolution5)

;; Empty List Test
(check-expect (replace-word 1Atest6var1 1Atest6var2 1Atest6)
              1Asolution6)

;; Multiple Replacements / Zero Replacements
(check-expect (replace-word 1Atest7var1 1Atest7var2 1Atest7)
              1Asolution7)
(check-expect (replace-word 1Atest8var1 1Atest8var2 1Atest8)
              1Asolution8)
(check-expect (replace-word 1Atest9var1 1Atest9var2 1Atest9)
              1Asolution9)
(check-expect (replace-word 1Atest10var1 1Atest10var2 1Atest10)
              1Asolution10)


;;
;;   Problem 1B
;;


;; (add num1 num2) Produces the the result of adding
;;   one number (num1) to another number (num2)
;; Examples:
(check-expect (add 2 3) 5)
(check-expect (add 2 7) 9)

;; add: Int Int -> Int
;; requires:
;;      num1 >= 0
;;      num2 >= 0
(define (add num1 num2)
  (cond [(= num2 0) num1]
        [else (add (add1 num1) (sub1 num2))]))

;; Basic Tests
(check-expect (add 1Btest1var1 1Btest1var2)
              1Bsolution1)
(check-expect (add 1Btest2var1 1Btest2var2)
              1Bsolution2)
(check-expect (add 1Btest3var1 1Btest3var2)
              1Bsolution3)
(check-expect (add 1Btest4var1 1Btest4var2)
              1Bsolution4)
(check-expect (add 1Btest5var1 1Btest5var2)
              1Bsolution5)

;; Empty List Test
(check-expect (add 1Btest6var1 1Btest6var2)
              1Bsolution6)
(check-expect (add 1Btest7var1 1Btest7var2)
              1Bsolution7)
(check-expect (add 1Btest8var1 1Btest8var2)
              1Bsolution8)
(check-expect (add 1Btest9var1 1Btest9var2)
              1Bsolution9)
(check-expect (add 1Btest10var1 1Btest10var2)
              1Bsolution10)


;;
;;   Problem 1C
;;


;; (mult num1 num2) Produces the the result of multiplying
;;   one number (num1) to another number (num2)
;; Examples:
(check-expect (mult 2 3) 6)
(check-expect (mult 2 7) 14)

;; mult: Int Int -> Int
;; requires:
;;      num1 >= 0
;;      num2 >= 0
(define (mult num1 num2)
  (cond [(= num2 0) 0]
        [else (add num1 (mult num1 (sub1 num2)))]))

;; Basic Tests
(check-expect (mult 1Ctest1var1 1Ctest1var2)
              1Csolution1)
(check-expect (mult 1Ctest2var1 1Ctest2var2)
              1Csolution2)
(check-expect (mult 1Ctest3var1 1Ctest3var2)
              1Csolution3)
(check-expect (mult 1Ctest4var1 1Ctest4var2)
              1Csolution4)
(check-expect (mult 1Ctest5var1 1Ctest5var2)
              1Csolution5)

;; Empty List Test
(check-expect (mult 1Ctest6var1 1Ctest6var2)
              1Csolution6)
(check-expect (mult 1Ctest7var1 1Ctest7var2)
              1Csolution7)
(check-expect (mult 1Ctest8var1 1Ctest8var2)
              1Csolution8)
(check-expect (mult 1Ctest9var1 1Ctest9var2)
              1Csolution9)
(check-expect (mult 1Ctest10var1 1Ctest10var2)
              1Csolution10)