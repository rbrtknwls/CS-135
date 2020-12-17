;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname future-b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Final Exam, Problem 2b(i-ii)
;; ******************************************

;; =================================
;;
;; [Global Struct Defns Defns]
;;
;; =================================


;; A Substitution Cipher (SCipher) is a (listof (list Char Char))
;; Requires: The first Char in each pair (the key) is unique

(define vowel-shift '((#\a #\e) (#\e #\i) (#\i #\o)
                      (#\o #\u) (#\u #\y) (#\y #\a)))



;; =================================
;;
;; Problem (2bi) [substitute]
;;
;; =================================

;; (subsitute mess ciph) Consumes a Str (mess) and a SCipher 
;;   (ciph), and produces a new string where each character is
;;   replaced by the cipher
;; Examples:

(check-expect (substitute "I, plain text!9" vowel-shift)
                          "I, pleon tixt!9")

;; subsitute: Str SCipher -> Str
(define (substitute mess ciph)
  (cond [(empty? mess) empty]
        [else
         (list->string
          (map (lambda (m_elem)
                 (foldr (lambda (c_elem rorr)
                          (cond [(char=? (first c_elem) m_elem)
                                         (second c_elem)]
                                [else               rorr]))
                        m_elem
                        ciph))
               (string->list mess)))]))
                                            


;; =================================
;; Testing Suite
;; =================================

;; === Empty Tests ===
(check-expect (substitute empty '((#\a #\e) (#\e #\i))) empty)
(check-expect (substitute empty '((#\n #\o))) empty)

;; === Empty Cipher Tests ===
(check-expect (substitute "Hi is the cypher working?" empty)
                          "Hi is the cypher working?")
(check-expect (substitute "I" empty)
                          "I")
(check-expect (substitute "Am robbie its (list nice) to 12313131 -1"
                               empty)
                          "Am robbie its (list nice) to 12313131 -1")

;; === General Tests ===
(check-expect (substitute "1 + 3 = 6" '((#\1 #\2) (#\+ #\*)))
                          "2 * 3 = 6")
(check-expect (substitute "This is a speeling mistake"
                          '((#\h #\n) (#\e #\l)))
                          "Tnis is a spllling mistakl")
(check-expect (substitute "This is a speeling mistake"
                          '((#\h #\l) (#\e #\l) (#\a #\l)))
                          "Tlis is l spllling mistlkl")
(check-expect (substitute "happy" '((#\h #\s) (#\p #\d) (#\y #\d)))
                          "saddd")
(check-expect (substitute "Hi!!! :)" '((#\i #\1) (#\) #\()))
                          "H1!!! :(")
(check-expect (substitute ":)" '((#\: #\;) (#\) #\())) ";(")
(check-expect (substitute "aaaabbbbccccc" '((#\a #\b) (#\c #\b)))
                          "bbbbbbbbbbbbb")
(check-expect (substitute "I" '((#\I #\i)))
                          "i")
(check-expect (substitute "damn" '((#\n #\!) (#\a #\!) (#\m #\!)))
                          "d!!!")



;; =================================
;;
;; Problem (2bii) [reverse-cipher]
;;
;; =================================

;; (reverse-cipher ciph) Consumes a SCipher (ciph)
;;   and produces a new SCipher that reverses the orgional
;;   cipher or returns false
;; Examples:

(check-expect (reverse-cipher vowel-shift)
              '((#\e #\a) (#\i #\e) (#\o #\i)
                (#\u #\o) (#\y #\u) (#\a #\y)))

(check-expect (reverse-cipher '((#\a #\b) (#\c #\b))) false)

;; reverse-cipher: SCipher -> (anyof SCipher bool)
(define (reverse-cipher ciph)
  
  (local [
          ;; (reverse) Produces the inverse of a SCipher, works if
          ;;  the SCipher is empty too!

          ;; reverse: SCipher
          (define reverse (map (lambda (c_elem) (list (second c_elem)
                                                     (first c_elem)))
                          ciph))]

         (foldr (lambda (x y rorr)
                  (cond [(char=? (first x) (first y))  rorr]
                        [else                         false]))
                reverse
                (quicksort ciph (lambda (x y)
                           (string<? (list->string
                                       (list (first x)))
                                     (list->string
                                       (list (first y))))))
                (quicksort reverse (lambda (x y)
                           (string<? (list->string
                                       (list (first x)))
                                     (list->string
                                       (list (first y)))))))))
         
                                            


;; =================================
;; Testing Suite
;; =================================

;; === Empty Tests ===
(check-expect (reverse-cipher empty) empty)

;; === Cant Cipher Tests ===
(check-expect (reverse-cipher '((#\a #\a) (#\c #\a))) false)
(check-expect (reverse-cipher '((#\: #\a) (#\: #\a))) false)
(check-expect (reverse-cipher '((#\b #\a) (#\c #\a)
                                (#\d #\a) (#\e #\a))) false)
(check-expect (reverse-cipher '((#\b #\b) (#\c #\c)
                                (#\d #\d) (#\e #\b))) false)
(check-expect (reverse-cipher '((#\1 #\1) (#\2 #\1)
                                (#\3 #\4) (#\3 #\7))) false)
(check-expect (reverse-cipher '((#\a #\b) (#\b #\c))) false)

;; === Can Cipher Tests ===
(check-expect (reverse-cipher '((#\1 #\2) (#\1 #\1)))
                              '((#\2 #\1) (#\1 #\1)))
(check-expect (reverse-cipher '((#\1 #\2) (#\1 #\3)))
                              '((#\2 #\1) (#\3 #\1)))
(check-expect (reverse-cipher '((#\a #\a) (#\a #\b) (#\a #\c)))
                              '((#\a #\a) (#\b #\a) (#\c #\a)))
(check-expect (reverse-cipher '((#\1 #\1) (#\1 #\2) (#\1 #\3)))
                              '((#\1 #\1) (#\2 #\1) (#\3 #\1)))
(check-expect (reverse-cipher '((#\1 #\1) (#\1 #\2)
                                (#\2 #\3) (#\2 #\4)))
                              '((#\1 #\1) (#\2 #\1)
                                (#\3 #\2) (#\4 #\2)))
(check-expect (reverse-cipher '((#\a #\2) (#\2 #\c)
                                (#\e #\f) (#\g #\h)))
                              '((#\2 #\a) (#\c #\2)
                                (#\f #\e) (#\h #\g)))
(check-expect (reverse-cipher '((#\2 #\2) (#\3 #\3)
                                (#\4 #\4) (#\5 #\5)))
                              '((#\2 #\2) (#\3 #\3)
                                (#\4 #\4) (#\5 #\5)))


