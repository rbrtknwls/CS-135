;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname future-b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Final Exams, Problem 2b(i-ii)
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

;; reverse-cipher: SCipher -> (anyof SCipher bool)
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

