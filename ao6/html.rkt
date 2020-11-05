;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 06, Problem 2(A-C)
;; ******************************************

;; ==================================
;; struct and data definitions For Q2
;; =================================

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))

;; =================================
;; constants used in Q2 examples
;; =================================


(define just-text "Hello, world!")
(define short-example '(p (h1 "Heading") "Text"))
(define html-example '(html (head (title "CS135"))
                            (body (h1 "Welcome")
                                  "More text...")))

  
;; =================================
;;
;; Question 2A
;;
;; NON RECURSIVE SOLUTION
;;(define (html->string HItem)
;;   (cond [(empty? HItem)                  ""]
;;         [(not (list? HItem))
;;            (cond [(symbol? HItem)
;;                   (string-append "<" (symbol->string HItem) ">"
;;                                  "</" (symbol->string HItem) ">")]
;;                  [else
;;                                                           HItem])]
;;         [(list? (first HItem))
;;            (string-append (html->string (first HItem))
;;                           (html->string (rest HItem)))]
;;         [(symbol? (first HItem))
;;            (string-append "<" (symbol->string (first HItem))
;;                           ">"
;;                           (html->string (rest HItem))
;;                           "</" (symbol->string (first HItem))
;;                            ">")]
;;         [else
;;            (html->string (first HItem))]))
;; =================================

;; (html->string HItem) Consumes a HItem (HI)
;;   and produces the string equivilent of HTML
;;   Text
;; Examples:

(check-expect (html->string "text") "text")
(check-expect (html->string html-example)
              (string-append
                 "<html><head><title>CS135</title></head>"
                 "<body><h1>Welcome</h1>More text...</body></html>"))

;; html->string: HI -> Str
(define (html->string HItem)
   (cond [(empty? HItem)                                  ""]
         [(string? HItem)                              HItem]
         [(symbol? (first HItem))        (tag->string HItem)]
                 
         [else   (string-append (html->string (first HItem))
                                (html->string (rest HItem)))]))


;; ===  Helper Functions  ===

;; (html->string HTag) Consumes a HTag (Tag)
;;   and produces the string equivilent of
;;   the tag
;; Examples:

(check-expect (tag->string '(abc "aaa"))
              "<abc>aaa</abc>")
(check-expect (tag->string '(abc (a "a") (b "b") (c "c")))
              (string-append "<abc><a>a</a>"
                                  "<b>b</b>"
                                  "<c>c</c></abc>"))

;; html->string: Tag -> String
(define (tag->string HTag)
      (string-append "<" (symbol->string (first HTag)) ">"
                         (html->string (rest HTag))
                    "</" (symbol->string (first HTag)) ">"))

;; Somewhat Empty Element Tests
(check-expect (html->string empty) "")
(check-expect (html->string '(a "")) "<a></a>")
(check-expect (html->string '(a "" "" "")) "<a></a>")

;; General Tests
(check-expect (html->string '(a (b "3" "5" "7" "9")
                                (c "3" "5" "7" "9")
                                (d "3" "5" "7" "9")
                                (e "3" "5" "7" "9")))
                (string-append "<a><b>3579</b>"
                                  "<c>3579</c>"
                                  "<d>3579</d>"
                                  "<e>3579</e></a>"))

(check-expect (html->string '(a (b "3" (ba "7") "9")
                                (c "3" (ca "7") "9")
                                (d "3" (da "7") "9")
                                (e "3" (ea "7") "9")))
                (string-append "<a><b>3<ba>7</ba>9</b>"
                                  "<c>3<ca>7</ca>9</c>"
                                  "<d>3<da>7</da>9</d>"
                                  "<e>3<ea>7</ea>9</e></a>"))

(check-expect (html->string '(a (b "3" "7" (ba "9"))
                                (c "3" "7" (ca "9"))
                                (d "3" "7" (da "9"))
                                (e "3" "7" (ea "9"))))
                (string-append "<a><b>37<ba>9</ba></b>"
                                  "<c>37<ca>9</ca></c>"
                                  "<d>37<da>9</da></d>"
                                  "<e>37<ea>9</ea></e></a>"))

(check-expect (html->string '(a (b (ba (bba "bbba")))
                                (c (ca "cca"))
                                (d "da")
                                "e"))
                (string-append "<a><b><ba><bba>bbba</bba></ba></b>"
                                  "<c><ca>cca</ca></c>"
                                  "<d>da</d>"
                                  "e</a>"))


(check-expect (html->string '(a
                               (b 
                                 (c "I" (c1 "am")
                                        (c2 "pyramid"))
                                 (d "I" (d1 "am")
                                        (d2 "pyramid")))
                               (e
                                 (f "I" (f1 "am")
                                        (f2 "pyramid"))
                                 (g "I" (g1 "am")
                                        (g2 "pyramid")))))
                (string-append "<a><b><c>I"
                                       "<c1>am</c1>"
                                       "<c2>pyramid</c2>"
                                     "</c>"
                                     "<d>I"
                                       "<d1>am</d1>"
                                       "<d2>pyramid</d2>"
                                     "</d>"
                                  "</b>"
                                  "<e><f>I"
                                       "<f1>am</f1>"
                                       "<f2>pyramid</f2>"
                                     "</f>"
                                     "<g>I"
                                       "<g1>am</g1>"
                                       "<g2>pyramid</g2>"
                                     "</g>"
                                  "</e></a>"))



;; =================================
;;
;; Question 2B
;;
;; =================================


;; (remove-tag Symb HItem) Consumes a Symb (Sym) and a HItem (HI)
;;   and produces a new list of HTML-Item (HI).
;; Examples:

(check-expect (remove-tag 'b html-example) html-example)
(check-expect (remove-tag 'b '(p "Hello, " (b "World") "!"))
                             '(p "Hello, " "World" "!"))
(check-expect (remove-tag 'p '(p "Hello, " (b "World") "!"))
                             '("Hello, " (b "World") "!"))


;; remove-tag: Sym HI -> (listof (anyof Str HI))
(define (remove-tag Sym HItem)
   (cond [(empty? HItem)                                  empty]
         [(string? HItem)                    (cons HItem empty)]
         [(and (symbol? (first HItem))
               (symbol=? (first HItem) Sym))
                                  (remove-tag Sym (rest HItem))]
                 
         [(list? (first HItem))
                (mv-child Sym HItem)]
         [else
                           (cons (first HItem)
                                 (remove-tag Sym (rest HItem)))]))

;; ===  Helper Functions  ===


;; (mv-child Symb HItem) Consumes a Symb and a Hitem (Hi) and checks
;;   to see if the current Tag is what we want to delete, also checks 
;;   to see what it contains

(check-expect (mv-child 'a '((a "hello") "hi" ))
                                '("hello" "hi"))
(check-expect (mv-child 'a '((a (b "hello")) "hi" ))
                                '((b "hello") "hi"))
;; mv-child: Sym HI -> (listof (anyof Str HI))
(define (mv-child Sym HItem)
   (cond [(symbol=? (first (first HItem)) Sym)
            (cond [(string? (second (first HItem)))
                    (append (rest (first HItem))
                            (remove-tag Sym (rest HItem)))]
                  [else
                    (cons (remove-tag Sym (second (first HItem)))
                          (remove-tag Sym (rest HItem)))])]
         [else
                    (cons (remove-tag Sym (first HItem))
                          (remove-tag Sym (rest HItem)))]))
                   
;; Somewhat Empty Element Tests
(check-expect (remove-tag 'a empty) empty)
(check-expect (remove-tag 'a "a") '("a"))
(check-expect (remove-tag 'a '(a "a" "a")) '("a" "a"))

;; General Tests
(check-expect (remove-tag 'b '(a (b "3" "5" "7" "9")
                                (c "3" "5" "7" "9")
                                (d "3" "5" "7" "9")
                                (e "3" "5" "7" "9")))
                            '(a "3" "5" "7" "9"
                                (c "3" "5" "7" "9")
                                (d "3" "5" "7" "9")
                                (e "3" "5" "7" "9")))

(check-expect (remove-tag 'ba '(a (b "3" (ba "7") "9")
                                  (c "3" (ca "7") "9")
                                  (d "3" (da "7") "9")
                                  (e "3" (ea "7") "9")))
                              '(a (b "3" "7" "9")
                                  (c "3" (ca "7") "9")
                                  (d "3" (da "7") "9")
                                  (e "3" (ea "7") "9")))

(check-expect (remove-tag 'ca '(a (b "3" "7" (ba "9"))
                                  (c "3" "7" (ca "9"))
                                  (d "3" "7" (da "9"))
                                  (e "3" "7" (ea "9"))))
                              '(a (b "3" "7" (ba "9"))
                                  (c "3" "7" "9")
                                  (d "3" "7" (da "9"))
                                  (e "3" "7" (ea "9"))))



;; =================================
;;
;; Question 2C
;;
;; =================================


;; (okay-tags? HItem) Consumes a HItem (HI) and produces if
;;   the HItem is valid! IE No 'li without a 'ol or 'ul parent
;;   and no 'hr having children!
;; Examples:

(check-expect (okay-tags? html-example) true)
(check-expect (okay-tags? '(body (hr "hello"))) false)
(check-expect (okay-tags? '(body (li "Q1") "text")) false)

;; okay-tags?: HI -> Bool
(define (okay-tags? HItem)
    (cond [(empty? HItem)                                  true]
          [(or (string? HItem) (string? (first HItem)))    true]
          [(list? (first HItem))
                                 (and (okay-tags? (first HItem))
                                      (okay-tags? (rest HItem)))]
          [(symbol=? (first HItem) 'hr)
                                   (= (length HItem) 1)]
          [(not (or (symbol=? (first HItem) 'ol)
                    (symbol=? (first HItem) 'ul)))
                                 (and (get-kids HItem)
                                      (okay-tags? (rest HItem)))]
          [else                   (and true
                                      (okay-tags? (rest HItem)))]))


;; ===  Helper Functions  ===

;; (get-kids? HItem) Consumes a HItem (HI) and produces if
;;   the children does not contain an 'li element.

;; Examples:

(check-expect (get-kids '(body (li "Q1") "text")) false)
(check-expect (get-kids '(body (a (li "Q1")) "text")) true)

;; get-kids?: HI -> Bool
(define (get-kids HItem)
   (cond [(empty? HItem)                                    true]
         [(or (string? (first HItem))
              (symbol? (first HItem)))   (and true
                                         (get-kids (rest HItem)))]
         [(and (list? (first HItem))
               (symbol=? (first (first HItem)) 'li))
                                                            false]
         [else                           (and true
                                         (get-kids (rest HItem)))]))

;; Somewhat Empty Element Tests
(check-expect (okay-tags? empty) true)
(check-expect (okay-tags? '(a "")) true)
(check-expect (okay-tags? '(a "" "" "")) true)

;; General Tests
(check-expect (okay-tags? '(ul (li "Q1") "text")) true)
(check-expect (okay-tags? '(ul (hr "Q1") "text")) false)
(check-expect (okay-tags? '(hr "hi")) false)
(check-expect (okay-tags? '(a (hr) "aa")) true)

(check-expect (okay-tags? '(a (b "3" (ba "7") "9")
                                (c "3" (ca "7") "9")
                                (d "3" (da "7") "9")
                                (e "3" (ea "7") "9"))) true)
                

(check-expect (okay-tags? '(ul (b "3" "7" (li "9"))
                                 (c "3" "7" (oi "9"))
                                 (d "3" "7" (pi "9"))
                                 (e "3" "7" (di "9")))) false)

(check-expect (okay-tags? '(ul (li (ba (bba "bbba")))
                                (li (ca "cca"))
                                (li "da")
                                "e")) true)



           

         

           

