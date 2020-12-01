;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htmlagain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 09, Problem 3 (A-B)
;; ******************************************



;; ==================================
;; struct and data definitions For Q3
;; =================================

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))



;; =================================
;;
;; Question 3A
;;
;; =================================


;; (tokenize hstring) Given a string will produce a list
;; of all the elements (opening, closing and strings)
;; Examples:
(check-expect (tokenize "<p><h1>Heading</h1>Text</p>")
              '("<p>" "<h1>" "Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize "Heading</h1>Text</p>")
              '("Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize "a b   c") '("a b   c"))
(check-expect (tokenize "") empty)

;; tokenize: Str -> (listof Str)
(define (tokenize hstring)
  (clist->nlist (string->list hstring)))


;; =================================
;; Helper Functions
;; =================================


;; (clist->nlist loc) Given a list of characters (los) will
;;   produce a list of all the elements (opening, closing and strings)
;; Examples:
(check-expect (clist->nlist (string->list "<p></p>"))
              '("<p>" "</p>"))

;; clist->nlist: (listof Char) -> (listof Str)
(define (clist->nlist loc)
     (cond  [(empty? loc) empty]
            [else (cons (list->string (cons (first loc)
                                            (curr-list (rest loc))))
                        (clist->nlist (rest-list (rest loc))))]))


;; (curr-list loc) Produces a list of all the values in a list form
;;   up to the first end bracket or to the end of the last
;; Examples:
(check-expect (curr-list (string->list "p></p>"))
              (string->list "p>"))
(check-expect (curr-list (string->list "a b c d"))
              (string->list "a b c d"))

;; curr-list: (listof Char) -> (listof Char)
(define (curr-list loc)
     (cond  [(empty? loc)                     empty]
            [(char=? (first loc) #\>)  (cons (first loc)
                                               empty)]
            [(char=? (first loc) #\<)  empty]
            [else (cons (first loc)
                        (curr-list (rest loc)))]))


;; (rest-list loc) Produces a list of all the values in a list form
;;   after to the first end brackets
;; Examples:
(check-expect (rest-list (string->list "p></p>"))
              (string->list "</p>"))
(check-expect (rest-list (string->list "a b c d"))
              empty)

;; rest-list: (listof Char) -> (listof Char)
(define (rest-list loc)
     (cond  [(empty? loc)                  empty]
            [(char=? (first loc) #\>) (rest loc)]
            [(char=? (first loc) #\<) loc]
            [else         (rest-list (rest loc))]))


;; =================================
;; Testing Suite
;; =================================


;; === Empty String ===
(check-expect (tokenize "") empty)
(check-expect (tokenize "empty") '("empty"))
(check-expect (tokenize "<empty>") '("<empty>"))
(check-expect (tokenize "<empty></empty>") '("<empty>" "</empty>"))

;; === General Tests ===

(check-expect (tokenize "<a><b><c>hihi</c></b></a>")
              '("<a>" "<b>" "<c>" "hihi" "</c>" "</b>" "</a>"))
(check-expect (tokenize "<a><b>hi</b><c>hi</c></a>")
              '("<a>" "<b>" "hi" "</b>" "<c>" "hi" "</c>" "</a>"))
(check-expect (tokenize "<a><b><c></c><d></d></b>1234</a>")
              '("<a>" "<b>" "<c>" "</c>" "<d>" "</d>" "</b>"
                      "1234" "</a>"))
(check-expect (tokenize "<1></1>")
              '("<1>" "</1>"))
(check-expect (tokenize "<1></1><2></2><3></3>")
              '("<1>" "</1>" "<2>" "</2>" "<3>" "</3>"))
(check-expect (tokenize "<hi>hi</hi>")
              '("<hi>" "hi" "</hi>"))
(check-expect (tokenize "<><><><></><></></></><></></>")
              '("<>" "<>" "<>" "<>" "</>" "<>" "</>" "</>"
                     "</>" "<>" "</>" "</>"))



;; =================================
;;
;; Question 3B
;;
;; =================================



;; (string->html hstring) Given a nonempty string will produce an
;;  HI which resprents the item, it will be similar to bracket
;;  match making
;; Examples:
(check-expect (string->html "<p><h1>Heading</h1>Text</p>")
              '(p (h1 "Heading") "Text"))
(check-expect (string->html "<h1>Heading</h1>")
              '(h1 "Heading"))


;; string->html: Str -> HI
;; Requires: hstring to be nonempty
(define (string->html hstring)
     (cond  [(= (get-loc hstring "<" 0) -1) hstring]
            [else (first (process hstring))]))


;; =================================
;; Helper Functions
;; =================================


;; (get-loc search_in search_for indx) Given a string (search_in)
;;   and a string to search for (search_for), will produce the indx
;;   in which that string exists (indx), note that it will return
;;   -1 if it is not in the bounds
;; Examples:
(check-expect (get-loc "<h1>Heading</h1>" "<h1>" 0)
              0)
(check-expect (get-loc "<h1>Heading</h1>" "</h1>" 0)
              11)

;; get-loc: Str Str Int -> Int

(define (get-loc search_in search_for indx)
  
         (local [(define searchsize (string-length search_for))
                 (define totalsize  (string-length search_in))]

           (cond [(> (+ searchsize indx)
                     totalsize)
                                                           -1]
                 [(string=? (substring
                                search_in
                                indx
                                (+ indx searchsize))
                            search_for)
                                                         indx]
                 [else
                    (get-loc search_in search_for (add1 indx))])))

;; (curr-list loc) Produces a list of all the values in a list form
;;   up to the first end bracket or to the end of the last
;; Examples:
(check-expect (process "<p><a>1</a><b>2</b><c>3</c></p>")
              '((p (a "1") (b "2") (c "3"))))
(check-expect (process "abcd")
              '("abcd"))

;; curr-list: (listof Char) -> (listof Char)
(define (process string)
   
   (local [

           ;; (<_indx) gets the index of the first
           ;;  opening bracket "<", returns -1 if
           ;;  not in list

           ;; (<_indx): Int
           (define <_indx (get-loc string "<" 0))
           
           ;; (>_indx) gets the index of the first
           ;;  closing bracket ">", returns -1 if
           ;;  not in list

           ;; (>_indx): Int
           (define >_indx (get-loc string ">" 0))]
     
       (cond [(= (string-length string) 0)
                            empty]
             [(= >_indx -1) (list string)]
             [else
              
               (local [

                       ;; (e_tag) gets the corrisponding
                       ;;  ending tag for the first tag
                       ;;  in the string

                       ;; (e_tag): Str
                       (define e_tag (string-append
                                      "</"
                                      (substring
                                        string
                                        (add1 <_indx)
                                        (add1 >_indx))))
                       
                       ;; (e_tag) gets the corrisponding
                       ;;  ending tag location for the first tag
                       ;;  in the string, it starts at the first
                       ;;  index

                       ;; (e_tag): Str
                       (define e_loc (+ (get-loc string e_tag 0)
                                        (string-length e_tag)))]

                 
                  (cond [(and (= (string-length string) e_loc)
                              (= <_indx 0))

                         ;; Contains only one tag, will thus
                         ;;  get the list of the symbol and
                         ;;  the children of the list
                         
                         (list
                           (append
                             (list
                               (string->symbol
                                 (substring
                                    string
                                    1
                                    >_indx)))
                           (process
                              (substring
                                  string
                                  (add1 >_indx)
                                  (- e_loc
                                     (string-length e_tag))))))]

                        
                        [else
                         
                          ;; Contains more then one tag at current
                          ;;  level, must recurse to do the same
                          ;;  process on the left and right elements
                         
                         (append
                          (process
                           (substring string
                                      0
                                      <_indx))
                          (process
                           (substring string
                                      <_indx
                                      e_loc))
                          (process
                           (substring string
                                      e_loc
                                      (string-length string))))]))])))

;; =================================
;; Testing Suite
;; =================================


;; === No Tag Tests ===
(check-expect (string->html "hihi") "hihi")
(check-expect (string->html "hiimnotatag") "hiimnotatag")

;; === General Tests ===

(check-expect (string->html "<p><h1>Heading</h1>Text</p>")
              '(p (h1 "Heading") "Text"))
(check-expect (string->html "<p><h1>Heading</h1></p>")
              '(p (h1 "Heading")))
(check-expect (string->html "<p><a>1</a><b>2</b><c>3</c></p>")
              '(p (a "1") (b "2") (c "3")))
(check-expect (string->html "<z>a</z>")
              '(z "a"))
(check-expect (string->html (string-append
                     "<let><a>ima</a><b>imb</b><c>imc</c></let>"))
              '(let (a "ima") (b "imb") (c "imc")))
(check-expect (string->html "<hi>hi</hi>")
              '(hi "hi"))
(check-expect (string->html (string-append "<a><b>3<ba>7</ba>9</b>"
                                              "<c>3<ca>7</ca>9</c>"
                                              "<d>3<da>7</da>9</d>"
                                           "<e>3<ea>7</ea>9</e></a>"))
                '(a (b "3" (ba "7") "9")
                    (c "3" (ca "7") "9")
                    (d "3" (da "7") "9")
                    (e "3" (ea "7") "9")))
(check-expect (string->html
               (string-append "<a><b><ba><bba>bbba</bba></ba></b>"
                                  "<c><ca>cca</ca></c>"
                                  "<d>da</d>"
                                  "e</a>"))
               '(a (b (ba (bba "bbba")))
                   (c (ca "cca"))
                   (d "da")
                    "e"))
(check-expect (string->html
               (string-append "<a><b>3579</b>"
                                 "<c>3579</c>"
                                 "<d>3579</d>"
                                 "<e>3579</e></a>"))
               '(a (b "3579")
                   (c "3579")
                   (d "3579")
                   (e "3579")))
(check-expect (string->html
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
                           '(a
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
(check-expect (string->html
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
                                  "</e></a>"))
                           '(a
                               (b 
                                 (c "I" (c1 "am")
                                        (c2 "pyramid"))
                                 (d "I" (d1 "am")
                                        (d2 "pyramid")))
                               (e
                                 (f "I" (f1 "am")
                                        (f2 "pyramid")))))
(check-expect (string->html
               (string-append "<a><b><c>I"
                                      "<c1>am</c1>"
                                      "<c2>pyramid</c2>"
                                     "</c>"
                                     "<d>I"
                                       "<d1>am</d1>"
                                       "<d2>pyramid</d2>"
                                     "</d>"
                                  "</b>"
                                  "</a>"))
                           '(a
                               (b 
                                 (c "I" (c1 "am")
                                        (c2 "pyramid"))
                                 (d "I" (d1 "am")
                                        (d2 "pyramid")))
                               ))

