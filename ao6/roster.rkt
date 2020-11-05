;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname roster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Robert (Robbie) Knowles (20878339)
;;   CS 135 Fall 2020
;;   Assigment 06, Problem 1(A-D)
;; ******************************************

;; ==================================
;; struct and data definitions For Q1
;; =================================

;; A StudentID is a Nat with at most 8 digits
;;                  (i.e. 0 <= id <= 99999999)

;; A Grade is one of:
;; * false
;; * Nat
;;   Requires: Nat is between 0 and 100 (inclusive)

(define-struct student (id name grade))
;; A Student is a (make-student StudentID Str Grade)


(define-struct rnode (student left right))
;; A Roster Node (RN) is a (make-rnode Student Roster Roster)
;; Requires: all students in the left subtree have
;;                                    an ID < student's ID
;;           all students in the right subtree have
;;                                    an ID > student's ID

;; A Roster is one of 
;; * empty
;; * RN


;; =================================
;; constants used in Q1 examples
;; =================================

(define beth (make-student 12345678 "Beth" 96))
(define jenny (make-student 08675309 "Jenny" 81))
(define john1 (make-student 48975311 "John" 95))
(define jenny/new (make-student 08675309 "Jen" 81))
(define john2 (make-student 20488192 "John" false))

(define sample-roster
  (make-rnode beth ; root
              (make-rnode jenny empty empty)   ; left child
              (make-rnode john1 empty empty))) ; right child

(define sample-roster-2
  (make-rnode beth 
              (make-rnode jenny/new empty empty)
              (make-rnode john1
                          (make-rnode john2 empty empty)
                          empty)))

;; More Tests

(define ta (make-student 50000000 "a" 99))
(define tb (make-student 40000000 "b" 30))
(define tc (make-student 30000000 "c" 93))
(define td (make-student 41000000 "d" 92))
(define te (make-student 25000000 "e" 85))
(define tf (make-student 31000000 "f" 81))
(define tg (make-student 70000000 "g" 87))
(define th (make-student 65000000 "h" false))
(define ti (make-student 90000000 "i" false))
(define tj (make-student 99990000 "j" false))

(define test1
  (make-rnode ta 
        (make-rnode tb
              (make-rnode tc
                    (make-rnode te empty empty)
                    (make-rnode tf empty empty))
              (make-rnode td empty empty))
        (make-rnode tg
              (make-rnode th empty empty)
              (make-rnode ti
                    empty
                    (make-rnode tj empty empty)))))

(define test2
  (make-rnode ta 
        (make-rnode tc
              empty
              (make-rnode td empty empty))
        (make-rnode th
              empty
              (make-rnode ti empty empty))))

(define test3
  (make-rnode th
        (make-rnode ti empty empty)
        (make-rnode tj empty empty)))

(define test4
  (make-rnode ta
        (make-rnode tb empty empty)
        (make-rnode tg empty empty)))

;; =================================
;;
;; Question 1A
;;
;; =================================



;; (find-student StuID Rost) Consumes a StuID (StudentID)
;;   and a Rost (Roster), and produces the student that
;;   has that id that matches or produces false.
;; Examples:

(check-expect (find-student 12345678 sample-roster) beth)
(check-expect (find-student 87654321 sample-roster) false)

;; find-student: StudentID Roster -> (anyof false Student)
(define (find-student StuID Rost)
   (cond [(empty? Rost)                        false]
         [(= (student-id (rnode-student Rost)) StuID)
                                (rnode-student Rost)]
         [(< (student-id (rnode-student Rost)) StuID)
             (find-student StuID (rnode-right Rost))]
         [else
             (find-student StuID (rnode-left Rost))]))
;; Tests Where The Element DNE

(check-expect (find-student 10000000 empty) false)
(check-expect (find-student 0 sample-roster) false)
(check-expect (find-student 55 sample-roster-2) false)
(check-expect (find-student 37000 test1) false)
(check-expect (find-student 90000 test2) false)

;; General Tests
(check-expect (find-student 12345678 sample-roster-2) beth)
(check-expect (find-student 08675309 sample-roster-2) jenny/new)
(check-expect (find-student 48975311 sample-roster-2) john1)
(check-expect (find-student 20488192 sample-roster-2) john2)

(check-expect (find-student 50000000 test1) ta)
(check-expect (find-student 40000000 test1) tb)
(check-expect (find-student 30000000 test1) tc)
(check-expect (find-student 41000000 test1) td)
(check-expect (find-student 25000000 test1) te)
(check-expect (find-student 31000000 test1) tf)
(check-expect (find-student 70000000 test1) tg)
(check-expect (find-student 65000000 test1) th)
(check-expect (find-student 90000000 test1) ti)
(check-expect (find-student 99990000 test1) tj)             
   
(check-expect (find-student 50000000 test2) ta)
(check-expect (find-student 30000000 test2) tc)
(check-expect (find-student 41000000 test2) td)
(check-expect (find-student 65000000 test2) th)
(check-expect (find-student 90000000 test2) ti)



;; =================================
;;
;; Question 1B
;;
;; =================================



;; (class-average Rost) Consumes a Rost (Roster) and
;;   produces the average of all the students in the Roster.
;; Examples:

(check-expect (class-average sample-roster) (+ 90 2/3))
(check-expect (class-average sample-roster-2) (+ 90 2/3))
(check-expect (class-average empty) 'N/A)
;; class-average: Roster -> (anyof Num Sym)
(define (class-average Rost)
   (cond [(or (empty? Rost) (only-f Rost))          'N/A]
         [else (/ (sum-of-rost Rost) (num-of-rost Rost))]))


;; ===  Helper Functions  ===


;; (only-f Rost) Consumes a Rost (Roster) and
;;   produces if the Roster only has students
;;   with false grades
;; Examples:
         
(check-expect (only-f test3) true)
(check-expect (only-f test4) false)

;; only-of: RN -> Sym
(define (only-f Rost)
   (cond [(empty? Rost)          false]
         [(boolean? (student-grade (rnode-student Rost)))
                                   true]
         [else
             (and (only-f (rnode-right Rost))
                 (only-f (rnode-left Rost)))]))


;; (sum-of-rost Rost) Consumes a Rost (Roster) and
;;   produces the sum of all the grades of all the 
;;   students
;; Examples:
         
(check-expect (sum-of-rost test3) 0)
(check-expect (sum-of-rost test4) 216)

;; sum-of-rost: RN -> Num
(define (sum-of-rost Rost)
   (cond [(empty? Rost)               0]
         [(boolean? (student-grade (rnode-student Rost)))
             (+ (sum-of-rost (rnode-right Rost))
                (sum-of-rost (rnode-left Rost)))]
         [else
             (+ (student-grade (rnode-student Rost))
                (sum-of-rost (rnode-right Rost))
                (sum-of-rost (rnode-left Rost)))]))


;; (num-of-rost Rost) Consumes a Rost (Roster) and
;;   produces the number of students with grades in the roster 
;; Examples:
         
(check-expect (num-of-rost test3) 0)
(check-expect (num-of-rost test4) 3)

;; num-of-rost: RN -> Num
(define (num-of-rost Rost)
   (cond [(empty? Rost)               0]
         [(boolean? (student-grade (rnode-student Rost)))
             (+ (num-of-rost (rnode-right Rost))
                (num-of-rost (rnode-left Rost)))]
         [else
             (+ 1
             (num-of-rost (rnode-right Rost))
             (num-of-rost (rnode-left Rost)))]))

;; Empty Test
(check-expect (class-average test3) 'N/A)
(check-expect (class-average empty) 'N/A)

;; General Tests
(check-expect (class-average test1) 81)
(check-expect (class-average test2) (+ 94 6/9))
(check-expect (class-average test4) 72)



;; =================================
;;
;; Question 1C
;;
;; =================================


;; (find-student/name name Rost) Consumes a Rost (Roster)
;;   and a name (String), and produces the students with
;;   the same name.
;; Examples:

(check-expect (find-student/name "Beth" sample-roster) (list beth))
(check-expect (find-student/name "Dan" sample-roster) empty)

;; find-student/name: Str Roster -> (listof Student)
(define (find-student/name name Rost)
   (cond [(empty? Rost)                   empty]
         [(string=? (student-name (rnode-student Rost)) name)
                   (append (append (list (rnode-student Rost))
                   (find-student/name name (rnode-left Rost)))
                   (find-student/name name (rnode-right Rost)))]
         [else
             (append
              (find-student/name name (rnode-left Rost))
              (find-student/name name (rnode-right Rost)))]))

;; Tests Where The Element DNE
(check-expect (find-student/name "Jenny" empty) empty)
(check-expect (find-student/name "Apple" sample-roster) empty)
(check-expect (find-student/name "Pie" sample-roster-2) empty)
(check-expect (find-student/name "Is" test1) empty)
(check-expect (find-student/name "good" test2) empty)

;; General Tests
(check-expect (find-student/name "Jenny" sample-roster)
                                        (list jenny))
(check-expect (find-student/name "John" sample-roster)
                                        (list john1))
(check-expect (find-student/name "Beth" sample-roster-2)
                                         (list beth))
(check-expect (find-student/name "Jenny" sample-roster-2)
                                         empty)
(check-expect (find-student/name "Jen" sample-roster-2)
                                         (list jenny/new))
(check-expect (find-student/name "John" sample-roster-2)
                                         (list john1 john2))

(check-expect (find-student/name "a" test1)
                                        (list ta))
(check-expect (find-student/name "b" test1)
                                        (list tb))
(check-expect (find-student/name "c" test1)
                                         (list tc))
(check-expect (find-student/name "dee" test1)
                                         empty)
(check-expect (find-student/name "e" test1)
                                         (list te))
(check-expect (find-student/name "f" test1)
                                         (list tf))    
   


;; =================================
;;
;; Question 1D
;;
;; =================================

;; (add-students los Rost) Consumes a Rost (Roster) and
;;   a los (listof (list StudentID Str)). Produces a new Rost with the
;;   new students contained.
;; Examples:

(check-expect (add-students (list (list 20488192 "John")
                                  (list 8675309 "Jen"))
                            sample-roster)
              sample-roster-2)

;; add-students: (listof (list StudentID Str)) Roster -> Roster
(define (add-students Stu Rost)
  (cond [(empty? Stu)              Rost]
        [(find-id-in-rost (first (first Stu)) Rost)
                (add-students (rest Stu)
                              (replace-student
                                 (make-student
                                   (first (first Stu))
                                   (second (first Stu))
                                   false)
                                  Rost))]
        [else
               (add-students (rest Stu)
                             (add-student
                                 (make-student
                                  (first (first Stu))
                                  (second (first Stu))
                                  false)
                                 Rost))]))


;; ===  Helper Functions  ===

;; (replace-student Stu Rost) Consumes a Rost (Roster) and
;;   a Stu (Student). produces a new Rost with the student,
;;  student grade is from the previous
;; Examples:
         
(check-expect (replace-student (make-student 12345678 "Bath" 96)
                           sample-roster)
              (make-rnode (make-student 12345678 "Bath" 96)
                (make-rnode jenny empty empty) 
                (make-rnode john1 empty empty)))

(check-expect (replace-student (make-student 08675309 "Janny" 96)
                           sample-roster)
              (make-rnode   beth
                           (make-rnode
                            (make-student 08675309 "Janny" 81)
                            empty empty)
                           (make-rnode john1 empty empty)))

;; replace-student: Student RN -> RN
(define (replace-student Stu Rost)
  (cond [(empty? Rost)                        empty]
        [(= (student-id (rnode-student Rost))
            (student-id Stu))
              (make-rnode (make-student
                                (student-id (rnode-student Rost))
                                (student-name Stu)
                                (student-grade (rnode-student Rost)))
                              (rnode-left Rost)
                              (rnode-right Rost))]
         [else
             (make-rnode  (rnode-student Rost)
                          (replace-student Stu (rnode-left Rost))
                          (replace-student Stu (rnode-right Rost)))]))


;; (find-id-in-rost StuID Rost) Consumes a StuID (StudentID)
;;   and a Rost (Roster), and produces if the ID is in the Roster
;; Examples:

(check-expect (find-id-in-rost 12345678 sample-roster) true)
(check-expect (find-id-in-rost 08675302 sample-roster) false)

;; find-id-in-rost: Num RN ->RN
(define (find-id-in-rost StuID Rost)
   (cond [(empty? Rost)                   false]
         [(= (student-id (rnode-student Rost)) StuID)
                                           true]
         [else
             (or
              (find-id-in-rost StuID (rnode-left Rost))
              (find-id-in-rost StuID (rnode-right Rost)))]))


;; (add-student Stu Rost) Consumes a Stu (Student) and a
;;   Rost (Roster), and produces produces a new roster with
;;   the student on it
;; Examples:

(check-expect (add-student jenny
                           empty)
              (make-rnode jenny empty empty))
(check-expect (add-student jenny
                           (make-rnode beth empty empty))
              (make-rnode beth (make-rnode jenny empty empty)
                                empty))

;; add-student: Student RN -> Bool
(define (add-student Stu Rost)
   (cond [(empty? Rost)  (make-rnode Stu empty empty)]
         [(> (student-id (rnode-student Rost)) (student-id Stu))
                         (make-rnode
                             (rnode-student Rost)
                             (add-student Stu (rnode-left Rost))
                             (rnode-right Rost))]
         [(< (student-id (rnode-student Rost)) (student-id Stu))
                         (make-rnode
                             (rnode-student Rost)
                             (rnode-left Rost)
                             (add-student Stu (rnode-right Rost)))]))

;; Tests Where The Element DNE
(check-expect (add-students (list (list 12345678 "Beth")
                                  (list 08675309 "Jenny")
                                  (list 48975311 "John")) empty)
              (make-rnode (make-student 12345678 "Beth" false)
                   (make-rnode (make-student 08675309 "Jenny" false)
                               empty empty)  
                   (make-rnode (make-student 48975311 "John" false)
                               empty empty)))

(check-expect (add-students (list (list 12345678 "Beth")
                                  (list 48975311 "John")
                                  (list 08675309 "Jenny")) empty)
              (make-rnode (make-student 12345678 "Beth" false)
                   (make-rnode (make-student 08675309 "Jenny" false)
                               empty empty)  
                   (make-rnode (make-student 48975311 "John" false)
                               empty empty)))

(check-expect (add-students empty sample-roster) sample-roster)
(check-expect (add-students (list (list 12345678 "Beth")) empty)
              (make-rnode (make-student 12345678 "Beth" false)
                          empty empty))

;; Rename Tests

(check-expect (add-students (list (list 12345678 "B")
                                  (list 48975311 "J")
                                  (list 08675309 "J"))
                            sample-roster)
              (make-rnode (make-student 12345678 "B" 96)
                   (make-rnode (make-student 08675309 "J" 81)
                               empty empty)  
                   (make-rnode (make-student 48975311 "J" 95)
                               empty empty)))

(check-expect (add-students (list (list 12345678 "B")
                                  (list 48975311 "J")) sample-roster)
              (make-rnode (make-student 12345678 "B" 96)
                   (make-rnode (make-student 08675309 "Jenny" 81)
                               empty empty)  
                   (make-rnode (make-student 48975311 "J" 95)
                               empty empty)))

(check-expect (add-students (list (list 12345678 "Beth is cool")
                                  (list 48975311 "Jack is"))
                            sample-roster-2)
              (make-rnode (make-student 12345678 "Beth is cool" 96)
                   (make-rnode (make-student 08675309 "Jen" 81)
                               empty empty)  
                   (make-rnode (make-student 48975311 "Jack is" 95)
                               (make-rnode john2 empty empty)
                               empty)))

(check-expect (add-students (list (list 12345678 "Jen"))
                            sample-roster-2)
              (make-rnode (make-student 12345678 "Jen" 96)
                   (make-rnode (make-student 08675309 "Jen" 81)
                               empty empty)  
                   (make-rnode (make-student 48975311 "John" 95)
                               (make-rnode john2 empty empty)
                               empty)))
;; Adding In Students
(check-expect (add-students (list (list 50000000 "a")
                                  (list 40000000 "b")
                                  (list 30000000 "c")
                                  (list 41000000 "d"))
                            empty)
              (make-rnode (make-student 50000000 "a" false)
                   (make-rnode (make-student 40000000 "b" false)
                        (make-rnode (make-student 30000000 "c" false)
                               empty empty)
                        (make-rnode (make-student 41000000 "d" false)
                        empty empty))
                   empty))
                   
  
(check-expect (add-students (list (list 50000000 "a")
                                  (list 40000000 "b")
                                  (list 30000000 "c")
                                  (list 41000000 "d")
                                  (list 25000000 "e")
                                  (list 31000000 "f"))
                            empty)
              (make-rnode (make-student 50000000 "a" false)
                   (make-rnode (make-student 40000000 "b" false)
                        (make-rnode (make-student 30000000 "c" false)
                               (make-rnode (make-student 25000000
                                                         "e" false)
                                           empty empty)
                               (make-rnode (make-student 31000000
                                                         "f" false)
                                           empty empty))
                        (make-rnode (make-student 41000000 "d" false)
                        empty empty))
                   empty))

(check-expect (add-students (list (list 50000000 "a")
                                  (list 40000000 "b")
                                  (list 30000000 "c")
                                  (list 41000000 "d")
                                  (list 25000000 "e")
                                  (list 31000000 "f")
                                  (list 70000000 "g")
                                  (list 65000000 "h")
                                  (list 90000000 "i")
                                  (list 99990000 "j"))
                            empty)
              (make-rnode (make-student 50000000 "a" false)
                   (make-rnode (make-student 40000000 "b" false)
                        (make-rnode (make-student 30000000 "c" false)
                               (make-rnode (make-student 25000000
                                                         "e" false)
                                           empty empty)
                               (make-rnode (make-student 31000000
                                                         "f" false)
                                           empty empty))
                        (make-rnode (make-student 41000000 "d" false)
                        empty empty))
                   (make-rnode (make-student 70000000 "g" false)
                        (make-rnode (make-student 65000000 "h" false)
                               empty empty)
                        (make-rnode (make-student 90000000 "i" false)
                            empty
                            (make-rnode (make-student 99990000
                                                      "j" false)
                                        empty empty)))))

(check-expect (add-students (list (list 50000000 "hi")
                                  (list 40000000 "hi")
                                  (list 30000000 "hi")
                                  (list 41000000 "hi")
                                  (list 25000000 "hi")
                                  (list 31000000 "hi")
                                  (list 70000000 "hi")
                                  (list 65000000 "hi")
                                  (list 90000000 "hi")
                                  (list 99990000 "hi"))
                            test1)
              (make-rnode (make-student 50000000 "hi" 99)
                   (make-rnode (make-student 40000000 "hi" 30)
                        (make-rnode (make-student 30000000 "hi" 93)
                               (make-rnode (make-student 25000000
                                                         "hi" 85)
                                           empty empty)
                               (make-rnode (make-student 31000000
                                                         "hi" 81)
                                           empty empty))
                        (make-rnode (make-student 41000000 "hi" 92)
                        empty empty))
                   (make-rnode (make-student 70000000 "hi" 87)
                        (make-rnode (make-student 65000000 "hi" false)
                               empty empty)
                        (make-rnode (make-student 90000000 "hi" false)
                            empty
                            (make-rnode (make-student 99990000
                                                      "hi" false)
                                        empty empty)))))

(check-expect (add-students (list (list 50000000 "hi")
                                  (list 40000000 "hi")
                                  (list 30000000 "hi")
                                  (list 41000000 "hi")
                                  (list 25000000 "hi")
                                  (list 31000000 "hi")
                                  (list 70000000 "hi")
                                  (list 65000000 "hi")
                                  (list 90000000 "hi")
                                  (list 99990000 "hi"))
                            test4)
              (make-rnode (make-student 50000000 "hi" 99)
                   (make-rnode (make-student 40000000 "hi" 30)
                        (make-rnode (make-student 30000000 "hi" false)
                               (make-rnode (make-student 25000000
                                                         "hi" false)
                                           empty empty)
                               (make-rnode (make-student 31000000
                                                         "hi" false)
                                           empty empty))
                        (make-rnode (make-student 41000000 "hi" false)
                        empty empty))
                   (make-rnode (make-student 70000000 "hi" 87)
                        (make-rnode (make-student 65000000 "hi" false)
                               empty empty)
                        (make-rnode (make-student 90000000 "hi" false)
                            empty
                            (make-rnode (make-student 99990000
                                                      "hi" false)
                                        empty empty)))))




