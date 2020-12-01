;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname u) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define (make-factor fac pfac)
     (cond [(empty? pfac) empty]
           [(>= (- fac (first pfac)) 0)
               (cons (first pfac)
                     (make-factor (- fac
                                (first pfac))
                             pfac))]
           [else
                (make-factor fac
                        (rest pfac))]))

(define (make-gcd fac pfac)
     (cond [(empty? pfac) 'a]
           [(= (remainder fac (first pfac)) 0)
               (build-list (/ fac
                              (first pfac))
                          (lambda (x) (first pfac)))]
           [else
                (make-gcd fac
                        (rest pfac))]))

(define (euclid-gcd n m)
        (cond [(zero? m) n]
              [else (euclid-gcd m (remainder n m))]))
                    


(define (contains mainlist checklist listw)
        (cond [(empty? checklist) (append listw   mainlist)]
              [(empty? mainlist)                      "a"]
              [(< (first mainlist) (first checklist)) "a"]
              [(= (first mainlist) (first checklist))
                                  (contains (rest mainlist)
                                           (rest checklist)
                                                     listw)]
              [else    (contains (rest mainlist)
                                checklist
                                    (append listw
                                          (list (first mainlist))))]))

               

(define (replace-multiples target pos prevpos changelist)
  
       
     (cond [(empty? pos)                     changelist]
           [else 
               (local [(define minrep (contains changelist
                                      (make-factor (first pos)
                                                   prevpos)
                                      empty))
                       (define gcd (euclid-gcd (first pos)
                                               (first prevpos)))
                      (define gcd-lmit (contains changelist
                                      (make-gcd (first pos)
                                                   prevpos)
                                      empty))
                                                
                       (define corrisponding
                             (contains changelist
                                      (build-list (/ (first pos)
                                                    gcd)
                                                 (lambda (x)
                                                  (first prevpos)))
                                      empty))]
                 

           (cond 
           [(= (first pos) 1) (replace-multiples
                                  target
                                  (rest pos)
                                  (list (first pos))
                                  (build-list target (lambda (x) 1)))]
           
           [(list? minrep)
                (replace-multiples target pos prevpos
                                  (append (list (first pos))
                                               minrep))]
           [(list? corrisponding)
                (replace-multiples target pos prevpos
                                  (append (build-list
                                             (/ (first prevpos)
                                                gcd)
                                             (lambda (x) 
                                              (first pos)))
                                          corrisponding))]
           [(list? gcd-lmit)
                (replace-multiples target pos prevpos
                                  (append (list (first pos))
                                          gcd-lmit))]
           [else
               (replace-multiples target (rest pos)
                                 (append (list (first pos))
                                               prevpos)
                                 changelist)]))]))

(define (fewest-coins target denomination)
  (replace-multiples target denomination '(1) empty))

(check-expect (fewest-coins 500 '(1 5 10 25 100 200))
              '(25 10 10))
(check-expect (fewest-coins 12 '(1 3 4))
              '(4 4 4))
(check-expect (fewest-coins 8 '(1 3 4))
              '(4 4))
(check-expect (fewest-coins 16 '(1 2 3 4 16))
              '(16))
(check-expect (fewest-coins 10 '(1 2 3 4))
              '(4 3 3))