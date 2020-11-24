;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus-a08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (addtosubparis n prev)
   (map (lambda (x) (append x (list n)))
      prev))


(define (subsets1 lon)
  (foldl (lambda (x rorr)
                        (append rorr
                        (addtosubparis x rorr)))

         '(())
         lon))


(define (subsets2 lon) (foldl (lambda (x rorr) (append rorr (map
(lambda (x1) (append x1 (list x)))rorr)))'(())lon))

   