#lang racket/base

(require racket/trace (for-syntax racket/base racket/splicing) racket/splicing)
(provide define/trace)


;; maps the hidden names to the original names
(define cs135-tracemap (make-hasheq))

;; alters trace so that when printing name, it consults the cs135-tracemap
(let ([default-trace-res (current-trace-print-results)]
      [default-trace-args (current-trace-print-args)])
  (current-trace-print-results
   (λ (name results depth)
     (default-trace-res (hash-ref cs135-tracemap name name) results depth)))
  (current-trace-print-args
   (λ (name args kw-names kw-values depth)
     (default-trace-args (hash-ref cs135-tracemap name name) args kw-names kw-values depth))))



(define-syntax (define/trace stx)
  (syntax-case stx ()
    [(_ (f) body ...) ;; put this first since arg ... means 0 or more
     (raise-syntax-error 'define/trace "expected at least one variable after the function name, but found none"
                         #'f)]
    [(_ (f arg ...) body)
     (with-syntax
         ; break hygiene to snag define from the current lexical context
         ([student-define (datum->syntax stx 'define)]
         ; generate a fake name for the real function.  temporaries are always distinct
          [(f1) (generate-temporaries #'(f))])
       (syntax/loc stx
           (begin
             (define (f1 arg ...) body)
             (trace f1)
             (hash-set! cs135-tracemap 'f1 'f)
             (student-define (f arg ...) (f1 arg ...)))))]
    
    [(_ name value) ; if they try to use it to define a constant
     (raise-syntax-error 'define/trace "define/trace can only be used to define functions" stx)]
    [(_ arg ...) ; other cases it just maps to a define so that that syntax can raise its own errors
     (with-syntax ([student-define (datum->syntax stx 'define)])
       (syntax/loc stx
         (student-define arg ...)))]
    [_
     (raise-syntax-error 'define/trace "expected an open parenthesis before define/trace, but found none"
                         stx)]))

