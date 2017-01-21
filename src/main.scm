(include "extden.scm")

(define-class <ctx> (<exc> <env>))

(define (main . args)
  (let* ((f (make <lambda> 'formal 'x
                           'body (make <add1> 'expr (make <var> 'name 'x))))
         (expr (make <app> 'op f 'arg (make <int> 'n 5))))
    (write (run (handle (interpret expr) (make <ctx> 'bindings '()))))))
