(include "extden.scm")

(define (main . args)
  (let ((expr (make <add1> 'expr (make <int> 'n 5))))
    (write (handle (interpret expr) (make <exc>)))))
