#lang racket

(require (only-in racket/control reset-at shift-at))
(require rackunit)

(define-syntax with-effect-handler
  (syntax-rules ()
    ((_ (name (effect-class initargs ...)) body ...)
     (let ((tag (make-continuation-prompt-tag 'eff-handler)))
       (parameterize ((name (new effect-class [prompt-tag tag] initargs ...)))
         (reset-at tag (send (name) pure (begin body ...))))))))

(define-syntax send!
  (syntax-rules ()
    ((_ eff-obj method args ...)
     (shift-at (send (eff-obj) get-prompt-tag) k
       (let ((v (send (eff-obj) method k args ...)))
         (abort-current-continuation
          (send (eff-obj) get-prompt-tag)
          (lambda () v)))))))

(define eff-handler%
  (class object%
    (super-new)
    
    (init prompt-tag)
    (define -prompt-tag prompt-tag)
    (define/public (get-prompt-tag)
      -prompt-tag)
    
    (define/public (pure value)
      value)))

;;;; Examples ------------------------------------------------------------------

(define (choice)
  (define choice-handler (interface () decide))
  
  (define c (make-parameter #f))

  (define always%
    (class* eff-handler% (choice-handler)
      (super-new)
      (define/public (decide k)
        (k #t))))

  (check-equal?
    (with-effect-handler (c (always%))
      (let ((x (if (send! c decide) 10 20))
            (y (if (send! c decide) 0 5)))
        (- x y)))
    10
    "choice:always")

  (define choose-all%
    (class* eff-handler% (choice-handler)
      (super-new)
      (define/public (decide k)
        (append (k #t) (k #f)))
      (define/override (pure value)
        (list value))))

  (check-equal?
    (with-effect-handler (c (choose-all%))
      (let ((x (if (send! c decide) 10 20))
            (y (if (send! c decide) 0 5)))
        (- x y)))
    '(10 5 20 15)
    "choice:all")

  (define c1 (make-parameter (c)))
  (define c2 (make-parameter (c)))

  (check-equal?
   (with-effect-handler (c1 (choose-all%))
     (with-effect-handler (c2 (choose-all%))
       (let ((x (if (send! c1 decide) 10 20))
             (y (if (send! c2 decide) 0 5)))
         (- x y))))
   '((10 5) (20 15))
   "choice:all-all"))

;  (check-equal?
;   (with-effect-handler (c2 (choose-all%))
;     (with-effect-handler (c1 (choose-all%))
;       (let ((x (if (send! c2 decide) 10 20))
;             (y (if (send! c1 decide) 0 5)))
;         (- x y))))
;   '((10 20) (5 15))
;   "choice:all-lla"))
    

(define (exn)
  (define exn-handler (interface () raise))

  (define optionalize%
    (class* eff-handler% (exn-handler)
      (super-new)
      (define/public (raise k exn)
        'none)
      (define/override (pure value)
        (cons 'some value))))
  
  (define e (make-parameter #f))
  
  (check-equal?
   (with-effect-handler (e (optionalize%))
     (+ 3 (send! e raise 'hell)))
   'none
   "exn:optionalize"))
    

;;;; Main ----------------------------------------------------------------------

(define (main)
  (choice)
  (exn))

(main)