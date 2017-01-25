#lang racket

(require (only-in racket/control reset-at shift-at))
(require rackunit)

(define-syntax with-effect-handler
  (syntax-rules ()
    ((_ (handler (effect-class initargs ...)) body ...)
     (let* ((tag (make-continuation-prompt-tag 'eff-handler))
            (handler (new effect-class [prompt-tag tag] initargs ...)))
       (send handler run
             (reset-at tag
               (send handler pure (begin body ...))))))))

(define-syntax send!
  (syntax-rules ()
    ((_ eff-obj method args ...)
     (let ((tag (send eff-obj get-prompt-tag)))
       (shift-at tag k
         (abort-current-continuation tag
           (lambda () (send eff-obj method k args ...))))))))

(define eff-handler%
  (class object%
    (super-new)
    
    (init prompt-tag)
    (define -prompt-tag prompt-tag)
    (define/public (get-prompt-tag)
      -prompt-tag)
    
    (define/public (pure value)
      value)
    
    (define/public (run value)
      value)))

;;;; Effect types (classes) ----------------------------------------------------

(define choice-handler (interface () decide))

(define always%
  (class* eff-handler% (choice-handler)
    (super-new)
    (define/public (decide k)
      (k #t))))

(define choose-all%
  (class* eff-handler% (choice-handler)
    (super-new)
    (define/public (decide k)
      (append (k #t) (k #f)))
    (define/override (pure value)
      (list value))))

(define exn-handler (interface () raise))

(define optionalize%
  (class* eff-handler% (exn-handler)
    (super-new)
    (define/public (raise k exn)
      'none)
    (define/override (pure value)
      (cons 'some value))))

(define state-handler (interface () lookup update!))

(define mstate%
  (class* eff-handler% (state-handler)
    (super-new)
    (init state)
    (define -state state)
    (define/public (lookup k)
      (lambda (s) ((k s) s)))
    (define/public (update! k s*)
      (lambda (s) ((k s*) s*)))
    (define/override (pure v)
      (lambda (s) v))
    (define/override (run f)
      (f -state))))

(define istate%
  (class* eff-handler% (state-handler)
    (super-new)
    (init state)
    (define -state state)
    ;; actually might as well ditch the continuations and use normal `send`:
    (define/public (lookup k)
      (k -state))
    (define/public (update! k s*)
      (k (set! -state s*)))))

;;;; Tests ------------------------------------------------------------------...

(test-case "choice"
  (check-equal?
    (with-effect-handler (c (always%))
      (let ((x (if (send! c decide) 10 20))
            (y (if (send! c decide) 0 5)))
        (- x y)))
    10
    "always")

  (check-equal?
    (with-effect-handler (c (choose-all%))
      (let ((x (if (send! c decide) 10 20))
            (y (if (send! c decide) 0 5)))
        (- x y)))
    '(10 5 20 15)
    "all")

  (check-equal?
   (with-effect-handler (c1 (choose-all%))
     (with-effect-handler (c2 (choose-all%))
       (let ((x (if (send! c1 decide) 10 20))
             (y (if (send! c2 decide) 0 5)))
         (- x y))))
   '((10 5) (20 15))
   "all-all")

  (check-equal?
   (with-effect-handler (c2 (choose-all%))
     (with-effect-handler (c1 (choose-all%))
       (let ((y (if (send! c2 decide) 0 5))
             (x (if (send! c1 decide) 10 20)))
         (- x y))))
   '((10 20) (5 15))
   "all-lla"))

(test-case "exception"
  (check-equal?
   (with-effect-handler (e (optionalize%))
     (+ 3 (send! e raise 'hell)))
   'none
   "exn:optionalize"))

(test-case "state"
  (check-equal?
    (with-effect-handler (r (mstate% [state 3]))
      (send! r update! (add1 (send! r lookup)))
      (send! r lookup))
    4
    "state:monadic")

  (check-equal?
    (with-effect-handler (r (istate% [state 3]))
      (send! r update! (add1 (send! r lookup)))
      (send! r lookup))
    4
    "state:field"))
