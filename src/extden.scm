(use coops)

;;;; Base Classes --------------------------------------------------------------

;; Value representation:
(define-class <value> ())

;; Program representation:
(define-class <ast> ())
(define-class <expr> (<ast>))

;; Result of an interpretation attempt:
(define-class <comp> ())

;; Requests to handle:
(define-class <req-msg> ())

;; Resources that handle requests:
(define-class <resource> ())

;;;; Computations --------------------------------------------------------------

;; Interpretation produced a value:
(define-class <pure> (<comp>)
  ((val reader: pure-val)))

;; Itp needs some help from context and supplies a continuation/callback:
(define-class <effect> (<comp>)
  (req
   cont))

;; Requesting to terminate with an error:
(define-class <error> (<req-msg>)
  (err-msg))

;;;; Interpretation Interface --------------------------------------------------

;; Interpret program as far as possible without assistance from context:
;; interpret : <expr> -> <comp>
(define-generic (interpret ctrl))

;; Handle interactions with context/resources:
;; handle : <comp>, <resource> -> <comp>, <resource>
(define-generic (handle comp res))

(define-method (handle (comp <pure>) res)
  (values (pure-val comp) res))

(define-method (handle (comp <effect>) res)
  (handle* (slot-value comp 'req) (slot-value comp 'cont) res))

;; This is used when handle got an <effect> as the comp:
;; handle* : <request>, (<value> -> <comp>), <resource> -> <comp>, <resource>
(define-generic (handle* req cont res))

;;;; Computations Are Monads ---------------------------------------------------

(define (pure v)
  (make <pure> 'val v))

(define-generic (bind comp f))

(define-method (bind (comp <pure>) f)
  (f (pure-val comp)))

(define-method (bind (comp <effect>) f)
  (make <effect> 'req (slot-value comp 'req)
                 'cont (lambda (v) (bind ((slot-value comp 'cont) v) f))))

;;;; Base Language (Error Handling) --------------------------------------------

;; An error literal (not very useful):
(define-class <err> (<expr>))

;; Handles errors:
(define-class <exc> (<resource>))

(define-method (interpret (ctrl <err>))
  (make <effect> 'req (make <error> 'err-msg "explicit error")
                 'cont (lambda (v) (pure v))))

(define-method (handle* (req <error>) (cont #t) (res <exc>))
  (error "fatal error" (slot-value req 'err-msg)))

;;;; Arithmetic ----------------------------------------------------------------

;; Integer literals:
(define-class <int> (<expr>)
  (n))

;; Increment expression:
(define-class <add1> (<expr>)
  (expr))

;; Decrement expression:
(define-class <sub1> (<expr>)
  (expr))

(define-method (interpret (ctrl <int>))
  (pure (slot-value ctrl 'n)))

(define-method (interpret (ctrl <add1>))
  (bind (interpret (slot-value ctrl 'expr))
        (lambda (n) (pure (add1 n)))))

(define-method (interpret (ctrl <sub1>))
  (bind (interpret (slot-value ctrl 'expr))
        (lambda (n) (pure (sub1 n)))))
