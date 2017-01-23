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
(define-class <request> ())

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

;;;; Computations Are Monads ---------------------------------------------------

(define (pure v)
  (make <pure> 'val v))

(define-generic (bind comp f))

(define-method (bind (comp <pure>) f)
  (f (pure-val comp)))

(define-method (bind (comp <effect>) f)
  (make <effect> 'req (slot-value comp 'req)
                 'cont (lambda (v) (bind ((slot-value comp 'cont) v) f))))

;; Lispy do-notation:
(define-syntax mlet
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((name expr) bindings ...) body ...)
     (bind expr (lambda (name) (mlet (bindings ...) body ...))))))

;;;; Interpretation Interface --------------------------------------------------

;; Interpret program as far as possible without assistance from context:
;; interpret : <expr> -> <comp>
(define-generic (interpret ctrl))

;; Handle interactions with context/resources:
;; handle : <comp>, <resource> -> <comp>, <resource>
(define-generic (handle comp res))

(define-method (handle (comp <pure>) res)
  (values comp res))

(define-method (handle (comp <effect>) res)
  (handle* (slot-value comp 'req) (slot-value comp 'cont) res))

;; This is used when handle got an <effect> as the comp:
;; handle* : <request>, (<value> -> <comp>), <resource> -> <comp>, <resource>
(define-generic (handle* req cont res))

(define-generic (run comp))

(define-method (run (comp <pure>))
  (pure-val comp))

(define-method (run (comp <effect>))
  (error "unhandled effect" comp))

;;;; Base Language (Error Handling) --------------------------------------------

;; An error literal (not very useful):
(define-class <err> (<expr>))

;; Requesting to terminate with an error:
(define-class <error> (<request>)
  (err-msg))

(define (err-eff msg)
  (make <effect> 'req (make <error> 'err-msg msg)
                 'cont pure))

;; Handles errors:
(define-class <exc> (<resource>))

(define-method (interpret (ctrl <err>))
  (err-eff "explicit error"))

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
        (o pure add1)))

(define-method (interpret (ctrl <sub1>))
  (bind (interpret (slot-value ctrl 'expr))
        (o pure sub1)))

;;;; Call-by-Value Lambda Calculus ---------------------------------------------

;; Closure:
(define-class <closure> (<value>)
  (f))

;; Variables:
(define-class <var> (<expr>)
  (name))

;; Lambda:
(define-class <lambda> (<expr>)
  (formal
   body))

;; Function application:
(define-class <app> (<expr>)
  (op
   arg))

 ;; Create closure:
 (define-class <close> (<request>)
   (formal
    body))

;; Read variable:
(define-class <deref> (<request>)
  (name))

;; Handles binding-related requests:
(define-class <env> (<resource>)
  (bindings))

(define (env-ref env name)
  (let ((binding (assoc name (slot-value env 'bindings))))
    (and binding (cdr binding))))

;; FIXME: need to copy unrelated slots to from env to result instance:
(define (env-extend env name value)
  (make (class-of env)
    'bindings (cons (cons name value) (slot-value env 'bindings))))

(define-method (interpret (ctrl <var>))
  (make <effect> 'req (make <deref> 'name (slot-value ctrl 'name))
                 'cont pure))

(define-method (interpret (ctrl <lambda>))
  (make <effect> 'req (make <close> 'formal (slot-value ctrl 'formal)
                                    'body (slot-value ctrl 'body))
                 'cont pure))

(define-method (interpret (ctrl <app>))
  (mlet ((f (interpret (slot-value ctrl 'op)))
         (a (interpret (slot-value ctrl 'arg))))
    (if (equal? (class-of f) <closure>)
      ((slot-value f 'f) a)
      (err-eff "not a closure"))))

(define-method (handle* (req <close>) (cont #t) (res <env>))
  (let* ((formal (slot-value req 'formal))
         (body (slot-value req 'body))
         (bindings (slot-value res 'bindings)))
    (values
      (handle (cont (make <closure>
                      'f (lambda (a)
                            (handle (interpret body)
                                    (env-extend res formal a)))))
              res)
      res)))

(define-method (handle* (req <deref>) (cont #t) (res <env>))
  (let ((value (env-ref res (slot-value req 'name))))
    (if value
      (handle (cont value) res)
      (err-eff "unbound"))))

;;;; Main ----------------------------------------------------------------------

(define-class <ctx> (<exc> <env>))

(define (main . args)
  (let* ((f (make <lambda> 'formal 'x
                           'body (make <add1> 'expr (make <var> 'name 'x))))
         (expr (make <app> 'op f 'arg (make <int> 'n 5))))
    (write (run (handle (interpret expr) (make <ctx> 'bindings '()))))))
