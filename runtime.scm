#lang racket/load

(module racket-libs racket/base
  (require racket/bool)
  (require racket/undefined)
  (require syntax/stx)
  (require syntax/srcloc)

  (require racket/port)
  (require racket/pretty)
  (require racket/runtime-path)
  (require racket/system)
  (require racket/tcp)
  (require racket/unsafe/ops)
  (require racket/path)
  (require racket/trace)
  (require racket/async-channel)
  (require racket/struct)

  (require ffi/unsafe)
  (require ffi/unsafe/define)
  (require ffi/vector)
  (require ffi/cvector)
  (require ffi/unsafe/cvector)

  (require json)
  (provide (all-defined-out))
  (provide (for-syntax (all-defined-out))))

(require 'racket-libs)

; (module racket 'racket-libs
;   (define true #t)
;   (define false #f)

;   (define nan +nan.0)
;   (define inf +inf.0)
;   (define -inf -inf.0)

;   (define-namespace-anchor namespace-anchor)
;   (define namespace (namespace-anchor->namespace namespace-anchor))

;   (provide (all-defined-out))
;   (provide (for-syntax (all-defined-out))))

(require (prefix-in scm- 'racket-libs))
(require (for-syntax (prefix-in scm- 'racket-libs)))
;(require 'racket-libs)

(require racket/bool)
(define t true)
(define nil '())

(require racket/undefined)
(define unset undefined)

(define nan +nan.0)
(define inf +inf.0)
(define -inf -inf.0)

(require (prefix-in scm- racket/base))

(define-syntax fn
  (syntax-rules ()
    ((_ (args ...) body ...)
     (lambda (args ...) body ...))))

(define-syntax if
  (syntax-rules ()
    ((_ a b . cs) (scm-if a b (if . cs)))
    ((_ a) (begin a))
    ((_) false)))

(define-syntax let
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     (scm-let ((var val) ...) body ...))
    ((_ var val body ...)
     (scm-let ((var val)) body ...))))

; (define-syntax iflet
;   (syntax-rules ()
;     ((_ var val then args ...)
;      (scm-let* ((v 'var)
;                 ( val))
;        (if 'var then (iflet 'var args ...) )))))

; (define-syntax aif
;   (syntax-rules ()
;     ((_ args ...)
;      (iflet it args ...))))

; https://stackoverflow.com/questions/20054263/capturing-macros-in-scheme
(require racket/stxparam)

(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside aif")))

(define-syntax aif
  (syntax-rules ()
    ((_ condition true-expr false-expr ...)
     (let ([tmp condition])
       (if (yes tmp)
         (syntax-parameterize ([it (make-rename-transformer #'tmp)])
                              true-expr)
         (aif false-expr ...))))
    ((_ condition) (begin condition))
    ((_) false)))


(define-syntax def
  (syntax-rules ()
    ((_ name (args ...) body ...)
     (define (name args ...) body ...))
    ((_ name args body ...)
     (define (name . args) body ...))))

(define-syntax set
  (syntax-rules ()
    ((_ var val)
     (namespace-set-variable-value! 'var val #t))
    ((_ var val more ...)
     (begin (set var val)
            (set more ...)))
    ((_) (begin))))

;(namespace-set-variable-value! 'cons scm-cons #t)
(set cons scm-cons)
(define (join (x nil) (y nil))
  (scm-cons x y))

(define (car x)
  (if (no x) x (scm-car x)))

(define (cdr x)
  (if (no x) x (scm-cdr x)))

(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (caddr x) (car (cdr (cdr x))))

(define (typeof x)
  (let ((tag (vector-ref (struct->vector x) 0)))
    (string->symbol (substring (symbol->string tag) (string-length "struct:")))))

(define (id x y)
  (or (scm-eq? x y)
      (and (scm-number? x) (scm-number? y) (scm-= x y))
      (and (scm-string? x) (scm-string? y) (scm-string=? x y))
      (and (scm-bytes? x) (scm-bytes? y) (scm-bytes=? x y))))

(define (no x)
  (or (id x nil)
      (id x false)))

(define (yes x)
  (not (no x)))

(define (atom x)
  (no (id (typeof x) 'pair)))

(define (all f xs)
  (if (no xs)      t
      (f (car xs)) (all f (cdr xs))
                   false))

(define (some f xs)
  (if (no xs)      false
      (f (car xs)) xs
                   (some f (cdr xs))))

(define (reduce f xs)
  (if (no (cdr xs))
      (car xs)
      (f (car xs) (reduce f (cdr xs)))))

(define (cons . args)
  (reduce join args))

(define (snoc . args)
  (append (car args) (cdr args)))

(def equal args
  (aif (no (cdr args))  t
       (some atom args) (all (fn (_) (id _ (car args))) (cdr args))
                        (and (apply equal (map car args))
                             (apply equal (map cdr args)))))

(def starts (xs pat (f equal))
  (if (no pat)               t
      (atom xs)              false
      (f (car xs) (car pat)) (starts (cdr xs) (cdr pat) f)
                             false))

(def caris (x y (f equal))
  (starts x (list y) f))

(def proper (x)
  (or (no x)
      (and (pair? x) (proper (cdr x)))))

(define (mev s r m)
  (let ((p (car m))
        (g (cadr m)))
      (if (no s)
          (if (yes p)
              (sched p g)
              (car r))
          (sched (if (cdr (binding 'lock s))
                     (cons (list s r) p)
                     (snoc p (list s r)))
                 g))))

(define globe (list (current-namespace)))

(define (bel e (g globe))
  (ev (list (list e nil))
      nil
      (list nil g)))

(define (sched p g)
  (define s_r (car p))
  (define s (car s_r))
  (define r (cadr s_r))
  (set! p (cdr p))
  (ev s r (list (cdr p) g)))

(define (ev s r m)
  (define e_a (car s))
  (define e (car e_a))
  (define a (cadr e_a))
  (set! s (cdr s))
  (aif (literal? e)           (evlit e a s r m)
       (variable? e)          (vref e a s r m)
       (no (proper e))        (sigerr 'malformed s r m)
       (get (car e) forms id) ((cdr it) (cdr e) a s r m)
                              (evcall e a s r m)))

(def isa? (name)
  (fn (_) (starts _ `(lit ,name) id)))

(def evlit (e a s r m)
  (if (starts e '(lit fut))
      ((caddr e) s r m)
      (mev s (cons e r) m)))

(def vref (v a s r m)
  (let g (cadr m)
    (if (inwhere s)
        (aif (or (lookup v a s g)
                 (and (car (inwhere s))
                      (let cell (cons v nil)
                        (xdr g (cons cell (cdr g)))
                        cell)))
             (mev (cdr s) (cons (list it 'd) r) m)
             (sigerr 'unbound s r m))
        (aif (lookup v a s g)
             (mev s (cons (cdr it) r) m)
             (sigerr (list 'unboundb v) s r m)))))

(set smark (join))

(def inwhere (s)
     #f)
  ; (let e (car (car s))
  ;   (and (starts e (list smark 'loc))
  ;        (cddr e))))

(def lookup (e a s g)
  (prs "lookup" e a s g "\n")
  (or (binding e s)
      (get e a id)
      (get e g id)
      (if (id e 'scope) (cons e a)
          (id e 'globe) (cons e g))))
      ; (case e
      ;   scope (cons e a)
      ;   globe (cons e g))))


(def binding (v s)
     #f)
  ; (get v
  ;      (map caddr (keep [starts _ (list smark 'bind) id]
  ;                       (map car s)))
  ;      id))

(define (variable? x)
  (symbol? x))


(set forms nil)

(def get (k l (test id))
  (if (namespace? l)
      (aif (namespace-variable-value k #t (lambda () #f) l)
           (join k it)
           #f)
      (pair? l) (aif (get k (car l) test)
                     it
                     (get k (cdr l) test))
      #f))

(define-syntax fu
  (syntax-rules ()
    ((_ (args ...) body ...)
     (list (list 'lit 'fut (fn (args ...) body ...)) nil))))

(def evcall (e a s r m)
  (mev (cons (list (car e) a)
             (fu (s r m)
               (evcall2 (cdr e) a s r m))
             s)
       r
       m))

(def snap (xs ys (acc nil))
  (if (no xs)
      (list acc ys)
      (snap (cdr xs) (cdr ys) (snoc acc (car ys)))))

(def evcall2 (es a s op_r m)
  (prs "evcall2" es a s op_r m "\n")
  (prn (map (fn (_) (list _ a)) es))
  (define op (car op_r))
  (define r (cdr op_r))
  (if ((isa? 'mac) op)
      (applym op es a s r m)
      (mev (append (map (fn (_) (list _ a)) es)
                   (cons (fu (s r m)
                           (let it (snap es r)
                             (define args (car it))
                             (define r2 (cadr it))
                             (applyf op (rev args) a s r2 m)))
                         s))
           r
           m)))

(def applyf (f args a s r m)
  (mev s (cons (apply f args) r) m))
