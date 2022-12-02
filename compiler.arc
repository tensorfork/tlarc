(load "runtime.arc")

(= nil* (sym "nil"))

(= environment* (make-parameter (list (obj) value)
                                false
                                'environment*))

(mac w/environment (frames . body)
  `(w/param ,environment* ,frames
     ,@body))

(mac w/frame body
  `(w/environment (+ (list (obj)) (environment*))
     ,@body))

(def get-frame (k frame)
  (let nothing (list 'nothing)
    (if (namespace? frame)
        (let v (namespace-variable-value k true (fn () nothing) frame)
          (unless (is v nothing) (cons k v)))
        (in (type frame) 'table 'fn)
        (let v (frame k nothing)
          (unless (is v nothing) (cons k v)))
        ;(is (type frame) 'cons)
        ;(if (caris frame k) frame)
        (err "Unexpected frame type" frame))))

(def get-environment (k (o env (environment*)))
  (point return
    (each frame env
      (aif (get-frame k frame) (return it)))))

(def set-environment (name k v (o env (environment*)))
  (withs (frame (car env)
          entry (aif (get-frame name frame) (cdr it) (obj)))
    (sref entry v k)
    (sref frame entry name)))

(def sym? (x)
  (and (is (type x) 'sym)
       (isnt x nil*)
       x))

(def rep? (x . kinds)
  (let nothing (list 'nothing)
    (point return
      (if (mem (type x) kinds)
          (rep x)
          (is (type x) 'table)
          (each p kinds
            (let v (x p nothing)
              (unless (is v nothing)
                (return v))))))))

(def get-env (k . ps)
  (when (sym? k)
    (whenlet (k . v) (get-environment k)
      (if ps (apply rep? v ps) v))))

(def set-env (name . kvs)
  (each (k v) (hug kvs)
    (set-environment name k v))
  nil)

(def unique-root (x)
  (sym:trim (coerce x 'string) 'end digit))

(def unique-count (x)
  (let name (unique-root x)
    (let x (cut (cat x) (len:cat name))
      (if (> (len x) 0)
          (int x)
          -1))))

(def unique (x)
  (if (sym? x)
      (if (get-environment x)
          (unique (sym:cat (unique-root x) (+ (unique-count x) 1)))
          x)
      (unique 'cons)))

(def transformer-function (k)
  (get-env k 'transformer))

(def transformer? (k)
  (is? (transformer-function k)))

(def transformer-form? (x)
  (and (acons x) (transformer? (car x))))

(def macro-function (x)
  (get-env x 'mac 'macro))

(def macro? (x)
  (is? (macro-function x)))

(def alias-expansion (k (o seen))
  (get-env k 'alias))

(def alias? (x)
  (is? (alias-expansion x)))

(def expand-macro (form)
  ;(prs 'expand-macro form "\n")
  (macroexpand (expand1 form)))

(def expand1 (form)
  (let (name . args) form
    (apply (macro-function name) args)))

(def expand-transformer (form)
  ((transformer-function (car (car form))) form))

(def expand-syntax (form)
  (when (ssyntax? form)
    (ssexpand form)))

(= expand-atom-functions* (list expand-syntax))

(def expand-atom (form)
  (point return
    (each f expand-atom-functions*
      (let x (f form)
        (unless (is x nil)
          (return x))))
    form))

(def expand-alias (x)
  (let form (alias-expansion x)
    (if (is? form)
        (if (id form x)
            form
            (macroexpand form))
        (macroexpand x))))

(def macroexpand-atom (form)
  (if (alias? form)
      (expand-alias form)
    (let expr (expand-atom form)
      (if (is expr form) expr (macroexpand expr)))))

(def macroexpand (form)
  (if (atom form) (macroexpand-atom form)
      ;(none? form) (y-map macroexpand form)
      (no form) form
    (withs (x (macroexpand (car form))
            args (cdr form)
            form (cons x args))
      (if (is x nil) (macroexpand args)
          (is x '%expansion) (car args)
          (macro? x) (expand-macro form)
          (transformer-form? x) (expand-transformer form)
        `(,x ,@(map macroexpand args))))))

(def expand-definition (kind name args body)
  (let id (sym:cat name "-" kind)
    ;(let (body ks) (y-unzip body)
    ;  `(let ,id (fn ,args ,@body)
    ;     (set-env ',name ',kind ,id ,@(each (k v) ks
    ;                                    (out `',k)
    ;                                    (out v)))))))
    `(let ,id (fn ,args ,@body)
       (set-env ',name ',kind ,id))))

(def bind-atom (lh rh)
  (if (atom lh) `(,lh ,rh)))

(def bind-optional (lh rh)
  (when (caris lh 'o)
    (let (_ var val) lh
      (bind var `(%if (%eq ,rh nil) ,(either val 'nil) ,rh)))))

(def bias (k) k)

(def bind-dotted (args)
  (mkproper args))

(def bind-destructuring (lh rh)
  (let lh (bind-dotted lh)
    (w/uniq id
      (let bs (list id rh)
        (each (k v) (y-pairs lh)
          (let x (if (is k 'rest)
                     `(tl ,id ,(y-length lh))
                   `(hd ,id ',(bias k)))
            (join! bs (bind v x))))
        bs))))

(def bind (lh rh)
  (or (bind-atom lh rh)
      (bind-optional lh rh)
      (bind-destructuring lh rh)))

(mac define-macro (name args . body)
  (expand-definition 'macro name args body))

(mac define-transformer (name args . body)
  (expand-definition 'transformer name args body))

(mac define-special (name args . body)
  (expand-definition 'special name args body))

(define-macro %function (args . body)
  `(%expansion (%function ,args ,(expand-body body))))

(define-macro %local (name value)
  ;(prn `(%local ,name ,value))
   (let x (macroexpand value)
     (if (sym? x)
         (do (set-env name 'alias x)
             `(%do))
         (let id (unique name)
           (set-env name 'alias id)
           (set-env id 'value t)
           `(%expansion (%local ,id ,x))))))

(define-macro fn (args . body)
  `(%function ,args ,@body))

(define-macro do body
  `(%do ,@body))

(def expand-body (body)
  (let forms (keep is? (each form (map macroexpand body)
                         (if (caris form '%do)
                             (map out (cdr form))
                             (out form))))
    (let forms (append (keep ~atom (cut forms 0 -1))
                       (list (last forms)))
      (if (one? forms) (car forms) `(%do ,@forms)))))

;(def expand-body (body)
;  `(%do ,@(map macroexpand body)))

(define-macro %do body
  `(%expansion ,(expand-body body)))

(define-macro assign (name value)
  `(%do (%set ,name ,value)
        ,name))

;(define-macro %set (name value)
;  (with (var (macroexpand name)
;         val (macroexpand value))
;    `(%do (%expansion (%set ,var ,val))
;          ,var)))

(define-macro no (x)
  (let x (macroexpand x)
    (if (caris x 'no) (cadr x) `(%expansion (no ,x)))))

(define-macro quote (x)
  `(%expansion (quote ,x)))

(def expand-quasiquote (x (o func macroexpand))
  (list 'quasiquote (expand-quasiquote-1 1 x func)))

; process the argument of a quasiquote. keep track of
; depth of nesting. handle unquote only at top level (level = 1).
; complete form, e.g. x or (fn x) or (unquote (fn x))

(def expand-quasiquote-1 (level x func)
  (if (is level 0)
      (func x)
      (caris x 'unquote)
      (list 'unquote (expand-quasiquote-1 (- level 1) (cadr x) func))
      (and (caris x 'unquote-splicing) (is level 1))
      (list 'unquote-splicing (expand-quasiquote-1 (- level 1) (cadr x) func))
      (caris x 'quasiquote)
      (list 'quasiquote (expand-quasiquote-1 (+ level 1) (cadr x) func))
      (acons x)
      (imap (fn (x) (expand-quasiquote-1 level x func)) x)
    (ac-quoted x)))

(define-macro quasiquote (x)
  `(%expansion ,(expand-quasiquote x)))

(define-macro compose args
  `(%expansion (compose ,@(map macroexpand args))))

(define-macro complement args
  `(%expansion (complement ,@(map macroexpand args))))

;(define-macro let (var val . body)
;  `(%expansion
;     (let ,var ,(macroexpand val)
;       ,@(map macroexpand body))))

(define-transformer %function (form)
  (let ((_ args . body) . vals) form
    (if (and (is (len vals) 1)
             (is (errsafe:len args) 1))
        (do (prn (car args) " " (car vals) " " body) form)
        form)))

(define-transformer %function (form)
  (let ((_ args . body) . vals) form
    (if (and (is (len vals) 1)
             (is (y-length args) 1))
        (w/frame
          (macroexpand
            `(%do ,@(each (k v) (zip args vals)
                      (out `(%local ,k ,v)))
                  ,@body)))
        form)))

(define-transformer compose (((_ . fns) . args))
  (let expr `(,(last fns) ,@args)
    (each f (rev:almost fns)
      (assign expr `(,f ,expr)))
    expr))

(define-transformer complement (((_ f) . args))
  `(no (,f ,@args)))

(define-transformer andf (((_ . fns) . args))
  (let gs (map unique args)
    `((fn ,gs
        (and ,@(each f (rev fns)
                 (out `(,f ,@gs)))))
      ,@args)))

