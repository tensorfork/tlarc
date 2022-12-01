
(def join ((o a nil) (o b nil))
  (#'cons a b))

(def id (a b)
  #`(or (eqv? #,a #,b)
        (and (number? #,a) (number? #,b) (= #,a #,b))
        (and (string? #,a) (string? #,b) (string=? #,a #,b))))

(def reduce (f xs)
  (if (no (cdr xs))
      (car xs)
      (f (car xs) (reduce f (cdr xs)))))

(def cons args
  (reduce join args))

(def snoc args
  (append (car args) (cdr args)))

(def caddr (x) (car (cdr (cdr x))))

(def equal args
  (if (no (cdr args))  t
      (some atom args) (all [id _ (car args)] (cdr args))
                       (and (apply equal (map car args))
                            (apply equal (map cdr args)))))

(def symbol (x) (id (type x) 'sym))

(def pair   (x) (id (type x) 'cons))

(def char   (x) (id (type x) 'char))

(def string (x) (id (type x) 'string))

(def number (x) (in (type x) 'int 'num))

(def function (x) (id (type x) 'fn))

(def proper (x)
  (or (no x)
      (and (pair x) (proper (cdr x)))))

(def get (k kvs (o f equal))
  (if (namespace? kvs)
      (kvs k)
      (id (type kvs) 'table)
      (aif (kvs k) (cons k it) nil)
      (pair kvs)
      (point break
        (each x kvs
          (aif (pair x)
               (each slot x
                 (if (f (car slot) k)
                     (break slot)))
               (get k x f)
               (break it))))))

(def put (k v kvs (o f equal))
  (cons (list (cons k v))
        kvs))

(def snap (xs ys (o acc))
  (if (no xs)
      (list acc ys)
      (snap (cdr xs) (cdr ys) (snoc acc (car ys)))))

(def begins (xs pat (o f equal))
  (if (no pat)               t
      (atom xs)              nil
      (f (car xs) (car pat)) (begins (cdr xs) (cdr pat) f)
                             nil))

(def caris (x y (o f equal))
  (begins x (list y) f))

(def literal (e)
  (or (in e t nil)
      (in (type e) 'char 'stream)
      (caris e 'lit)
      (string e)
      (number e)))

(def variable (e)
  (if (atom e)
      (no (literal e))
      (id (car e) vmark)))

(assign vmark (list '%vmark))

(def isa? (name)
  [begins _ `(lit ,name) id])

(or= globe (list (current-namespace)))

(def bel (e (o g globe))
  (ev (list (list e nil))
      nil
      (list nil g)))

(def mev (s r (p g))
  (if (no s)
      (if p
          (sched p g)
          (car r))
      (sched (if (cdr (binding 'lock s))
                 (cons (list s r) p)
                 (snoc p (list s r)))
             g)))

(def sched (((s r) . p) g)
  (ev s r (list p g)))

(def ev (((e a) . s) r m)
  (aif (literal e)            (mev s (cons e r) m)
       (variable e)           (vref e a s r m)
       (no (proper e))        (sigerr 'malformed s r m)
       (get (car e) forms id) ((cdr it) (cdr e) a s r m)
                              (evcall e a s r m)))

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

(assign smark (join '%smark))

(def inwhere (s)
  (let e (car (car s))
    (and (begins e (list smark 'loc))
         (cddr e))))

(def lookup (e a s g)
  (or (binding e s)
      (get e a id)
      (get e g id)
      (case e
        scope (cons e a)
        globe (cons e g))))

(def binding (v s)
  (get v
       (list (map caddr (keep [begins _ (list smark 'bind) id]
                              (map car s))))
       id))

(def sigerr (msg s r m)
  (aif (binding 'err s)
       (applyf (cdr it) (list msg) nil s r m)
       (err 'no-err (coerce msg 'string))))

(mac fu args
  `(list (list smark 'fut (fn ,@args)) nil))

(def evmark (e a s r m)
  (case (car e)
    fut  ((cadr e) s r m)
    bind (mev s r m)
    loc  (sigerr 'unfindable s r m)
    prot (mev (cons (list (cadr e) a)
                    (fu (s r m) (mev s (cdr r) m))
                    s)
              r
              m)
         (sigerr 'unknown-mark s r m)))

(assign forms (list (list (cons smark evmark))))

(mac form (name parms . body)
  `(assign forms (put ',name ,(formfn parms body) forms)))

;(def formfn (parms body)
;  (with (v  (uvar)
;         w  (uvar)
;         ps (parameters (car parms)))
;    `(fn ,v
;       (eif ,w (apply (fn ,(car parms) (list ,@ps))
;                      (car ,v))
;               (apply sigerr 'bad-form (cddr ,v))
;               (let ,ps ,w
;                 (let ,(cdr parms) (cdr ,v) ,@body))))))

(def formfn (parms body)
  `(fn ,parms ,@body))

(form quote ((e) a s r m)
  (mev s (cons e r) m))

(form if (es a s r m)
  (if (no es)
      (mev s (cons nil r) m)
      (mev (cons (list (car es) a)
                 (if (cdr es)
                     (cons (fu (s r m)
                             (if2 (cdr es) a s r m))
                           s)
                     s))
           r
           m)))

(def if2 (es a s r m)
  (mev (cons (list (if (car r)
                       (car es)
                       (cons 'if (cdr es)))
                   a)
             s)
       (cdr r)
       m))

(form where ((e (o new)) a s r m)
  (mev (cons (list e a)
             (list (list smark 'loc new) nil)
             s)
       r
       m))

(form dyn ((v e1 e2) a s r m)
  (if (variable v)
      (mev (cons (list e1 a)
                 (fu (s r m) (dyn2 v e2 a s r m))
                 s)
           r
           m)
      (sigerr 'cannot-bind s r m)))

(def dyn2 (v e2 a s r m)
  (mev (cons (list e2 a)
             (list (list smark 'bind (cons v (car r)))
                   nil)
             s)
       (cdr r)
       m))

(def evcall (e a s r m)
  (mev (cons (list (car e) a)
             (fu (s r m)
               (evcall2 (cdr e) a s r m))
             s)
       r
       m))

(def evcall2 (es a s (op . r) m)
  (if ((isa? 'mac) op)
      (applym op es a s r m)
      (mev (append (map [list _ a] es)
                   (cons (fu (s r m)
                           (let (args r2) (snap es r)
                             (applyf op (rev args) a s r2 m)))
                         s))
           r
           m)))

(def applym (mac args a s r m)
  (applyf (caddr mac)
          args
          a
          (cons (fu (s r m)
                  (mev (cons (list (car r) a) s)
                       (cdr r)
                       m))
                s)
          r
          m))

(def applyf (f args a s r m)
  (if (id f apply)   (applyf (car args) (reduce join (cdr args)) a s r m)
      (function f)   (mev s (cons (apply f args) r) m)
      (caris f 'lit) (if (proper f)
                         (applylit f args a s r m)
                         (sigerr 'bad-lit s r m))
                     (sigerr 'cannot-apply s r m)))

(def applylit (f args a s r m)
  (aif (and (inwhere s) (find [(car _) f] locfns))
       ((cadr it) f args a s r m)
       (let (tag . rest) (cdr f)
         (case tag
           prim (applyprim (car rest) args s r m)
           clo  (let ((o env) (o parms) (o body) . extra) rest
                  (if (and (okenv env) (okparms parms))
                      (applyclo parms args env body s r m)
                      (sigerr 'bad-clo s r m)))
           mac  (applym f (map [list 'quote _] args) a s r m)
           cont (let ((o s2) (o r2) . extra) rest
                  (if (and (okstack s2) (proper r2))
                      (applycont s2 r2 args s r m)
                      (sigerr 'bad-cont s r m)))
                (aif (get tag virfns)
                     (let e ((cdr it) f (map [list 'quote _] args))
                       (mev (cons (list e a) s) r m))
                     (sigerr 'unapplyable s r m))))))

