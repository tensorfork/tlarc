(mac guard body
  `(on-err (fn (c) (list false c))
           (fn () (list true (do ,@body)))))

(mac eif (var val (o fail) . ok)
  (w/uniq res
    `(let (,res ,var) (guard ,val)
       (if ,res (do ,@ok) ,fail))))

(or= environment* (make-param (list (obj)) #f 'environment*))

(assign id #'eq?)

(def nil? (x) (if (id x nil) true false))
(def is? (x) (if (id x nil) false true))

(def obj? (x) (acons x))

;(def proper (x)
;  (or (no x)
;      (and (acons x) (proper (cdr x)))))

(def mkproper (x (o rest '#:rest))
  (if (nil? x) nil
      (acons x) (cons (car x) (mkproper (cdr x) rest))
    (list rest x)))

(def y-unzip (l)
  (unzip-list (mkproper l)))

(def y-zip (vs ks (o f idfn))
  (accum a
    (each v vs
      (a (f v)))
    (each (k v) ks
      (a k)
      (a (f v)))))

(def y-map (f l)
  (let (vs ks) (y-unzip l)
    (y-zip vs ks f)))

(def y-list (l)
  (y-map idfn l))

(def y-pairs (l)
  (let (vs ks) (y-unzip l)
    (+ (zip (range 0 (len vs)) vs)
       (map (fn ((k v))
              (list (coerce k 'sym) v))
            ks))))

(def y-length (x)
  (let (vs ks) (y-unzip x)
    (len vs)))

(def none? (x) (is (y-length x) 0))
(def some? (x) (> (y-length x) 0))
(def many? (x) (> (y-length x) 1))
(def one? (x) (is (y-length x) 1))
(def two? (x) (is (y-length x) 2))
(def edge (x) (- (y-length x) 1))

(def table? (x) (is (type x) 'table))
(def number? (x) (in (type x) 'int 'num))
(def function? (x) (is (type x) 'fn))

(def either (x . args)
  (if (is? x) x
      (none? args) x
    (apply either (hd args) (tl args))))

(def hd (x (o k 0) (o fail))
  (if (table? x)
      (x k fail)
    (let (vs ks) (y-unzip x)
      (eif key (coerce k 'keyword)
           (eif val (vs (if (< k 0)
                            (+ k (len vs))
                            k))
                fail
                val)
           (aif (assoc key ks)
                (cadr it)
                fail)))))

(def tl (x (o from 1) (o upto nil))
  (let (vs ks) (y-unzip x)
    (y-zip (cut vs from upto) ks)))

(def has? (l k)
  (let nothing (obj)
    (let x (hd l k nothing)
      (if (id x nothing) false true))))

(def almost (x) (tl x 0 -1))
(def inner (x) (tl x 1 -1))

(def hd? (l (o x))
  (and (obj? l)
       (if (function? x)
           (x (hd l))
           (nil? x) l
         (id (hd l) x))))

(mac step (v l . body)
  `(each ,v (car (y-unzip ,l))
     ,@body))

(mac join! (l . args)
  `(assign ,l (+ ,l ,@args)))

(mac add! (l x)
  `(join! ,l (list ,x)))

(mac drop! (l)
  `(do1 (hd ,l -1)
        (assign ,l (almost ,l))))
