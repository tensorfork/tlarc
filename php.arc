(def php-encode-string (x)
  (let s (string x)
    (+ "s:" (len s) ":\"" s "\";")))

(def php-encode-kv ((k v))
  (+ (php-encode k)
     (php-encode v)))

(def php-encode-pairs (kvs)
  (+ "a:@(len kvs):{" (mapcat php-encode-kv kvs) "}"))

(def arrlist (l)
  (let i 0
    (each x l
      (out (list i x))
      (++ i))))

(def php-encode (x)
  (case (type x)
    num          "d:@x;"
    int          "i:@x;"
    bool         (+ "b:" (if x "true" "false") ";")
    (sym string) (if x
                     (php-encode-string x)
                     (php-encode-pairs (arrlist x))) ; empty list
    cons         (php-encode-pairs (arrlist x))
    table        (php-encode-pairs (tablist x))
    (err "Bad type @x")))
