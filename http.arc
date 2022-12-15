#'(require net/http-client)
#'(require net/url)
#'(require net/url-string)

(mac defget body
  `(annotate 'prop [do ,@body]))

(deftem url
  scheme "http"
  host   nil
  port   (defget (case _!scheme
                   "https" 443
                   "http" 80))
  path   nil
  query  nil
  anchor nil)

(def url-parse (url)
  (unless (string-contains? url "://")
    (= url (+ "http://" url)))
  (aand (cdr:vector->list:struct->vector:string->url url)
        (let (scheme user host port path-absolute? path query anchor) it
          (inst 'url
                'scheme (or scheme nil)
                'host (or host nil)
                'port (or port nil)
                'path (map path/param-path path)
                'query (or query nil)
                'anchor (or anchor nil)))))

(def url-unparse (url)
  (make-url (or url!scheme false)
            (or url!user false)
            (or url!host false)
            (and (isnt url!port (if (is url!scheme "https") 443 80))
                 (or url!port false))
            t ; path-absolute?
            (map [make-path/param _ nil] url!path)
            (or url!query false)
            (or url!anchor false)))

(def fetch (url binary: (o binary? false))
  (withs (u (url-parse url)
          path (+ "/" (string-join u!path "/"))
          ssl (is u!scheme "https")
          port (or u!port (if ssl 443 80))
          (status hdr i) (w/values:http-sendrecv
                           u!host
                           port: port
                           ssl?: ssl
                           path)
          (version code . msg) (tokens:bytes->string/utf-8 status)
          code (or (errsafe:int code) code)
          msg (string-join msg " "))
      (obj version version
           code    code
           status  msg
           headers (each h hdr
                     (aand (bytes->string/utf-8 h)
                           (string-split it ": ")
                           (let (k v) it
                             (out (sym:downcase k) v))))
           body    (if binary?
                       (allbytes i)
                       (allchars i)))))

(def http-demo ((o url "https://news.ycombinator.com") (o filename))
  (aand (fetch url binary: t)
        (if filename
            (do (savebytes it!body filename)
                (ero "Saved" (len it!body) "bytes to" filename))
            (do (each (k v) it!headers
                  (ero k ": " v sep: ""))
                (ero)
                (writebytes it!body)))))

http-demo

; Print an image to the terminal:
;
;   $ imgcat <(bin/arc http.arc https://sep.yimg.com/ay/paulgraham/index-1.gif)
;
; Fetch hn:
;
;   $ bin/arc http.arc https://news.ycombinator.com
;
; Extract urls from hn's html
; 
;   $ bin/arc http.arc https://news.ycombinator.com | rg '"(http.*?)"' -o
