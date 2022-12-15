(mac defprop (name body)
  `(def ,name (annotate 'prop (fn (self) ,body))))

(def make (parent . props)
  (aand (obj parent* (listify parent))
        (do (each (k v) (hug props)
              (= (it k) v))
            it)))

(= polygon (obj name "unknown" x 0 y 0))

(def (polygon mesh) (self x: (o x0 self!x) y: (o y0 self!y))
  (each (x y) self!vertices
    (out (+ x0 x) (+ y0 y))))

(def (polygon draw) (self x: (o x self!x) y: (o y self!y) . args)
  "draw on some display"
  (apply ero 'drawing "polygon" self!name "at" 'x x 'y y 'mesh (self.mesh x: x y: y) args))

(def (polygon copy) (self)
  "return a copy of the receiver"
  (ero 'copying self))

(= filled-polygon (obj parent* 'polygon
                       pattern 'solid))

(def (filled-polygon draw) (self x: (o x self!x) y: (o y self!y) . args)
  "draw and fill on some display"
  (apply ero 'filling self!name "with pattern" self!pattern "at" 'x x 'y y (if args "with") args))

(= rectangle (obj))

(defprop (rectangle extents) (obj x (/ self!width 2)
                                  y (/ self!height 2)))

(defprop (rectangle left)   (- self!extents!x))
(defprop (rectangle right)  (+ self!extents!x))
(defprop (rectangle top)    (- self!extents!y))
(defprop (rectangle bottom) (+ self!extents!y))

(defprop (rectangle vertices)
         (list (list self!left  self!top)
               (list self!left  self!bottom)
               (list self!right self!bottom)
               (list self!right self!top)))

(def rect (width height filled: (o filled? false) . props)
  (apply make (list 'rectangle
                    (if filled? 'filled-polygon 'polygon))
         'width width 'height height props))


