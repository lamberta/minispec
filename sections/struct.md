## Struct

~~~
(defstruct point x (y 10)) ;defines point to be a structure with 2 fields, y with default
;;also implicitly defines the functions make-point, point-p, copy-point, point-x, point-y
(defparameter *p* (make-point :x 0 :y 0) ;create point, specify fields with keyword args
*p* => #S(POINT :X 0 :Y 0)
(point-x *p*) => 0 ;get slot
(setf (point-y *p*) 2)) ;set slot
(point-p *p*) => T ;check type
(typep *p* 'point) => T
~~~
