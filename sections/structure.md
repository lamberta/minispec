## Structures

* [CLtL2: Structures](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node168.html)

### [defstruct] structure-name \[slot-options\*\] \[doc\] => structure-name

Defines a named structured type, with named slots as
specified by the *slot-options*. Implictly defines
a constructor function, slot accessors, and convenience
functions for the structure: `make-[struct]`, `[struct]-p`,
`copy-[struct]`, `[struct]-[slotname]`.

~~~
(defstruct point
  x
  (y 10))

(setf pt (make-point :x 5)) ;=> #S(POINT :X 5 :Y 10)
(point-y pt)                ;=> 10
(setf (point-y pt) 20)
(point-p pt)                ;=> T
(typeof pt 'point)          ;=> T
(copy-point pt)             ;=> #S(POINT :X 5 :Y 20)
~~~

### [copy-structure] structure => copy

Returns a *copy* of the *structure*. Only the structure
itself is copied; not the values of the slots.
