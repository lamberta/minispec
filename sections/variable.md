### [defparameter] name value \[doc\] => name

Unconditionally assign *value* to a global dynamic
variable. The convention for naming a (global) special
variable is: `*name*`.

~~~
(defparameter *x* 1) ;=> *X*
~~~

### [defvar] name \[value \[doc\]\] => name

Unless bound already, assign *value* to a global dynamic
variable. The convention for naming a (global) special
variable is: `*name*`.

~~~
(defvar *x* 'a) ;=> *X*
(defvar *x* 'b)
*x*             ;=> A
~~~

### [defconstant] name value \[doc\] => name

Assign *value* to a global constant variable. A constant can
be redefined by [defconstant]. The convention for naming a
constant is: `+name+`.

~~~
(defconstant +x+ 'a) ;=> +X+
(setf +x+ 'b)        ;=> [error]
~~~

### [setf] \{place value\}\* => result\*

Sets the value of *place* to *value*. Multiple pairs are set
sequentially, use `psetf` to set in parallel. `setf` works
with symbols and forms, and is preferred, while [setq] and
[psetq] can only set symbols.

### [let] \(\{var | \(var \[value\]\)\}\*\) forms\* => result\*

Evaluate *forms* with *vars* lexically bound (or [NIL]) to
values, assigned in parallel. Use [let\*] to assign sequentially.

~~~
(let ((x 1))
  (list x))       ;=> (1)

(let* ((x 1)
       (y (+1 x)))
  (list x y))      ;=> (1 2)
~~~

### [multiple-value-bind] \(vars\*\) values-form body-forms\* => result\*

Evaluate *body-forms* with *vars* lexically bound to the
return values of *values-form*. Return values of *body-forms*.

~~~
(multiple-value-bind (q r) (floor 7.5)
  (list q r))                          ;=> (7 0.5)
~~~

### [destructuring-bind] lambda-list expression forms\* => result\*

Evaluate *forms* with variables from tree *lambda-list*
bound to corresponding elements of tree *expression*, and
return their values.

* [HyperSpec: Destructuring Lambda Lists](http://www.lispworks.com/documentation/HyperSpec/Body/03_de.htm)
* [PCL: Destructuring-Bind](http://www.gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html#destructuring-bind)

~~~
(destructuring-bind (a b c) (list 1 (list 2 20) 3)
  (list a b c))       ;=> (1 (2 20) 3)

(destructuring-bind (a (b &optional c) d) (list 1 (list 2) 3)
  (list a b c d))     ;=> (1 2 NIL 3)

(destructuring-bind (&whole whole &key a b c) (list :c 3 :b 2 :a 1)
  (list a b c whole)) ;=> (1 2 3 (:C 3 :B 2 :A 1))
~~~

### [multiple-value-setq] \(vars\) form => result

Assign multiple values returned by *form* to *vars*.

~~~
(multiple-value-setq (a b) (values 1 2))
~~~

### [progv] symbols values forms\* => result\*

Evaluate *forms* with locally established dynamic bindings of
*symbols* to *values*.

~~~
(setf *a* 1)
(progv '(*a*) '(2)
  (print *a*))     ;=> [prints 2]
~~~

### [shiftf] place\* newvalue => first-value

Store *newvalue* in rightmost *place*, shifting values of
places left, returning first *place*.

~~~
(setf a :a b :b)
(shiftf a b :c)  ;=> :A
(list a b)       ;=> (:B :C)
~~~

### [rotatef] places\* => nil

Rotate values of *places* left, the first becoming new last place’s value.

~~~
(setf a :a b :b c :c)
(rotatef a b c)       ;=> NIL
(list a b c)          ;=> (:B :C :A)
~~~

### [makunbound] symbol => symbol

Makes *symbol* unbound, regardless of whether it was previously bound.

~~~
(setf a 1)
(boundp 'a)     ;=> T
(makunbound 'a) ;=> A
(boundp 'a)     ;=> NIL
~~~
