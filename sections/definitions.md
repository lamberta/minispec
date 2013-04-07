# Define Variables, Functions, Macros

* [Variables](#variables)
* [Functions](#functions)
* [Macros](#macros)

Style Guides

* [Naming Conventions](http://www.cliki.net/Naming%20conventions)
* [Google Common Lisp Style Guide](http://google-styleguide.googlecode.com/svn/trunk/lispguide.xml)

## Variables

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

Delete a special variable, if it exists.

~~~
(setf a 1)
(boundp 'a)     ;=> T
(makunbound 'a) ;=> A
(boundp 'a)     ;=> NIL
~~~


## Functions

### [defun] name lambda-list \[\[declare\* | doc\]\] form* => name

Define a [function] with a given *name*, which acts as an
implicit [block] when evaluating *forms*. Optionally
[declare] type information about the function parameters.

~~~
(defun adder (a b)
  (declare (number a b))
  "Sum numbers and return the result."
  (+ a b))

(adder 3 4) ;=> 7
~~~

### [lambda] lambda-list \[\[declare\* | doc\]\] form* => function

Return an anonymous [function].

~~~
(funcall #'(lambda (x) (* x x)) 3) ;=> 9
~~~

### [labels] \(\(name lambda-list \[\[local-declare\ | local-doc\]\] local-forms\*)\*\) \[declare\*\] forms\* => result\*

Evaluate *forms* with a locally defined function. Functions
defined by [labels] are visible within *local-forms*,
functions defined by [flet] are not.

~~~
(labels ((adder (a b)
           (+ a b))
		 (adder1 (a b)
		   (1+ (adder a b))))
  (adder1 4 3))               ;=> 8

(flet ((adder (a b)
         (+ a b)))
  (adder 4 3))                ;=> 7
~~~

### [function] name => function

Return the lexically innermost [function] for *name*. The
notation `#'` can be used as an abbreviation for `(function name)`.

~~~
(funcall #'list 'a 'b) ;=> (A B)
~~~

### [funcall] function &rest args\* => result\*

Applies *function* to *args*.

~~~
(funcall #'+ 1 2 3) ;=> 6
~~~

### [apply] function \[args\*\] arg-list => result\*

Applies *function* to arguments in a [list].

~~~
(apply #'+ '(1 2 3))   ;=> 6
(apply #'+ 1 2 '(3 4)) ;=> 10
~~~

### [multiple-value-call] function forms\* => result\*

Call *function* with all the values of each form as its arguments.

~~~
(multiple-value-call #'+ (floor 5.5) (floor 4.3)) ;= (+ 5 0.5 4 0.3) => 9.8
~~~

### [values] objects\* => objects\*

Return *objects* as multiple values. Is `setf`able.

~~~
(values 'a 'b)      ;=> A, B
(list (values 1 2)) ;=> (A)

(multiple-value-bind (a b) (floor 7.5)
  (list a b))                          ;=> (7 0.5)

(setf (values a b) (floor 7.5))
(list a b)                             ;=> (7 0.5)
~~~

### [values-list] list => objects\*

Return elements of *list* as multiple values.

~~~
(values-list '(A B)) ;=> A, B

(multiple-value-bind (a b) (values-list '(1 2))
  (list a b))  ;=> (1 2)
~~~

### [multiple-value-list] form => list

Evaluates *form* and creates a [list] of the multiple values it returns.

~~~
(multiple-value-list (floor 7.5)) ;=> (7 0.5)
~~~

### [nth-value] n form => object

Return the *n*th value yielded by *form*, zero-indexed.

~~~
(nth-value 0 (values 'a 'b)) ;=> A
(nth-value 1 (values 'a 'b)) ;=> B
~~~

### [complement] function => complement-function

Return new [function] with same arguments and side effects
as *function*, but with the opposite truth value.

~~~
(funcall (complement #'numberp) 1)          ;=> NIL
(funcall (complement #'member) 'd '(a b c)) ;=> T

(complement fn) ;≈ #'(lambda (&rest args) (not (apply fn args)))
~~~

### [constantly] value => function

Returns a [function] that always returns *value*, and takes
any number of arguments.

~~~
(mapcar (constantly 3) '(a b c d)) ;=> (3 3 3 3)
~~~

### [identity] object => object

Returns its argument *object*. Intended for use with
functions that require a [function] as an argument.

~~~
(eql x (identity x)) ;=> T
~~~

### [function-lambda-expression] function => lambda-expression, closure-p, name

If available, return lambda expression of *function*, `[nil]
if *function* was defined in an environment without
bindings, and name of *function*.

~~~
(function-lambda-expression (funcall #'(lambda (x) #'(lambda () x)) nil)) ;=> NIL, T, NIL
~~~

### [fdefinition] name => definition

Definition of global function foo; `setf`able.

~~~
(fdefinition 'list) ;=> #<Compiled-function LIST>
~~~

### [fmakunbound] name => name

Removes the function or macro definition, if any, of name in the global environment.

~~~
(defun add-some (x) (+ x 19))   ;=> ADD-SOME
(fboundp 'add-some)             ;=> T
(flet ((add-some (x) (+ x 37)))
  (fmakunbound 'add-some)
  (add-some 1))                 ;=> 38
(fboundp 'add-some)             ;=> NIL
~~~

## Function Composition

### [alexandria:compose] fn &rest fns\* => function

Compose function that applies its arguments to each in turn.

~~~
(compose #'fn2 #'fn1) ;=> (lambda (x) (fn2 (fn1 x)))
~~~

### [alexandria:multiple-value-compose] fn &rest fns\* => function

Compose functions that return multiple values. *fn1(x,y)* => *fn2(x1,y1)* => x2,y2

~~~
(funcall (multiple-value-compose #'fn2 #'fn1) x y) ;=> x2,y2
~~~

### [alexandria:disjoin] pred &rest preds\* => function

~~~
(disjoin #'zerop #'oddp) ;≈ (lambda (x) (or (zerop x) (oddp x)))
~~~

### [alexandria:conjoin] pred &rest preds\* => function

~~~
(conjoin #'zerop #'oddp) ;≈ (lambda (x) (and (zerop x) (oddp x)))
~~~

### [alexandria:curry] fn &rest args\* => function

~~~
(funcall (curry #'list 'a 'b) 'c 'd) ;=> (A B C D)
~~~

### [alexandria:rcurry] fn &rest args\* => function

~~~
(funcall (rcurry #'list 'a 'b) 'c 'd) ;=> (C D A B)
~~~


## Macros

The Common Lisp macro facility allows the user to define
arbitrary functions that convert certain Lisp forms into
different forms before evaluating or compiling them. This is
done at the expression level, not at the character-string
level as in most other languages. Macros are important in
the writing of good code: they make it possible to write
code that is clear and elegant at the user level but that is
converted to a more complex or more efficient internal form
for execution.

* [CLtL2: Macros](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node97.html)
* [PCL: Macros: Defining Your Own](http://www.gigamonkeys.com/book/macros-defining-your-own.html)

### [defmacro] name lambda-list \[\[declare\* | doc\]\] forms\* => name

Define a macro with a given *name*, which acts as an
implicit [block] when evaluating
*forms*. `define-compiler-macro` is similar with a few
differences.


~~~
(defmacro adder (a b)
  "Sums two numbers and returns the result"
  `(+ ,a ,b))

(adder 4 5) ;=> 9
~~~

### [macrolet] \(\(name lambda-list \[\[local-declare\ | local-doc\]\] local-forms\*)\*\) \[declare\*\] forms\* => result\*

Defines local macro definitions, using the same format used by [defmacro].

~~~
(macrolet ((adder (a b) `(+ ,a ,b)))
  (adder 4 5))                       ;=> 9
~~~

### [define-symbol-macro] symbol expansion => symbol

Defines a macro expansion for *symbol*. `setf`able.

~~~
(setf a '(1 2 3))
(define-symbol-macro b (car a)) ;=> B
b                               ;=> 1
(setf b 5)
a                               ;=> (5 2 3)
b                               ;=> 5
~~~

### [symbol-macrolet] \(\(symbol expansion\)\*\) declare\* forms\* => result\*

Evaluate *forms* with locally defined symbol macros.

~~~
(symbol-macrolet ((x 'foo))
  (list x (let ((x 'bar)) x))) ;=> (FOO BAR)
~~~

### [define-modify-macro] name lambda-list function \[doc\] => name

Defines a macro to read and write a place.

~~~
(define-modify-macro new-incf (&optional (delta 1)) +)
(setf a 1)
(new-incf a) ;=> 2
~~~

### [defsetf] access-fn update-fn \[doc\] => access-fn

Specify how to `setf` a place accessed by function. There is
a "short form" and "long form" of [defsetf] as distinguished
by the type of arguments.

~~~
(defun middle (x)
  (nth (truncate (* (list-length x) 0.5)) x))
(defun set-middle (x val)
  (let ((idx (truncate (* (list-length x) 0.5))))
    (setf (nth idx x) val)))
(defsetf middle set-middle)

(setf a '(a b c))   ;=> (A B C)
(middle a)          ;=> B
(setf (middle a) 2)
a                   ;=> (A 2 C)
~~~

### [get-setf-expansion] place &optional env => vars, vals, store-vars, writer-form, reader-form

Returns lists of temporary variables, values, and get and
set forms for [setf] to read *place*.

~~~
(defvar x)
(get-setf-expansion 'x) ;=> NIL, NIL, (#:G6725), (SETQ X #:G6725), X
~~~

* [Setf Expansions](http://www.lispworks.com/documentation/HyperSpec/Body/05_aab.htm)

### [define-setf-expander] access-fn lambda-list \[\[declaration* | doc\]\] form\* => access-fn

Specifies the means by which [setf] updates a place that is referenced by *access-fn*.

~~~
(defun last-element (x) (car (last x)))
(define-setf-expander last-element (x &environment env)
  "Set the last element in a list to the given value."
  (multiple-value-bind (dummies vals newval setter getter) (get-setf-expansion x env)
    (let ((store (gensym)))
          (values dummies
                  vals
                  `(,store)
                  `(progn (rplaca (last ,getter) ,store) ,store)
                  `(lastguy ,getter)))))

(setf l '(a b c d))       ;=> (A B C D)
(last-element l)          ;=> D
(setf (last-element l) 4)
l                         ;=> (A B C 4)
~~~
