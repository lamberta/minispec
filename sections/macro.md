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
