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

The shorthand notation is `#'`. Return the [function] for *name*.

~~~
(funcall #'list 'a 'b) ;=> (A B)
~~~

### [funcall] function args\* => result

Apply values as arguments to *function*.

~~~
(funcall #'+ 1 2 3) ;=> 6
~~~

### [apply] function \[args\*\] arg-list => result

Apply values as arguments to *function*, the final one a [list].

~~~
(apply #'+ '(1 2 3))   ;=> 6
(apply #'+ 1 2 '(3 4)) ;=> 10
~~~

### [multiple-value-call] function forms\* => result

Apply all values returned from *forms* as arguments to *function*.

~~~
(multiple-value-call #'+ (floor 5.5) (floor 4.3)) ;= (+ 5 0.5 4 0.3) => 9.8
~~~

### [values] objects\* => objects\*

Return *objects* as multiple values. `setf`able.

~~~
(values 'a 'b)        ;=> A, B
(list (values 'a 'b)) ;=> (A)

(setf (values a b) (floor 7.5))
(list a b)                             ;=> (7 0.5)
~~~

### [values-list] list => objects\*

Return *list* elements as multiple values.

~~~
(values-list '(A B)) ;=> A, B

(multiple-value-bind (a b) (values-list '(1 2))
  (list a b))  ;=> (1 2)
~~~

### [multiple-value-list] form => list

Evaluates *form* returning multiple values and returns a
[list] containing them.

~~~
(multiple-value-list (floor 7.5)) ;=> (7 0.5)
~~~

### [fmakunbound] name => name

Removes the function or macro definition.

~~~
(defun add-some (x) (+ x 19))   ;=> ADD-SOME
(fboundp 'add-some)             ;=> T
(flet ((add-some (x) (+ x 37)))
  (fmakunbound 'add-some)
  (add-some 1))                 ;=> 38
(fboundp 'add-some)             ;=> NIL
~~~

## Advanced

### [flet] \(\(name lambda-list \[\[local-declare\ | local-doc\]\] local-forms\*)\*\) \[declare\*\] forms\* => result\*

### [nth-value] n form => object

Return the *n*th value yielded by *form*, zero-indexed.

~~~
(nth-value 0 (values 'a 'b)) ;=> A
(nth-value 1 (values 'a 'b)) ;=> B
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
