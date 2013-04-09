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
