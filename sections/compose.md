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

### [complement] function => complement-function

Return new [function] with same arguments and side effects
as *function*, but with the opposite truth value.

~~~
(funcall (complement #'numberp) 1)          ;=> NIL
(funcall (complement #'member) 'd '(a b c)) ;=> T

(complement fn) ;≈ #'(lambda (&rest args) (not (apply fn args)))
~~~

### [constantly] value => function

Return a [function] taking any number of arguments, which
always returns *value*.

~~~
(setf f (constantly "hi"))
(funcall f)                        ;=> "hi"
(mapcar (constantly 3) '(a b c d)) ;=> (3 3 3 3)
~~~

### [identity] object => object

Returns its own argument *object*. Intended for functions
that require a [function] argument.

~~~
(identity 23)                                    ;=> 23
(mapcan #'identity (list (list 1 2 3) '(4 5 6))) ;=> (1 2 3 4 5 6)
~~~
