## Conditionals

### [if] test then \[else\] => result\*

Return values of *then* if *test* is [t], otherwise return *else*.

~~~
(if (evenp 2) 'a) ;=> A
(if (evenp 3)
  'b
  'c)             ;=> C
~~~

### [and] forms\* => result\*

Evaluate *forms* from left to right. Immediately return
[nil] if one form’s value is [nil], otherwise return the
value of the last form.

~~~
(and (numberp 2) (evenp 2)) ;=> T
~~~

### [or] forms\* => results\*

Evaluate *forms* from left to right. Immediately return
value of the first non-[nil] evaluating form, or all values
if last form is reached. Return [nil] if no form returns [t].

~~~
(or (numberp 3) (evenp 3)) ;=> T
~~~

### [when] test forms\* => result\*

Evaluate *forms* if *test* returns [t].

~~~
(when (evenp 2)
  (print 'a)
  'b)           ;=> B [prints A]
~~~

### [unless] test forms\* => result\*

Evaluate *forms* if *test* returns [nil].

~~~
(unless (evenp 3)
  (print 'a)
  'b)           ;=> B [prints A]
~~~

### [cond] \(test then\*\)\* => result\*

Return the values of the first *then* whose *test* returns
[t], otherwise return [nil] if no tests pass.

~~~
(defun test (x)
  (cond ((= x 1) 'clause1)
        ((= x 2) 'clause2)
        (t 'clause3)))

(test 2)                   ;=> CLAUSE2
(test 9)                   ;=> CLAUSE3
~~~

### [case] test \(key | \(key\*\) | otherwise form) => result\*

Return the *form* for the first clause whose *key* is [eql]
to *test*. If there is no *otherwise* clause, return [nil].

If no clause matches using [ccase], a *correctable* [error]
is signaled allowing a [store-value] restart to be invoked.

If no clause matches using [ecase], a *non-correctable*
[error] is signaled.

~~~
(defun test (x)
  (case x
    (:a 'clause1)
    ((:b :c) 'clause2)
    (otherwise 'clause3)))

(test :a)                  ;=> CLAUSE1
(test :g)                  ;=> CLAUSE3
~~~


## Sequencing

### [progn] forms\* => result\*

Evaluates *forms*, in the order in which they are given.

~~~
(progn
  (print 'a)
  'b)        ;=> B [prints A]
~~~

### [multiple-value-prog1] first-form forms\* => first-form-results

Save values from *first-form*, evaluate remaining *forms*,
then return the the saved values from *first-form*.

~~~
(multiple-value-prog1
  (values 1 2 3)
  (+ 2 2))            ;=> 1, 2, 3
~~~

### [prog1] first-form forms\* => first-form-result

Evaluates *first-form* and then remaining *forms*, yielding
the value from *first-form*.

~~~
(prog1 'a 'b 'c) ;=> A
~~~

### [prog2] first-form second-form forms\* => second-form-result

Evaluates *first-form*, then *second-form*, and then remaining *forms*, yielding
the value from *second-form*.

~~~
(prog2 'a 'b 'c) ;=> B
~~~

### [prog] \(\{name | \(name \[value\]\)\}\*\) declare\* \{tag | form\}\* => result\*

Evaluate [tagbody]-like body with *names* lexically bound in
parallel. [prog] permits the use of the [return] and [go]
statements. Use [prog\*] to bind local variables sequentially.

~~~
(prog ((x 1))
  tag1
    (go tag3)
  tag2
    (return x)
  tag3
    (go tag2)) ;=> 1
~~~

## Blocks and Exits

* [HyperSpec: Transfer of Control to an Exit Point](http://www.lispworks.com/documentation/HyperSpec/Body/05_b.htm)
* [PCL: Local Flow of Control](http://www.gigamonkeys.com/book/the-special-operators.html#local-flow-of-control)

### [block] name forms\* => result\*

Establishes a named block and then evaluates forms as an
implicit [progn] unless interrupted by [return-from].

~~~
(block test
  (print 'printed)
  (return-from test)
  (print 'not-printed)) ;=> NIL [prints PRINTED]
~~~

### [return-from] name \[result\] => result\*

Returns control to the nearest enclosing [block] *name*, returning *results*.

~~~
(defun test ()
  (return-from test 'done)
  'a)

(test)                     ;=> DONE
~~~

### [return] \[result\] => result\*

Returns, as if by [return-from], from a [block] named
[nil], which many standard control structures
generate. Equivalent to `(return-from nil ...)`.

~~~
(dotimes (i 10)
  (print i)
  (if (> i 5) (return 'done))) ;=> DONE [prints 0..6]
~~~

### [tagbody] \{tag | form\}* => nil

Evaluates *forms* in a lexical environment that provides
*tags* for control transfers which are targets for [go]. Not
often used since it's easier to write iterative constructs
using existing looping macros.

~~~
(tagbody
  tag1
    (if (zerop (random 2))
      (go tag2)
      (go tag3))
  tag2
    (print 'counting)
    (go tag1)
  tag3
    (print 'exiting))
~~~

### [go] tag

Transfers control to an enclosing [tagbody] form labeled by a tag [eql] to *tag*.

~~~
(tagbody
  (print 'printed)
  (go skip)
  (print 'not-printed)
  skip (print 'leaving)) ;=> NIL [prints PRINTED and LEAVING]
~~~

### [catch] tag forms\* => result\*

Evaluate *forms* and return their values unless interrupted by [throw].

~~~
(defun foo ()
  (catch 'exit
    (print 'enter-foo)
    (bar)
    (print 'not-printed-foo))
  (print 'leave-foo))

(defun bar ()
  (print 'enter-bar)
  (throw 'exit nil)
  (print 'not-printed-bar))

(foo) ;=> [prints ENTER-FOO, ENTER-BAR, LEAVE-FOO
~~~

### [throw] tag form

Have the nearest dynamically enclosing [catch] with a tag
[eq] *tag* return with the values of *form*.

~~~
(catch 'tag
  (print 'printed)
  (throw 'tag 'done)
  (print 'not-printed)) ;=> DONE [prints PRINTED]
~~~

### [unwind-protect] protected-form cleanup-forms\* => protected-form-result\*

Evaluate *protected-form* and then, no matter how control
leaves *protected-form*, evaluate *cleanup-form*. Return
values of *protected-form*.

~~~
(catch 'exit
  (unwind-protect
    (progn
      (print 'printed)
      (throw 'exit 'a)
      (print 'not-printed))
	(print 'also-printed)
	(print 'another-printed))) ;=> A [prints PRINTED, ALSO-PRINTED, ANOTHER-PRINTED]
~~~
