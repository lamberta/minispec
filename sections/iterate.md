### [iterate:iter] \[clauses\*\] forms\*

A powerful iteration facility that provides abstractions for
many common iteration patterns and allows for the definition
of additional patterns. While it is similar to [loop],
*iterate* offers a more Lisp-like syntax and enhanced
extensibility.

## Numerical Iteration

#### [repeat][iterate:repeat] n

Iterate loop *n* times. If *n* is <= 0 the loop will not execute.

~~~
(iter (repeat 5)
  (print 'hello))   ;=> NIL [prints HELLO five times]
~~~

#### [for][iterate:for] var sequence-keywords

Iterate over a sequence of numbers with a variable and one
or more keywords that provide the bounds and step size of
the iteration. Valid *sequence-keywords* are: `from`,
`upfrom`, `downfrom`, `to`, `downto`, `above`, `below` and `by`.

~~~
(for i from 5)              ;i => 5 6 7 ... (same as upfrom)
(for i downfrom 0)          ;i => 0 -1 -2 ...
(for i from 1 to 3)         ;i => 1 2 3
(for i from 5 downto 3)     ;i => 5 4 3
(for i from 1 below 3)      ;i => 1 2
(for i from 3 above 1)      ;i => 3 2
(for i from 1 to 3 by 2)    ;i => 1 3
(for i from 1 below 3 by 2) ;i => 1
~~~


## Sequence Iteration

The *step-function*, which defaults to [cdr], is used to
obtain the next sublist.

Valid *sequence-keywords* are as before: `from`, `upfrom`,
`downfrom`, `to`, `downto`, `above`, `below` and
`by`. In addition, `with-index` takes a symbol as argument
and uses it for the index variable.

#### [for][iterate:for...in] var __in__ list \[__by__ step-function\]

Set *var* to successive elements of *list*.

~~~
(iter (for x in '(a b c d) by #'cddr)
  (collect x))                        ;=> (A C)
~~~

#### [for][iterate:for...on] var __on__ list \[__by__ step-function\]

Set *var* to successive sublists of *list*.

~~~
(iter (for x on '(a b c))
  (collect x))            ;=> ((A B C) (B C) (C))
~~~

#### [for][iterate:for...in-sequence] var __in-sequence__ seq sequence-keywords

Set *var* to successive elements of *seq*.

~~~
(iter (for x in-sequence '(a b c) with-index i)
  (collect (list i x)))                         ;=> ((0 A) (1 B) (2 C))
~~~

#### [for][iterate:for...index-of-sequence] var __index-of-sequence__ sequence sequence-keywords

Set *var* to the index number of *sequence* element.

~~~
(iter (for i index-of-sequence #(a b c))
  (collect i))                           ;=> (0 1 2)
~~~

#### [for][iterate:for...in-vector] var __in-vector__ vector sequence-keywords

Set *var* to successive elements of *vector*.

~~~
(iter (for x in-vector #(a b c))
  (collect x))                   ;=> (A B C)
~~~

#### [for][iterate:for...index-of-vector] var __index-of-vector__ vector sequence-keywords

Set *var* to the index number of *vector* element.

~~~
(iter (for i index-of-vector #(a b c))
  (collect i))                         ;=> (0 1 2)
~~~

#### [for][iterate:for...in-string] var __in-string__ string sequence-keywords

Set *var* to successive elements of *string*.

~~~
(iter (for x in-string "hello" downfrom 4)
  (collect x))                             ;=> (#\o #\l #\l #\e #\h)
~~~

#### [for][iterate:for...index-of-string] var __index-of-string__ string sequence-keywords

Set *var* to the index number of *string* character.

~~~
(iter (for i index-of-string "hello" downfrom 4)
  (collect i))                                   ;=> (4 3 2 1 0)
~~~

#### [for][iterate:for...in-hashtable] \(key value\) __in-hashtable__ table

Iterate over the *keys* and *values* of a [hash-table].

~~~
(setf ht (alexandria:plist-hash-table '(:a 1 :b 2)))
(iter (for (key val) in-hashtable ht)
  (collect (list key val)))           ;=> ((:A 1) (:B 2))
~~~

#### [for][iterate:for...in-package] var __in-package__ package \[__external-only__ ext\]

Iterate over all symbols in a [package], or only external
symbols if specified.

~~~
(iter (for sym in-package :iterate external-only t)
  (collect sym))       :=> (SUM FIRST-ITERATION-P NCONCING FIRST-TIME-P ...)
~~~

#### [for][iterate:for...in-packages] \(sym access-type pkg\) __in-packages__ \(packages\) \[__having-access__ \(symbol-types\)\]

Iterates over all symbols from the list of *packages* and
having visibility given by *symbol-types*, which defaults to
the list `(:external :internal :inherited)`.

~~~
(iter (for (sym access-type pkg) in-packages '(:iterate) having-access (:external))
  (collect (list sym access-type pkg)))
~~~

#### [for][iterate:for...in-file] var __in-file__ name \[__using__ reader\]

Opens the file *name* ([string] or [pathname]) and iterate
over its contents. *reader* defaults to [read] and will bind
*var* to successive forms in the file. The file is closed no
matter how the iterate loop exits.

~~~
(iter (for line in-file "/etc/passwd" using #'read-line)
  (collect line))   ;=> ("root:*:0:0:System Administrator:/var/root:/bin/sh" ...)
~~~

#### [for][iterate:for...in-stream] var __in-stream__ stream \[__using__ reader\]

Like [for...in-file][iterate:for...in-file] except that
*stream* should be an existing [stream] object that supports
input operations.

~~~
(with-input-from-string (in "hello 1 2 three")
  (iter (for sym in-stream in)
    (collect line)))                           ;=> (HELLO 1 2 THREE)
~~~

## Variable Binding

#### [with][iterate:with] var \[__=__ value\]

Causes *var* to be bound to *value* before the loop body is entered.

~~~
(setf n 0)
(iter (for x in '(a b c))
      (with i = n)
  (incf n)
  (collect (list i x)))   ;=> ((0 A) (0 B) (0 C))
~~~

#### [for][iterate:for...=] var __=__ form

On each iteration, *form* is evaluated and *var* is set to its value.

~~~
(setf n 0)
(iter (for x in '(a b c))
      (for i = n)
  (incf n)
  (collect (list i x)))   ;=> ((0 A) (1 B) (2 C))
~~~

#### [for][iterate:for...initially...then] var __initially__ init-expr __then__ then-expr

Before loop begins, *var* is set to *init-expr*; after the
first iteration, it is set to *then-expr*.

~~~
(iter (repeat 4)
      (for i initially 10 then (1+ i))
  (collect i))                         ;=> (10 11 12 13)
~~~

#### [for][iterate:for...first...then] var __first__ first-expr __then__ then-expr

On the first iteration, *var* is set to *init-form*; on
subsequent iterations, it is set to *then-expr*. This
differs from [for...initially][iterate:for...initially...then]
in that *var* is set inside the loop body.


#### [for][iterate:for...previous] pvar __previous__ var \[__initially__ init\] \[__back__ n\]

Sets *pvar* to the previous value of *var*, another loop variable.

~~~
(iter (for x in '(1 2 3))
      (for y previous x initially 0)
  (collect (list x y)))              ;=> ((1 0) (2 1) (3 2))
~~~


## Accumulate

#### [collect][iterate:collect] value \[__into__ var __at__ place __result-type__ type\]

Returns a sequence of *values* produced from each
iteration. Each value is *placed* in the collected sequence
at the `start` or `end` (default).

~~~
(iter (for i in '(1 2 3))
  (collect x)             ;=> (1 2 3)

(iter (for i in '(1 2 3))
  (collect x at start)    ;=> (3 2 1)
~~~

#### [adjoining][iterate:adjoining] value \[__into__ var __test__ test __at__ place __result-type__ type\]

Like [collect][iterate:collect], but only adds the *value*
if it is not already present.

~~~
(iter (for i in '(1 2 3 2 3 3 1))
  (adjoining i))                  ;=> (1 2 3)
~~~

#### [appending][iterate:appending] value \[__into__ var __at__ place\]

Like [collect][iterate:collect], but using [append].

#### [nconcing][iterate:nconcing] value \[__into__ var __at__ place\]

Like [collect][iterate:collect], but using [nconc].

#### [unioning][iterate:unioning] value \[__into__ var __test__ test __at__ place\]

Like [collect][iterate:collect], but using [union]. Assumes
that the *value* contains no duplicates.

#### [nunioning][iterate:nunioning] value \[__into__ var __test__ test __at__ place\]

Like [collect][iterate:collect], but using [nunion]. Assumes
that the *value* contains no duplicates.

#### [accumulate][iterate:accumulate] value __by__ fn \[__initial-value__ init-val __into__ var\]

The general-purpose accumulation clause. Function *fn* takes
two arguments, the *value* and the value accumulated so far
in the iteration, and it should return the updated value. If
no *initial-value* is supplied, [nil] is used.


## Reduce

An iteration pattern in which the results of successive
applications of a binary operation are accumulated.

#### [sum][iterate:sum] value \[__into__ var\]

On each iteration, *value* is added to a variable, which is
initially bound to zero.

~~~
(iter (for i in '(1 2 3 4))
  (sum i))                  ;=> 10
~~~

#### [multiply][iterate:multiply] value \[__into__ var\]

Like [sum][iterate:sum], but the initial value of the result
is 1, and is updated by multiplying *value* into it.

~~~
(iter (for i in '(1 2 3 4))
  (multiply i))             ;=> 24
~~~

#### [counting][iterate:counting] value \[__into__ var\]

On each iteration, if *value* evaluates to non-[nil],
increment the counter, which initially starts at zero.

~~~
(iter (for x in '(a 2 nil d))
  (counting x))               ;=> 3
~~~

#### [maximize][iterate:maximize] value \[__into__ var\]

Evaluate *value* on each iteration and store the maximum in
the accumulation variable.

~~~
(iter (for i in '(1 4 2 2))
  (maximize i))             ;=> 4
~~~

#### [minimize][iterate:minimize] value \[__into__ var\]

Evaluate *value* on each iteration and store the minimum in
the accumulation variable.

~~~
(iter (for i in '(1 4 2 2))
  (minimize i))             ;=> 1
~~~

#### [reducing][iterate:reducing] value __by__ fn \[__initial-value__ init-val __into__ var\]

The general way to perform reductions. Function *fn* takes
two arguments, the first is the value computed so far and
the second is the *value*. It should return a new value.

~~~
(iter (for i in '(1 2 3 4))
  (reducing i by #'(lambda (x y) (+ x y)))) ;=> 10
~~~


## Tests

#### [finding][iterate:finding...such-that] value __such-that__ test \[__into__ var __on-failure__ failure-value\]

If *test* ever evaluates to non-[nil], the loop is stopped
and the current *value* is returned.

~~~
(iter (for i in '(1 2 3))
  (finding i such-that #'oddp))  ;=> 1

(iter (for i in '(1 2 3))
  (finding i such-that #'evenp)) ;=> 2
~~~

#### [finding][iterate:finding...maximizing] expr __maximizing__ m-expr \[__into__ var\]

Computes the maximum value of *m-expr* over all iterations,
and returns the value of the corresponding *expr*.

~~~
(iter (for lst in '((a b c) (x y) (1 2 3 4)))
  (finding lst maximizing (length lst)))      ;=> (1 2 3 4)
~~~

#### [finding][iterate:finding...minimizing] expr __minimizing__ m-expr \[__into__ var\]

Computes the minimum value of *m-expr* over all iterations,
and returns the value of the corresponding *expr*.

~~~
(iter (for lst in '((a b c) (x y) (1 2 3 4)))
  (finding lst minimizing #'length))          ;=> (X Y)
~~~

#### [first-iteration-p][iterate:first-iteration-p]

Returns [t] in the first cycle of the loop, otherwise [nil].

~~~
(iter (repeat 3)
  (collect (first-iteration-p))) ;=> (T NIL NIL)
~~~

#### [first-time-p][iterate:first-time-p]

Returns [t] the first time the expression is evaluated, and
then [nil] forever.

~~~
(iter (for i in '(1 2 3))
  (collect (list (first-time-p) i))) ;=> ((T 1) (NIL 2) (NIL 3))
~~~

#### [always][iterate:always] expr

If *expr* ever evaluates to [nil], then [nil] is immediately
returned; the epilogue code is not executed.

~~~
(iter (for i in '(1 2 3 4))
      (always (evenp i)))   ;=> NIL
~~~

#### [never][iterate:never] expr

Like `(always (not expr))`, but does not influence the last
value returned by a possible other always clause.

~~~
(iter (for i in '(2 4 6 8))
      (always (evenp i)))   ;=> T
~~~

#### [thereis][iterate:thereis] expr

If *expr* is ever non-[nil], its value is immediately returned
without running epilogue code.

~~~
(iter (for i in '(1 2 3 4))
      (thereis (characterp i))) ;=> NIL
~~~


## Control Flow

Alter the usual flow of control in a loop.

#### [finish][iterate:finish]

Stops the loop and runs the epilogue code.

~~~
(iter (for x in '(a b c 1 2 3))
  (if (numberp x)
    (finish))
  (collect x))                  ;=> (A B C)
~~~

#### [leave][iterate:leave] \[value\]

Immediately returns *value* (default [nil]) from the loop,
skipping the epilogue code. Equivalent to using
[return-from].

~~~
(iter (for x in '(a b c 1 2 3))
  (if (numberp x)
    (leave x)))                 ;=> 1
~~~

#### [next-iteration][iterate:next-iteration]

Skips the remainder of the loop body and begins the next iteration.

~~~
(iter (for x in '(a b c 1 2 d e))
  (if (numberp x)
    (next-iteration))
  (collect x))                    ;=> (A B C D E)
~~~

#### [while][iterate:while] expr

If *expr* ever evaluates to [nil], the loop stops and the
epilogue code is run. Equivalent to `(if (not expr) (finish))`.

~~~
(iter (for x in '(1 2 a b 3 4))
  (while (numberp x))
  (collect x))                    ;=> (1 2)
~~~

#### [until][iterate:until] expr

Equivalent to `(if expr (finish))`.

~~~
(iter (for x in '(a b c 1 2 d e))
  (until (numberp x))
  (collect x))                    ;=> (A B C)
~~~

#### [if-first-time][iterate:if-first-time] then \[else\]

If this clause is executed for the first time in the iterate
form, the *then* code is evaluated; otherwise the *else*
code is evaluated.

~~~
(iter (for i in '(1 2 3 4))
  (collect (if-first-time
             nil
             i)))           ;=> (NIL 2 3 4)
~~~


## Code Placement

For control over where code is placed in a loop.

* [Problems with Code Movement](http://common-lisp.net/project/iterate/doc/Problems-with-Code-Movement.html)

#### [initially][iterate:initially] forms\*

Place *forms* in the prologue section of the loop. They are
executed once, before the loop body is entered.

#### [after-each][iterate:after-each] forms\*

Place *forms* at the end of the loop body, where they are
executed after each iteration.

#### [else][iterate:else] forms\*

Place *forms* in the epilogue section of the loop, where
they are executed if this *else* clause is never met during
execution of the loop and the loop terminates normally.

#### [finally][iterate:finally] forms\*

Place *forms* in the epilogue section of the loop, where
they are executed after the loop has terminated normally.

#### [finally-protected][iterate:finally-protected] forms\*

Place *forms* in the second form of an [unwind-protect]
outside the loop. They are always executed after the loop
has terminated, regardless of how the termination occurred.

#### [in][iterate:in] name &forms\*

Evaluate *forms* as if they were part of the iterate form *name*.

~~~
(iter outer (for i in '(1 2 3))
  (iter (for j in '(10 20))
    (in outer (collect (* i j))))) ;=> (10 20 20 40 30 60)
~~~


## Destructuring

In many places where a variable is expected, a list can be
written instead. The value to be assigned is destructured
according to the pattern described by the list.

~~~
(for (x y) in '((1 2) (3 4)))
(for (key . val) in alist)
(for (values (a . b) c d) = (three-valued-function ...))
~~~

#### [dsetq][iterate:dsetq] template expr

Performs destructuring of *expr* using *template*. May be
used outside of an `iterate` form. Yields the primary value
of *expr*.

~~~
(dsetq (values a b) (floor 4.5)) ;=> 4
(list a b)                       ;=> (4 0.5)
~~~


## Extend

Write new clauses that embody new iteration patterns.

* [Generators](http://common-lisp.net/project/iterate/doc/Generators.html)
* [Rolling Your Own](http://common-lisp.net/project/iterate/doc/Rolling-Your-Own.html)

#### [for][iterate:for...next] var __next__ expr

Set *var* to *expr* each time through the
loop. Destructuring is performed. When the clause is used as
a generator, *expr* is the code that is executed when (next
*var*) is encountered.

#### [for][iterate:for...do-next] var __do-next__ form

Evaluate *form* each time through the loop. Its value is not
set to *var*; that's done in *form*. *var* is only present
so that `iterate` knows it is a driver variable.

## Advanced

#### [defmacro-clause][iterate:defmacro-clause] arglist body-form

Defines a new `iterate` clause. *arglist* is a list of
symbols which are alternating keywords and arguments.

~~~
(defmacro-clause (MULTIPLY expr &optional INTO var)
  `(reducing ,expr by #'* into ,var initial-value 1))
~~~

#### [defmacro-driver][iterate:defmacro-driver] arglist body-form

Defines a driver clause in both the for and generate forms,
and provides a parameter generate which body can examine to
determine how it was invoked.

#### [defsynonym][iterate:defsynonym] syn word

Makes *syn* a synonym for the existing iterate keyword *word*.

#### [defclause-sequence][iterate:defclause-sequence] element-name index-name &keys\*

Provides a simple way to define sequence clauses. Generates
two clauses, one for iterating over the sequence's elements,
the other for iterating over its indices.

#### [display-iterate-clauses][iterate:display-iterate-clauses] \[clause-spec\]

Displays a list of [iterate][iter] clauses. If *clause-spec*
is not provided, all clauses are shown.
