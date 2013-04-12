## Loop Simple Form

### [loop] form\* => result\*

Evaluate *forms* forever in a loop within an implict block named [nil].

~~~
(setf i 0)
(loop
  (print i)
  (if (< i 10)
    (incf i)
    (return))) ;=> NIL [prints 0...10]
~~~

## Loop Extended Form

* [CLtL2: Loop](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node235.html)
* [PCL: LOOP for Black Belts](http://www.gigamonkeys.com/book/loop-for-black-belts.html)

### [loop] \[name\] \[variable-clause\*\] \[main-clause\*\] => result\*

Use *loop keywords* to iterate over forms and accumulate values.

~~~
(loop for item in (list 'a 'b 'c 'd)
  do (print item))                         ;=> NIL [prints A, B, C, D]

(loop for item in (list 'a 'b 'c 'd)
  collect item)                            ;=> (A B C D)

(loop for item across (vector 'a 'b 'c 'd)
  collect item)                            ;=> (A B C D)

(loop for item in '(1 2 3 4)
      for x = (1+ item)
  collect (* item x))                      ;=> (2 5 12 20)

(loop for x on (list 10 20 30)
  collect x)                               ;=> ((10 20 30) (20 30) (30))
~~~

Variables declared using `with` are local and cease to
exists when the loop terminates.

~~~
(loop with x = 0
      with y = (1+ x)
  return (list x y))    ;=> (0 1)
~~~

## Counting

~~~
(loop for i upto 4 collect i) ;=> (0 1 2 3 4)
~~~
