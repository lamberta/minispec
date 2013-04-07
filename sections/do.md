## do

### [dotimes] (var i \[result\]) form\* => result\*

Iterate over a series of integers. Terminate loop immediately with [return].

~~~
(dotimes (i 10)
  (print i))

(dotimes (i 10 (print 'done))
  (print i))
~~~

### [dolist] (var list \[result\]) form\* => result\*

Iterate over the elements of a [list]. Terminate loop immediately with [return].

~~~
(dolist (x '(1 2 3 4))
  (print x))

(dolist (x '(1 2 3 4) (print 'done))
  (print x))
~~~

### [do] ((var \[start \[step\]\])\*) (stop result\*) form\* => result\*

Iterate over a group of statements while a test condition
holds. Variables are bound within the iteration and stepped
in parallel.

Using [do\*] causes the variable bindings and steppings to be
performed sequentially rather than in parallel.

~~~
(do ((i 0 (1+ i)))              ;init to 0, step 1
    ((>= i 4) (print 'done))    ;end test, result-form
  (print i))                    ;=> NIL [prints 0..3, done]

(do ((n 0 (1+ n))               ;init to 0, step 1
     (current 0 next)           ;init to 0, read old value of next
     (next 1 (+ current next))) ;init to 1, reads old values
    ((= 10 n) current))         ;=> 55 end test and result-form

(do ()
    ((> (get-universal-time) *some-future-date*))
  (print "waiting...")
  (sleep 60))
~~~
