## Loop

The `loop` function has a *simple* and *extended* form.

### [loop] form\* => result\*

The *simple* loop form.

~~~
(setf i 0)
(loop
  (print i)
  (if (< i 10)
    (incf i)
    (return))) ;=> NIL [prints 0...10]
~~~
