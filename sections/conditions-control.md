## Condition Control Flow

* [HyperSpec: Condition System Concepts](http://www.lispworks.com/documentation/HyperSpec/Body/09_a.htm)
* [CLtL2: Conditions](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node312.html)
* [PCL: Beyond Exception Handling: Conditions and Restarts](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html)

### [handler-case] expr \[\[{error-clause\}\* | no-error-clause\]\] => result\*

Executes *expr* in a dynamic environment where various
condition handlers are active. Each *error-clause* specifies
how to handle a matching condition type. A *no-error-clause*
allows a particular action if control returns normally.

~~~
(defun adder-test (a b)
  (handler-case (+ a b)
    (type-error () :wrong-type)
	(error (condition) (list :any-other-error condition))
    (:no-error (val) (list :success val))))

(adder-test 6 7)  ;=> (:SUCCESS 13)
(adder-test 6 'a) ;=> :WRONG-TYPE
~~~

### [handler-bind] \(\(condition-type handler-function\)*\) forms\* => result\*

Executes *forms* in a dynamic environment where the
condition *handler-function* bindings are active.

~~~
(handler-bind ((simple-error #'(lambda (condition)
                                 (print (list :got-error condition)))))
  (error 'simple-error)
  (print 'not-printed)) ;=> [prints (:GOT-ERROR #<SIMPLE-ERROR...>) and throws error
~~~

### [restart-case] form \{restarts\*\} => result\*

Return values of *form* or, if during its evaluation one of
the dynamically established *restarts* is called, the values
of its restart-forms.

~~~
(restart-case (error 'simple-error)
  (do-nothing () nil)
  (do-something () (print 'doing-something)))
~~~

~~~
(defun run-test ()
  (print 'a)
  (error 'simple-error))

(defun run-handler (condition)
  (print 'b)
  (invoke-restart (find-restart :do-something))
  (print 'not-printed))

(defun run-restart ()
  (print 'c))

(handler-bind ((simple-error #'run-handler)) ;catch and run, invoke restart
  (restart-case (run-test)                   ;throws error
    (:do-nothing () nil)
    (:do-something () (run-restart)))) ;=> C [prints A, B, C]
~~~

### [with-simple-restart] (name format-control format-arg*) forms\* => result*

Return value of *forms* unless a restart is invoked, at
which control is transferred to [with-simple-restart], which
returns two values, [nil] and [t]. The restart is a prompt
at the debugger which serves as a position to resume from.

~~~
(with-simple-restart (abort "Return to command level.")
  (print 'a)
  (error "an error")
  (print 'not-printed))
~~~

Provide multiple restarts and prompt user for direction:

~~~
(defun collect-all-or-half (n)
  (let ((nums '())
        (halfway (floor (/ n 2))))
    (with-simple-restart (collect-half "Only collect ~a ?" halfway)
      (dotimes (i n)
        (with-simple-restart (collect-all "Collect all ~a ?" n)
         (if (= i halfway)
		   (error "Prompt for number"))
		 (push i nums))))
    (reverse nums)))

(collect-all-or-half 8) ;=> (0 1 2 3 4 5 6 7) [user selects COLLECT-ALL restart]
(collect-all-or-half 8) ;=> (0 1 2 3) [user selects COLLECT-HALF restart]
~~~

### [restart-bind]

### [invoke-restart]

### [invoke-restart-interactively]

### [find-restart]

### [compute-restarts]

### [restart-name]

### [with-condition-restarts]
