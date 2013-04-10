## Formatted Output

* [HyperSpec: Formatted Output](http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm)
* [CLtL2: Formatted Output to Character Streams](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node200.html)
* [PCL: A Few FORMAT Recipes](http://www.gigamonkeys.com/book/a-few-format-recipes.html)

### [format] destination control-string &rest args => result

Creates formatted output by outputting the characters of
control-string and observing that a tilde introduces a
directive.

If *destination* is a [string], a [stream], or [t], then the
*result* is [nil]. Otherwise, the *result* is a [string]
containing the formatted output.

~~~
(format nil "Hello, ~a" 'lisp) ;=> "Hello, LISP!"
~~~
