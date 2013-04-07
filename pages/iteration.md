# Iteration

m4_include(../sections/do.md)

m4_include(../sections/plist-iteration.md)

m4_include(../sections/hashtable-iteration.md)

m4_include(../sections/loop.md)

## Recursion

Tesing if a list contains an element using tail-recursion:

~~~
(defun our-member (obj lst)
  (if (null lst)                    ;finished if empty list, return nil
	nil
	(if (eql (car lst) obj)         ;if obj is first list element, return list
	  lst
	  (our-member obj (cdr lst))))) ;else, test against rest of list
~~~

Using an accumulator:

~~~
(defun fac (x)
  (labels ((fac2 (x acc)          ;function takes current step and accumulated results
	(if (= x 0)
	  acc                         ;if done, return accumulated results list
	  (fac2 (- x 1) (* acc x))))) ;else, call again with stepped down arg and accumulation
    (fac2 x 1)))                  ;initial call to start
~~~
