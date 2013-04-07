## Association List (alist)

### [acons] key datum alist => new-alist

Append a *cons* and return a new *alist*. To modify an
*alist* in place, use `push`.

~~~
(acons :c 3 '((:B . 2) (:A . 1))) ;=> ((:C . 3) (:B . 2) (:A . 1))
(push (cons :c 3) alist)          ;=> ((:C . 3) (:B . 2) (:A . 1))
~~~

### [pairlis] keys data &optional alist => new-alist

Pair up elements in *keys* and *data* lists to make an *alist*.

~~~
(pairlis '(:a :b) '(1 2)) ;=> ((:B . 2) (:A . 1))
~~~

### [assoc] item alist &key key test test-not => entry

Return the alist entry whose *car* satisfies the test, or
*nil*. Functional variants are `assoc-if` and
`assoc-if-not`. `rassoc` uses *cdr* to test.

~~~
(assoc :a '((:A . 1) (:B . 2)))         ;=> (:A . 1)
(rassoc 1 '((:A . 1) (:B . 2)))         ;=> (:A . 1)
(assoc-if #'evenp '((1 . :A) (2 . :B))) ;=> (2 . :B)
~~~

### [copy-alist] alist => new-alist

Copies the *cons* cells that make up the alist structure.

### [alexandria:alist-plist] alist

Converts an *alist* to a *plist* format.

~~~
(alist-plist '((:x . 10) (:y . 20))) ;=> (:x 10 :y 20)
~~~
