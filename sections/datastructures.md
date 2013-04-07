# Data Structures

## Array

Fixed-size/resizable vectors and multi-dimensional arrays.

### make-array dimensions [&keys...]

Params:
1. `dimensions` ---[valid array dimension](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_v.htm#valid_array_dimension)

Keys:
* `element-type`
* `initial-element`
* `initial-contents`
* `adjustable`
* `fill-pointer`
* `displaced-to`
* `displaced-index-offset`

~~~
(make-array 3 :initial-element nil) => #(nil nil nil) ;array length with init value
(make-array 3 :fill-pointer 0) => #() ;resizable, holds 3 elements. adds to pointer position
(make-array 3 :fill-pointer 0 :adjustable t) ;arbitrarily resizable vector
(make-array '(3 3)) => #2A((0 0 0) (0 0 0) (0 0 0)) ;3x3 multi-dimensional array
~~~

### vector [values...]

One-dimensional array with a fixed length.

Inherits:
1. array
2. sequence
3. t

Params:
1. `values`

* [HS](http://www.lispworks.com/documentation/HyperSpec/Body/t_vector.htm)
* [PCL](http://gigamonkeys.com/book/collections.html#vectors)

~~~
(vector 1 2 3) ;=> #(1 2 3)
#(1 2 3) ;literal notation
~~~


### Typed Array

Create specialized vector of certain type, this creates an adjustable string:

~~~
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) => ""
~~~

~~~
(vector-push 'a *vector*) => 0 ;returns index where added, *vector* => #(A)
(vector-pop *vector*)     => A ;returns most recently added item, decrements fill-pointer
(vector-push-extend 'b *vector*) => 3 ;auto-expand vector to hold additional element, return position
~~~


Used with sequences, vectors and lists:

~~~
(defvar *v* (vector 'a 'b 'c))
(length *v*) => 3
(elt *v* 0)  => A
(elt *v* 3)  => error

(setf (elt *v* 1) 'two) => TWO ;*v* => #(A TWO C)

(aref *v* 0)  => A ;get array position reference
(svref *v* 2) => C ;simple vector reference, faster then general array ref
~~~

Basic sequence functions (vectors, lists, strings):

~~~
(defparameter *seq* #(1 2 3 1))
(count 1 *seq*)        => 2	      ;number of times item appears in sequence
(find 1 *seq*)         => 1	      ;return first item found, nil if not found
(position 1 *seq*)     => 0	      ;position of first item in sequence, nil if not found
(remove 1 *seq*)       => #(2 3)     ;sequence with instances of items removed

(substitute 7 1 *seq*) => #(7 2 3 7) ;sequence with items replaced
~~~

Recycling versions of remove (non-consing):
`delete`, `delete-if`, `delete-if-not`, `delete-duplicates`

Recycling versions of substitute: `nsubstitute`, `nsubstitute-if`, `nsubstitute-if-not`

Standard sequence function keyword arguments

* `:test = #'eql` 2 arg function to compare item (or value extracted by `:key` function)
* `:key = nil` 1 arg function to extract key value from sequence element
* `:start = 0` Starting index of subsequence
* `:end = nil` Ending index of subsequence, nil indicates end of sequence
* `:from-end = nil` If true, sequence will be traversed in reverse order
* `:count = nil` Number or elements to effect (remove or substitute only)

The *example-test* function takes 2 args and returns a boolean:

~~~
(count x *seq* :test #'example-test)               ;default is #'eql
(count x *seq* :test (complement #'example-test))  ;opposite, instead of :test-not
(count "foo" '("foo" "bar" "baz") :test #'string=) ;=> 1
~~~

`:key` takes a 1 arg function for each sequence element to return val to be compared:

~~~
(find 'b #((a 10) (b 20) (c 30)) :key #'first) => (C 30)
~~~

Higher-order function variants, each has a `*-if`, and `*-if-not suffix` takes same keyword arguments as vanilla versions, except `:test`:

~~~
(defparameter *seq* #(1 2 3 1)
(count-if #'evenp *seq*) => 1
(count-if-not #'evenp *seq*) => 3 
(find-if #'evenp *seq*) => 2
(find-if-not #'evenp *seq*) => 1
(position-if #'digit-char-p "abcd0001") => 4
(remove-if #'(lambda (x) (char= (elt x 0) #\f)) '("foo" "bar")) => ("bar")
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f)) '("foo" "bar")) => ("foo") ;filter
(remove-if-not #'alpha-char-p '("foo" "0bar") :key #'(lambda (x) (elt x 0))) => ("foo")
(remove-duplicates *seq*) => #(1 2 3) ;doesn't take :count keyword
(substitute-if 0 #'evenp '(1 2 3 4)) => (1 0 3 0)
(substitute-if-not #'evenp '(1 2 3 4)) => (0 2 0 4)
~~~

Whole sequence manipulations:

~~~
(copy-seq *seq*) ;returns new sequence of same type with same elements
(reverse *seq*) ;returns new sequence of same type with elements reversed
(setf *seq* (nreverse *seq*)) ;recycling function
;return new sequence of type specified from sequences. usually 'vector, 'list, or 'string
(concatenate 'vector *seq1* *seq2* ...)
~~~

Copying

~~~
copy-seq
copy-list ;makes copy of cons cells, references underlying structure, doesn't copy sub-lists
copy-tree ;makes new cons cell for each, copies sub-lists, copies references to atomic values
copy-alist
~~~

;;sorting and merging, also takes :key argument
;;sort is destructive, use copy-seq or copy-list to pass copy
(sort *seq* #'>) => #(3 2 1 1) ;takes sequence and 2 arg predicate function
(stable-sort *seq* #'>) ;guaranteed not to reorder any elements considered equivalent
;;if both sequences are sorted by given predicate, then result will also be sorted
(merge 'list *seq* '(6 5 2) #'>) => '(6 5 3 2 2 1 1)

;;subsequence manipulations
(subseq "foobarbaz" 3) => "barbaz"
(subseq "foobarbaz" 3 6) => "bar"
(setf (subseq "foobarbaz" 3 6) "xxx") => "fooxxxbaz" ;ignores extra or short values

(fill "foobar" #\x :start 1 :end 3) => "fxxbar"

(search "bar" "foobarbaz") => 3 ;same as position, except first arg is a sequence
;;standard keyword args, :key, :test, :from-end, :start1, :end1, :start2, :end2 
(mismatch "foobarbaz" "foom") => 3 ;index where 2 sequences first diverge, nil if match

;;sequence predicates
;;predicate should take as many args as sequences passed
(every #'evenp '(1 2 3)) => NIL
(some #'evenp '(1 2 3)) => T
(notany #'evenp '(1 2 3)) => NIL
(notevery #'evenp '(1 2 3) => T

(every #'> '(1 2 3) '(4 3 2)) => NIL
(some #'> '(1 2 3) '(4 3 2)) => T
(notany #'> '(1 2 3) '(4 3 2)) => NIL
(notevery #'> '(1 2 3) '(4 3 2)) => T

;;sequence mapping functions
;;takes n-arg function and n sequences, returns new sequence applying elements of each
(map 'vector #'* #(1 2 3) #(10 9 8)) => #(10 18 24)
;;like map, instead it places results in sequence passed as first arg
(map-into *seq* #'+ '(1 2 3) '(4 5 6)) => #(5 7 9)

;;map over a sequence, apply 2 arg func to first 2 elements, then returned value with next
;;takes keyword args, :key :from-end :start :end and :initial-value
(reduce #'+ '(1 2 3 4)) => 10


;;working with lists

;;slicing and dicing
(destructuring-bind (x y z) '(1 2 3)
  (list :x x :y y :z z))  => (:X 1 :Y 2 :Z 3)

;;lists
;;cons cell
(cons 1 2) => (1 . 2)
(car (cons 1 2)) => 1
(cdr (cons 1 2)) => 2
(defparameter *cons* (cons 1 2))
(setf (car *cons*) 10 => 10 ;*cons* => (10 . 2)

;;building lists
(cons 1 nil) => (1)
(cons 1 (cons 2 nil)) => (1 2)
(list 1) => (1)
(list 1 2) => (1 2)
(defparameter *list* (list 1 2 3))
(first *list*) => 1
(rest *list*) => (2 3)
(first (rest *list*)) => 2

(append '(1 2) '(3 4)) => (1 2 3 4) ;takes any number of lists
(nconc '(1 2) '(3 4)) => (1 2 3 4) ;recycling version of append, builds on first list

(revappend x y) ;same as (append (reverse x) y)
(nreconc x y) ;same as (nconc (reverse x) y)

(defparameter *list0* '(1 2))
(defparameter *list1* '(3 4))
(defparameter *list2* (append *list0* *list1*))
*list2* => '(1 2 3 4)
;shares structure of given lists by returning pointers to them
(setf (first *list1*) 0) => 0
*list1* => (0 4)
*list2* => (1 2 0 4)

(push 3 *list0*) => (3 1 2) ;push on front of place
(pushnew 2 *list0*) => (3 2 1) ;push onto list if not already there
(pop *list0*) => 3 ;*list* => (1 2)

;;build list and return, recycling idiom
(let ((result nil))
  (dotimes (i 4)
	(push i result))
  (nreverse result))  => (0 1 2 3)

*list1* => (0 4)
*list2* => (1 2 0 4)
;;idiomatic use of recycling remove, changes shared structure as well
(setf *list2* (delete 4 *list2*)) => (1 2 0)
*list1* => (0)

;;list manipulating functions, all setf'able
car, cdr, caar, cadr, caadr ...
first ... tenth
(first '(1 2 3)) => 1
(rest '(1 2 3)) => (2 3)
(nth 0 '(1 2 3)) => 1
(nthcdr 1 '(1 2 3)) => (2 3)

(defparameter *list* '(1 2 3 4))
(last *list*) => 4
(butlast *list*) => (1 2 3) ;recycling version is nbutlast
(defparameter *list2* (cddr *list*)) ;*list2* => (3 4)
(ldiff *list* *list2*) => (1 2) ;list difference, checks shared structure, returns up to
(ldiff *list* '(3 4)) => (1 2 3 4) ;no match, sunlist not eq to part of list
(tailp *list2* *list*) => T ;returns true if object shares structure of list
(tailp '(3 4) *list*) => nil ;not eq

(list* 1 2 3 '(4 5)) => (1 2 3 4 5) ;list/append, builds list with final list as cdr

(consp '(1 . 2)) => T ;test if cons cell
(atom '(1 . 2)) => nil ;tests if not a cons cell
(listp '(1 . 2)) => T ;test if cons cell or nill
(null '()) => T ;test if nill, same as not, but preferable when testing empty list

;;mapping functions for lists
(mapcar #'(lambda (x) (* 2 x)) '(1 2 3)) => (2 4 6) ;apply function to each element
(mapcar #'+ '(1 2 3) '(4 5 6)) => (5 7 9) ;takes as many lists as function args
(mapcar #'cons '(1 2) '(3 4)) => ((1 . 3) (2 . 4))

(mapcan #'cons '(1 2) '(3 4)) => (1 2 . 4) ;like mapcar, but uses nconc to build list
;;returns first list, useful for side effects
(mapc #'(lambda (x y) (format t "~a " (* x y))) '(1 2) '(3 4)) => (1 2) ;prints 3 8

(maplist #'append '(1 2 3) '(4 5 6) '(7 8 9)) ;applied to successive sublists
	=> ((1 2 3 4 5 6 7 8 9) (2 3 5 6 8 9) (3 6 9))
(mapcon #'list '(1 2 3)) => ((1 2 3) (2 3) (3)) ;like maplist, but uses nconc to build list
;;returns first list, use rest of list
(defparameter *acc* nil)
(mapl #'(lambda (x) (push x *acc*)) '(1 2 3)) => (1 2 3) ;*acc* => ((3) (2 3) (1 2 3))
