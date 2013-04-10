# Strings

Inherits: [vector] => [array] => [sequence] => [t]

A string is a specialized vector (one-dimensional array)
whose elements are characters.

* [CLtL2](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node164.html)
* [PCL: Ch 10. Numbers, Characters, and Strings](http://www.gigamonkeys.com/book/numbers-characters-and-strings.html)
* [HyperSpec: Strings Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_string.htm)

## Create

### [string] x => string

Convert [symbol], [keyword], or [character] to a [string].

~~~
(string :hello) ;=> "hello"
~~~

### [make-string] size &key initial-element element-type => string

Return a [simple-string] of length *size*.

~~~
(make-string 10 :initial-element #\5) ;=> "5555555555"
~~~

### [parse-integer] str &key start end radix junk-allowed => int, idx

Parses an [integer] in the specified *:radix*.

~~~
(parse-integer "24h" :junk-allowed t) ;=> 24, 2
~~~

m4_include(../sections/array-create.md)


## Select

### [char] str idx => character

Specialized element accessor for type [string]. Is [setf]able.

~~~
(char "hello" 1) ;=> #\e
(elt "hello" 1)  ;=> #\e
~~~

m4_include(../sections/array-select.md)

m4_include(../sections/sequence-select.md)


## Modify

m4_include(../sections/string-modify.md)

m4_include(../sections/vector-modify.md)

m4_include(../sections/sequence-modify.md)

## Comparison

m4_include(../sections/string-compare.md)

## Predicates

m4_include(../sections/string-pred.md)

m4_include(../sections/sequence-pred.md)
