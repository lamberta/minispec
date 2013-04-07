# Strings

Inherits: [vector] => [array] => [sequence] => [t]

A string is a specialized vector (one-dimensional array)
whose elements are characters.

* [CLtL2](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node164.html)
* [PCL: Ch 10. Numbers, Characters, and Strings](http://www.gigamonkeys.com/book/numbers-characters-and-strings.html)
* [HyperSpec: Strings Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_string.htm)


## Construction

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


## Access

### [char] str idx => character

Specialized element accessor for type [string]. Is [setf]able.

~~~
(char "hello" 1) ;=> #\e
(elt "hello" 1)  ;=> #\e
~~~


## Manipulation

### [string-trim] chars str => new-str

Returns a substring of *str*, with all characters in *chars*
stripped off the beginning and end.

~~~
(string-trim "abc" "abcaakaaakabcaaa") ;=> "kaaak"
(string-trim '(#\Space #\Tab #\Newline) " garbanzo beans ") ;=> "garbanzo beans"
(string-trim " (*)" " ( *three (silly) words* ) ") ;=> "three (silly) words"
~~~

### [string-left-trim] chars str => new-str

~~~
(string-left-trim "abc" "labcabcabc") ;=> "labcabcabc"
(string-left-trim " (*)" " ( *three (silly) words* ) ") ;=> "three (silly) words* ) "
~~~

### [string-right-trim] chars str => new-str

~~~
(string-right-trim " (*)" " ( *three (silly) words* ) ") ;=> " ( *three (silly) words"
~~~

### [string-upcase] str &key start end => new-str

Also [nstring-upcase]

### [string-downcase] str &key start end => new-str

Also [nstring-downcase]

### [string-capitalize] str &key start end => new-str

Also [nstring-capitalize]


## Comparison

### [stringp] obj => boolean

Test if an object is a [string].

### [string=] str1 str2 &key start1 end1 start2 end2 => boolean

Returns [t] if the given strings are of the same length
and contain the same characters, otherwise return
[nil]. Ignore differences in case using [string-equal].

~~~
(string= "foo" "foo") ;=> T
(string= "foo" "Foo") ;=> NIL
(string= "abcd" "01234abcd9012" :start2 5 :end2 9) ;=> T
(string-equal "foo" "Foo") ;=> T
~~~

### [string/=] str1 str2 &key start1 end1 start2 end2 => mismatch-idx

Returns [t] if the given strings are different, otherwise
[nil]. Ignore differences in case using [string-not-equal].

~~~
(string-not-equal "AAAA" "aaaA") ;=> NIL
~~~

### [string<] str1 str2 &key start1 end1 start2 end2 => mismatch-idx

Returns [t] if *str1* is less than *str2*, otherwise
[nil]. Ignore differences in case using [string-lessp].

~~~
(string< "aaaa" "aaab") ;=> 3
(string-lessp "012AAAA789" "01aaab6" :start1 3 :end1 7 :start2 2 :end2 6) ;=> 6
~~~

### [string>] str1 str2 &key start1 end1 start2 end2 => mismatch-idx

Returns [t] if *str1* is greater than *str2*, otherwise
[nil]. Ignore differences in case using [string-greaterp].

### [string<=] str1 str2 &key start1 end1 start2 end2 => mismatch-idx

Returns [t] if *str1* is less than or equal to *str2*,
otherwise [nil]. Ignore differences in case using [string-not-greaterp].

~~~
(string>= "aaaaa" "aaaa") ;=> 4
(string-not-greaterp "Abcde" "abcdE") ;=> 5
~~~

### [string>=] str1 str2 &key start1 end1 start2 end2 => mismatch-idx

Returns [t] if *str1* is greater than or equal to *str2*,
otherwise [nil]. Ignore differences in case using [string-not-lessp].
