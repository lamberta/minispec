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
otherwise [nil]. Ignore differences in case using
[string-not-lessp].
