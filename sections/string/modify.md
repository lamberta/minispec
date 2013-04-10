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
