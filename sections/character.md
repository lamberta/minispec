# Characters

The [standard-char] type comprises a-z, A-Z, 0-9, Newline,
Space, and !?$"’‘.:,;*+-/|\~^<=>#%@&()[]{}.

* [HyperSpec: Characters Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_charac.htm)
* [CLtL2: Ch 13. Characters](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node135.html)
* [PCL: Ch 10. Numbers, Characters, and Strings](http://www.gigamonkeys.com/book/numbers-characters-and-strings.html#characters)


## Comparison

### [characterp] object => boolean

Test if *object* is of type [character].

### [standard-char-p] char => boolean

Test if *char* is of type [standard-char].

* [List of standard characters](http://www.lispworks.com/documentation/HyperSpec/Body/02_ac.htm).

### [graphic-char-p] char => boolean

Test if *char* is a graphic (printing) [character].

~~~
(graphic-char-p #\Space) ;=> T
~~~

### [alpha-char-p] char => boolean

Test if *char* is an alphabetic [character].

~~~
(alpha-char-p #\a) ;=> T
(alpha-char-p #\5) ;=> NIL
~~~

### [alphanumericp] char => boolean

Test if *char* is an alphabetic or numeric [character].

~~~
(alpha-char-p #\a) ;=> T
(alpha-char-p #\5) ;=> T
~~~

### [upper-case-p] char => boolean

Test if *char* is an uppercase [character].

~~~
(upper-case-p #\A) ;=> T
~~~

### [lower-case-p] char => boolean

Test if *char* is an lowercase [character].

~~~
(lower-case-p #\A) ;=> NIL
~~~

### [both-case-p] char => boolean

Test if *char* is a [character] with case.

~~~
(both-case-p #\5) ;=> NIL
~~~

### [digit-char-p] char &optional radix => weight

Return its weight if *char* is a digit, or [nil] otherwise.

~~~
(digit-char-p #\5)    ;=> 5
(digit-char-p #\a)    ;=> NIL
(digit-char-p #\a 11) ;=> 10
~~~

### [char=] &rest chars... => boolean

Returns [t] if all characters are the same, otherwise
[nil]. Ignore differences in case using [char-equal].

~~~
(char= #\d #\d)      ;=> T
(char= #\A #\a)      ;=> NIL
(char-equal #\A #\a) ;=> T
~~~

### [char/=] &rest chars... => boolean

Returns [t] if all characters are different, otherwise
[nil]. Ignore differences in case using [char-not-equal].

~~~
(char/= #\d #\d)         ;=> NIL
(char/= #\A #\a)         ;=> T
(char-not-equal #\A #\a) ;=> NIL
~~~

### [char<] &rest chars... => boolean

Returns [t] if the characters are monotonically
increasing, otherwise [nil]. Ignore differences in case
using [char-lessp].

~~~
(char< #\a #\e #\e #\y) ;=> T
~~~

### [char<=] &rest chars... => boolean

Returns [t] if the characters are monotonically
non-decreasing, otherwise, it [nil]. Ignore differences in
case using [char-not-greaterp].

~~~
(char<= #\a #\e #\e #\y) ;=> T
~~~

### [char>] &rest chars... => boolean

Returns [t] if the characters are monotonically
decreasing, otherwise [nil]. Ignore differences in case
using [char-greaterp].

~~~
(char> #\e #\d) ;=> T
~~~

### [char>=] &rest chars... => boolean

Returns [t] if the characters are monotonically
non-increasing, otherwise, it [nil]. Ignore differences in
case using [char-not-lessp].

~~~
(char>= #\e #\d) ;=> T
~~~


## Conversion

### [character] char => denoted-char

Returns the [character] denoted by the character
designator. This is equivalent to `(coerce char 'character)`.

~~~
(character "a") ;=> #\a
~~~

### [digit-char] i &optional radix => char

Return a [character] representing non-negative [integer] *i*.

~~~
(digit-char 10 11) ;=> #\A
~~~

### [char-name] char => name

Returns a [string] that is the name of the character, or [nil].

~~~
(char-name #\Space) ;=> "Space"
~~~

### [name-char] name => char

Return character name, as determined by [string-equal], or [nil].

~~~
(name-char " ") ;=> #\Space
~~~

### [char-code] char => code

Returns the encoding of a [character]. If the character has no
implementation-defined attributes, the result of [char-int]
is the same.

~~~
(char-code #\A) ;=> 65
~~~

### [code-char] code => char

Return the [character] given its code.

~~~
(code-char 65) ;=> #\A
~~~

### [char-upcase] char => corresponding-char

If *char* is lowercase return the corresponding
uppercase [character], or the given character.

~~~
(char-upcase #\a) ;=> #\A
~~~

### [char-downcase] char => corresponding-char

If *char* is lowercase return the corresponding
uppercase [character], or the given character.

~~~
(char-downcase #\A) ;=> #\a
~~~
