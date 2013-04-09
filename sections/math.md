# Numbers

## Predicates

### [=] &rest numbers\* => boolean

Returns [t] if all numbers are the same in value, otherwise [nil].

~~~
(+ 6 6 6) ;=> T
~~~

### [/=] &rest numbers\* => boolean

### [>] &rest numbers\* => boolean

### [>=] &rest numbers\* => boolean

### [<] &rest numbers\* => boolean

### [<=] &rest numbers\* => boolean

### [zerop] number => boolean

### [plusp] real => boolean

### [minusp] real => boolean

Returns [t] if *number* is zero ([integer], [float], or [complex]); otherwise [nil].

### [evenp] integer => boolean

### [oddp] integer => boolean

### [numberp] object => boolean

### [realp] object => boolean

### [rationalp] object => boolean

### [floatp] object => boolean

### [integerp] object => boolean

### [complexp] object => boolean

### [random-state-p] object => boolean


## Numeric Functions

### [+] &rest numbers\* => sum

### [\*] &rest numbers\* => product

### [-] &rest numbers\* => difference

Also used for negation, as in `(- 3) => -3`.

### [/] &rest numbers\* => quotient

Also used for the reciprocal, as in `(/ 3) => 1/3`.

### [1+] number => successor

### [1-] number => predecessor

### [incf] place [delta] => new-value

### [decf] place [delta] => new-value

### [exp] number => result

### [expt] base-number power-number => result

### [log] number &optional base => logarithm

### [sqrt] number => root

### [isqrt] natural => natural-root

### [lcm] &rest integers => least-common-multiple

### [gcd] &rest integers => greatest-common-denominator

### [pi]

Constant variable. The long float approximation to the mathematical constant *PI*.

~~~
pi ;=> 3.141592653589793D0
~~~

### [sin] radians => number

### [cos] radians => number

### [tan] radians => number

### [asin] number => radians

### [acos] number => radians

### [atan] number1 &optional number2 => radians

### [sinh] number => result

### [cosh] number => result

### [tanh] number => result

### [asinh] number => result

### [acosh] number => result

### [atanh] number => result

### [cis] radians => number

### [conjugate] number => conjugate

### [max] &rest reals\* => max-real

### [alexandria:maxf] place &rest numbers env

### [min] &rest reals\* => min-real

### [alexandria:minf] place &rest numbers env

### [round] number &optional divisor => quotient, remainder

Returns an [integer] quotient, [fround] returns a [float].

### [floor] number &optional divisor => quotient, remainder

Returns an [integer] quotient, [ffloor] returns a [float].

### [ceiling] number &optional divisor => quotient, remainder

Returns an [integer] quotient, [fceiling] returns a [float].

### [truncate] number &optional divisor => quotient, remainder

Returns an [integer] quotient, [ftruncate] returns a [float].

### [mod] number divisor => modulus

### [rem] number divisor => remainder

### [random] limit &optional random-state => random-number

### [make-random-state] &optional state => new-state

### [\*random-state\*]

The current *random state*, which is used, for example, by
the function random when a random state is not explicitly
supplied.

### [signum] number => signed-prototype

### [numerator] rational => numerator

### [denominator] rational => denominator

### [realpart] number => real

### [imagpart] number => real

### [complex] realpart &optional imagpart => complex

### [phase] number => phase

### [abs] number => absolute-value

### [rational] number => rational

### [rationalize] number => rational

### [float] number &optional prototype => float


## Logic Functions

### [boole] op integer1 integer2 => integer

### [lognot] integer => integer

### [logeqv] &rest integers => integer

### [logand] &rest integers => integer

### [logandc1] integer1 integer2 => integer

### [logandc2] integer1 integer2 => integer

### [lognand] integer1 integer2 => integer

### [logxor] &rest integers => integer

### [logior] &rest integers => integer

### [logorc1] integer1 integer2 => integer

### [logorc2] integer1 integer2 => integer

### [lognor] integer1 integer2 => integer

### [logbitp] index integer => boolean

### [logtest] integer1 integer2 => boolean

### [logcount] integer => number-of-on-bits


## Integer Functions

### [integer-length] integer => number-of-bits

### [ldb-test] bytespec integer => boolean

### [ash] integer count => shifted-integer

### [ldb] bytespec integer => byte

### [deposit-field] newbyte bytespec integer => integer

### [dpb] newbyte bytespec integer => integer

### [mask-field] bytespec integer => masked-integer

### [byte] size position => bytespec

### [byte-size] bytespec => size

### [byte-position] bytespec => position


## Implementation Dependent

### [short-float-epsilon]
### [short-float-negative-epsilon]

### [single-float-epsilon]
### [single-float-negative-epsilon]

### [double-float-epsilon]
### [double-float-negative-epsilon]

### [long-float-epsilon]
### [long-float-negative-epsilon]

### least-negative-{short-float|single-float|double-float|long-float}

### least-negative-normalized-{short-float|single-float|double-float|long-float}

### least-positive-{short-float|single-float|double-float|long-float}

### least-positive-normalized-{short-float|single-float|double-float|long-float}

### most-negative-{short-float|single-float|double-float|long-float|fixnum}

### most-positive-{short-float|single-float|double-float|long-float|fixnum}

### [decode-float] float => significand, exponent, sign

### [integer-decode-float] float => significand, exponent, integer-sign

### [scale-float] float integer => scaled-float

### [float-radix] float => float-radix

### [float-sign] float-1 &optional float-2 => signed-float

### [float-digits] float => digits1

### [float-precision] float => digits2

### [upgraded-complex-part-type]


## Extras

### [alexandria:binomial-coefficient] n k => integer

### [alexandria:count-permutations] n &optional k => integer

### [alexandria:clamp] number min max => number

### [alexandria:lerp] v a b => number

### [alexandria:factorial] n => number

### [alexandria:subfactorial] n => number

### [alexandria:gaussian-random] &optional min max => number, number

### [alexandria:iota] n &key start step => list

~~~
(iota 4)                      ;=> (0 1 2 3)
(iota 3 :start 1 :step 1.0)   ;=> (1.0 2.0 3.0)
(iota 3 :start -1 :step -1/2) ;=> (-1 -3/2 -2)
~~~

### [alexandria:map-iota] function n &key start step => number

~~~
(map-iota #'print 3 :start 10 :step 2) ;=> 3 [prints 10,12,14]
~~~

### [alexandria:mean] sample => number

*sample* is a [sequence] of numbers.

### [alexandria:median] sample => number

*sample* is a [sequence] of real numbers.

### [alexandria:variance] sample &key biased => number

*sample* is a [sequence] of numbers.

### [alexandria:standard-deviation] sample &key biased => number

*sample* is a [sequence] of numbers.

### [cl-utilities:extremum] sequence fn &key key start end => smallest-element

### [cl-utilities:extrema] sequence fn &key key start end => smallest-elements

### [cl-utilities:n-most-extreme] n sequence fn &key key (start 0) end => n-smallest-elements

### [cl-utilities:expt-mod] n exponent divisor => result

Returns *n* raised to the *exponent* power, modulo *divisor*.
