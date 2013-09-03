### [\*read-base\*] = 10

Controls the interpretation of tokens by [read] as being
integers or ratios.

### [\*read-default-float-format\*] = 'single-float

Controls the floating-point format that is to be used when
reading a floating-point number that has no exponent marker.

### [\*read-eval\*] = t

If true, the `#.` reader macro has its normal effect,
otherwise, signals an error. 

### [\*read-suppress\*] = nil

Supports the operation of the read-time conditional notations `#+` and `#-`.

### [read] \[input-stream eof-error-p eof-value recursive-p\] => object

### [read-preserving-whitespace] \[input-stream eof-error-p eof-value recursive-p\] => object

### [read-delimited-list] char \[input-stream recursive-p\] => list

### [read-from-string] string \[eof-error-p eof-value :start :end :preserve-whitespace\] => object, position

### [read-char]

### [read-char-no-hang]

### [peek-char]

### [unread-char]

### [read-byte]

### [read-line]

### [read-sequence]

### [yes-or-no-p]

Or [y-or-n-p].

### [clear-input]

### [with-input-from-string]
