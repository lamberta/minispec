# Output

* [CLtL2: Output Functions](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node197.html)

## Character Streams

### [write] object \[&key\*\] => object

### [prin1] object \[output-stream\] => object

produces output suitable for input to read.

### [print] object \[output-stream\] => object

like prin1 except that the printed representation of object is preceded by a newline and followed by a space.

### [pprint] object \[output-stream\] => object

just like print except that the trailing space is omitted and object is printed with the *print-pretty* flag non-nil to produce pretty output.

### [princ] object \[output-stream\] => object

is just like prin1 except that the output has no escape
characters. The general rule is that output from princ is intended to look good to people, while output from prin1 is intended to be acceptable to read.

### [write-to-string] object \[&key\*\] => object

### [prin1-to-string] object => object

### [princ-to-string] object => object

### [write-char] character \[output-stream\] => character

### [write-string] string \[output-stream\] &key :start :end => string

### [write-line] string \[output-stream\] &key :start :end => string

### [terpri] \[output-stream\] => nil

### [fresh-line] \[output-stream\] => boolean

### [finish-output] \[output-stream\] => nil

### [force-output] \[output-stream\] => nil

### [clear-output] \[output-stream\] => nil

### [print-unreadable-object] \(object stream &key type identity\) form\* => nil

### [\*print-readably\*]


## Binary Streams

### [write-byte] integer binary-output-stream


m4_include(../sections/format.md)
