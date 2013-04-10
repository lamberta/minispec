# Output

* [CLtL2: Output Functions](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node197.html)

## Character Streams

### [write]

### [prin1] object \[output-stream\]

### [print] object \[output-stream\]

### [pprint] object \[output-stream\]

### [princ] object \[output-stream\]

### [write-to-string] object &key :escape :radix :base :circle :pretty :level :length :case :gensym :array

### [prin1-to-string] object

### [princ-to-string] object

### [write-char] character \[output-stream\]

### [write-string] string \[output-stream\] &key :start :end

### [write-line] string \[output-stream\] &key :start :end

### [terpri] \[output-stream\]

### [fresh-line] \[output-stream\]

### [finish-output] \[output-stream\]

### [force-output] \[output-stream\]

### [clear-output] \[output-stream\]

### [print-unreadable-object] \(object stream \[\[ :type type | :identity id \]\]\) \{declaration\}\* \{form\}\*


### [\*print-readably\*]


## Binary Streams

### [write-byte] integer binary-output-stream


m4_include(../sections/format.md)
