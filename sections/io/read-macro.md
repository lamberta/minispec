### [\*readtable\*] = current-readtable

Controls the parsing behavior of the reader, and can also
influence the printer.

### [copy-readtable] \[from-readtable to-readtable\] => readtable

### [make-dispatch-macro-character] char \[non-terminating-p readtable\] => t

### [readtable-case] readtable => mode

`setf`able.

### [get-dispatch-macro-character] disp-char sub-char \[readtable\] => function

### [set-dispatch-macro-character] disp-char sub-char new-function \[readtable\] => t

### [get-macro-character] char \[readtable\] => function, non-terminating-p

### [set-macro-character] char new-function \[non-terminating-p readtable\] => t

### [set-syntax-from-char] to-char from-char \[to-readtable from-readtable\] => t

### [with-standard-io-syntax] form\* => result
