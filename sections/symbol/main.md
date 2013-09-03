### [make-symbol] name => new-symbol

### [copy-symbol] symbol \[copy-properties\] => new-symbol

### [gensym] \[name\] => new-symbol

Uses [\*gensym-counter\*]

~~~
(gensym)        ;=> #:G1123 (value of *gensym-counter*)
(gensym "name") ;=> #:|name1124|
(gensym 99)     ;=> #:G99
~~~

### [symbol-function] symbol => contents

### [symbol-name] symbol => name

### [symbol-package] symbol => contents

### [symbol-plist] symbol => plist

### [symbol-value] symbol => value

### [get] symbol indicator \[default\] => value

### [set] symbol value => value

### [remprop] symbol indicator => boolean

### [boundp] symbol => boolean

### [makunbound] symbol => symbol
