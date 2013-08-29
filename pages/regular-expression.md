# Regular Expressions

* [CL-PPCRE](http://weitz.de/cl-ppcre/)

## Install

~~~
(ql:quickload :cl-ppcre)
~~~

## Scanning

### [cl-ppcre:create-scanner]

* __create-scanner__ (string string) &key
  case-insensitive-mode multi-line-mode single-line-mode
  extended-mode destructive => scanner, register-names
* __create-scanner__ (function function)&key
  case-insensitive-mode multi-line-mode single-line-mode
  extended-mode destructive => scanner
* __create-scanner-__ (parse-tree t)&key
  case-insensitive-mode multi-line-mode single-line-mode
  extended-mode destructive => scanner, register-names

### [cl-ppcre:scan]

### [cl-ppcre:scan-to-strings]

### [cl-ppcre:register-groups-bind]

### [cl-ppcre:do-scans]

### [cl-ppcre:do-matches]

### [cl-ppcre:do-matches-as-strings]

### [cl-ppcre:all-matches]

### [cl-ppcre:all-matches-as-strings]


## Splitting and Replacing

### [cl-ppcre:split]

### [cl-ppcre:regex-replace]

### [cl-ppcre:regex-replace-all]


## Modifying Scanner Behaviour

### [cl-ppcre:\*property-resolver\*]

### [cl-ppcre:parse-tree-synonym]

### [cl-ppcre:\*regex-char-code-limit\*]

### [cl-ppcre:\*use-bmh-matchers\*]

### [cl-ppcre:\*optimize-char-classes\*]

### [cl-ppcre:\*allow-quoting\*]

### [cl-ppcre:\*allow-names-registers\*]


## Misc

### [cl-ppcre:parse-string]

### [cl-ppcre:create-optimized-test-function]

### [cl-ppcre:quote-meta-chars]

### [cl-ppcre:regex-apropos]

### [cl-ppcre:regex-apropos-list]


## Conditions

### [cl-ppcre:ppcre-error]

### [cl-ppcre:ppcre-invocation-error]

### [cl-ppcre:ppcre-syntax-error]

### [cl-ppcre:ppcre-syntax-error-string]

### [cl-ppcre:ppcre-syntax-error-pos]


## Unicode

### [cl-ppcre:unicode-property-resolver]
