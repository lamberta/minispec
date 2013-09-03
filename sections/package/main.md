### [\*package\*]

### [\*modules\*]

### [load] filespec \[:verbose :print :if-does-not-exist :external-format\] => boolean

### [defpackage] name \[options...\] => package

*options* are:

* :nicknames
* :use
* :import-from
* :shadow
* :shadowing-import-from
* :export
* :intern
* :size
* :documentation

~~~
(defpackage :example-package
  (:nicknames :expkg :ex-pkg)
  (:use
    :cl
    :text-db
    :text)
  (:import-from :email :parse-email-address)
  (:shadow :my-name) ;ignore import version, use local
  (:shadowing-import-from :text-db ;resolve conflicting imports
    :save)
  (:export
    :my-fn))
(in-package :example-package)
(load "./src/example-package")
*package* ;=> #<Package "EXAMPLE-PACKAGE">
~~~

### [export] symbols \[package\] => t

### [find-symbol] string \[package\] => symbol, status

### [find-package] name => package

### [find-all-symbols] string => symbols

### [import] symbols \[package\] => t

### [list-all-packages] => packages

### [rename-package] package new-name \[new-nicknames\] => package-object

### [shadow] symbol-names \[package\] => t

### [shadowing-import] symbols \[package\] => t

### [delete-package] package => boolean

### [make-package] package-name \[:nicknames :use\] => package

### [unexport] symbols \[package\] => t

### [unintern] symbol \[package\] => boolean

### [in-package] name => package

### [unuse-package] packages-to-unuse \[package\] => t

### [use-package] packages-to-use \[package\] => t

### [intern] string \[package\] => symbol, status

### [package-name] package => name

### [package-nicknames] package => nicknames

### [package-shadowing-symbols] package => symbols

### [package-use-list] package => use-list

### [package-used-by-list] package => used-by-list

### [package-error-package] condition => package
