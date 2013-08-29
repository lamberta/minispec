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
