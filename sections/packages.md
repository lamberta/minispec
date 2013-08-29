[defpackage](http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm#defpackage)

~~~
(defpackage :example-package
  (:nicknames :expkg :ex-pkg)
  (:use
    :cl
    :com.gigamonkeys.text-db
    :com.acme.text)
  (:import-from :com.acme.email :parse-email-address)
  (:shadow :my-name) ;ignore import version, use local
  (:shadowing-import-from :com.gigamonkeys.text-db ;resolve conflicting imports
    :save)
  (:export
    :my-fn))
  

(in-package :example-package)
~~~

~~~
(load "./src/example-package")
~~~

[HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Body/f_use_pk.htm#use-package)
(use-package :example-package)


## Systems

In `example.asd`:

~~~
(asdf:defsystem :example
  :serial t                ;load files in order
  :depends-on (:alexandria :cl-utilities)
  :components ((:file "package")
               (:file "example")))
~~~


`(ql:quickload :example)`

[quickproject](https://github.com/xach/quickproject)

~~~
(quickproject:make-project "~/src/lisp/example/"
  :depends-on '(alexandria cl-utilities))
~~~

Any project in `~/quicklisp/local-projects/` will be available.

~~~
(ql:quickload :example)
~~~
