In `example.asd`:

~~~
(asdf:defsystem :example
  :serial t                ;load files in order
  :depends-on (:alexandria :cl-utilities)
  :components ((:file "package")
               (:file "example")))
~~~

~~~
`(ql:quickload :example)`
~~~
