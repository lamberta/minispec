### [\*default-pathname-defaults\*] = pathname

Implementation-dependent *pathname* used as the default
whenever a function needs a [pathname] and one is not
supplied. Typically, the working directory where Common Lisp
was started up.

### [user-homedir-pathname] \[host\] => pathname

### [pathname] pathspec => pathname

### [make-pathname] \[:host :device :directory :name :type :version :defaults :case\] => pathname

### [pathname-host] pathname &key case => host

### [pathname-device] pathname &key case => device

### [pathname-directory] pathname &key case => directory

### [pathname-name] pathname &key case => name

### [pathname-type] pathname &key case => type

### [pathname-version] pathname &key case => version

### [load-logical-pathname-translations] host => just-loaded

### [logical-pathname-translations] host => translations

`setf`able.

### [logical-pathname] pathspec => logical-pathname

### [namestring] pathname => namestring

### [file-namestring] pathname => namestring

### [directory-namestring] pathname => namestring

### [host-namestring] pathname => namestring

### [enough-namestring] pathname \[defaults\] => namestring

### [parse-namestring] thing \[host default-pathname :start :end :junk-allowed\] => pathname, position

### [translate-logical-pathname] pathname &key => physical-pathname

### [translate-pathname] source from-wildcard to-wildcard &key => translated-pathname

### [merge-pathnames] pathname \[default-pathname default-version\] => merged-pathname
