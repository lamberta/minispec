# Common Lisp Object System

* [HyperSpec: Objects](http://www.lispworks.com/documentation/HyperSpec/Body/07_.htm)
* [CLtL2: Common Lisp Object System](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node260.html)
* [PCL: Generic Functions](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html)
* [PCL: Classes](http://www.gigamonkeys.com/book/object-reorientation-classes.html)

## Classes

### [defclass] name ([superclass]) definitions... => new-class

Defines a new named [class], returning the new class object as its result.

~~~
(defclass mammal ()
  (legs sound))

(defclass dog (mammal)
  ((name
     :initarg :name
     :initform (error "A dog needs a name.")
     :reader title                           ;getter
	 :writer (setf name)                     ;setter
     :documentation "Dog name.")
   (sound
     :accessor sound                         ;getter/setter
	 :initform "woof")))
~~~

And to pretty-print the object at the REPL:

~~~
(defmethod print-object ((dog dog) stream)
  (print-unreadable-object (dog stream :type t :identity t)
    (with-slots (name) dog
      (format stream ":NAME ~S" name))))
~~~

### [make-instance] class => instance

~~~
(setf mydog (make-instance 'dog :name "Sparky"))
~~~

### [slot-value] instance slot => value

~~~
(setf (slot-value mydog 'legs) 4)
(slot-value mydog 'legs)          ;=> 4
(slot-value mydog 'breed)         ;=> [throws error]
~~~

### [with-slots] (slots...) instance forms... => result

~~~
(with-slots (name sound) mydog
  (list name sound))                  ;=> ("Sparky" "woof")

(with-slots ((n name) (l legs)) mydog
  (list n l))                         ;=> ("Sparky" 4)
~~~

### [with-accessors] (accessors...) instance forms... => result

~~~
(with-accessors (sound) mydog
  (list sound))                       ;=> ("woof")

(with-accessors ((s sound)) mydog
  (list s))                           ;=> ("woof")
~~~

### [slot-exists-p] instance name => boolean

Test if object instance has a given slot.

### [slot-boundp] instance name => boolean

Test if a slot is bound on a given object instance. Throw
error if no such slot.

### [slot-makunbound] instance name => instance

Unbind the slot on the given object instance.

### [class-of] instance => class

~~~
(class-of mydog)  ;=> #<STANDARD-CLASS DOG>
~~~

### [find-class] symbol &optional errorp environment => class

~~~
(find-class 'dog) ;=> #<STANDARD-CLASS DOG>
~~~

### [class-name] class => name

Returns the name of the given [class].

~~~
(class-name (find-class 'dog))                ;=> DOG
(setf (class-name (find-class 'dog)) 'doggie)
~~~


## Methods

### [defgeneric] name lambda-list &rest options-and-methods... => generic-fn

Define or modify a *generic function*.

~~~
(defgeneric say-hello (mammal)
  (:documentation "Say hi."))

(defgeneric say-hello (mammal)
  (:method ((m mammal)) (print "blink"))
  (:method ((d dog)) (print "wag tail")))
~~~

### [defmethod] name lambda-list &rest forms... => method

Defines a method on a *generic function*.

~~~
(defgeneric make-sound (dog))

(defmethod make-sound ((dog dog))
  (slot-value dog 'sound))

(make-sound mydog) ;=> "woof"
~~~

Use [setf] to define a class setter method:

~~~
(defmethod (setf make-sound) (noise (dog dog))
  (setf (slot-value dog 'sound) noise))

(setf (make-sound mydog) "bark!")
~~~

### [initialize-instance] instance &rest initargs &key &allow-other-keys => instance

Called by [make-instance] to initialize a newly created
instance. Use with method combination to create a
constructor-like function.

~~~
(defmethod initialize-instance :after ((dog dog) &key)
  (setf (slot-value dog 'legs) 4))
~~~

### [add-method] generic-fn method => generic-fn

Adds a [method] to a *generic function*.

### [remove-method] generic-fn method => generic-fn

Removes a [method] from a *generic-function*.

### [ensure-generic-function] fn-name &key... => generic-fn

Define or modify a *generic function*.

### [invalid-method-error] method format-control &rest args => implementation-dependent

Signal error on applicable method with invalid qualifiers.

### [method-combination-error] format-control &rest args => implementation-dependent

Signal error on applicable method with invalid method combination.


## Method Combination

* [PCL: Method Combination](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html#method-combination)
* [CLtL2: Method Selection and Combination](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node283.html)

Using standard method combination:

~~~
(defgeneric bedtime (dog))

(defmethod bedtime ((d dog))
  (print 'zzzzz))

(defmethod bedtime :before ((d dog))
  (print 'lay-down))

(defmethod bedtime :after ((d dog))
  (print 'wake-up))

(defmethod bedtime :around ((d dog))
  (print 'yawn)
  (let ((val (call-next-method)))  ;=> save original return value
    (print 'get-up)
    val))

(bedtime mydog) ;=> ZZZZZ [prints YAWN, LAY-DOWN, ZZZZZ, WAKE-UP, GET-UP]
~~~

### [next-method-p] => boolean

Called within a *method* to determine whether a *next method* exists.

### [call-next-method] &rest args... => result

Used within a method to call the *next method*.

~~~
(defgeneric itch (mammal body-part))

(defmethod itch ((m mammal) body-part)
  (format t "twitch ~a" body-part))

(defmethod itch ((d dog) body-part)
  (format t "scratch ~a~%" body-part)
  (if (next-method-p)
    (call-next-method)))

(itch mydog "leg") ;=> [prints "scratch leg" then "twitch leg"]
~~~

### [define-method-combination] name lambda-list &rest forms... => name

Define a new [method-combination] type.

Combination options: progn|+|list|and|or|max|min|append|nconc
Default is *progn*, this executes all applicable methods in
most-specific to least-specific order.

~~~
(defgeneric eat (mammal)
  (:method-combination progn))

(defmethod eat progn ((m mammal))
  (print 'digest))

(defmethod eat progn ((d dog))
  (print 'lick))

(eat mydog) ;=> DIGEST [prints LICK, DIGEST]
~~~

### [call-method] method-name &optional next-method-list => result

From within a [method], call *method-name* with the
arguments of the generic function and with information about
its next-methods; return its values.


## Method Selection

### [find-method] generic-fn method-qualifiers specializers &optional errorp => method

Return suitable method, or signal *error*.

~~~
(find-method #'say-hello '() '(mammal)) ;=> #<STANDARD-METHOD SAY-HELLO (MAMMAL)>
~~~

### [compute-applicable-methods] generic-fn fn-args => methods

List of methods suitable for *fn-args*, most specific first.

~~~
(compute-applicable-methods #'say-hello (list mydog))
  ;=> (#<STANDARD-METHOD SAY-HELLO (DOG)> #<STANDARD-METHOD SAY-HELLO (MAMMAL)>)
~~~

### [function-keywords] method => keys, allow-other-keys-p

Returns the keyword parameter specifiers for a method.

### [method-qualifiers] method => qualifiers

Returns the keyword parameter specifiers for a method.


## Redefine

### [reinitialize-instance] instance &rest initargs &key &allow-other-keys => instance

Change the values of local slots of an object instance according to *initargs*.

### [change-class] instance new-class &key &allow-other-keys => instance

Changes the class of an instance to *new-class*. It
destructively modifies and returns the instance.

### [make-instances-obsolete] class => class

Update all existing instances of class using [update-instance-for-redefined-class].

### [allocate-instance] class &rest initargs &key &allow-other-keys => new-instance

Creates and returns a new instance of the class, without
initializing it. Called by [make-instance].

### [shared-initialize] instance slot-names &rest initargs &key &allow-other-keys => instance

Fill the slots of an object instance using *initargs* and
*:initform* forms. Called when an instance is created.


## Internal

### [slot-missing] class instance slot-name operation &optional new-value => result

Called on attempted access to non-existing slot.

### [slot-unbound] class instance slot-name => result

Called on attempted access to unbound slot.

### [update-instance-for-redefined-class] instance args...  => result

Set any initarg slots to their corresponding values.

### [update-instance-for-different-class] previous current &rest... => implementation-dependent

Set slots on behalf of [change-class] by means of [shared-initialize].

### [no-applicable-method] generic-fn &rest fn-args => result

Called on invocation of generic-function on args if there is no applicable method.

### [no-next-method] generic-fn method &rest args => result

Called on invocation of [call-next-method] when there is no next method.
