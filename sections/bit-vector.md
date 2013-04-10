## Bit-Vectors

* [CLtL2: Bit-Vectors](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node161.html)

### [bit] bit-array &rest subscripts => bit

Access the bit-array element specified by *subscripts*.
[sbit] is similar but takes a [simple-bit-vector].

~~~
(setf b #*00100)
(bit b 2)          ;=> 1
(setf (bit b 2) 0) ;=> 0 [b => #\*00000]
~~~

### [bit-not] bit-arrray \[opt\] => resulting-bit-array

### [bit-eqv] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-and] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-andc1] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-andc2] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-nand] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-ior] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-orc1] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-orc2] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-xor] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-nor] bit-array1 bit-array2 \[opt\] => resulting-bit-array
