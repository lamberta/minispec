# Symbol Definition

## Style Guides

* [Naming Conventions](http://www.cliki.net/Naming%20conventions)
* [Google Style Guide](http://google-styleguide.googlecode.com/svn/trunk/lispguide.xml)

## Variables

m4_include(../sections/variable.md)

## Functions

* [PCL: Functions](http://www.gigamonkeys.com/book/functions.html)

m4_include(../sections/function.md)

## Macros

The Common Lisp macro facility allows the user to define
arbitrary functions that convert certain Lisp forms into
different forms before evaluating or compiling them. This is
done at the expression level, not at the character-string
level as in most other languages. Macros are important in
the writing of good code: they make it possible to write
code that is clear and elegant at the user level but that is
converted to a more complex or more efficient internal form
for execution.

* [CLtL2: Macros](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node97.html)
* [PCL: Macros: Defining Your Own](http://www.gigamonkeys.com/book/macros-defining-your-own.html)

m4_include(../sections/macro.md)

## Packages

* [PCL: Programming in the Large: Packages and Symbols](http://www.gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html)

~~~
*package* ;=> #<Package "EXAMPLE-PACKAGE">
~~~

m4_include(../sections/packages.md)
