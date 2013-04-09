# Common Lisp TurboSpec

This is a *Work In Progress*; don't expect much, yet.

The *TurboSpec* started out as a cheat-sheet and then evolved
into something more. The project's goal is to help Common
Lisp programmers quickly find what they need. To that end,
its focus is on navigation and usability, using
example-driven documentation paired with concise
descriptions and readily available links to more exhaustive
references.

## Structure

* `sections` ---Groups of functions and symbols documented
  in [Markdown](http://daringfireball.net/projects/markdown/)
  These files should be split up so that each symbol is listed
  in only one section---and documented once.
* `pages` ---A documentation page navigable from the
  website. A page may contain *sections*, and a section may
  appear in multiple pages.
* `build` ---Contains build scripts, HTML templates, and
  reference links used to generate the documentation.
* `html` ---The output directory of the generated documentation.

## Build

Building a copy of the documentation requires
[Pandoc](http://johnmacfarlane.net/pandoc/) and
[M4](http://www.gnu.org/software/m4/). [PhantomJS](http://phantomjs.org/)
is required to generate all the reference links.

~~~
$ make
~~~

To re-generate all of the supporting reference links before building:

~~~
$ make all
~~~

## Style Guide

    # Page Level Heading
    
    ## Section Level Heading
    
    Description of the section.
    
    * [Supporting Link 1](http://example.com/section1)
    * [Supporting Link 2](http://example.com/section2)
    
    ### [fn-name] arg \[optional-arg\] many-args\* => return-type
    
    Short description. Links to a valid [fn-name] are resolved
    through external reference links. Here's a `code snippet` in
    the description.
    
    ~~~
    (fn-name 'a '(b c) 'd) ;=> (A B C D) [prints B, C]
    ~~~
