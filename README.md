# The Common Lisp MiniSpec

This is a *Work in Progress* that still requires much
filling-out. Contributing is easy and would be appreciated.

The *MiniSpec* started out as a cheat-sheet that evolved
into something more. The project's goal is to help Common
Lisp programmers quickly find what they need. To that end,
its focus is on navigation and usability, using
example-driven documentation paired with concise
descriptions and readily available links to more exhaustive
references.

## Project Structure

* `sections` ---Collections of symbol defintions documented
  in [Markdown](http://daringfireball.net/projects/markdown/).
  These files are split up so that each definition is only
  listed once (hopefully).
* `pages` ---Pages navigable from the website. These can be
  built out of *sections* which can appear in multiple pages.
* `build` ---Build scripts, HTML templates, and
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

## Frontend Dependencies

* [jQuery](http://jquery.com/)
* [Bootstrap](http://twitter.github.io/bootstrap/)
* [Highlight-Lisp](https://github.com/orthecreedence/highlight-lisp)
* [Typeahead.js](https://github.com/twitter/typeahead.js)

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
