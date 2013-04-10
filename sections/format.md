## Formatted Output

* [HyperSpec: Formatted Output](http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm)
* [CLtL2: Formatted Output to Character Streams](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node200.html)
* [PCL: A Few FORMAT Recipes](http://www.gigamonkeys.com/book/a-few-format-recipes.html)

### [format] destination control-string args\* => result

Creates formatted output using the *control-string* and
observing tilde indicated directives. If *destination* is a
[string], a [stream], or [t], then the *result* is
[nil]. Otherwise, the *result* is a [string] containing the
formatted output.

~~~
(format nil "Hello, ~a" 'lisp) ;=> "Hello, LISP!"
~~~

#### Basic Directives:

* [`~A`] ---Aesthetic. Print without escape characters ([princ]).
* [`~S`] ---Standard. Print with escape characters ([prin1]).
* [`~%`] ---Newline character. `~n%` outputs *n* newlines.

#### Number Directives:

* [`~D`] ---Decimal.
* [`~B`] ---Binary. Prints radix 2.
* [`~O`] ---Octal. Prints radix 8.
* [`~X`] ---Hexadecimal. Prints radix 16.
* [`~R`] ---Radix. `~nR` prints in radix *n*.
* [`~F`] ---Float. `~width,count,scale,overflowchar,padcharF`
* [`~E`] ---Exponential notation. `~width,count,scale,overflowchar,padchar,exptcharE.`
* [`~G`] ---General floating-point. `~width,count,scale,overflowchar,padchar,exptcharG.`
* [`~$`] ---Dollars. `~count,min-count,min-width,padchar$`

The `@` modifer causes the sign to always print. `:` adds
commas between groups of digits. The most general form of
`~D` is `~mincol,padchar,commachar,comma-intervalD.`

#### Formatting Directives:

* [`~W`] ---Write. Obey every printer-control variable ([write]).
* [`~C`] ---Character.
* [`~~`] ---Escape tilde character.
* [`~&`] ---Fresh line.
* [`~<newline>`] ---Ignores the newline and any whitespace that follows.
* [`~_`] ---Conditional newline.
* [`~|`] ---Page separator character.
* [`~T`] ---Tabulate.
* [`~I`] ---Indent.

#### Control Directives:

* [`~:>`] ---Logical Block.
* [`~/`] ---Call function.
* [`~<`] ---Justification.
* [`~>`] ---End justifcation.
* [`~*`] ---Go-To.
* [`~[`] ---Conditional expression.
* [`~]`] ---End conditional expression.
* [`~{`] ---Iteration.
* [`~}`] ---End iteration.
* [`~?`] ---Recursive processing.
* [`~(`] ---Case conversion.
* [`~)`] ---End case conversion.
* [`~P`] ---Plural.
* [`~;`] ---Clause separator.
* [`~^`] ---Escape upward.

[`~C`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_caa.htm
[`~%`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cab.htm
[`~&`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cac.htm
[`~|`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cad.htm
[`~~`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cae.htm
[`~R`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cba.htm
[`~D`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbb.htm
[`~B`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbc.htm
[`~O`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbd.htm
[`~X`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbe.htm
[`~F`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cca.htm
[`~E`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccb.htm
[`~G`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccc.htm
[`~$`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccd.htm
[`~A`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cda.htm
[`~S`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cdb.htm
[`~W`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cdc.htm
[`~_`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cea.htm
[`~:>`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ceb.htm
[`~I`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cec.htm
[`~/`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ced.htm
[`~T`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfa.htm
[`~<`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfb.htm
[`~>`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfc.htm
[`~*`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cga.htm
[`~[`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgb.htm
[`~]`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgc.htm
[`~{`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgd.htm
[`~}`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cge.htm
[`~?`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgf.htm
[`~(`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cha.htm
[`~)`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_chb.htm
[`~P`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_chc.htm
[`~;`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cia.htm
[`~^`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cib.htm
[`~<newline>`]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cic.htm
