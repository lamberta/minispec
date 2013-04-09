## Tree

Lists of lists.

* [PCL: Beyond Lists: Other Uses for Cons Cells](http://www.gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html#trees)

### [tree-equal]

Compares two trees, equal if structure is same shape and
leaves are eql (or :test).

### [copy-tree] tree => new-tree

Creates a copy of a *tree* of conses.

### [subst] new old tree &key key test test-not => new-tree

Make a copy of *tree* with each subtree or leaf matching
*old* replaced by *new*, the tree counterpart to
[substitute]. Functional variants are [subst-if] and
[subst-if-not], called on each atomic value in the tree. The
destructive version is [nsubst] and variants.

~~~
(subst "two" 2 '(1 (1 2) (1 (2 3)))) ;=> (1 (1 "two") (1 ("two" 3)))
(subst-if "one" #'(lambda (x) (equal x 1)) '(1 2 (3 2 1))) ;=> ("one" 2 (3 2 "one")))
~~~

### [sublis] alist tree &key key test test-not => new-tree

Make a tree copy and substitute multiple leaves using an *alist*.

~~~
(sublis '((1 . "one") (2 . "two")) '(1 (1 2) (1 (2 3))))
  ;=> ("one" ("one" "two") ("one" ("two" 3))))
~~~

### [alexandria:flatten] tree => list

Collect non-null leaves into a [list].

~~~
(flatten '(1 2 (3 2 1) ((1 1 nil) (2 2)))) ;=> (1 2 3 2 1 1 1 2 2)
~~~

### [alexandria:circular-tree-p] object => boolean

Test if *object* is a circular *tree*.
