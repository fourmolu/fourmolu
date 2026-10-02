* Comments are now attached to the syntax tree by position, before anything
  is printed, rather than by a cursor advanced as the printer walks the
  tree. Which element owns a comment no longer depends on the order in which
  the printer happens to visit things, so comments stop escaping the
  construct they were written in when Ormolu sorts or regroups it: a comment
  inside an import list stays there, a comment after a quasi-quote stops
  floating to the bottom of the file, and a comment attached to an import
  travels with that import when the imports are sorted. [Issue
  1074](https://github.com/tweag/ormolu/issues/1074) and [issue
  1076](https://github.com/tweag/ormolu/issues/1076).

* Haddock comments are now printed as they were written instead of being
  rebuilt from the documentation string GHC parsed out of them. A `{- | …
  -}` stays a block comment rather than becoming `--` lines, an empty `-- |`
  is no longer dropped, and a `{- *** … -}` section heading keeps its
  meaning. [Issue 641](https://github.com/tweag/ormolu/issues/641), [issue
  822](https://github.com/tweag/ormolu/issues/822), and [issue
  1159](https://github.com/tweag/ormolu/issues/1159).

  Ormolu still puts a space after a Haddock's trigger, re-indents a block
  Haddock to line up with the code it documents, and rewrites a trailing `--
  ^ X` as a leading `-- | X` when it moves the comment in front of what it
  documents.

* Backslashes are no longer added to lines in the middle of a comment block,
  where Haddock does not look for a trigger anyway. [Issue
  1131](https://github.com/tweag/ormolu/issues/1131).

* A comment written on its own line in front of an operator no longer
  strands the operator at the start of the next line. In a `do` block that
  changed what the code meant, because `$` at the beginning of a line is
  read as a new statement rather than as a continuation of the previous one.
  [Issue 1028](https://github.com/tweag/ormolu/issues/1028).

* A comment written after `=`, `->`, or a lambda arrow now stays on that
  line instead of being pushed onto the next one, and the result is
  idempotent. `f x = -- note` no longer becomes an `=` stranded on a line of
  its own. [Issue 786](https://github.com/tweag/ormolu/issues/786), [issue
  810](https://github.com/tweag/ormolu/issues/810), and [issue
  936](https://github.com/tweag/ormolu/issues/936).

* Layout decisions now take comments into account. A comment that falls
  inside a construct can no longer be squeezed into a single-line rendering
  of it.

* A comment block that trails a line of code and continues below it no
  longer drops to the start of the line, which could put the rest of the
  block outside the construct it was written in.

* A construct that brackets its contents is no longer put on one line when
  something inside it is documented with a `-- |` Haddock. Such a Haddock
  takes whole lines, so it used to swallow the closing bracket: a documented
  `deriving` clause came out as `deriving (-- | B`, and a documented field
  of a short record as `{-- | …`, which did not even parse. [Issue
  752](https://github.com/tweag/ormolu/issues/752) and [issue
  1164](https://github.com/tweag/ormolu/issues/1164).

  A `{- | … -}` Haddock is self-delimiting and does not force anything, so
  a declaration documented that way is left as it was written rather than
  being broken up: `data A = A {- | a number -} Int Bool` stays on one line
  where it used to be spread over five.

* Only pragmas in the file header are hoisted to the top of the module now.
  A `LANGUAGE` or `OPTIONS_GHC` pragma written after the first import or
  declaration stays where it is, and no longer drags the comments above it
  to the top of the file. GHC reads the header and stops, so such a pragma
  never affected compilation; moving it was giving it an effect it did not
  have. [Issue 1168](https://github.com/tweag/ormolu/issues/1168).

* A comment above a `{-# LANGUAGE A, B #-}` pragma is no longer duplicated
  when the pragma is split into one per extension; it stays with the first.
  [Issue 787](https://github.com/tweag/ormolu/issues/787).

* Ormolu now checks that the comments of the output correspond to the
  comments of the input—none dropped, duplicated, invented, or reordered—and
  refuses to format when they do not. This runs alongside the existing check
  that the AST is unchanged, is disabled by `--unsafe`, and costs nothing
  extra: the printer already records where it put each comment.
