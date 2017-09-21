# Little model finder for much simplified iota formulas

## Comparison to iota

We only have presence/absence of nodes (no links, parents, labels).

## Overview

`./run sim [test]` to compile&execute the program.
No arguments gives an interactive mode, "test" shows some interesting
test formulas.

The models given by the program respect
  * Minimality w.r.t. actions
* Non-Asimov implication (`=>`)


## About the implication =>

An implication F ⇒ G classically means: _F is false or G is true_. 

Here, it (informally) means that :

* if F is false, iota does nothing
* if F is true, 
  - iota is allowed to add actions that make G true
  - if G can't become true by adding actions, iota is NOT allowed to add actions
  that make F false.

The still informal but less so definition is as follows:

Given a formula F, we write F[g,α] for: _The graph g and actions α satisfy F._
Now iota accepts a model (g,α) of F iff (g,α) satisfies the formula

    F[a] ∧ (¬∃ β<α. F{α,β})

  where < is the action order, and

    a {α,β}      = a[g,α] for any atom a
    ~F {a,β}     = not (F{a,β})
    F ∧ G {α,β}  = F{α,β} and F{α,β}
    F ∨ G {α,β}  = F{α,β} or F{α,β}
    F ⇒ G {α,β} = if F[g,α] and F[g,β] then F{α,β}

Note: this turns out to be a slight generalisation of what's known as
the FLP semantics.


## Usage example and tutorial

    $~/miniota/./run sim
    > ~a ^ ~b # (~a => b)
      Pre   : (~a ^ ~b)
      Constr: (~a => b)

      a   b  #   actions
      ----------------
      ~a ~b  #  +b

The line starting with `> ` is given by the user. On the left of `#`, we describe the
preconditions.  `a` means _`a` is present_, while `~a` means _`a is absent`_. The
preconditions can be omitted together with `#`, in which case we assume that the
precondition is just "true".

On the right of `#`, we give constraints. By default, atoms (`a`, `b`, etc) refer to
nodes in the *postcondition*. You can reference the status of a node in the
precondition by by prepending the character ', e.g. `'a ^ c => b` means _If `a`
is present initially and `c` is present at the end, then `b` is present at the
end_.

Quoted atoms (`'a`, `'b`, etc) cannot be used in the precondition (i.e. before `#`)

In the example above, the precondition is: _Neither `a` nor `b` are present
initially_. The postcondition is: _If `a` is not present at the end, then `b` is._

The lines after `> ` are printed by the program. 

The first 2 restate the preconditions and constraints as understood by the
parser.

Then all possible models are given. 

On the left of `#`, we have the preconditions. An
atom in green means _the atom is present_; in red means _the atom is absent_.

If the atom is not printed on the line, that means its presence does not
matter.

On the right of `#`, the actions are given. `+a` means _add `a`_, `-a` means _remove
`a`_.

In the example above, the special meaning of `=>` only leaves one option: add `b`.
Compare to

    > ~a ^ ~b # (a v b)
      Pre   : (~a ^ ~b)
      Constr: (a v b)

      a b  #   actions
      ----------------
      a b  #  +a
      a b  #  +b

Usually, `~a => b` is logically equivalent to `a v b`, but here it isn't: the
`a v b` case allows us to add `a`, while in the case of `~a => b`, `+a` isn't
allowed since this would be adding an action just to make `~a` false.
