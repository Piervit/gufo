Syntax
======

Comments
--------
A comment start with a '#' and finish with an end of line::
    % # this is a comment

Variables
---------
A variable is defined by the keyword 'let' and its name always start with a $.

There is two kind of variables:
Toplevel variables
------------------
toplevel variables: they are directly declared without beiing within a function.
For exemple::
    % let $topv = 5

  We here have a toplevel variable usable everywhere in the code by calling it::
    % topv
      5

Scoped variables
---------------

A scoped variable is limited to a given perimeters.::
    % let $scopedv = 5 in ($scopedv + 4)
    9

The variable only exist in the in (...) code and is not reachable elsewhere.

In a function only scoped variables can be used.

Condition
---------
Condition can be expressed like this::
    % if (boolean_expr) then (value_when_true) else (value_when_false)


Instructions sequence
---------------------
You can do instruction sequence using ";;"::
    % (instr1) ;; (instr2)

Take care that in this case, instr1 will be only usefull for its side-effects.
