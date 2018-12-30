Getting started
===============

Lets try
--------

Gufo is both a language with its own syntax and a shell system.

It tries to do a lot of pre-execution controls to only allow valid execution.

It uses the '%' symbol as a start of prompt.

To start, just type '1 + 1' ::

    % 1 + 1
    2

The result '2' appears, showing than everything went fine.

You can do a second trivial operation '1.4 + 1.6', you will not be surprized
much by the result::

    % 1.4 + 1.6
    3.000000

You can also call an external program, such as 'ls' or 'pwd' (or every available external program)::

    % pwd
    /home/fpierre/Gufo
    
    % ls
    _build  configure  docs  exemples  guforun.native  LICENSE  Makefile  _oasis  opam  README.md  setup.data  setup.log  setup.ml  src  testKeyboard  THANKS.md  TODO.md
    
If you type something invalid, you will be warned before the executions of the command::

    % 1 + "toto"
    Expression do not have a common type: int  and: string

You can also use variables::

    % let $a = 5
    
    % let $b = $a * 10
    
    % $b
    50

Functionnality overwiew
-----------------------
  * Inference type system and variable coloration depending of type.
  * Pre-execution alerts.
  * completion (using 'tab' key).
  * variables and functions in a fonctional way.
  * history (use arrows key to find previous expression).
  * research (using CTRL + R).
