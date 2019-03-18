Function
========
A function is a variable which take parameters. 

A function is declared as a variable but followed by its arguments::
    % let $helloFun $name = echo ("hello " + $name)

$helloFun is a function, $name is the argument taken by function.


A function is used by calling it with argument::
    % $helloFun "Bob"
    hello Bob

A function can be partially "called"::
    % let $add $a $b = $a + $b  #declaration of a $add function which take 2 arguments
    % let $addOne = $add 1      #declaration of $addOne function which take one argument, using a partial call to $add
    % $addOne 5                 #application of the addOne function which return the value 6.
    6

A call is effectively executed once it become a "total" call.

Functions can be recursive::
    % let $factoriel $c = if ($c != 1) then ($factoriel ($c - 1)) * $c else 1 in ($factoriel 5)

