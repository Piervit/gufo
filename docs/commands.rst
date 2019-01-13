Commands
========

basic usage
-----------

Commands are calls to externals programs.
Here is a simple command::

    % pwd
    /home/fpierre/gufo

The command have a similar syntax than the bash one. For exemple commands, can
be sequencialized::

    % pwd; echo "hello"
    /home/fpierre/gufo
    hello

They can be piped (well, the current implementation is weak, but a threaded
strong one is in the TODO list.)::

    % ls | wc -l
    11

Gufo also support redirection similarly to bash::

    % echo "result" >  file
    
    % Mail -s "Subject" to-address < tmpfile

within variables
----------------

You can apply the result within a variable::

    % let $a = ls

In this case, the command will be immediatly played but the output will not be
displayed on screen but stored within the variable.

The variable type is 'cmd', this type is basically just a struct having three fields:
  * res: the integer returned by the command.
  * print: a string containing the standard output of the command.
  * print_err: a string containing the error output of the command.

playing with the variable::

    % let $a = ls
    % if ($a.Cmd.res == 0) then ($a.Cmd.print) else (echo "command failed")
    
within function
---------------

Of course, if a command call occurs within a function, it will be executed on
the function calls (even if 'functionnaly', it could have been runned earlier).
You can have a look at this ssh function::

    % let $ssh $user $addr = ssh ($user + "@" + $addr)
    
Or at this one::
    % let $ssh $user $addr = ssh ($user + "@" + $addr)
