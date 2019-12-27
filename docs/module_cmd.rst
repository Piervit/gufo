Cmd module
==========

The Cmd module is the one linked to unix command.

types:
------

cmd
***

The type cmd represent a possibly executed unix command. It as a result
(positive integer), a main output and an error output.

Fields:
^^^^^^^

res: int option
##############
The integer representing the result of an execution or None if execution is
running. (Some 0) means a succesfull execution while another number means an
error occured.

print: string
##############
The main output of a command as a string.

print_err: string
##############
The error output of a command.

Exemples:
^^^^^^^
.. code-block:: gufo

    % let $nbCmd = grep "^#PROGRAM" $file | wc -l in
      ( $Opt.get ($Int.fromString $nbCmd.Cmd.print) 0)

