String module
==========

The String modules defines functions helper for strings.

variables:
----------

asList: string -> list string
******

Split each line of the given string into a list.

Exemples:
^^^^^^^
.. code-block:: gufo

    % let $res = $String.asList (let $cmd = ls -1 in( $cmd.Cmd.print ))
    
    % $res
     [ file1.txt , file2.txt , file3.txt ]
    



