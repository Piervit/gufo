BUG 1

using the * pattern for file result in parsing error:
 % ls *

 This is because of a confusion with the mathematical * (for 5 * 5)


BUG 2

% let $a = ls

% if ($a.Cmd.res == (Some 0)) then ($a.Cmd.print) else ("command failed")
