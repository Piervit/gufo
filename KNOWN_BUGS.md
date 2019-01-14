BUG 1

using the * pattern for file result in parsing error:
 % ls *

 This is because of a confusion with the mathematical * (for 5 * 5)


BUG 2: a type issue

% let $a = ls

% if ($a.Cmd.res == (Some 0)) then ($a.Cmd.print) else ("command failed")

BUG 3: Invalid syntax:

let $myList = [ 1, 2 ] in ( $myList[0] )

#For information, the valid syntax when getting a list elements:
let $myList = [ 1, 2 ] in ( $myList.[0] )
