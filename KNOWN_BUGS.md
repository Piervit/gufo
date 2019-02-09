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


BUG 4: GufoEngine assert false

let $myList = [ 1 , 2] in (let $myList = $myList with 3)

#We can still add an element to the list:
let $myList = [ 1 , 2] in (let $myList = $myList with [3])

BUG 5: 

% let $inta = 1

% let $intb = 2

% $inta with $intb
Fatal error: exception File "src/gufoEngine.ml", line 1053, characters 9-15: Assertion failed
[fpierre@localhost Gufo]$
 ----> Look in gufoParsedToOpt.ml, function determine_type_basic_fun (for with and wouth operators)

BUG 6:
% let $checkset $aset $aval = $aset has? $aval

% let $myset = -< 1 >-

% let $mymap = -< 1 : "toto" >-

% $checkset $myset 1

% $checkset $myset "toto"

BUG 7:
let $a = $Int.toString

BUG 8: this is not really a bug, but a strong interogation

% let $echofun $a = echo ("hello " + $a)
% $echofun "Alain"; echo "toto"


