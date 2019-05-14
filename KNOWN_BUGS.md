
BUG 6:
let $checkset $aset $aval = $aset has? $aval in $checkset -< 1 -> "toto"

BUG 7:
let $a = $Int.toString

BUG 8: this is not really a bug, but a strong interogation

% let $echofun $a = echo ("hello " + $a)
% $echofun "Alain"; echo "toto"

BUG 9:

% let $afunction $i = $i
% $afunction 3 4

BUG 10:

Maybe for later:
gufoModules.ml:72 TODO: can raise bugs.

BUG 11:

"oo" > "pp"

BUG 12:

let $a = $Opt.get ($Int.fromString $x) "23" 

Selon que la chaine $x soit convertible en int ou non, $a sera de type int ou string -> il devrait y avoir une erreur de type.
