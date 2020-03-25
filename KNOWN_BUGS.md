This file track potential bugs and issues of Gufo.
Information here might be transfered to github issues.

#### BUG 8 ####

This is not sure it is really a bug, but a strong interogation

% let $echofun $a = echo ("hello " + $a)
% $echofun "Alain"; echo "toto"

#### BUG 10 ####

Maybe for later:
gufoModules.ml:72 TODO: can raise bugs.

#### BUG 11 ####

"oo" > "pp"

#### BUG 13 ####

Try tiping:
git commit _oasis

#### BUG 15 ####
echo ( $List.fold_left (fun $acc $el  -> $acc ^ $el )  )

#### BUG 16 ####
let _ = echo $str in (()) )

#### BUG 17 ####
let $f $a $b = $a + $b

($f 5) 6

#### BUG 18 ####
(priorité d'opérateur)

1 + 1 ;; 3
4

