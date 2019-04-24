#
# This file is part of Gufo.
#
#   Gufo is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   Gufo is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with Gufo. If not, see <http://www.gnu.org/licenses/>. 
#
#   Author: Pierre Vittet
#

# This file contains multiples valid programs.
# Valid programs are separated by "#PROGRAM comments".
# They are dedicated to tests non-regressions in Gufo: if one program is not
# recognize as valid, this means a regression occured.

#PROGRAM 1
**START**
echo "toto"

#END PROGRAM 
#PROGRAM 2
# a simple cmd
**START**
ls

#END PROGRAM 
#PROGRAM 3
#a simple int affectation

let $a = 5

#END PROGRAM 
#PROGRAM 4
#a simple string affectation
let $b = "toto"
#END PROGRAM 
#PROGRAM 5
#a simple float affectation
let $c = 1.1

#END PROGRAM 
#PROGRAM 6
#a simple command affectation
let $a = ls

#END PROGRAM 
#PROGRAM 7
#The factoriel function (recursive)
let $factoriel $c = if ($c != 1) then ($factoriel ($c - 1)) * $c else 1
#END PROGRAM 

#PROGRAM 8
#The factoriel function (recursive) with direct let - in usage 
**START**
let $factoriel $c = if ($c != 1) then ($factoriel ($c - 1)) * $c else 1 in ($factoriel 5)

#END PROGRAM 

#PROGRAM 9
#a call ssh function
let $ssh $user $addr = ssh ($user + "@" + $addr)

#END PROGRAM 
#PROGRAM 10
#a call ssh function
**START**
let $checkset $aset $aval = $aset has? $aval in ($checkset -< 1 >- 1)

#END PROGRAM 

