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
#a simple int affectation
let $a = 5
**START**
$a

#END PROGRAM 
#PROGRAM 2
#a simple string affectation
let $b = "toto"
**START**
$b


#END PROGRAM 
#PROGRAM 3
#a simple float affectation
let $c = 1.1
**START**
$c


#END PROGRAM 
#PROGRAM 4
#a simple command affectation
let $a = ls
**START**
$a


#END PROGRAM 
#PROGRAM 5
#a simple boolean affectation
let $a = true
**START**
$a

#END PROGRAM 

#PROGRAM 6
#a simple int affectation with direct use
**START**
let $a = 5 in ($a)


#END PROGRAM 

#PROGRAM 7
#a simple string affectation with direct use
**START**
let $a = "toto" in ($a)
#END PROGRAM 


#PROGRAM 8
#a simple float affectation with direct use
**START**
let $a = 5.3 in ($a)
#END PROGRAM 

#PROGRAM 9
#a simple cmd affectation with direct use
**START**
let $a = ls in ($a)
#END PROGRAM 

#PROGRAM 10
#a simple boolean affectation with direct use
**START**
let $a = true in ($a)
#END PROGRAM 


