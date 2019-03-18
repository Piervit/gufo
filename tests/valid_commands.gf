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
**START**
echo toto


#END PROGRAM 

#PROGRAM 3
#a sequence of commands
**START**

echo "toto"; echo titi; ls

#END PROGRAM 

#PROGRAM 4
#file redirection
**START**

cat toto > tmp


#END PROGRAM 

#PROGRAM 5
#pipe
**START**

echo "poney" | sed "s/e/a/g"


#END PROGRAM 


#PROGRAM 6
#file redirection
**START**

sed "s/e/a/g" < tmp


#END PROGRAM 

#PROGRAM 7
#file redirection
**START**

(echo "toto") ;; (echo "tata"; echo "titi") 

#END PROGRAM 


