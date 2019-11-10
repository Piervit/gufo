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

# This file contains multiples unvalid programs.
#Pprograms are separated by "#PROGRAM comments".
# They are dedicated to tests non-regressions in Gufo: Theses programs are
#considered as invalid and the gufo compiler should thread them as invalid.



**START**
#PROGRAM 1
#this should fail as $a is not previously defined.
let $a = $a + 1 

