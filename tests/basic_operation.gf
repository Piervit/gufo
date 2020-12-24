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
if ((1 * 10) == 10 ) then (true) else (assert_false)

#END PROGRAM 

#PROGRAM 2
**START**
if ((1 + 2 + 3 + 4 + 5 ) == 15 ) then (true) else (assert_false)

#END PROGRAM 


#PROGRAM 3
**START**
if ((1.0 *. 13.0) == 13.0 ) then (true) else (assert_false)
#END PROGRAM 

#PROGRAM 4
**START**
if ((1.0 +. 4.0 %. 2.0) == 3.0 ) then (true) else (assert_false)
#END PROGRAM 

#PROGRAM 5
**START**
if ( (1 ;; 2 )   == 2 ) then (true) else (assert_false)
#END PROGRAM 

#PROGRAM 6
**START**
if ( ("1" ;; 4 )   == 4 ) then (true) else (assert_false)
#END PROGRAM 

#PROGRAM 7
**START**
if ( ( 5 * 2 % 5 )   == 2 ) then (true) else (assert_false)
#END PROGRAM 
