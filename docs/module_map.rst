Map module
==========

The Map modules defines functions helper for maps.

variables:
----------

cardinal : map ('v) -> int 
**************************

cardinal aset : return the number of elements in the set.

Exemple:
^^^^^^^
.. code-block:: gufo

    % let $myMap = -< 1: "Tintin" , 2: "Haddock" >-
    % $Map.cardinal $myMap
    2
 

is_in: map ('k, 'v) -> 'k -> bool
********************************

is_in map key: return True if the key given as second argument exist in the map
given as first argument. Else return False.

Exemples:
^^^^^^^
.. code-block:: gufo

    % let $myMap = -< 1: "Tintin" , 2: "Haddock" >-
    % $Map.is_in $myMap 3
    False 
    % $Map.is_in $myMap 2
    True
 
get : map ('k, 'v) -> 'k -> option 'v
*************************************

get m k: return the optionnal value at position k in map m.

Exemples:
^^^^^^^
.. code-block:: gufo

    % let $myMap = -< 1: "Tintin" , 2: "Haddock" >-
    % $Map.get $myMap 2
    Some ("Haddock")
    % $Map.get $myMap 3
    None
 
   
union: map ('k, 'v) -> map ('k, 'v) -> map ('k, 'v)
***************************************************

union m1 m2 do the union of map m1 and m2. If both map contains an entry for
the same key, the value of the second map is associated to it.

Exemples:
^^^^^^^
.. code-block:: gufo

    % $Map.union -< 1: "Camembert" , 2: "Beaufort">- -< 0: "Reblochon", 1: "Comté" >-
    -< 0 : Reblochon , 1 : Comté  , 2 : Beaufort >-

add: map ('k, 'v) -> 'k -> 'v
*****************************

add m k v: add the value v at position k to map m.

Exemples:
^^^^^^^
.. code-block:: gufo

    % let $myMap = -< 1 : "toto", 2 : "tata" >-
    % $Map.add $myMap 3 "titi"
    -< 1 : toto , 2 : tata  , 3 : titi >-

rm: map ('k, 'v) -> 'k 
**********************

rm m k : remove key k and its associated value from map m.

Exemples:
^^^^^^^
.. code-block:: gufo

    % let $myMap = -< 1 : "toto", 2 : "tata" >-
    % $Map.rm $myMap 1 
    -< 2 : tata  >-
