Set module
==========

The Set modules defines functions helper for sets.

variables:
----------

is_in: set ('v) -> 'v -> bool
********************************

is_in set value: return True if the value given as second argument exist in the
set given as first argument. Else return False.

Exemple:
^^^^^^^
.. code-block:: gufo

    % let $mySet = -< "Tintin" , "Haddock" >-
    % $Map.is_in $myMap "Castafiore"
    False 
    % $Map.is_in $myMap "Haddock"
    True
    
union: set ('v) -> set ('v) -> set ('v)
***************************************************

union s1 s2 do the union of set s1 and s2. 

Exemple:
^^^^^^^
.. code-block:: gufo

    % let $mySet = -< 1, 2, 3 >-

    % $Set.union $mySet -< 3, 4, 5 >-
    -< 1 , 2  , 3  , 4  , 5 >-

add: set ('v) -> 'v -> set ('v)
*******************************

add s v: add the value v to set s.

Exemple:
^^^^^^^
.. code-block:: gufo

    % let $mySet = $Set.add -< 1, 2, 3 >- 4
    -< 1 , 2  , 3  , 4  >-

rm: set ('v) -> 'v -> set ('v)
*******************************

rm s v: If v is a value of s, return the set s without v, else return s. 

Exemple:
^^^^^^^
.. code-block:: gufo

    % let $mySet = $Set.rm -< 1, 2, 3 >- 2
    -< 1 , 3   >-


