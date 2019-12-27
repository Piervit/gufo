Types
===============


Gufo has a strict type system and do not allow implicit conversion. For exemple we cannot add a string to an integer.

Gufo’s types are implicitely infered.

basic types:
------------

  * int 
  * string
  * bool : standard true/false boolean
  * float
  * cmd: this is a special types representing the application of an external program. This is a basic type but owning field attributes.

Aggregation types:
------------------

  * list
  * set : no order, no duplicated value.
  * map : a map is a set of key values binded to values.
  * tuple: a tuple is a sequence of value with potentially different types.

List, set and maps can only contains element of same type.

How to use a list
-----------------

A list is a structure where the new elements are added at the end of the
elements already present.

List creation::

    % let $myList = [ 1 , 2 ]

This create a list of 2 elements: 1 and then 2.

Access an element::

    % $myList.[0] #Acess the first element

Concatenate a list with another one::

    % let $myList = $myList with [ 3, 4 ]

So if you want to add only a single element::
    
    % let $myList = $myList with [ 3 ]

You can use the list module for a deeper uses (length, iterator, map...)::

    % $List.length $myList 
    5

How to use a set
-----------------

A set contains elements without order. For a set of type T, a value T is
present or absent from it. It cannot be present several times.

Similarly to a list::

    % let $mySet = -< 1 , 2 >-

Check if an element is in a set::

    % $Set.is_in $mySet 1
    True

Concatenate a set with another one::

    % let $mySet = $Set.union $mySet -< 3, 4 >-

So if you want to add only a single element::
    
    % let $mySet = $mySet with [ 5 ]

You can use the set module for a deeper uses (length, iterator ...)::

    % $Set.length $mySet 
    5

How to use a map
----------------

A map is a set of key, with each key element beiing mapped to a given value.
Each key have same type, and each value have same type.

Similarly to a list::

    % let $myMap = -< 1: "Toto" , 2 : "Tata" >-

Check if an element has a key in the map::

    % $Map.is_in $myMap 1
    True

Concatenate a list with another one::

    % let $myMap = $myMap with -< 3: "Titi", 4 : "Tutu">-

So if you want to add only a single element::
    
    % let $myMap = $myMap with -< 3: "Tyty" >-

You can use the list module for a deeper uses (length, iterator, map...)::

    % $Map.length $myMap 
    5

How to use a tuple
-----------------
A tuple is a sequence of value with potentially different types. The separation
character for a tuple is "--". This is mainly used as a shortcut to variable
declaration::

    % let $a -- $b = 3 -- "Hello" 

Allows to declare in a single line two variables, $a with value 3 and $b with
value "Hello".

    
  

Struct type
-----------

Gufo has ‘struct’ composite types: a struct defines a type with determined and statically typed fields.
Operators




Operators
---------

Gufo has predefined operators: every operator takes two arguments, here denoted by ‘a’ and ‘b’:

  * a + b : with a and b as int (float) represents the addition of two integers(floats).
  * a + b : with a and b as string represents the string concatenation operation.
   
  * a – b : with a and b as int (float) represents the soustraction of two int (float)
   
  * a * b : with a and b as int (float) represents the multiplication of two int (float)
   
  * a / b : with a and b as int (float) represents the division of two int (float)
   
  * a with b : with a and b a list of elements A represent the list concatenation.
   
  * a with b : with a and b a set of elements A represent the set union.
   
  * a with b : with a and b a of elements A as key and B as values represent a map merge (a union of the key set). if a key is present in both, the binded value becomes the one from b.
   
  * a with b : with a a list of elements A and b an element of type A, return the list a with b added at the head of the list.
   
  * a with b : with a a set of elements A and b an element of type A, return the set a union the singleton set containing b.
  
  * a wout b : with a and b a set of elements A, return the set a without the elements from b.
   
  * a wout b : with a and b a map of elements A as key and B as values, return the map a without the elements from b.

The types also contains standard libraries functions accessible though system
modules: for exemple $Int.toStr is the function taking an integer argument and
returning the corresding value as a string.

