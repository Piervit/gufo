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

List, set and maps can only contains element of same type.

How to use a list
-----------------

List creation::

    % let $myList = [ 1 , 2 ]

This create a list of 2 elements: 1 and then 2.

Access an element::

    % $myList.[0] #Acess the first element

Concatenate a list with another one::

    % let $myList = $myList with [ 3, 4 ]

So if you want to add only a single element::
    
    % let $myList = $myList with [ 3 ]

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

