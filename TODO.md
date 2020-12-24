CAN BE DONE QUICKLY
===================================

NEEDED FOR A FIRST USABLE RELEASE
===================================

Provide documentation.
Provide logical operator (or , and)
Add standard libraries (type transformations, iterator, various utility functions).
Add localisation for errors

NEEDED FOR A STABLE RELEASE
=========================

Engine: improve performance of piped command (in exemple "c1 | c2", c2 wait c1 to finish to start.... ).
Improve coloration rules.
Provide line of the errors found during the typechecking 
Provide easy configuration though a config file.
Improvement or rewriting of the type-checker.
Improvement of the error messages (precision)
Improvement of coloration in case of error.

NEEDED FOR A VERY FAR VERSION
=========================

Way to export a function coded in the console to a file ( <- might be a dynamic way to develop )

Imperative functionalities (sequel of expression)
Add inheritance system for the struct.
We could have a + operand over cmd.
  echo "toto" + echo "tata"
  the cmd code will be 0 if both command code are 0
  The cmd string is the concatenation of the two commands.
