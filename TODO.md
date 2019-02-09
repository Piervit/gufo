CAN BE DONE QUICKLY
===================================
the 'has?' operator has been added to the low level structure -> it should be
added to the main engine.


NEEDED FOR A FIRST USABLE RELEASE
===================================

Add non-regression tests system
Character '\*' as file wildcard, '.' as curdir, '..' as upper dir, '~' as home dir.
Provide documentation.
Add standard libraries (type transformations, iterator, various utility functions).
Test advanced functionalities (structure,list, set, map, tuple).
Improve shell completion.
improve coloration.
Debug and improve the console(user interface): multiline commands and errors.
Test the module system

NEEDED FOR A STABLE RELEASE
=========================

Engine: improve performance of piped command (in exemple "c1 | c2", c2 wait c1 to finish to start.... ).
Improve coloration rules.
Provide easy configuration though a config file.
Improvement or rewriting of the type-checker.
Improvement of the error messages (precision)
Improvement of coloration in case of error.

NEEDED FOR A VERY FAR VERSION
=========================

Way to export a function coded in the console to a file ( <- might be a dynamic way to develop )

Imperative functionalities (sequel of expression)
Add inheritance system for the struct.

