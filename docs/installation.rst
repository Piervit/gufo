Installation
============

Gufo only works on Linux system.

This is a project written in OCaml,

The project is in early stage, please be benevolent with the installation (and
with the rest).

With opam
---------

When a first official release will be ready, it will be installable though opam::

    opam install gufo

From source
-----------

You can get the sources from https://github.com/piervit/gufo.

You should have a recent ocaml (4.05.0 or higher).

You should have to following dependancies installed:

* camomile,
* react,
* lwt,
* zed,
* lambda-term,
* str,
* unix,
* sedlex,
* menhir,
* menhirLib

Then it might be possible to build the project::

    make

It will create an executable guforun.native wich you can run to launch gufo.
You can then create a gufo executable known to your system by copying it into a
path directory, such as::

    cp ./guforun.native /usr/bin/gufo
