You need to install oasis:
  opam install oasis

We retrieve the project depedancy using opam: 

  opam pin add gufo . --no-action
  opam install gufo --deps-only

Then just run make:
