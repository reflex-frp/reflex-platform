Try Reflex GC Roots
===================

When the `try-reflex` script is run, it will add a symbolic link in
this directory, creating a garbage collection root for the Nix
store. Nix commmands, by default, creates roots inside the Nix store
(/nix/store/). Creating a symlink outside the Nix store prevents the
build results from being garbage collected by
`nix-collect-garbage`. To explicitly allow the garbage collector to
delete the environment, first delete the contents of this directory
(except this file), then run `nix-collect-garbage`.
