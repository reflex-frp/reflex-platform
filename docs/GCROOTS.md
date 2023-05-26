Reflex Platform gc-roots
========================

When the `try-reflex` script is run, it will add a symbolic link to this directory creating a garbage collection root for the nix store.  This will prevent `nix-collect-garbage` from deleting the try-reflex environment.  To allow the garbage collector to delete the environment, first delete the contents of this directory (except this file), then run `nix-collect-garbage`.
