{ system ? builtins.currentSystem
, config ? {}
}:
let reflex-platform = import ../../. { inherit system config; };
    projSrc = reflex-platform.hackGet ./reflex-project-skeleton;
    proj = import projSrc { inherit reflex-platform; };
in proj
