ghcjs-setup
===========

```bash
git clone --recursive git@github.com:ryantrinkle/try-reflex
cd try-reflex
./try-reflex
```

This will install the Nix package manager, grab the ghcjs package information, build everything, then give you access to a sandbox with ghcjs and node.js available.  The first time you run it, the build process will take a long time - up to several hours.  After that, it should only take a few moments.

You will need sudo access in order to install Nix (unless you are on NixOS).  However, you should run the script as yourself, not root - it will prompt for your password when necessary.
