# Migrating from old reflex-platform

Reflex-platform has been moved over to haskell.nix this changes a few things which should be covered here. If we missed something please make an issue!

## Getting all of your current package versions

Drop into a shell to provide you access to your cabal command and run:
```bash
cabal v2-freeze
```
This will spit out a <name>.cabal.freeze that will pin all of your package versions. Move this to somewhere safe because we'll need it later!


## Making a cabal.project and extending it

*NOTE**: cabal.project documentation is available at https://cabal.readthedocs.io/en/3.4/cabal-project.html

Create a cabal.project boiler-plate with
```cabal
packages: .
```

now you can add a field `constraints` that provides all of your previous package versions

```cabal
constraints: <everything from cabal freeze>
```

## Modifying your nix code

### Overrides
Overrides are no longer available via nixpkgs overlays, the new interface for overrides is the nixos module system. This is described in the [haskell.nix documentation](https://input-output-hk.github.io/haskell.nix/reference/library.html?highlight=modules#modules)

an example override is:
```nix
[ 
 # Turn on verbose linker logging on when building the executable of "reflex-todomvc"
 ({ config, pkgs, lib, ... }: { packages.reflex-todomvc.components.exes.reflex-todomvc.configureFlags = [ "--ld-option=-v" ]; })
]
```

### Setting up your nix files
The nix interface has changed and requires you to rewrite your nix files, a simple boilerplate is defined below

```nix
{
  system ? builtins.currentSystem,
  project ? import <reflex-platform thunk> {
    inherit system;
    android_sdk_accept_license = true;
    allowUnfree = true;
  },

  # Optional, reflex-platform provides some nix-thunk functions
  nix-thunk ? import <nix-thunk thunk> {}
}:
project ({ pkgs, thunkSource }: {
    name = "<project-name>";
    src = ./.;
    compiler-nix-name = "<compiler, covered below>";
    ghcjs-compiler-nix-name = "<compiler, covered below>";
    shells = ps: with ps; [
        <name>
    ];
    overrides = [ ]; # Covered above
})
```

### Selecting your compiler
We no longer have a set flag that enables or disables certain compiler versions for your build. The new way to set this is via `compiler-nix-name`.

```nix
project ({pkgs, thunkSource}{
    compiler-nix-name = "ghc8107Splices";
})
```

We also provide an attribute to set the ghcjs compiler version (`ghcjs-compiler-nix-name`), you can do this via:
```nix
project ({ pkgs, thunkSource }: {
    compiler-nix-name = "ghc8107Splices";
    ghcjs-compiler-nix-name = "ghcjs8107JSString";
})
```


