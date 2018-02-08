# Editor setup

## Spacemacs

The following features are known to work in Emacs /
[Spacemacs](https://github.com/syl20bnr/spacemacs) for developing Reflex
applications structured around reflex-platform's
[project](project-development.md) setup:

1. Syntax highlighting
1. GHCi repl
1. Error navigation

Firstly, Emacs must be launched from the GHC `nix-shell` so that it knows where
to find cabal and ghci:

```
nix-shell -A shells.ghc
> emacs
```

Now add [the haskell
layer](https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/haskell#install)
to your `.spacemacs`, and then configure `haskell-mode` to use `cabal new-repl`
by adding the following to the `dotspacemacs/user-init` function of your
`.spacemacs` file:

```
(setq haskell-process-type 'cabal-new-repl)
```

After editing a source file you can compile and load it into ghci using
`, s b`. This automatically launches GHCi in an Emacs buffer unless it was
already running.

Compile errors are displayed in the GHCi buffer. Use `SPC e` (and `SPC E`) to
navigate between the corresponding source lines which produced the errors.
