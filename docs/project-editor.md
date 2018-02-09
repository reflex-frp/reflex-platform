# Editor setup

## Spacemacs

The following features are known to work in Emacs /
[Spacemacs](https://github.com/syl20bnr/spacemacs) for developing Reflex
applications structured around reflex-platform's
[project](project-development.md) setup:

- Syntax highlighting
- All of dante functionality
- Flycheck
- GHCi repl
- Error navigation

Firstly, Emacs must be launched from the GHC `nix-shell` so that it knows where
to find `runghc` (a
[limitation](https://github.com/flycheck/flycheck-haskell/issues/65) of
flycheck-haskell):

```
nix-shell -A shells.ghc --run emacs &
```

1. Configure Spacemacs by adding the following to your `dotspacemacs-configuration-layers`:
   ```
   syntax-checking
   (haskell :variables haskell-completion-backend 'dante)
   ```
1. Configure haskell-mode to use new-style cabal commands by adding the following to the `dotspacemacs/user-config` function:
   ```
   (setq haskell-process-type 'cabal-new-repl)
   ```
1. Restart Emacs, and access your project sources.

After editing a source file you can compile and load it into ghci using
`, s b` (see [Spacemacs
keybindings](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/haskell#key-bindings)
for Haskell). This automatically launches GHCi in an Emacs buffer unless it was
already running.

Compile errors are displayed in the GHCi buffer. Use `SPC e` (and `SPC E`) to
navigate between the corresponding source lines which produced the errors.

Flycheck also works, which means you get immediate feedback on type errors thus
obviating compiling and loading modules more often.
 
Moreover you may simultaneously edit the backend and frontend sources. There
will be two ghci buffers corresponding to each. Dante too will start independent
ghci sessions for them.
