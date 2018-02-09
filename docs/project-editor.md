# Editor setup

## Spacemacs

The following features are known to work in Emacs / [Spacemacs](https://github.com/syl20bnr/spacemacs) for developing Reflex applications structured around reflex-platform's [project](project-development.md) setup:

- Syntax highlighting
- All of dante functionality
- Flycheck
- GHCi repl
- Error navigation

1. Install Spacemacs from its develop (not master) branch:
   ```
   git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
   ```
1. Emacs must always be launched from the GHC nix-shell of your Reflex project (flycheck [will not work](https://github.com/flycheck/flycheck-haskell/issues/65) otherwise):
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
1. Restart Emacs (in nix-shell), and access your project sources.

### What should work:

- Opening a Haskell file should automatically start the [dante](https://github.com/jyp/dante)'s GHCi in background. Play around with the dante functions (eg: `, h t` with cursor under an identifier) and make sure that they work.

- Feedback on compile errors should appear instantaneously (without explicitly compiling) in the UI--via red squiggle underlines and mouseover popups--thanks to Flycheck.

- Start a GHCi repl (which is different from that of dante) using `, s B` (see [Spacemacs keybindings](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/haskell#key-bindings) for Haskell). 

- Compile and load a module using `, s b`--compile errors should appear in the GHCi buffer. Use `SPC e` (and `SPC E`) to navigate between source locations for errors.

- Editing both frontend and backend sources in the same Emacs session should work. dante opens *separate* GHCi processes for them, so does `, s B`. This means you will have two GHCi buffers called `*frontend*` and `*backend*`. Compiling and loading a Haskell file will use appropriate GHCi buffer.
