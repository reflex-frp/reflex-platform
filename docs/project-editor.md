# Editor setup

## Spacemacs

The following features are known to work in Emacs / [Spacemacs](https://github.com/syl20bnr/spacemacs) for developing Reflex applications structured around reflex-platform's [project](project-development.md) setup:

- Syntax highlighting
- Dante features
- Flycheck
- GHCi repl
- Error navigation

1. Install Spacemacs from its develop (not master) branch:
   ```
   mv ~/.emacs.d ~/.emacs.d-old
   git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
   ```
1. Launch Emacs and complete Spacemacs's setup as instructed (select Vim style keybindings).
1. Open the .spacemacs file using `SPC f e d` and add the following to your `dotspacemacs-configuration-layers`:
   ```elisp
   syntax-checking
   (haskell :variables haskell-completion-backend 'dante)
   ```
1. We need `nix-sandbox`; add it to the `dotspacemacs-additional-packages` list. It should look like this:
   ```elisp
   dotspacemacs-additional-packages '(nix-sandbox)
   ```
1. Configure dante, flycheck and haskell-mode to work with Nix and cabal new-style builds by adding the following to the `dotspacemacs/user-config` function:
   ```elisp
   ;; Connect flycheck to dante
   (add-hook 'dante-mode-hook
             '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                    '(warning . haskell-hlint))))
 
   ;; Configure flycheck to use Nix
   ;; https://github.com/travisbhartwell/nix-emacs#flycheck
   ;; Requires `nix-sandbox` package added to dotspacemacs-additional-packages
   (setq flycheck-command-wrapper-function
         (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command)))
   (setq flycheck-executable-find
         (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))
 
   ;; Configure haskell-mode (haskell-cabal) to use Nix
   (setq haskell-process-wrapper-function
         (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))
 
   ;; Configure haskell-mode to use cabal new-style builds
   (setq haskell-process-type 'cabal-new-repl)
 
   ;; We have limit flycheck to haskell because the above wrapper configuration is global (!)
   ;; FIXME: How? Using mode local variables?
   (setq flycheck-global-modes '(haskell-mode))
   ```
1. Restart Emacs

### Project setup

Before editing the sources you should configure your Reflex project:

1. Create a `shell.nix` so the `nix-sandbox` elisp package (see above) knows to use GHC:
   ```
   cd /my/reflex-project
   echo '(import ./.).shells.ghc' > shell.nix
   ```
1. Tell dante how to manage the frontend/ sources, by creating a `frontend/.dir-locals.el` file:
   ```elisp
   ((nil . (
     (dante-target . "frontend")
     (eval . (setq dante-project-root (file-name-directory (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file)))))
     (dante-repl-command-line . ("nix-shell" "--run" (concat "cabal new-repl " dante-target " --builddir=dist/dante")))
   )))
   ```
1. Repeat the same for `backend/.dir-locals.el` (note that the only part that differs here is the `dante-target` variable):
   ```elisp
   ((nil . (
     (dante-target . "backend")
     (eval . (setq dante-project-root (file-name-directory (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file)))))
     (dante-repl-command-line . ("nix-shell" "--run" (concat "cabal new-repl " dante-target " --builddir=dist/dante")))
   )))
   ```

Now you are ready to start editing the source files.

### What should work:

- Opening a Haskell file should automatically start the [dante](https://github.com/jyp/dante)'s GHCi in background. Play around with the dante functions (eg: `, h t` with cursor under an identifier) and make sure that they work.

- Feedback on compile errors should appear instantaneously (without explicitly compiling) in the UI--via red squiggle underlines and mouseover popups--thanks to Flycheck.

- Start a GHCi repl (which is different from that of dante) using `, s B` (see [Spacemacs keybindings](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/haskell#key-bindings) for Haskell). 

- Compile and load a module using `, s b`--compile errors should appear in the GHCi buffer. Use `SPC e` (and `SPC E`) to navigate between source locations for errors.

- Editing both frontend and backend sources in the same Emacs session should work. dante opens *separate* GHCi processes for them, so does `, s B`. This means you will have two GHCi buffers called `*frontend*` and `*backend*`. Compiling and loading a Haskell file will use appropriate GHCi buffer.

### Flycheck's performance

Flycheck can cause sluggishness. If this is the case [disable it by default](https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Bcheckers/syntax-checking#disabling-by-default) and enable when needed.
