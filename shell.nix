{ system ? builtins.currentSystem }:
(import ./. { inherit system; }).tryReflexShell
