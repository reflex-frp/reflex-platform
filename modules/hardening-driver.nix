{ dontHarden ? [ ], hardeningOpts ? [ ] }@top: { attrs }: let
  removeFromList = { toRemove, baseList }: builtins.attrNames (removeAttrs (builtins.listToAttrs (builtins.concatMap (a: [{ name = a; value = a; }]) baseList)) toRemove);
in builtins.concatMap (a: 
  [{ packages.${a}.ghcOptions = hardeningOpts; }]
) (removeFromList {
  toRemove = dontHarden;
  baseList = (builtins.attrNames attrs);
})
