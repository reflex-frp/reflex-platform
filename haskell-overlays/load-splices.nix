{ haskellLib, fetchFromGitHub, lib, splicedHaskellPackages }:

self: super: {

  # Add some flags to load splices from nativeHaskellPackages
  mkDerivation = drv: super.mkDerivation (drv // (let
    LOCAL_SPLICE_DIR = "$TMPDIR/my-splices/";
    attrName = "${drv.pname}_${lib.replaceStrings ["."] ["_"] drv.version}";
    pkg = if builtins.hasAttr drv.pname splicedHaskellPackages
          then builtins.getAttr drv.pname splicedHaskellPackages
          else if builtins.hasAttr attrName splicedHaskellPackages
          then builtins.getAttr attrName splicedHaskellPackages
          else null;
  in {
    buildFlags = (drv.buildFlags or [])
               ++ ["--ghc-option=-load-splices=${LOCAL_SPLICE_DIR}"];
    postConfigure = (drv.postConfigure or "")
      + lib.optionalString (pkg ? SPLICE_DIR) ''
      # We need to patch splices to have the cross target's package hash.
      # Unfortunately, this requires using sed on each .hs-splice
      # file. So we must copy all of the splice files into
      # LOCAL_SPLICE_DIR before we write.
      mkdir -p "${LOCAL_SPLICE_DIR}"
      if [ -d "${pkg}${pkg.SPLICE_DIR}" ]; then
          (cd "${pkg}${pkg.SPLICE_DIR}" && \
           find . -name '*.hs-splice' \
                  -exec install -D '{}' "${LOCAL_SPLICE_DIR}/{}" \;)
      fi
      chmod -R +w "${LOCAL_SPLICE_DIR}"

      # The target package also needs to be replaced.
      seds="$(strings dist/setup-config | sed -n 's/^!\(${drv.pname}\)-\([0-9.]\+\)-\(.\{22\}\).*$/-e "s,\1-\2-[a-zA-Z0-9]\{22\},\1-\2-\3, "/p' | head -n1)"

      # Generate a list of sed expressions from a package list. Each
      # expression will match a package name with a random hash and replace it
      # with our package db's expected hash. This relies on the hash being
      # exactly 22 characters.
      seds+="$(ghc-pkg --package-db="$packageConfDir" list -v 2>/dev/null | sed -n 's/^ .*(\(\(.*\)-.\{22\}\))$/-e "s,\2-[a-zA-Z0-9]\{22\},\1,"/p')"

      if [ -n "$seds" ]; then
          echo reticulating splices...
          echo "$seds"
          find "${LOCAL_SPLICE_DIR}" -name '*.hs-splice' -exec sed -i '{}' "$seds" \;
      fi
    '';
  }));

  haddock = super.haddock.overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [ ./haddock.patch ];
  });

}
