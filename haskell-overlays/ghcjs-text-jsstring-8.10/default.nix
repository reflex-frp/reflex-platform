{ lib, fetchgit }:

self: super: {
  _dep = super._dep or {} // {
    textSrc = fetchgit {
      url = "https://github.com/dfordivam/text.git";
      rev = "126174753ea8e5f45df8fcbba609e3f1c453bf27";
      sha256 = "0l7nbln2w77s12fm4ybhi0jsfnxkyiwskfx3b682pfisa6n32rgm";
    };
    ghcjsBaseTextJSStringSrc = super.ghcjs-base.src.overrideAttrs (drv: {
      outputHash = "1pdxlb67f94bl2b6k0m4flpjbf07g7fgqbyjnki4y57a5r0iympd";
      postFetch = (drv.postFetch or "") + ''
        ( cd $out
          patch -p1 < ${./ghcjs-base-text-jsstring.patch}
        )
      '';
    });
  };

  ghc = super.ghc.overrideAttrs (drv: {
    postUnpack = ''
      set -x
      (
        echo $sourceRoot
        cd $sourceRoot
        rm -r lib/boot/pkg/text
        # unpackFile ${self._dep.textSrc}
        # chmod +w text-*
        # mv text-* lib/boot/pkg/text
        cp --no-preserve=mode -r "${self._dep.textSrc}" lib/boot/pkg/text
        unpackFile ${self._dep.ghcjsBaseTextJSStringSrc}
        chmod +w ghcjs-base-*
        mv ghcjs-base-* lib/boot/pkg/ghcjs-base
        unpackFile ${super.dlist.src}
        chmod +w dlist-*
        mv dlist-* lib/boot/pkg/dlist
        unpackFile ${super.vector.src}
        chmod +w vector-*
        mv vector-* lib/boot/pkg/vector
        unpackFile ${super.primitive.src}
        chmod +w primitive-*
        mv primitive-* lib/boot/pkg/primitive
        sed -i 's/    - mtl/    - mtl\n    - dlist\n    - primitive\n    - vector\n    - ghcjs-base/' lib/boot/boot.yaml
        cat lib/boot/boot.yaml
      )
    '';
  });

  dlist = null;
  ghcjs-base = null;
  primitive = null;
  vector = null;
}
