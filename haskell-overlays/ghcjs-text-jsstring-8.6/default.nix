{ lib, fetchgit }:

self: super: {
  _dep = super._dep or {} // {
    textSrc = fetchgit {
      url = "https://github.com/obsidiansystems/text.git";
      rev = "50076be0262203f0d2afdd0b190a341878a08e21";
      sha256 = "1vy7a81b1vcbfhv7l3m7p4hx365ss13mzbzkjn9751bn4n7x2ydd";
    };
    ghcjsBaseTextJSStringSrc = (fetchgit {
      url = "https://github.com/ghcjs/ghcjs-base.git";
      rev = "85e31beab9beffc3ea91b954b61a5d04e708b8f2";
      sha256 = "sha256-7VYfQS7qFE/itNIv/Nx5B7glL3WkgmmWoIskd8yivd0=";
    }).overrideAttrs (old: {
      outputHash = "sha256-ZeuEu0neF8ku7Yk7rLTl9lNZmRvKJtjweO3vpOuMPZ0=";
      postFetch = (old.postFetch or "") + ''
        cd $out
        patch -p1 < ${./ghcjs-base-text-jsstring.patch}
        patch -p1 < ${../ghcjs-8.6/ghcjs-base-cabal-version.patch}
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
        cp --no-preserve=mode -r "${self._dep.textSrc}" lib/boot/pkg/text
        cp --no-preserve=mode -r "${self._dep.ghcjsBaseTextJSStringSrc}" lib/boot/pkg/ghcjs-base
        unpackFile ${super.dlist.src}
        chmod +w dlist-*
        mv dlist-* lib/boot/pkg/dlist
        unpackFile ${super.vector.src}
        chmod +w vector-*
        mv vector-* lib/boot/pkg/vector
        unpackFile ${super.primitive.src}
        chmod +w primitive-*
        mv primitive-* lib/boot/pkg/primitive
        sed -i 's/.\/pkg\/mtl/.\/pkg\/mtl\n    - .\/pkg\/ghcjs-base\n    - .\/pkg\/dlist\n    - .\/pkg\/primitive\n    - .\/pkg\/vector/' lib/boot/boot.yaml
        cat lib/boot/boot.yaml
      )
    '';
  });

  dlist = null;
  ghcjs-base = null;
  primitive = null;
  vector = null;
}
