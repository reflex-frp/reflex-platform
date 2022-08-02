{ lib, fetchgit }:

self: super: {
  _dep = super._dep or {} // {
    textSrc = fetchgit {
      url = "https://github.com/obsidiansystems/text.git";
      rev = "50076be0262203f0d2afdd0b190a341878a08e21";
      sha256 = "1vy7a81b1vcbfhv7l3m7p4hx365ss13mzbzkjn9751bn4n7x2ydd";
    };
    ghcjsBaseTextJSStringSrc = super.ghcjs-base.src.overrideAttrs (drv: {
      outputHash = "19bsvv8g4kgjj2z7a8r8in4g8sshvvwn717n4664fnfn6xhzm2i6";
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
