{ lib, fetchgit }:

self: super: {
  _dep = super._dep or {} // {

    textSrc = fetchgit {
      url = "https://github.com/obsidiansystems/text.git";
      rev = "50076be0262203f0d2afdd0b190a341878a08e21";
      sha256 = "1vy7a81b1vcbfhv7l3m7p4hx365ss13mzbzkjn9751bn4n7x2ydd";
    };
    dlistSrc = fetchgit {
      url = "https://github.com/spl/dlist.git";
      rev = "03d91a3000cba49bd2c8588cf1b0d71e229ad3b0"; #v0.8.0.4
      sha256 = "0asvz1a2rp174r3vvgs1qaidxbdxly4mnlra33dipd0gxrrk15sq";
    };
    vectorSrc = fetchgit {
      url = "https://github.com/haskell/vector.git";
      rev = "19189a884434ecd2c96212395f5705347b55775b"; #v0.12.0.3
      sha256 = "13idbrajpqr594b277d4zd7iw2qvgvpb9w7aa2zw8icpsmnf6nc2";
    };
    primitiveSrc = fetchgit {
      url = "https://github.com/haskell/primitive.git";
      rev = "53f72ce69a4dfde5345cf5809a8b4a1993523367";
      sha256 = "0ywmn7pc7n7qafd7478dvih8pwyq0d9mrggfd8wnb5jdk3lf5xba";
      postFetch = ''
        sed -i.bak 's/\(base .*\)4\.12/\15/' $out/primitive.cabal
        # substituteInPlace $out/primitive.cabal --replace 'base >= 4.5 && < 4.12' 'base >= 4.5 && < 5'
        cat $out/primitive.cabal
      '';

    };
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
        cp --no-preserve=mode -r "${self._dep.dlistSrc}" lib/boot/pkg/dlist
        cp --no-preserve=mode -r "${self._dep.vectorSrc}" lib/boot/pkg/vector
        cp --no-preserve=mode -r "${self._dep.primitiveSrc}" lib/boot/pkg/primitive
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
