{ nixpkgs }:

let inherit (nixpkgs.haskell) lib;
    overrideCabal = pkg: f: if pkg == null then null else lib.overrideCabal pkg f;
    hspecGit = nixpkgs.fetchgit {
      url = git://github.com/ryantrinkle/hspec;
      rev = "937c0ae61d70dcd71c35a170b800c30f14a5bc9c";
      sha256 = "1819d5b3f973b432339256ba783b33ada691a785d059e83009e5e2edc6178f6d";
    };
    extendHaskellPackages = with lib; haskellPackages: haskellPackages.override {
      packageSetConfig = nixpkgs.callPackage
        <nixpkgs/pkgs/development/haskell-modules/configuration-lts-3.10.nix>
        {};
      overrides = self: super: {
        ########################################################################
        # Reflex packages
        ########################################################################
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};


      } // (let
        f = if super.ghc.isGhcjs or false
            then nixpkgs.lib.attrsets.mapAttrs (_: _: null)
            else nixpkgs.lib.id;
      in f {
        ########################################################################
        # ghcjs-boot packages
        ########################################################################
        aeson = overrideCabal super.aeson (drv: {
          version = "0.9.0.1";
          sha256 = "1g7qdq7zpyvqwmh4sfhizqpb51cg24lrcj9vq5msz8k896y7vfcj";
        });
        async = overrideCabal super.async (drv: {
          version = "2.0.1.6";
          sha256 = "06fzkqjliccxqiygms7v1xff3wlkg54n9xwzv7m1yxylkzlikjkz";
          jailbreak = true;
        });
        attoparsec = overrideCabal super.attoparsec (drv: {
          version = "0.13.0.0";
          sha256 = "12b4xi6nlnhpwz8apn4mk880mkhcv1sfvf4j3z1h5dgkadi2zgbi";
        });
        case-insensitive = overrideCabal super.case-insensitive (drv: {
          version = "1.2.0.4";
          sha256 = "07nm40r9yw2p9qsfp3pjbsmyn4dabrxw34p48171zmccdd5hv0v3";
        });
        dlist = overrideCabal super.dlist (drv: {
          version = "0.7.1.1";
          sha256 = "1zayvxvkan2s2ixajdr3f5rn1gzhprzv6cww4cbpwjhzw0l7zc08";
        });
        extensible-exceptions = overrideCabal super.extensible-exceptions (drv: {
          version = "0.1.1.3";
          sha256 = "1i8rjfczsx1wjfaq423a7cp7qrnxh053865z7bg6hwhk2pxsrxkm";
        });
        hashable = overrideCabal super.hashable (drv: {
          version = "1.2.3.2";
          sha256 = "0h9295pv2sgbaqlwpwbx2bap6nngm0jcdhkqham1wpjwyxqgqrlc";
        });
        mtl = overrideCabal super.mtl (drv: {
          version = "2.2.1";
          sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
        });
        old-locale = overrideCabal super.old-locale (drv: {
          version = "1.0.0.7";
          sha256 = "0l3viphiszvz5wqzg7a45zp40grwlab941q5ay29iyw8p3v8pbyv";
        });
        old-time = overrideCabal super.old-time (drv: {
          version = "1.1.0.3";
          sha256 = "1h9b26s3kfh2k0ih4383w90ibji6n0iwamxp6rfp2lbq1y5ibjqw";
        });
        parallel = overrideCabal super.parallel (drv: {
          version = "3.2.0.6";
          sha256 = "0hp6vf4zxsw6vz6lj505xihmnfhgjp39c9q7nyzlgcmps3xx6a5r";
        });
        primitive = overrideCabal super.primitive (drv: {
          version = "0.5.4.0";
          sha256 = "05gdgj383xdrdkhxh26imlvs8ji0z28ny38ms9snpvv5i8l2lg10";
          revision = "1";
          editedCabalFile = "df0a129c168c61a06a02123898de081b1d0b254cce6d7ab24b8f43ec37baef9e";
          });
        scientific = overrideCabal super.scientific (drv: {
          version = "0.3.3.3";
          sha256 = "1hngkmd1kggc84sz4mddc0yj2vyzc87dz5dkkywjgxczys51mhqn";
          jailbreak = true;
        });
        stm = overrideCabal super.stm (drv: {
          version = "2.4.4";
          sha256 = "0gc8zvdijp3rwmidkpxv76b4i0dc8dw6nbd92rxl4vxl0655iysx";
        });
        syb = overrideCabal super.syb (drv: {
          version = "0.5.1";
          sha256 = "0iiqz5mamk1nsij99rypms7dhx5flm2n02k1x6miqgnhg075zc41";
        });
        unordered-containers = overrideCabal super.unordered-containers (drv: {
          version = "0.2.5.1";
          sha256 = "06l1xv7vhpxly75saxdrbc6p2zlgz1az278arfkz4rgawfnphn3f";
        });
        vector = overrideCabal super.vector (drv: {
          version = "0.10.12.2";
          sha256 = "01hc71k1z9m0g0dv4zsvq5d2dvbgyc5p01hryw5c53792yi2fm25";
          jailbreak = true;
        });


      }) // {
        ########################################################################
        # Other packages
        ########################################################################
        ref-tf = overrideCabal super.ref-tf (drv: {
          version = "0.4";
        });
        these = overrideCabal super.these (drv: {
          version = "0.6.1.0";
        });
        ghcjs-dom = overrideCabal super.ghcjs-dom (drv: {
          version = "0.2.2.0";
          sha256 = "1r94rj3i6y0zpysa48di4ndmy4whkr73aqa2c50wifmnks03n2gk";
          doCheck = false;
        });
      };
    };
in rec {
  inherit overrideCabal extendHaskellPackages;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7102;
  ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghcjs;
}
