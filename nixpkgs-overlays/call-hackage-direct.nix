{nixpkgs}:
{pkg, ver, sha256}:
    let pkgver = "${pkg}-${ver}";
    in nixpkgs.haskellPackages.callCabal2nix pkg (nixpkgs.fetchzip {
         url = "mirror://hackage/${pkgver}/${pkgver}.tar.gz";
         inherit sha256;
  })
