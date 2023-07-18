{ src,
  pkgs,
  extraCabalProject
}: let
  extraCabal = map (a: ''
    echo -e "\n${a}\n" >> $out/cabal.project
  '') extraCabalProject;
in

pkgs.runCommand "modify-project" {  } (''
 cp -r ${src} $out
 chmod +w $out/cabal.project
'' + builtins.concatStringsSep "\n" extraCabal)
