{ src, pkgs, hackage, extraCabalProject }: pkgs.runCommand "inject-repos" { } (''
  set -eux
  cp -r ${src} $out
  chmod +w $out/cabal.project
'' + builtins.concatStringsSep "\n" ((map (a: ''
  echo -e "\nsource-repository-package\n    type: ${a.type}\n    location: ${a.repo}\n    tag: ${a.tag}\n" >> $out/cabal.project
'') hackage) ++ (map (b: ''echo -e "\n${b}\n" >> $out/cabal.project'') extraCabalProject)))
