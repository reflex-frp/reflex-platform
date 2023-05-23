{ src,
  pkgs,
  hackage,
  constraints ? [  ],
  extraCabalProject
}: let
  constraint = if constraints == [  ] then
    ""
               else ''
                echo -e "\nconstraints" >> $out/cabal.project
               '' + builtins.concatStringsSep "\n" (map (x: ''
                echo -e "\n,${x}"
               ''));

in
pkgs.runCommand "inject-repos" { } (''
  set -eux
  cp -r ${src} $out
  chmod +w $out/cabal.project
'' + builtins.concatStringsSep "\n" ((map (a: ''
  echo -e "\nsource-repository-package\n    type: ${a.type}\n    location: ${a.repo}\n    tag: ${a.tag}\n" >> $out/cabal.project
'') hackage) ++ (map (b: ''echo -e "\n${b}\n" >> $out/cabal.project'') extraCabalProject)))
