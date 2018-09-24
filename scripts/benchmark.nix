{ reflex-platform ? import ./.. {} }:
let pkgs = reflex-platform.nixpkgs;
    nodejs = pkgs.nodejs-8_x;
    # TODO remove in reunification since it is already bundled
    yarn = pkgs.callPackage ({ stdenv, nodejs, fetchzip }:
      stdenv.mkDerivation rec {
        name = "yarn-${version}";
        version = "1.9.4";

        src = fetchzip {
          url = "https://github.com/yarnpkg/yarn/releases/download/v${version}/yarn-v${version}.tar.gz";
          sha256 = "0lxncqvz66167ijhsi76ds2yp8140d9ywn89y5vm92010irsgs20";
        };

        buildInputs = [ nodejs ];

        installPhase = ''
          mkdir -p $out/{bin,libexec/yarn/}
          cp -R . $out/libexec/yarn
          ln -s $out/libexec/yarn/bin/yarn.js $out/bin/yarn
          ln -s $out/libexec/yarn/bin/yarn.js $out/bin/yarnpkg
        '';

        meta = with stdenv.lib; {
          homepage = https://yarnpkg.com/;
          description = "Fast, reliable, and secure dependency management for javascript";
          license = licenses.bsd2;
          maintainers = [ maintainers.offline ];
        };
      }) { inherit nodejs; };
    shellHook = linkNodeModulesHook + ''
      export PATH=node_modules/.bin:$PATH
    '';
    inherit (pkgs) fetchzip fetchFromGitHub;
    inherit (reflex-platform) js-framework-benchmark-src;
    yarn2nixSrc = fetchzip {
      url = "https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz";
      sha256 = "02bzr9j83i1064r1r34cn74z7ccb84qb5iaivwdplaykyyydl1k8";
    };
    yarn2nix = import yarn2nixSrc { inherit pkgs nodejs yarn; };
    inherit (yarn2nix) mkYarnPackage linkNodeModulesHook defaultYarnFlags;
    nodePkgs = {
      webdriver-ts = mkYarnPackage {
        name = "webdriver-ts";
        src = js-framework-benchmark-src + /webdriver-ts;
        preInstall = "yarn --offline run build-prod";
        inherit shellHook;
      };
      webdriver-ts-results = mkYarnPackage {
        name = "webdriver-ts-results";
        src = js-framework-benchmark-src + /webdriver-ts-results;
        preInstall = "yarn --offline run build-prod";
        inherit shellHook;
      };
      vanillajs-keyed = mkYarnPackage {
        name = "vanillajs-keyed";
        src = js-framework-benchmark-src + /vanillajs-keyed;
        preInstall = "yarn --offline run build-prod";
        inherit shellHook;
      };
      js-framework-benchmark = mkYarnPackage {
        name = "js-framework-benchmark";
        src = js-framework-benchmark-src;
        inherit shellHook;
      };
    };
    bin = pkgs.writeScript "benchmark.sh" ''
#!/usr/bin/env bash
set -euo pipefail

exec 3>&1
exec 1>&2

PATH="${yarn}/bin:${nodejs}/bin:${pkgs.nodePackages.npm}/bin:${pkgs.chromedriver}/bin:$PATH"
CHROME_BINARY="${if reflex-platform.system == "x86_64-darwin"
  then ""
  else ''--chromeBinary "${pkgs.chromium}/bin/chromium"''
}"
CHROMEDRIVER="${if reflex-platform.system == "x86_64-darwin"
  then ""
  else ''--chromeDriver "${pkgs.chromedriver}/bin/chromedriver"''
}"

CLEAN=$(mktemp -d 2>/dev/null || mktemp -d -t 'clean') # This crazy workaround ensures that it will work on both Mac OS and Linux; see https://unix.stackexchange.com/questions/30091/fix-or-alternative-for-mktemp-in-os-x
trap "rm -rf \"$CLEAN\"" EXIT

cd "$CLEAN"

cp -a "${reflex-platform.js-framework-benchmark-src}/"* .
chmod -R +w .

ln -s ${nodePkgs.js-framework-benchmark.node_modules} .
rm -r yarn.lock vanillajs-keyed webdriver-ts-results
ln -s ${nodePkgs.vanillajs-keyed}/node_modules/js-framework-benchmark-vanillajs ./vanillajs-keyed
ln -s ${nodePkgs.webdriver-ts-results}/node_modules/webdriver-ts-results .

REFLEX_DOM_DIST=reflex-dom-v0.4-keyed/dist
mkdir -p "$REFLEX_DOM_DIST"
cp -a "${reflex-platform.ghcjs.reflex-dom}/bin/krausest.jsexe/"* "$REFLEX_DOM_DIST"

yarn run start > server.out &
SERVER_PID=$!

# ensures that grep will block execution but tail won't
# https://superuser.com/questions/270529/monitoring-a-file-until-a-string-is-found/900134#900134
SERVER_PORT="$((tail -f -n0 server.out & ) | grep -m 1 '127.0.0.1' | sed -e 's/.*127.0.0.1://')"

cd webdriver-ts
ln -s "${nodePkgs.webdriver-ts}/node_modules/webdriver-ts/dist" .

yarn run selenium --framework vanillajs-keyed reflex --count 1 --headless $CHROME_BINARY $CHROMEDRIVER --port $SERVER_PORT

kill "$SERVER_PID"

exec 1>&3

echo "[";
paste -d ',' results/*;
echo "]";
'';
in { inherit bin nodePkgs; }
