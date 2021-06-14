{ reflex-platform ? import ../.. { hideDeprecated = true; } }:
let pkgs = reflex-platform.nixpkgs;
    inherit (pkgs) nodejs;
    shellHook = linkNodeModulesHook + ''
      export PATH=node_modules/.bin:$PATH
    '';
    inherit (pkgs) fetchzip fetchFromGitHub;
    dep = reflex-platform.thunkSet ./dep;
    yarn2nix = import (dep.yarn2nix) { inherit pkgs; };
    inherit (yarn2nix) mkYarnPackage linkNodeModulesHook defaultYarnFlags;
    nodePkgs = {
      webdriver-ts = mkYarnPackage {
        name = "webdriver-ts";
        src = dep.js-framework-benchmark + /webdriver-ts;
        preInstall = "yarn --offline run build-prod";
        inherit shellHook;
      };
      webdriver-ts-results = mkYarnPackage {
        name = "webdriver-ts-results";
        src = dep.js-framework-benchmark + /webdriver-ts-results;
        preInstall = "yarn --offline run build-prod";
        inherit shellHook;
      };
      js-framework-benchmark = mkYarnPackage {
        name = "js-framework-benchmark";
        src = dep.js-framework-benchmark;
        inherit shellHook;
      };
    };
    bin = pkgs.writeScript "benchmark.sh" ''
#!/usr/bin/env bash
set -euo pipefail

exec 3>&1
exec 1>&2

PATH="${pkgs.yarn}/bin:${nodejs}/bin:${pkgs.chromedriver}/bin:${pkgs.chromium}/bin:${pkgs.coreutils}/bin:${pkgs.gnugrep}/bin:${pkgs.gnused}/bin"
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

cp -a "${dep.js-framework-benchmark}/"* .
chmod -R +w .

ln -s ${nodePkgs.js-framework-benchmark.node_modules} .
rm -r yarn.lock frameworks/keyed/vanillajs webdriver-ts-results
ln -s ${nodePkgs.webdriver-ts-results}/node_modules/webdriver-ts-results .

REFLEX_DOM_DIST=frameworks/keyed/reflex-dom/bundled-dist
mkdir -p "$REFLEX_DOM_DIST"
cp -a "${reflex-platform.ghcjs.reflex-dom}/bin/krausest.jsexe/"* "$REFLEX_DOM_DIST"

yarn run start > server.out &
SERVER_PID=$!

# ensures that grep will block execution but tail won't
# https://superuser.com/questions/270529/monitoring-a-file-until-a-string-is-found/900134#900134
SERVER_PORT="$((tail -f -n0 server.out & ) | grep -m 1 '127.0.0.1' | sed -e 's/.*127.0.0.1://')"

cd webdriver-ts
ln -s "${nodePkgs.webdriver-ts.node_modules}" .
ln -s "${nodePkgs.webdriver-ts}/node_modules/webdriver-ts/dist" .

yarn run selenium --framework reflex-dom-v0.4-keyed --count 1 --headless $CHROME_BINARY $CHROMEDRIVER --port $SERVER_PORT

kill "$SERVER_PID"

exec 1>&3

echo "[";
paste -d ',' results/*;
echo "]";
'';
in { inherit bin dep nodePkgs; }
