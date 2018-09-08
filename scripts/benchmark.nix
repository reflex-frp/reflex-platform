{ reflex-platform ? import ./.. {} }:
let pkgs = reflex-platform.nixpkgs;
    shellHook = pkgs.yarn2nix.linkNodeModulesHook + ''
      export PATH=node_modules/.bin:$PATH
    '';
    inherit (pkgs) mkYarnPackage;
    inherit (reflex-platform) js-framework-benchmark-src;
    nodePkgs = {
      webdriver-ts = mkYarnPackage {
        name = "webdriver-ts";
        src = js-framework-benchmark-src + /webdriver-ts;
        preInstall = "npm run build-prod";
        inherit shellHook;
        yarnLock = ./webdriver-ts.yarn.lock;
      };
      webdriver-ts-results = mkYarnPackage {
        name = "webdriver-ts-results";
        src = js-framework-benchmark-src + /webdriver-ts-results;
        preInstall = "npm run build-prod";
        inherit shellHook;
        yarnLock = ./webdriver-ts-results.yarn.lock;
      };
      vanillajs-keyed = mkYarnPackage {
        name = "vanillajs-keyed";
        src = js-framework-benchmark-src + /vanillajs-keyed;
        preInstall = "npm run build-prod";
        inherit shellHook;
        yarnLock = ./vanillajs-keyed.yarn.lock;
      };
      js-framework-benchmark = mkYarnPackage {
        name = "js-framework-benchmark";
        src = js-framework-benchmark-src;
        yarnLock = ./js-framework-benchmark.yarn.lock;
        inherit shellHook;
      };
    };
in pkgs.writeScript "benchmark.sh" ''
#!/usr/bin/env bash
set -euo pipefail

exec 3>&1
exec 1>&2

PATH="${pkgs.yarn}/bin:${pkgs.nodejs-8_x}/bin:${pkgs.nodePackages.npm}/bin:${pkgs.chromedriver}/bin:$PATH"
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
ln -s ${nodePkgs.vanillajs-keyed}/node_modules/vanillajs-keyed .
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

yarn run selenium --framework reflex --count 1 --headless $CHROME_BINARY $CHROMEDRIVER --port $SERVER_PORT

kill "$SERVER_PID"

exec 1>&3

echo "[";
paste -d ',' results/*;
echo "]";
''
