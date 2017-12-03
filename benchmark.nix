{ reflex-platform ? import ./. {} }:
let pkgs = reflex-platform.nixpkgs;
in pkgs.writeScript "benchmark.sh" ''
#!/usr/bin/env bash
set -euo pipefail

exec 3>&1
exec 1>&2

PATH="${pkgs.nodejs-8_x}/bin:${pkgs.nodePackages.npm}/bin:${pkgs.chromedriver}/bin:$PATH"
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

npm install
for package in webdriver-ts webdriver-ts-results vanillajs-keyed; do
    cd $package
    npm install
    npm run build-prod
    cd ..
done

REFLEX_DOM_DIST=reflex-dom-v0.4-keyed/dist
mkdir -p "$REFLEX_DOM_DIST"
cp -a "${reflex-platform.ghcjs.reflex-dom}/bin/krausest.jsexe/"* "$REFLEX_DOM_DIST"

npm start &
SERVER_PID=$!

cd webdriver-ts

npm run selenium -- --framework vanillajs-keyed reflex --count 1 --headless $CHROME_BINARY $CHROMEDRIVER

kill "$SERVER_PID"

exec 1>&3

echo "[";
paste -d ',' results/reflex-dom*;
echo "]";
''
