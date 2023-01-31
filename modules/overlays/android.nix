final: prev: (prev.lib.optionalAttrs prev.stdenv.targetPlatform.isAndroid) {
  log = final.runCommandNoCC "log-headers" { } ''
    mkdir -p $out/include/android
    cp ${final.androidndkPkgs_23b.libraries.headers}/android/log.h $out/include/android/log.h
  ''; # Stub for the android "log.h" library
}

