self: _: {
    chromedriver = with self;
        let
        allSpecs = {
            x86_64-linux = {
            system = "linux64";
            sha256 = "1yxcy6gxg1pwyrfjlnk2c5d8rpfz652bbsmpy61012c35anwhvhk";

            };

            #FIXME:  get sha
            x86_64-darwin = {
            system = "mac64";
            # sha256 = "0f8j7m8ardaaw8pv02vxhwkqbcm34366bln0np0j0ig21d4fag09";
            sha256 = "0000000000000000000000000000000000000000000000000000";
            };
        };

        spec = allSpecs.${stdenv.hostPlatform.system}
            or (throw "missing chromedriver binary for ${stdenv.hostPlatform.system}");

        libs = stdenv.lib.makeLibraryPath [
            stdenv.cc.cc.lib
            cairo fontconfig freetype
            gdk-pixbuf glib gtk2 gnome2.GConf
            xorg.libX11 nspr nss pango xorg.libXrender
            gnome2.GConf xorg.libXext xorg.libXi
        ];
        in
        stdenv.mkDerivation rec {
        pname = "chromedriver";
        version = "78.0.3904.105";

        src = fetchurl {
            url = "https://chromedriver.storage.googleapis.com/${version}/chromedriver_${spec.system}.zip";
            sha256 = spec.sha256;
        };

        nativeBuildInputs = [ unzip makeWrapper ];

        unpackPhase = "unzip $src";

        installPhase = ''
            install -m755 -D chromedriver $out/bin/chromedriver
        '' + stdenv.lib.optionalString (!stdenv.isDarwin) ''
            patchelf --set-interpreter ${glibc.out}/lib/ld-linux-x86-64.so.2 $out/bin/chromedriver
            wrapProgram "$out/bin/chromedriver" --prefix LD_LIBRARY_PATH : "${libs}:\$LD_LIBRARY_PATH"
        '';

        meta = with stdenv.lib; {
            homepage = https://sites.google.com/a/chromium.org/chromedriver;
            description = "A WebDriver server for running Selenium tests on Chrome";
            license = licenses.bsd3;
            maintainers = [ maintainers.goibhniu ];
            platforms = attrNames allSpecs;
        };
        };
}
