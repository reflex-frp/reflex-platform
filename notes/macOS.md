### -dynamic and -threaded flags on macOS

When invoking ghc directly on macOS, you'll need to give it `-dynamic` - otherwise, compiling fails with something like

```
Undefined symbols for architecture x86_64:
  "_OBJC_CLASS_$_NSURL", referenced from:
      objc-class-ref in libHSjsaddle-wkwebview-0.9.4.0-2SHufpCZa9lIA8lareALCO.a(WKWebView.o)
ld: symbol(s) not found for architecture x86_64
```

and `-threaded` - otherwise running the binary yields

```
user error (RTS doesn't support multiple OS threads (use ghc -threaded when linking))
```

### Installing Nix on MacOS Catalina (10.15)
On a pre-macOS 10.15 with nix installed:
```
git clone https://github.com/obsidiansystems/nix.git
cd nix
git checkout darwin-10.15-install
nix build -f release.nix binaryTarball.x86_64-darwin
cp result/*.tar.xz ~/nix.tar.xz
```
Copy ~/nix.tar.xz to your Catalina macOS and open a terminal there
```
tar xf nix.tar.xz
./nix-*/create-volume.sh
./nix-*/install --daemon
```

