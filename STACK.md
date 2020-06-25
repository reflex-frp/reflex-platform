### ```reflex, reflex-dom, reflex-dom-contrib and stack```

Although reflex-platform is provided as a reliable way of working with reflex and related packages, many people prefer to integrate individual components into their existing build system instead.  This file details some successful setups using stack.  People are encouraged to detail other successful setups in this file via PR.



#### Platform: OSX 10.11.4 (shouldn't matter)
#### Compiler: GHCJS (ghcjs-0.2.0.20151230.3_ghc-7.10.2)


stack.yaml
```
# For more information, see:
# https://github.com/commercialhaskell/stack/blob/release/doc/yaml_configuration.md

# Specifies the GHC version and set of packages available (e.g., lts-3.5, nightly-2015-09-21, ghc-7.10.2)
resolver: lts-3.21

compiler: ghcjs-0.2.0.20151230.3_ghc-7.10.2
compiler-check: match-exact
setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.20151230.3_ghc-7.10.2:
        url: "https://github.com/nrolland/ghcjs/releases/download/v.0.2.0.20151230.3/ghcjs-0.2.0.20151230.3.tar.gz"


# Local packages, usually specified by relative directory name
packages:
- location:
    git: https://github.com/ryantrinkle/reflex
    commit: "cc62c11a6cde31412582758c236919d4bb766ada"
- location:
    git: https://github.com/ryantrinkle/reflex-dom
    commit: "639d9ca13c2def075e83344c9afca6eafaf24219"
- location:
    git: https://github.com/ghcjs/ghcjs-dom
    commit: "1644ab2a058618ebbffefa12ee95cd7484169275"
- location:
    git: https://github.com/reflex-frp/reflex-dom-contrib
    commit: "41c67daf2ad74281f2488cb80ceab7d12292142b"
- '.'

# Packages to be pulled from upstream that are not in the resolver (e.g., acme-missiles-0.3)
extra-deps:
   - dependent-map-0.2.1.0
   - dependent-sum-0.3.2.1
   - ref-tf-0.4
   - these-0.6.2.0
   - readable-0.3.1
   - string-conv-0.1

# Override default flag values for local packages and extra-deps
flags:
  ghcjs-dom:
    jsffi: true


# Extra package databases containing global packages
extra-package-dbs: []
```

NB: You will get some warnings about mismatches between the ghcjs boot package versions and the resolver versions.  I haven't had this cause any issue nor do I know how to fix it.  Adding the ghcjs boot package version in extra-deps does eliminate the warning but caused me other issues.

#### Platform: OSX 10.11.4
#### Compiler: GHC 7.10.3 (so ```reflex-dom/ghcjs-dom``` requires webkit)

Building webkitgtk on osx is painful.  However, there is a working (almost, see below) nix derivation for this so using stack's nix support might be the easiest way to go. This requires a working nix setup on your machine.  For that, follow the instructions [here.] (https://iilab.org/news/2015-03-27-nix-osx-haskellng-hakyll.html#step-by-step) or google "nix on osx" and follow whichever set of instructions make the most sense.

Then configure nix to allow "broken" packages to be installed:
```
mkdir ~/.nixpkgs
echo "{ allowBroken = true; }" > ~/.nixpkgs/config.nix
```

you need a different stack yaml file in order to compile in a nix shell:

webkit_stack.yaml
```
# For more information, see: https://github.com/commercialhaskell/stack/blob/release/doc/yaml_configuration.md

# Specifies the GHC version and set of packages available (e.g., lts-3.5, nightly-2015-09-21, ghc-7.10.2)
resolver: lts-5.1

nix:
  enable: true
  pure: true
#  path: [nixpkgs=/path/to/local/nixpkgs] # in case you want to use a version other than what is on your $NIX_PATH
  shell-file: shell.nix

# Local packages, usually specified by relative directory name
packages:
- location:
    git: https://github.com/ryantrinkle/reflex
    commit: "cc62c11a6cde31412582758c236919d4bb766ada"
- location:
    git: https://github.com/ryantrinkle/reflex-dom
    commit: "639d9ca13c2def075e83344c9afca6eafaf24219"
- location:
    git: https://github.com/ghcjs/ghcjs-dom
    commit: "a316f84795d8c4a444eb0d0b46a4ff2a4387e104"  # ghcjs-dom-0.2.3.0
- location:
    git: https://github.com/reflex-frp/reflex-dom-contrib
    commit: "41c67daf2ad74281f2488cb80ceab7d12292142b"
- '.'

# Packages to be pulled from upstream that are not in the resolver (e.g., acme-missiles-0.3)
extra-deps:
   - dependent-map-0.2.1.0
   - dependent-sum-0.3.2.1
   - ref-tf-0.4
   - these-0.6.2.0
   - readable-0.3.1
   - string-conv-0.1
   - webkitgtk3-0.14.1.1
   - webkitgtk3-javascriptcore-0.13.1.2
   - haskell-src-exts-1.16.0.1
   - syb-0.5.1


# Override default flag values for local packages and extra-deps
flags:
  ghcjs-dom:
    gtk3: true
    webkit: true

# Extra package databases containing global packages
extra-package-dbs: []
```

shell.nix (in root of project directory along with stack.yaml and weblit_stack.yaml)
```
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "myEnv";
  buildInputs = [ webkitgtk24x darwin.security_tool gcc49 git ];
  GIT_SSL_CAINFO="${cacert}/etc/ssl/certs/ca-bundle.crt";
  SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  FONTCONFIG_FILE="${fontconfig}/etc/fonts/fonts.conf";
}
```


Notes:

1.  You run ```stack --stack-yaml webkit_stack.yaml``` wherever you would have run ```stack``` before

2. This version of webkit is somewhat broken. Issues have been observed with dropdown menus and out-of-memory errors.  This may be fixed soon.  Watch this space!

3. You might be able to get away with fewer buildInputs.  For webkitgtk, all you should need is webkitgtk24x and darwin.security_tool.  But if you download sources from github (as in the example webkit_stack.yaml), you will need git.  And if your project uses packages that require other binaries (e.g., alex or happy), you will likely need a gcc.  The default gcc in nixpkgs is gcc 5.3 but that build failed for me so I switched to gcc49.  YMMV.  Also, you could install all the binaries by doing a stack install of each one--using the same resolver!!--and they should be found when you build using nix.  I found it easier to equip the nix-shell with gcc so things could get built as stack needed them.

4.  There is a cached build of webkitgtk in Ryan's cache (Ryan, do you want to put that info here?) but it didn't work for me--some dependency must not have matched.  If you need to build webkit from source it will take a long time.

5. If you use both configurations with the same cabal file you will need to make the ghcjs-base dependency conditional on ghcjs by placing the following under your build-depends rather than just depending on ghcjs-base:
```
 if impl(ghcjs)
    build-depends:     ghcjs-base
```
