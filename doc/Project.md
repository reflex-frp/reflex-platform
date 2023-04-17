# Project interface
General usage of the project interface is shown in example.nix.

We expose most things directly from haskell.nix though there are some differences

Please refer to https://input-output-hk.github.io/haskell.nix/ for most usage

### General Project Overview:
The project interface is completely extensible (you have self,super available).  A good example of this usage is in [obelisk](https://github.com/obsidiansystems/obelisk/blob/dylang/ob_mars_com/default.nix#L392)

## inputMap & sha256Map
Haskell.nix doesn't really document this super well. So here's an example on how to use them

Assume you have this in your cabal.project:
```cabal
source-repository-package
  type: git
  location: https://github.com/obsidiansystems/obelisk-oauth.git
  tag: a528c0542e9c30851e7c4542468a053fa5e482ef
  subdir: backend/
          common/
```

The relevant mapping for haskell.nix/mars + thunks is
```nix
inputMap = {
  "https://github.com/obsidiansystems/obelisk-oauth.git/a528c0542e9c30851e7c4542468a053fa5e482ef" = thunkSource ./dep/{thunk};
};
```

If you don't want to use thunks, and just have haskell.nix fetch NON-Private repos when building the plan-nix you can define
```nix
sha256map = {
    "https://github.com/obsidiansystems/obelisk-oauth.git"."a528c0542e9c30851e7c4542468a053fa5e482ef" = lib.fakeHash;
}
```
Nix should spit out the proper hash for the downloaded repo, and you'll put that in place of `lib.fakeHash`

## iOS/Android 
In project.nix we define android and ios attributes, to build those you can run
`nix-build -A {ios/android}.app.{arch}`

The backend of these modules are defined in ./modules/android and ./modules/ios respectively.
We link these together in project.nix which provides the attributes in the top level project.

Example configuration:
```nix
  android = {
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc";
    displayName = "Reflex TodoMVC";
  };
  ios = {
    executableName = "reflex-todomvc";
    bundleIdentifier = "org.reflexfrp.todomvc";
    bundleName = "Reflex TodoMVC";
  };
```
Currently the executable is hardcoded to `hsPkgs.${projectName}.components.exes.${projectName}`, though this will change to be configurable soon. 

## Hackage Overlays
Hackage overlays are defined via
```nix
hackageOverlays = [ 
  {
    name = "android-activity";
    version = "0.1.1";
    src = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "android-activity";
        rev = "2bc40f6f907b27c66428284ee435b86cad38cff8";
        sha256 = "sha256-AIpbe0JZX68lsQB9mpvR7xAIct/vwQAARVHAK0iChV4=";
    };
  }
]
```
This will automatically pull and put the defined packages into a "fake" hackage. This allows the cabal solver see packages that can't be conventially added to cabal.project. A good example of this is [obelisk-generated-static](https://github.com/obsidiansystems/obelisk/blob/dylang/ob_mars_com/default.nix#L416)

This is a purely helper-code for the nix-side of haskell.nix

The hackageOverlay driver is defined in ./modules/hackage-driver

## Shells
We don't use the default haskell.nix shell either. We currently setup our own shells.
Those shells are
`shells.ghc` and `shells.ghc`, they are (somewhat) overridable via `shellTools` and `shellBuildInputs` via the default project.nix interface
