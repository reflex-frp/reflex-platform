{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "directory-tree"; version = "0.12.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011, Brandon Simmons <brandon.m.simmons@gmail.com>";
      maintainer = "Brandon Simmons <brandon.m.simmons@gmail.com>";
      author = "Brandon Simmons";
      homepage = "http://brandon.si/code/directory-tree-module-released/";
      url = "";
      synopsis = "A simple directory-like tree datatype, with useful IO functions";
      description = "A simple directory-like tree datatype, with useful IO functions and Foldable and Traversable instance\n\nProvides a simple data structure mirroring a directory tree on the\nfilesystem, as well as useful functions for reading and writing\nfile and directory structures in the IO monad.\n\nImporting the library and optional (useful) Foldable and Traverable libraries:\n\n> import System.Directory.Tree\n> import qualified Data.Foldable as F\n> import qualified Data.Traversable as T\n\nWrite a hand-made directory tree of textfiles (strings) to the disk.\nSimulates creating a new user Tux's home directory on a unix machine:\n\n> writeDirectory$ \"/home\" :/ Dir \"Tux\" [File \"README\" \"Welcome!\"]\n\n\"read\" a directory by opening all the files at a filepath with readFile,\nreturning an 'AnchoredDirTree String' (d2). Then check for any IO failures:\n\n> do (base :/ d2) <- readDirectory \"../parent_dir/dir2/\"\n>    let failed = anyFailed d2\n>    if failed then ...\n\nUse Foldable instance function to concat a directory 'dir' of text files into a\nsingle file under the same directory:\n\n> do (b :/ dt) <- readDirectory dir\n>    let f = F.concat dt\n>    return$ b :/ File \"ALL_TEXT\" f\n\nOpen all the files in the current directory as lazy bytestrings, ignoring\nthe base path in Anchored wrapper:\n\n> import qualified Data.ByteString.Lazy as B\n> do (_ :/ dTree) <- readDirectoryWith B.readFile \"./\"\n\nThis version also offers an experimental function `readDirectoryWithL` that does\nlazy directory IO, allowing you to treat the returned `DirTree` as if it were a\nnormal lazily-generated data structure.\n\nFor example, the following does only the amount of IO necessary to list the file\nnames of the children of the root directory, similar to \"ls /\":\n\n> do d <- readDirectoryWithL readFile \"/\"\n>    mapM_ (putStrLn . name) $ contents $ free d\n\nAny ideas or suggestions for improvements are most welcome :-)\n\n/CHANGES/: from 0.11\n\n- export 'System.Directory.Tree.transformDir' as requested\n\n- add test suite to cabal file\n\n- remove redundant @removeNonexistent@ (thanks to dmwit for patch)\n";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/directory-tree-0.12.1.tar.gz";
      sha256 = "e2084495b3a226cf54d949635c86fc14e89daa09d86cce39e3c3cf898ae6e517";
      });
    }) // {
    package-description-override = "name:            directory-tree\nversion:         0.12.1\nhomepage:        http://brandon.si/code/directory-tree-module-released/\nsynopsis:        A simple directory-like tree datatype, with useful IO functions\ndescription:     A simple directory-like tree datatype, with useful IO functions and Foldable and Traversable instance\n .\n Provides a simple data structure mirroring a directory tree on the\n filesystem, as well as useful functions for reading and writing\n file and directory structures in the IO monad.\n .\n Importing the library and optional (useful) Foldable and Traverable libraries:\n .\n > import System.Directory.Tree\n > import qualified Data.Foldable as F\n > import qualified Data.Traversable as T\n .\n Write a hand-made directory tree of textfiles (strings) to the disk.\n Simulates creating a new user Tux's home directory on a unix machine:\n .\n > writeDirectory$ \"/home\" :/ Dir \"Tux\" [File \"README\" \"Welcome!\"]\n .\n \"read\" a directory by opening all the files at a filepath with readFile,\n returning an 'AnchoredDirTree String' (d2). Then check for any IO failures:\n .\n > do (base :/ d2) <- readDirectory \"../parent_dir/dir2/\"\n >    let failed = anyFailed d2\n >    if failed then ...\n .\n Use Foldable instance function to concat a directory 'dir' of text files into a\n single file under the same directory:\n .\n > do (b :/ dt) <- readDirectory dir\n >    let f = F.concat dt\n >    return$ b :/ File \"ALL_TEXT\" f\n .\n Open all the files in the current directory as lazy bytestrings, ignoring\n the base path in Anchored wrapper:\n .\n > import qualified Data.ByteString.Lazy as B\n > do (_ :/ dTree) <- readDirectoryWith B.readFile \"./\"\n .\n This version also offers an experimental function `readDirectoryWithL` that does\n lazy directory IO, allowing you to treat the returned `DirTree` as if it were a\n normal lazily-generated data structure.\n .\n For example, the following does only the amount of IO necessary to list the file\n names of the children of the root directory, similar to \"ls /\":\n .\n > do d <- readDirectoryWithL readFile \"/\"\n >    mapM_ (putStrLn . name) $ contents $ free d\n .\n Any ideas or suggestions for improvements are most welcome :-)\n .\n /CHANGES/: from 0.11\n .\n - export 'System.Directory.Tree.transformDir' as requested\n .\n - add test suite to cabal file\n .\n - remove redundant @removeNonexistent@ (thanks to dmwit for patch)\n .\n\ncategory:        Data, System\nlicense:         BSD3\nlicense-file:    LICENSE\ncopyright:       (c) 2011, Brandon Simmons <brandon.m.simmons@gmail.com>\nauthor:          Brandon Simmons\nmaintainer:      Brandon Simmons <brandon.m.simmons@gmail.com>\ncabal-version:   >= 1.8.0.4\nbuild-type:      Simple\ntested-with:     GHC <=7.8.2\nextra-source-files: EXAMPLES/Examples.hs, EXAMPLES/LazyExamples.hs\n\nsource-repository head\n    type:     git\n    location: https://github.com/jberryman/directory-tree.git\n\nlibrary\n    exposed-modules: System.Directory.Tree\n    build-depends: base <5, filepath <2, directory <2\n    ghc-options:       -Wall\n\ntest-suite test\n    main-is: Test.hs\n    type: exitcode-stdio-1.0\n    build-depends: base <5, filepath <2, directory <2\n                 , process\n    ghc-options:       -Wall\n";
    }