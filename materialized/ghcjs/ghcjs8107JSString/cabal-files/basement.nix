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
      specVersion = "1.18";
      identifier = { name = "basement"; version = "0.0.11"; };
      license = "BSD-3-Clause";
      copyright = "2015-2017 Vincent Hanquez <vincent@snarc.org>\n, 2017-2018 Foundation Maintainers";
      maintainer = "vincent@snarc.org";
      author = "";
      homepage = "https://github.com/haskell-foundation/foundation#readme";
      url = "";
      synopsis = "Foundation scrap box of array & string";
      description = "Foundation most basic primitives without any dependencies";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if compiler.isGhc && (compiler.version).lt "8.0"
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/basement-0.0.11.tar.gz";
      sha256 = "67582b3475a5547925399f719df21f8bbbd0ca4d4db27795c22a474f8ee6346b";
      });
    }) // {
    package-description-override = "name:                basement\r\nversion:             0.0.11\r\nx-revision: 3\r\nsynopsis:            Foundation scrap box of array & string\r\ndescription:         Foundation most basic primitives without any dependencies\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\ncopyright:           2015-2017 Vincent Hanquez <vincent@snarc.org>\r\n                   , 2017-2018 Foundation Maintainers\r\nmaintainer:          vincent@snarc.org\r\ncategory:            Web\r\nbuild-type:          Simple\r\nhomepage:            https://github.com/haskell-foundation/foundation#readme\r\nbug-reports:         https://github.com/haskell-foundation/foundation/issues\r\ncabal-version:       1.18\r\nextra-source-files:  cbits/*.h cbits/basement_rts.c\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/haskell-foundation/foundation\r\n  subdir: basement\r\n\r\nlibrary\r\n  hs-source-dirs:    .\r\n  exposed-modules:\r\n                     Basement.Imports\r\n\r\n                     Basement.Base16\r\n                     Basement.Bindings.Memory\r\n                     Basement.Endianness\r\n                     Basement.Environment\r\n                     Basement.PrimType\r\n\r\n                     Basement.Exception\r\n                     Basement.Cast\r\n                     Basement.From\r\n\r\n                     Basement.Types.Char7\r\n                     Basement.Types.CharUTF8\r\n                     Basement.Types.OffsetSize\r\n                     Basement.Types.Ptr\r\n                     Basement.Types.AsciiString\r\n                     Basement.Types.Word128\r\n                     Basement.Types.Word256\r\n                     Basement.Monad\r\n                     Basement.MutableBuilder\r\n                     Basement.FinalPtr\r\n\r\n                     Basement.Nat\r\n\r\n                     -- Extended Types\r\n                     Basement.BoxedArray\r\n                     Basement.Block\r\n                     Basement.Block.Mutable\r\n                     Basement.Block.Builder\r\n                     Basement.UArray\r\n                     Basement.UArray.Mutable\r\n                     Basement.String\r\n                     Basement.String.Builder\r\n                     Basement.NonEmpty\r\n\r\n                     -- Extended Types with explicit type level size\r\n                     Basement.Sized.Block\r\n                     Basement.Sized.UVect\r\n                     Basement.Sized.Vect\r\n                     Basement.Sized.List\r\n                     Basement.BlockN\r\n\r\n                     -- Utils\r\n                     Basement.NormalForm\r\n                     Basement.These\r\n\r\n                     -- Terminal\r\n                     Basement.Terminal\r\n                     Basement.Terminal.ANSI\r\n\r\n                     -- numeric stuff\r\n                     Basement.IntegralConv\r\n                     Basement.Floating\r\n                     Basement.Numerical.Number\r\n                     Basement.Numerical.Additive\r\n                     Basement.Numerical.Subtractive\r\n                     Basement.Numerical.Multiplicative\r\n                     Basement.Bounded\r\n\r\n                     -- exported algorithms\r\n                     Basement.Alg.XorShift\r\n\r\n                     -- compat / base redefinition\r\n                     Basement.Compat.AMP\r\n                     Basement.Compat.Base\r\n                     Basement.Compat.Bifunctor\r\n                     Basement.Compat.CallStack\r\n                     Basement.Compat.C.Types\r\n                     Basement.Compat.ExtList\r\n                     Basement.Compat.IsList\r\n                     Basement.Compat.Identity\r\n                     Basement.Compat.Primitive\r\n                     Basement.Compat.PrimTypes\r\n                     Basement.Compat.MonadTrans\r\n                     Basement.Compat.Semigroup\r\n                     Basement.Compat.Natural\r\n                     Basement.Compat.NumLiteral\r\n                     Basement.Compat.Typeable\r\n\r\n                     Basement.Bits\r\n\r\n  other-modules:\r\n                     Basement.Error\r\n                     Basement.Show\r\n                     Basement.Runtime\r\n\r\n                     Basement.Alg.Class\r\n                     Basement.Alg.Mutable\r\n                     Basement.Alg.PrimArray\r\n\r\n                     Basement.Alg.UTF8\r\n                     Basement.Alg.String\r\n\r\n                     Basement.Numerical.Conversion\r\n\r\n                     Basement.Block.Base\r\n\r\n                     Basement.UTF8.Base\r\n                     Basement.UTF8.Helper\r\n                     Basement.UTF8.Table\r\n                     Basement.UTF8.Types\r\n\r\n                     Basement.UArray.Base\r\n\r\n                     Basement.String.CaseMapping\r\n                     Basement.String.Encoding.Encoding\r\n                     Basement.String.Encoding.UTF16\r\n                     Basement.String.Encoding.UTF32\r\n                     Basement.String.Encoding.ASCII7\r\n                     Basement.String.Encoding.ISO_8859_1\r\n\r\n                     Basement.Terminal.Size\r\n\r\n  -- support and dependencies\r\n  build-depends: base >= 4.9.0.0 && < 4.15\r\n  if impl(ghc < 8.0)\r\n    build-depends:     base\r\n  else\r\n    build-depends:     base\r\n                     , ghc-prim\r\n    if os(windows)\r\n      build-depends:   Win32\r\n\r\n  default-language:    Haskell2010\r\n  default-extensions: NoImplicitPrelude\r\n                      RebindableSyntax\r\n                      TypeFamilies\r\n                      BangPatterns\r\n                      DeriveDataTypeable\r\n  if (arch(i386) || arch(x86_64))\r\n    cpp-options: -DARCH_IS_LITTLE_ENDIAN\r\n  else\r\n    cpp-options: -DARCH_IS_UNKNOWN_ENDIAN\r\n  include-dirs:      cbits\r\n  c-sources:         cbits/foundation_mem.c\r\n  if impl(ghc < 8.2)\r\n    c-sources:       cbits/basement_rts.c\r\n";
    }