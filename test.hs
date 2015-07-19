{-# LANGUAGE OverloadedStrings #-}
-- | This script provides basic tests for the try-reflex functionality; it should be run before committing code to important branches, such as 'develop'
module Main where

import Test.Hspec
import Shelly
import Control.Monad
import Data.Monoid
import Data.String
import qualified Data.Text as T

main :: IO ()
main = hspec $ parallel $ do
  describe "try-reflex" $ do
    forM_ ["ghc", "ghcjs"] $ \platform -> do
      -- Test that the try-reflex shell is able to build a simple "Hello, world!" application with both ghc and ghcjs
      it ("can build hello world with " <> platform) $ do
        shelly $ silently $ do
          d <- pwd
          withTmpDir $ \tmp -> do
            cd tmp
            let helloFilename = "hello.hs"
            writefile (fromText helloFilename) "import Reflex.Dom\nmain = mainWidget $ text \"Hello, world!\""
            run (d </> ("try-reflex" :: String)) ["--pure", "--command", fromString platform <> " " <> helloFilename <> " ; exit $?"] -- The "exit $?" will no longer be needed when we can assume users will have this patch: https://github.com/NixOS/nix/commit/7ba0e9cb481f00baca02f31393ad49681fc48a5d
        return () :: IO ()
  describe "work-on" $ do
    forM_ ["ghc", "ghcjs"] $ \platform -> do
      forM_ ["reflex", "reflex-dom", "reflex-todomvc"] $ \package -> do
        forM_ [False, True] $ \workOnPath -> do
          it ("can enter the environment for " <> package <> " with " <> platform <> " by importing the " <> (if workOnPath then "package" else "path")) $ do
            shelly $ silently $ do
              d <- pwd
              srcDir <- liftM (fromText . T.filter (/= '\n')) $ run "nix-build" ["-E", "(import ./nixpkgs {}).fetchgit (import ./" <> fromString package <> "/git.nix)"]
              withTmpDir $ \tmp -> do
                cp_r srcDir $ tmp </> package
                cd $ tmp </> package
                run "chmod" ["-R", "u+w", "."]
                let packageSpec = if workOnPath then "./." else fromString package
                run (d </> ("work-on" :: String)) [fromString platform, packageSpec, "--pure", "--command", "cabal configure" <> (if platform == "ghcjs" then " --ghcjs" else "") <> " ; cabal build ; exit $?"] -- The "exit $?" will no longer be needed when we can assume users will have this patch: https://github.com/NixOS/nix/commit/7ba0e9cb481f00baca02f31393ad49681fc48a5d
            return () :: IO ()
