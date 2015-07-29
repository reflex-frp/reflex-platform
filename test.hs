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
  let silently = id -- Temporarily disable 'silently'
  describe "try-reflex" $ do
    -- Test that the try-reflex shell is able to build a simple "Hello, world!" application with both ghc and ghcjs
    forM_ ["ghc", "ghcjs"] $ \platform -> do
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
    -- Test that the work-on shell can build the core reflex libraries in a variety of configurations
    forM_ ["ghc", "ghcjs"] $ \platform -> do
      forM_ ["reflex", "reflex-dom", "reflex-todomvc"] $ \package -> do
        forM_ [False, True] $ \workOnPath -> do
          it ("can build " <> package <> " with " <> platform <> " by importing the " <> (if workOnPath then "package" else "path")) $ do
            shelly $ silently $ do
              d <- pwd
              srcDir <- liftM (fromText . T.filter (/= '\n')) $ run "nix-build" ["-E", "(import ./nixpkgs {}).fetchgit (import ./" <> fromString package <> "/git.nix)"]
              withTmpDir $ \tmp -> do
                cp_r srcDir $ tmp </> package
                cd $ tmp </> package
                run "chmod" ["-R", "u+w", "."]
                let packageSpec = if workOnPath then "./." else fromString package
                run (d </> ("work-on" :: String)) [fromString platform, packageSpec, "--pure", "--command", "cabal configure" <> (if platform == "ghcjs" then " --ghcjs" else "") <> " ; exit $?"] -- The "exit $?" will no longer be needed when we can assume users will have this patch: https://github.com/NixOS/nix/commit/7ba0e9cb481f00baca02f31393ad49681fc48a5d
            return () :: IO ()
  describe "hack-on" $ do
    forM_ ["nixpkgs", "reflex", "reflex-dom", "reflex-todomvc"] $ \repo -> do
      let checkThatRepoIsNotAlreadyBeingHackedOn = shelly $ silently $ do
            d <- pwd
            gitNixExists <- test_e $ d </> repo </> ("git.nix" :: String)
            let instructions = "; to test hack-on, please ensure that " <> show repo <> " is in a clean, not-being-hacked-on state"
            when (not gitNixExists) $ fail $ show (repo </> ("git.nix" :: String)) <> " does not exist" <> instructions
            dotGitExists <- test_d $ d </> repo </> (".git" :: String)
            when dotGitExists $ fail $ show (repo </> (".git" :: String)) <> " exists" <> instructions
            return ()
      before_ checkThatRepoIsNotAlreadyBeingHackedOn $ do
        let withSetup a = do
              shelly $ silently $ do
                d <- pwd
                withTmpDir $ \tmp -> do
                  cd tmp
                  cp_r (d </> repo) tmp
                  run "git" ["init"]
                  run "git" ["add", "-A"]
                  run "git" ["commit", "-m", "Initial commit"]
                  a d tmp
              return () :: IO ()
            writefileTest filename = withSetup $ \d tmp -> do
              let fileToChange = repo </> (filename :: String)
                  contents = "test"
              writefile fileToChange contents
              True <- (liftM (const False) $ run (d </> ("hack-on" :: String)) [fromString repo]) `catchany_sh` (\_ -> return True)
              newContents <- readfile fileToChange
              when (newContents /= contents) $ fail $ "hack-on changed the contents of " <> show fileToChange
              return ()
        it ("won't trample changes in " <> repo) $ writefileTest "default.nix" -- default.nix is an already-existing file
        it ("won't trample new files in " <> repo) $ writefileTest "test" -- test is a non-existing file
        it ("can checkout " <> repo) $ withSetup $ \d tmp -> do
          run (d </> ("hack-on" :: String)) [fromString repo]
          False <- test_e $ tmp </> repo </> ("git.nix" :: String)
          True <- test_d $ tmp </> repo </> (".git" :: String)
          return ()
  describe "shell.nix" $ do
    it "can be entered using a bare nix-shell" $ do
      shelly $ silently $ do
        run "nix-shell" []
      return () :: IO ()
