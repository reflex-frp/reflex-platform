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
          os <- T.stripEnd <$> run "uname" ["-s"]
          d <- pwd
          withTmpDir $ \tmp -> do
            cd tmp
            let helloFilename = "hello.hs"
                flags = if os == "Darwin" then " -dynamic" else ""
            writefile (fromText helloFilename) "{-# LANGUAGE OverloadedStrings #-}\nimport Reflex.Dom\nmain = mainWidget $ text \"Hello, world!\""
            run (d </> ("try-reflex" :: String)) ["--pure", "--command", fromString platform <> flags <> " " <> helloFilename <> " ; exit $?"] -- The "exit $?" will no longer be needed when we can assume users will have this patch: https://github.com/NixOS/nix/commit/7ba0e9cb481f00baca02f31393ad49681fc48a5d
        return () :: IO ()
  describe "work-on" $ do
    -- Test that the work-on shell can build the core reflex libraries in a variety of configurations
    forM_ ["ghc", "ghcjs"] $ \platform -> do
      forM_ ["reflex", "reflex-todomvc"] $ \package -> do
        forM_ [False, True] $ \workOnPath -> do
          it ("can build " <> package <> " with " <> platform <> " by importing the " <> (if workOnPath then "package" else "path")) $ do
            shelly $ silently $ do
              d <- pwd
              withTmpDir $ \tmp -> do
                cp_r (d </> package) $ tmp </> package
                cd tmp
                run "chmod" ["-R", "u+w", "."]
                run "git" ["init"]
                run "git" ["add", "-A"]
                run "git" ["commit", "-m", "Initial commit"]
                run (d </> ("hack-on" :: String)) [T.pack package]
                cd $ fromString package
                let packageSpec = if workOnPath then "./." else fromString package
                run (d </> ("work-on" :: String)) [fromString platform, packageSpec, "--pure", "--command", "cabal configure" <> (if platform == "ghcjs" then " --ghcjs" else "") <> " ; exit $?"] -- The "exit $?" will no longer be needed when we can assume users will have this patch: https://github.com/NixOS/nix/commit/7ba0e9cb481f00baca02f31393ad49681fc48a5d
            return () :: IO ()
  let checkThatRepoIsNotAlreadyBeingHackedOn repo = shelly $ silently $ do
        d <- pwd
        gitNixExists <- test_e $ d </> repo </> ("git.json" :: String)
        githubNixExists <- test_e $ d </> repo </> ("github.json" :: String)
        let instructions = "; to test hack-on, please ensure that " <> show repo <> " is in a clean, not-being-hacked-on state"
        when (not $ or [gitNixExists, githubNixExists]) $ fail $ show (repo </> ("{git,github}.json" :: String)) <> " does not exist" <> instructions
        dotGitExists <- test_d $ d </> repo </> (".git" :: String)
        when dotGitExists $ fail $ show (repo </> (".git" :: String)) <> " exists" <> instructions
        return ()
  describe "hack-on" $ do
    forM_ ["nixpkgs", "reflex", "reflex-dom", "reflex-todomvc"] $ \repo -> do
      before_ (checkThatRepoIsNotAlreadyBeingHackedOn repo) $ do
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
          False <- or <$> sequence
            [ test_e $ tmp </> repo </> ("git.json" :: String)
            , test_e $ tmp </> repo </> ("github.json" :: String)
            ]
          True <- test_d $ tmp </> repo </> (".git" :: String)
          return ()
  describe "hack-off" $ do
    let existingFilename = "test"
        newFilename = "new"
        ignoredFilename = "ignored"
        writefileTest filename = do
          shelly $ silently $ flip catchany_sh (\_ -> return True) $ liftM (const False) $ do
            d <- pwd
            withTmpDir $ \tmp1 -> withTmpDir $ \tmp2 -> do
              cd tmp1
              run "git" ["init"]
              writefile (fromText existingFilename) "originalcontents"
              writefile ".gitignore" ignoredFilename
              run "git" ["add", existingFilename, ".gitignore"]
              run "git" ["commit", "-m", "Initial commit"]
              run "git" ["clone", toTextArg tmp1, toTextArg tmp2]
              writefile (tmp2 </> fromText filename) "testcontents"
              run (d </> ("hack-off" :: String)) [toTextArg tmp2]
          return () :: IO ()
    it "won't trample changes" $ writefileTest existingFilename
    it "won't trample new files" $ writefileTest newFilename
    it "won't trample new files even if they are in the .gitignore" $ writefileTest ignoredFilename
  describe "shell.nix" $ do
    it "can be entered using a bare nix-shell" $ do
      shelly $ silently $ do
        run "nix-shell" []
      return () :: IO ()
  describe "benchmark" $ do
    it "can build and run reflex-dom benchmarks" $ do
      shelly $ silently $ do
        d <- pwd
        run (d </> ("benchmark" :: String)) []
      return () :: IO ()
