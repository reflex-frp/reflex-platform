## Test Plan for Reflex Platform

First give yourself a fresh place for testing and copy of Reflex Platform:
```bash
mkdir rp-sandbox
cd rp-sandbox
git clone https://github.com/reflex-frp/reflex-platform
cd reflex-platform
git checkout <git commit that you are testing>
```

### Making sure that the try-reflex shell works

1. ```bash
   ./try-reflex
   ```
   This command will *not* immediately exit.
   It will instead make a new "shell"; the program where you type commands inside the old shell.

2. If you are running this command on a macOS you will need to add the `dynamic` and `threaded` flags each time you invoke ghc.
   So to make this easier on macOS do:
   ```bash
   GHC='ghc -dynamic -threaded'
   ```
   and on Linux do
   ```bash
   GHC='ghc'
   ```

### Testing "Hello, world"

3. Copy the following into the shell (all at once):
   ```bash
   cat >hello.hs <<EOF
   {-# LANGUAGE OverloadedStrings #-}
   import Reflex.Dom
   main = mainWidget $ text "Hello, world!"
   EOF
   ```

4. Test the tiny program you pasted with GHC:
   ```
   $GHC hello.hs
   ./hello
   ```
   You should see a window with "Hello, world!".

5. Test it with GHCJS
   ```bash
   ghcjs hello.hs
   ```
   On Linux:
   ```bash
   chromium hello.jsexe/index.html
   ```
   on macOS:
   ```bash
   open hello.jsexe/index.html
   ```

### Testing the host.hs example

```bash
ghc examples/host.hs
./examples/host
```
Follow the instructions and confirm that the output event is a sequence of all of the letters you have entered
To exit, ctrl-c.
Exit the `./try-reflex` shell with the `exit` command
```bash
exit
```

### Generating Documentation

Run the following commands. When you run them, the docs should open in your
browser.
```bash
./scripts/docs-for reflex
./scripts/docs-for reflex-dom
```
After the browser window has opened, you can close it.

### Running a build on the web

```bash
nix-build -A ghcjs.reflex-todomvc
```
If successful this will give you a file path. Run the following command depending on your system:

| Linux                                                 | Mac                                               |
|-------------------------------------------------------|---------------------------------------------------|
| `chromium result/bin/reflex-todomvc.jsexe/index.html` | `open result/bin/reflex-todomvc.jsexe/index.html` |

You should test this app.

### Android App:

If you are running qa from a macOS machine please skip the android tests, but be sure to QA it from a linux device.
```bash
nix-build release.nix -A x86_64-linux.unprofiled.androidReflexTodomvc -o deploy-android
```
Make sure that your android device has been plugged in, that you have deleted any app titled "reflex-todomvc" from the phone, and that you have unlocked and accepted permissions on the phone before running the following:
```bash
deploy-android/bin/deploy
```
This command may take a couple of tries before you see `Success`. Sometimes you must unplug and replug your phone, or try a different cable. The app should be installed on your phone if this command was successful.
Once the app is installed, test it and verify it works the same as the web version.

### Testing TODOMVC app on IOS

Using a macOS run the following:
Make sure the device that you are installing the app on is plugged in.
```bash
nix-build release.nix -A x86_64-darwin.unprofiled.iosReflexTodomvc
result/bin/deploy <team-id>
```
For more information on finding out what your team-id is, please see the [Obelisk's `README.md`](https://github.com/obsidiansystems/obelisk/blob/develop/README.md#ios).

### Testing the 'work-on' and 'work-on-multi' scripts

```bash
scripts/hack-on haskell-overlays/reflex-packages/dep/reflex-dom
printf 'packages:\n  haskell-overlays/reflex-packages/dep/reflex-dom/reflex-dom-core\n  haskell-overlays/reflex-packages/dep/reflex-dom/reflex-dom\n  examples/WorkOnTest/' > cabal.project.local
scripts/work-on ghc examples/WorkOnTest
```

This may take a while, but ultimately you should end up in a new shell. Run:
```bash
cabal new-run WorkOnTest --allow-newer
```

After the app builds, you should see a window pop up which says "Hello, world!".
Type `exit` to leave the shell.

Run the work-on-multi script:
```bash
scripts/work-on-multi ghc reflex-dom reflex-dom-core
cabal new-build all --allow-newer
cabal new-run WorkOnTest --allow-newer
```
Again, you should see a window pop up which says "Hello, world!".
```bash
exit
scripts/hack-off haskell-overlays/reflex-packages/dep/reflex-dom
```
Note that `hack-off` might fail unless GitHub knows about your ssh key.

### Removing the sandbox dir:

```bash
cd ../../
rm -rf rp-sandbox
```
