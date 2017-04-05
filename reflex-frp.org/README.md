
source for reflex-frp.org



Preview the site at [http://reflex-frp.github.io/reflexfrp.org/](http://reflex-frp.github.io/reflexfrp.org/)

---

# Contributions

Contributions are greatly appreciated for this project. 

# Structure

Main branch is `master`

Currently the preview is on the `gh-pages` branch, which is a subtree containing the `index.jsexe` directory


---

# Installation

Install reflex-platform, by following instructions listed at [https://github.com/reflex-frp/reflex-platform#setup](https://github.com/reflex-frp/reflex-platform#setup)

quick reference of commands shown here:

```sh
git clone https://github.com/reflex-frp/reflex-platform
cd reflex-platform
./try-reflex
ghcjs --make index.hs
```

# Staging Instructions

```sh
$ ghcjs index.hs
$ git add index.jsexe && git commit -m "subtree commit message"
$ git subtree push --prefix index.jsexe origin gh-pages
```
