## Arch Linux Build Instructions

### Installing Nix
```
git clone https://aur.archlinux.org/nix.git
cd nix
makepkg -sic
sudo systemctl enable --now nix-daemon.socket
```
You must restart your system for nix's environmental variables to take effect.

### Running reflex-platform
```
git clone https://github.com/reflex-frp/reflex-platform/
cd reflex-platform
./try-reflex
```
