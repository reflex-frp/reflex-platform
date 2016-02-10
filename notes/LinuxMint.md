### Fixing nix-shell on Linux Mint

By default, nix-shell does not work on Linux Mint - the environment, including the PATH, is destroyed by the default /etc/bash.bashrc.  To fix this, please comment out the line that runs "mint-fortune" in that file.

Thanks to @cgibbard for finding this issue and to http://www.pavelkogan.com/2014/07/13/nix-shell-on-linux-mint/ for tracking down the line in question.
