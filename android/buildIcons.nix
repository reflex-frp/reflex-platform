{ runCommand, imagemagick }:
{ src }:
runCommand "android-icons" {
  inherit src;
  buildCommand = ''
    mkdir "$out"

    launcherIconSize() {
      case "$1" in
        l) echo 36x36 ;;
        m) echo 48x48 ;;
        tv) echo 64x64 ;;
        h) echo 72x72 ;;
        xh) echo 96x96 ;;
        xxh) echo 144x144 ;;
        xxxh) echo 192x192 ;;
      esac
    }

    for x in l m tv h xh xxh xxxh ; do
      local dir="$out/drawable-''${x}dpi"
      mkdir "$dir"
      convert -resize "$(launcherIconSize "$x")" -flatten "$src" "$dir/ic_launcher.png"
    done
  '';
  nativeBuildInputs = [
    imagemagick
  ];
} ""
