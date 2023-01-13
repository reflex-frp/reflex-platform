{ runCommand, imagemagick, lib }:
{ src ? null # Base raster image for fixed-size icons (must be a path)
, adaptiveIcon ? null
  # Path to a XML file containing the adaptive icon specification (SDK
  # 26 and up).
}:
let
  rasterInput =
    if (src == null) && (adaptiveIcon == null)
    then abort "Either src or adaptiveIcon must be specified!"
    else src;
in
runCommand "android-icons"
{
  buildCommand = lib.optionalString (src != null) ''
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
      convert -resize "$(launcherIconSize "$x")" -flatten "${rasterInput}" "$dir/ic_launcher.png"
    done
  '' + lib.optionalString (adaptiveIcon != null) ''
    mkdir -p "$out/mipmap-anydpi-v26/"
    cp "${adaptiveIcon}" "$out/mipmap-anydpi-v26/ic_launcher.xml"
  '';
  nativeBuildInputs = [
    imagemagick
  ];
} ""
