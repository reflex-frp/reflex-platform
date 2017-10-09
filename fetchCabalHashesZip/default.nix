# This is a modified version of fetchzip from nixpkgs
# it deletes case insensitive duplicates the downloaded
# all-cabal-hashes zip file.

# This function downloads and unpacks an archive file, such as a zip
# or tar file. This is primarily useful for dynamically generated
# archives, such as GitHub's /archive URLs, where the unpacked content
# of the zip file doesn't change, but the zip file itself may
# (e.g. due to minor changes in the compression algorithm, or changes
# in timestamps).

{ lib, fetchurl, unzip, zip }:

{ # Optionally move the contents of the unpacked tree up one level.
  stripRoot ? true
, url
, extraPostFetch ? ""
, ... } @ args:

lib.overrideDerivation (fetchurl ({
  name = args.name or (baseNameOf url);

  recursiveHash = true;

  downloadToTemp = true;

  postFetch =
    ''
      export PATH=${unzip}/bin:$PATH

      unpackDir="$TMPDIR/unpack"
      mkdir "$unpackDir"
      cd "$unpackDir"

      renamed="$TMPDIR/${baseNameOf url}"
      mv "$downloadedFile" "$renamed"
      # Generated with:
      #   unzip -l all-cabal-hashes-34023ac79f5254ad999fabe197e6b2a568a35450.zip | grep 34023ac79f5254ad999fabe197e6b2a568a35450 | sed 's|^.*34023ac79f5254ad999fabe197e6b2a568a35450/\([^/]*\).*$|\1|' | sort -u > packs.txt
      #   cat packs.txt | tr '[:upper:]' '[:lower:]' | sort | uniq -c | grep -v '^ *1 ' | sed 's|^ *[0-9]* ||' > packs2.txt
      # Then manually updated BerkeleyDB and Cabal where the lower case version
      # is the one we want to keep.
      zip -d "$renamed" \
        "*/berkeleydb/*" \
        "*/Buster/*" \
        "*/cabal/*" \
        "*/Checked/*" \
        "*/CLI/*" \
        "*/Compactable/*" \
        "*/Command/*" \
        "*/Condor/*" \
        "*/Dao/*" \
        "*/Data-Rope/*" \
        "*/DBus/*" \
        "*/Digit/*" \
        "*/DocTest/*" \
        "*/Empty/*" \
        "*/Eq/*" \
        "*/Extra/*" \
        "*/FileManip/*" \
        "*/FilePather/*" \
        "*/Focus/*" \
        "*/Geodetic/*" \
        "*/GiST/*" \
        "*/Hangman/*" \
        "*/HDBC-postgresql-hstore/*" \
        "*/Hermes/*" \
        "*/HLogger/*" \
        "*/HMM/*" \
        "*/HPath/*" \
        "*/Hricket/*" \
        "*/HSet/*" \
        "*/Hydrogen/*" \
        "*/IndentParser/*" \
        "*/Interpolation/*" \
        "*/Irc/*" \
        "*/JackMiniMix/*" \
        "*/Javasf/*" \
        "*/Javav/*" \
        "*/Kalman/*" \
        "*/KyotoCabinet/*" \
        "*/Lattices/*" \
        "*/Mecha/*" \
        "*/Mechs/*" \
        "*/Metrics/*" \
        "*/Modulo/*" \
        "*/Moe/*" \
        "*/Noise/*" \
        "*/Nomyx-Core/*" \
        "*/Nomyx-Language/*" \
        "*/Nomyx-Web/*" \
        "*/Numbers/*" \
        "*/Omega/*" \
        "*/Only/*" \
        "*/Peano/*" \
        "*/PerfectHash/*" \
        "*/Quickson/*" \
        "*/Range/*" \
        "*/Ref/*" \
        "*/Safe/*" \
        "*/SCalendar/*" \
        "*/SDL2-ttf/*" \
        "*/SeqAlign/*" \
        "*/SmtLib/*" \
        "*/Stack/*" \
        "*/Stream/*" \
        "*/SVG2Q/*" \
        "*/Tables/*" \
        "*/Tensor/*" \
        "*/Thrift/*" \
        "*/Tic-Tac-Toe/*" \
        "*/Top/*" \
        "*/Unique/*" \
        "*/Validation/*" \
        "*/Vulkan/*" \
        "*/WAVE/*" \
        "*/WaveFront/*" \
        "*/XAttr/*" \
        "*/Yocto/*"
      unpackFile "$renamed"
    ''
    + (if stripRoot then ''
      if [ $(ls "$unpackDir" | wc -l) != 1 ]; then
        echo "error: zip file must contain a single file or directory."
        echo "hint: Pass stripRoot=false; to fetchzip to assume flat list of files."
        exit 1
      fi
      fn=$(cd "$unpackDir" && echo *)
      if [ -f "$unpackDir/$fn" ]; then
        mkdir $out
      fi
      mv "$unpackDir/$fn" "$out"
    '' else ''
      mv "$unpackDir" "$out"
    '') #*/
    + extraPostFetch;
} // removeAttrs args [ "stripRoot" "extraPostFetch" ]))
# Hackety-hack: we actually need unzip hooks, too
(x: {nativeBuildInputs = x.nativeBuildInputs++ [unzip zip];})
