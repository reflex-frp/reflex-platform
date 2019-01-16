{ lib, pkgs, config, options }:

{
  options = {
    options-docs = lib.mkOption {
      type = lib.types.attrsOf lib.types.package;
      internal = true;
      visible = false;
    };

    options-docs-md = lib.mkOption {
      type = lib.types.package;
      internal = true;
      visible = false;
    };

    project-dev-docs-md = lib.mkOption {
      internal = true;
      visible = false;
    };
  };

  config = {
    options-docs = import
      (pkgs.path + /nixos/doc/manual/default.nix)
      {
        inherit pkgs config options;
        version = throw "undefined";
        revision = throw "undefined";
      };

    options-docs-md =
      let
        inherit (lib) concatMapStringsSep sort mapAttrsToList
          isAttrs concatStringsSep isList isString optionalString isBool;

        fromFile = f: builtins.fromJSON (builtins.readFile f);
        sortAndRender = attrs:
          concatMapStringsSep "\n" render
            (sort (a: b: a.name < b.name)
              (mapAttrsToList (name: opt: { inherit name opt; }) attrs));
        headed = header: field: ''
          #### ${header}

          ${builtins.toString field}
        '';
        ppExample' = tab: ex:
          if ex._type or null == "literalExample" then ex.text
          else if isAttrs ex then (if ex == {} then "{}" else ''
            {
            ${concatStringsSep "\n" (mapAttrsToList (name: val: let t = tab+"  "; in "${t}${name} = ${ppExample' t val};") ex)}
            ${tab}}'')
          else if isList ex then (if ex == [] then "[]" else ''
            [
            ${concatMapStringsSep "\n" (val: let t = tab+"  "; in "${t}(${ppExample' t val})") ex}
            ${tab}]'')
          else if isString ex then builtins.toJSON ex
          else if ex == null then "null"
          else if isBool ex then (if ex then "true" else "false")
          else toString ex;
        ppExample = ex: ''
          ```nix
          ${ppExample' "" ex}
          ```
        '';
        render = { name, opt }: ''
          ---

          ## `${name}`

          ${optionalString (opt ? description) (headed "Description" opt.description)}
          ${optionalString (opt ? example) (headed "Example" (ppExample opt.example))}
          ${optionalString (opt ? default) (headed "Default" (ppExample opt.default))}
        '';

        inputMd = pkgs.writeText "in-doc.md" (sortAndRender (fromFile (config.options-docs.optionsJSON + /share/doc/nixos/options.json)));
      in pkgs.runCommand "out-doc.md" { nativeBuildInputs = [pkgs.pandoc]; } ''
        pandoc --toc-depth=1 --toc -s -o $out -f gfm -t markdown_github ${inputMd}
      '';

    project-dev-docs-md = inputMd:
      let
        header = ''
          Project Development
          -------------------

          This document describes how to build real-world applications written in
          Reflex. You will see how to:
        '';

      in pkgs.runCommand "out-doc.md" { nativeBuildInputs = [pkgs.pandoc]; } ''
          echo "${header}" > $out
          pandoc --toc-depth=1 --toc -s -f gfm -t markdown_github ${inputMd} >> $out
        '';
  };
}
