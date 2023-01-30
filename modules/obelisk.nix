pkgs: [
    {
      name = "obelisk-frontend";
      version = "1.0.0.0";
      src = (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";

        repo = "obelisk";
        rev = "e7ccc91806b94b424b086bf75087a0a5fd3ff0b5";
        sha256 = "sha256-z9A+XMWdsRQj+8UbGK7Orn5UFMl/U1W7auetMVn5rvY=";
      }) + "/lib/frontend";
    }
    {
      name = "obelisk-backend";
      version = "1.0.0.0";
      src = (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";
        rev = "e7ccc91806b94b424b086bf75087a0a5fd3ff0b5";
        sha256 = "sha256-z9A+XMWdsRQj+8UbGK7Orn5UFMl/U1W7auetMVn5rvY=";
      }) + "/lib/backend";
    }
    {
      name = "obelisk-asset-serve-snap";
      version = "1.0.0.0";
      src = (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";
        rev = "e7ccc91806b94b424b086bf75087a0a5fd3ff0b5";
        sha256 = "sha256-z9A+XMWdsRQj+8UbGK7Orn5UFMl/U1W7auetMVn5rvY=";
      }) + "/lib/asset/serve-snap";
    }
    {
      name = "obelisk-snap-extras";
      version = "1.0.0.0";
      src = (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";
        rev = "e7ccc91806b94b424b086bf75087a0a5fd3ff0b5";
        sha256 = "sha256-z9A+XMWdsRQj+8UbGK7Orn5UFMl/U1W7auetMVn5rvY=";
      }) + "/lib/snap-extras";
    }
    {
      name = "obelisk-route";
      version = "1.0.0.0";
      src = (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";
        rev = "e7ccc91806b94b424b086bf75087a0a5fd3ff0b5";
        sha256 = "sha256-z9A+XMWdsRQj+8UbGK7Orn5UFMl/U1W7auetMVn5rvY=";
      }) + "/lib/route";
    }
    {
      name = "obelisk-executable-config-lookup";
      version = "1.0.0.0";
      src = (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";
        rev = "e7ccc91806b94b424b086bf75087a0a5fd3ff0b5";
        sha256 = "sha256-z9A+XMWdsRQj+8UbGK7Orn5UFMl/U1W7auetMVn5rvY=";
      }) + "/lib/executable-config/lookup";

    }
    {
      name = "tabulation";
      version = "1.0.0.0";

      src = (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";

        rev = "e7ccc91806b94b424b086bf75087a0a5fd3ff0b5";
        sha256 = "sha256-z9A+XMWdsRQj+8UbGK7Orn5UFMl/U1W7auetMVn5rvY=";
      }) + "/lib/tabulation";
    }
    {
      name = "obelisk-executable-config-inject";
      version = "1.0.0.0";
      src = (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";
        rev = "e7ccc91806b94b424b086bf75087a0a5fd3ff0b5";
        sha256 = "sha256-z9A+XMWdsRQj+8UbGK7Orn5UFMl/U1W7auetMVn5rvY=";
      }) + "/lib/executable-config/inject";
    }
]
