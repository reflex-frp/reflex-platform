{
  owner = "nixos";

  # Could also just be nixpkgs. However this makes it clearer that we
  # intend to use a revision that benefits from the binary cache.
  repo = "nixpkgs-channels";

  rev = "cd7242d09d64669c05246a4005731b1fa1cbaa15";
  sha256 = "0ivnkbn6w969m5dmavy0yj0fgbznabc3krw1nh05pqf2anhn6p12";
}
