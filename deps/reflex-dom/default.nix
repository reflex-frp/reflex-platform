{ callPackage, fetchFromGitHub, reflex-dom }:

callPackage (fetchFromGitHub {
  owner = "ryantrinkle";
  repo = "reflex-dom";
  rev = "ec3e459e69113402ba6edf860e1d20670c5452be";
  sha256 = "0gjc8gzdggwfhwmbhalpwrq5rkvbyavz4bxcafqnfk0q0s9xn4a1";
}) {}
