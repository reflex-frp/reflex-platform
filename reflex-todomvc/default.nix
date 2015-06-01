{ callPackage, fetchFromGitHub, reflex-todomvc }:

callPackage (fetchFromGitHub {
  owner = "ryantrinkle";
  repo = "reflex-todomvc";
  rev = "9a768eb8d1113649fc1f8c7ac5790167b4146887";
  sha256 = "0ljmajyl4shx6rvvy68583xagalhl4kpvjbk0ggwg3ww45n829lx";
}) {}
