with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc801.ghc;
   name = "ml-labs";
   buildInputs = [ git openssl ];
   LANG = "en_US.UTF-8";
}

