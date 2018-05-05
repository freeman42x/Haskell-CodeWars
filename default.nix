{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "CodeWars";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
