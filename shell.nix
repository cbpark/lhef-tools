with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, attoparsec, base, bytestring, conduit
             , conduit-extra, containers, hep-kinematics, pipes
             , pipes-attoparsec, pipes-bytestring, resourcet, stdenv
             , transformers
             }:
             mkDerivation {
               pname = "lhef-tools";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 attoparsec base bytestring conduit conduit-extra containers
                 hep-kinematics pipes pipes-attoparsec pipes-bytestring resourcet
                 transformers
               ];
               homepage = "http://github.com/cbpark/lhef-tools";
               description = "Tools for the LHEF analyses";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
