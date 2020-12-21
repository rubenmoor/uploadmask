{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, directory, filepath, gerippe, heterocephalus, http-media
, monad-logger, mtl, optparse-applicative, persistent
, persistent-mysql-haskell, persistent-template, raw-strings-qq
, resource-pool, resourcet, safe, servant, servant-multipart
, servant-server, stdenv, text, time, timerep, transformers, unix
, warp
}:
mkDerivation {
  pname = "uploadmask";
  version = "0.9.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html blaze-markup bytestring directory filepath gerippe
    heterocephalus http-media monad-logger mtl optparse-applicative
    persistent persistent-mysql-haskell persistent-template
    raw-strings-qq resource-pool resourcet safe servant
    servant-multipart servant-server text time timerep transformers
    unix warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
