{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, directory, filepath, gerippe, heterocephalus, http-media
, monad-logger, mtl, mysql, network, optparse-applicative
, persistent, persistent-mysql, persistent-template, raw-strings-qq
, resource-pool, resourcet, safe, servant, servant-multipart
, servant-server, stdenv, text, text-show, time, timerep
, transformers, unix, warp
}:
mkDerivation {
  pname = "uploadmask";
  version = "0.9.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html blaze-markup bytestring directory filepath gerippe
    heterocephalus http-media monad-logger mtl mysql network
    optparse-applicative persistent persistent-mysql
    persistent-template raw-strings-qq resource-pool resourcet safe
    servant servant-multipart servant-server text text-show time
    timerep transformers unix warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
