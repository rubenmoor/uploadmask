{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, directory, filepath, heterocephalus, http-media, persistent
, persistent-sqlite, persistent-template, raw-strings-qq, servant
, servant-multipart, servant-server, stdenv, text, time, timerep
, warp
}:
mkDerivation {
  pname = "uploadmask";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html blaze-markup bytestring directory filepath
    heterocephalus http-media persistent persistent-sqlite
    persistent-template raw-strings-qq servant servant-multipart
    servant-server text time timerep warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
