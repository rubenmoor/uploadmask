{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, heterocephalus, http-media, persistent, persistent-sqlite
, persistent-template, raw-strings-qq, servant, servant-multipart
, servant-server, stdenv, text, warp
}:
mkDerivation {
  pname = "uploadmask";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html blaze-markup bytestring heterocephalus http-media
    persistent persistent-sqlite persistent-template raw-strings-qq
    servant servant-multipart servant-server text warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
