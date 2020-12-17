{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Html
  ( uploadForm
  ) where

import           Data.Text                   (Text)
import           Text.Blaze.Html             (Html)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (form, label, style, title)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.RawString.QQ

import           Prelude                     (($))

myStyle :: Html
myStyle = [r|
body {
  font-family: sans-serif;
}

div {
  margin: 15px;
}

input#title {
  width: 400px;
}

input#duration {
  width: 400px;
}

textarea#description {
  width: 400px;
}

div#submit {
  text-align: right;
}
|]

myScript :: Html
myScript = [r|
  window.onload = function() {
    const audioFileInput = document.getElementById("audioFile")
    audioFileInput.onchange = async (evt) => {
      const mediainfo = await new Promise( (res) => MediaInfo(null, res) )
      const inputDuration = document.getElementById("duration")
      const file = audioFileInput.files[0]
      const getSize = () => file.size
      const readChunk = async (chunkSize, offset) =>
        new Uint8Array( await file.slice(offset, offset + chunkSize).arrayBuffer() )

      const info = await mediainfo.analyzeData(getSize, readChunk)
      const audio_track = info.media.track.find( (track) => track[ "@type" ] === "Audio" )
      inputDuration.value = Math.round(audio_track.Duration)
    }
  }
|]

uploadForm :: Html
uploadForm = docTypeHtml $ do
  head $ do
    meta ! content "text/html;charset=utf-8" ! httpEquiv "Content-Type"
    meta ! content "utf-8" ! httpEquiv "encoding"
    title "Upload new episode"
    style myStyle
    script ! src "https://unpkg.com/mediainfo.js@0.1.4/dist/mediainfo.min.js" $ ""
    script myScript
  body $
    form ! action "" ! method "post" ! enctype "multipart/form-data" $ do
      div $ do
        label ! for "title" $ "Title: "
        br
        input ! type_ "text" ! name "title" ! id "title"
      div $ do
        label ! for "audioFile" $ "Audio file: "
        br
        input ! type_ "file" ! name "audioFile" ! id "audioFile"
      div $ do
        label ! for "duration" $ "Audio duration in seconds: "
        br
        input ! type_ "text" ! name "duration" ! id "duration" ! readonly "readonly"
      div $ do
        label ! for "description" $ "Description: "
        br
        textarea ! name "description" ! id "description" $ ""
      div $ do
        label ! for "thumbnailFile" $ "Thumbnail file (optional): "
        br
        input ! type_ "file" ! name "thumbnailFile"
      div ! id "submit" $ do
        hr
        input ! type_ "submit" ! value "submit"
