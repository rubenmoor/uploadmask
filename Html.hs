{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Html
  ( uploadForm
  , homepage
  ) where

import           Control.Monad               (forM_)
import           Data.Text                   (Text)
import           Data.Time.Format            (defaultTimeLocale, formatTime)
import           Text.Blaze.Html             (Html)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (form, label, span, style,
                                              title)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.RawString.QQ

import Hosting (staticLink, protocol, mkFileUrl)
import           Model                       (Episode (..))

import           Prelude                     ((<>), ($))

myScript :: Html
myScript = [r|
  formSubmit = function() {
    var form = document.getElementById("form")
    var xhr = new XMLHttpRequest()
    var progressBar = document.getElementById("progressBar")
    var errorSpan = document.getElementById("errorMessage")

    xhr.upload.addEventListener("progress", function(evt){
      if (evt.lengthComputable) {
        var percent = evt.loaded / evt.total * 100
        progressBar.value = Math.round(percent)
      }
    }, false)

    xhr.onload = function() {
      window.location.href = '/?msg=' + xhr.response
    }
    xhr.upload.addEventListener("error", function(e) {
      errorSpan.textContent = "upload failed"
    });
    xhr.upload.addEventListener("abort", function(e) {
      errorSpan.textContent = "upload cancelled"
    });

    xhr.open("post", "")
    xhr.send(new FormData(form))
    for(var i = 0; i < form.elements.length; i++) {
      form.elements[i].disabled = true
    }
  }

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

staticFile :: Text -> Text
staticFile filename = protocol <> staticLink <> "/" <> filename

uploadForm :: Text -> Html
uploadForm today =
  docTypeHtml $ do
    head $ do
      meta ! content "text/html;charset=utf-8" ! httpEquiv "Content-Type"
      meta ! content "utf-8" ! httpEquiv "encoding"
      title "Upload new episode"
      link ! rel "stylesheet" ! href (textValue $ staticFile "styles.css")
      script ! src "https://unpkg.com/mediainfo.js@0.1.4/dist/mediainfo.min.js" $ ""
      script myScript
    body $
      form ! action "" ! method "post" ! enctype "multipart/form-data" ! id "form" $ do
        div $ do
          label ! for "title" $ "Title: "
          br
          input ! type_ "text" ! name "title" ! id "title"
        div $  do
          label ! for "date" $ "Date: "
          br
          input ! type_ "text" ! name "date" ! value (textValue today)
        div $ do
          label ! for "audioFile" $ "Audio file: "
          br
          input ! type_ "file" ! name "audioFile" ! id "audioFile"
          br
          progress ! id "progressBar" ! value "0" ! max "100" $ ""
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
          span ! id "errorMessage" ! A.style "color: red" $ ""
          button ! type_ "button" ! onclick "formSubmit()" ! autofocus "autofocus" $ "submit"

homepage :: [Episode] -> Html
homepage episodes = docTypeHtml $ do
  head $ do
    meta ! content "text/html;charset=utf-8" ! httpEquiv "Content-Type"
    meta ! content "utf-8" ! httpEquiv "encoding"
    title "Völlig irrelevant - Der Podcast"
    link ! rel "stylesheet" ! href (textValue $ staticFile "styles.css")
  body $ forM_ episodes $ \Episode{..} ->
    div $ do
      div $ string $ formatTime defaultTimeLocale "%F" episodePubdate
      h3 $ text episodeTitle
      audio ! controls "controls" ! preload "none" $
        source ! src (textValue $ mkFileUrl episodeFtExtension episodeSlug)
      div $ text episodeDescription
