{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Html
  ( uploadForm
  , homepage
  , episode
  , formatDuration
  ) where

import           Text.Printf                   (printf)
import           Control.Monad               (forM_)
import           Data.Text                   (Text, toLower, null)
import qualified Data.Text as Text
import           Data.Time.Format            (defaultTimeLocale, formatTime)
import           Database.Gerippe            (Entity (..), keyToId)
import           Text.Blaze.Html             (Html)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (form, label, span, style,
                                              title)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.RawString.QQ
import           TextShow                    (showt)

import           Hosting                     (schnackUrl, mkFileUrl, protocol)
import           Model                       (Episode (..))
import qualified GHC.Real as Real

import           Prelude                     ((*), mod, Int, ($), (<>), (-))

-- TODO
-- -[ ] custom player to allow css
-- -[ ] add schnack comments
-- -[ ] prerequisite: page per episode
-- -[ ] embed video
-- -[ ] extend basic styling to be presentable

formatDuration :: Int -> Text
formatDuration d =
  let seconds = d `mod` 60
      minutes = (d `Real.div` 60) `mod` 60
      hours = d `Real.div` (60 * 60)
  in  Text.pack $ printf "%02d:%02d:%02d" hours minutes seconds

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

uploadForm :: Text -> Text -> Text -> Html
uploadForm staticLoc today currentIndex =
  docTypeHtml $ do
    head $ do
      meta ! content "text/html;charset=utf-8" ! httpEquiv "Content-Type"
      meta ! content "utf-8" ! httpEquiv "encoding"
      title "Upload new episode"
      link ! rel "stylesheet" ! href (textValue $ staticLoc <> "/styles.css")
      link ! rel "preconnect" ! href "https://fonts.gstatic.com"
      link ! href "https://fonts.googleapis.com/css2?family=Abel&display=swap" ! rel "stylesheet"
      script ! src "https://unpkg.com/mediainfo.js@0.1.4/dist/mediainfo.min.js" $ ""
      script myScript
    body $
      form ! action "" ! method "post" ! enctype "multipart/form-data" ! id "form" $ do
        div $ do
          label ! for "index" $ "Index: "
          br
          input ! type_ "text"
                ! name "index"
                ! id "index"
                ! value (textValue currentIndex)
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

homepage :: Text -> [Episode] -> Html
homepage staticLoc episodes = docTypeHtml $ do
  head $ do
    meta ! content "text/html;charset=utf-8" ! httpEquiv "Content-Type"
    meta ! content "utf-8" ! httpEquiv "encoding"
    title "Völlig irrelevant - Der Podcast"
    link ! rel "stylesheet" ! href (textValue $ staticLoc <> "/styles.css")
    link ! rel "preconnect" ! href "https://fonts.gstatic.com"
    link ! href "https://fonts.googleapis.com/css2?family=Abel&display=swap" ! rel "stylesheet"
  body $
    div ! id "content" $
      forM_ episodes $ \Episode{..} ->
        div ! class_ "episode" $ do
          h3 $ a ! href (textValue $ "/" <> episodeSlug) ! A.title "episode details" $
            text $ "#" <> episodeCustomIndex <> " " <> toLower episodeTitle
          span ! class_ "pubdate" $ string $ formatTime defaultTimeLocale "%F" episodePubdate
          div ! class_ "belowTitle" $ ""
          audio ! class_ "list" ! controls "controls" ! preload "none" $
            source ! src (textValue $ mkFileUrl staticLoc episodeFtExtension episodeSlug)
          div ! class_ "duration" $ text $ "Duration: " <> formatDuration episodeDuration
          div ! class_ "description" $ text episodeDescriptionLong
          div ! class_ "belowDescription" $ ""
          div ! class_ "footer" $
            span ! class_ "download" $ do
              "("
              a ! href (textValue $ mkFileUrl staticLoc episodeFtExtension episodeSlug)
                ! A.title (textValue episodeFtExtension)
                ! class_ "download"
                $ "download"
              ")"

episode :: Text -> Episode -> Html
episode staticLoc Episode{..} = docTypeHtml $ do
  head $ do
    meta ! content "text/html;charset=utf-8" ! httpEquiv "Content-Type"
    meta ! content "utf-8" ! httpEquiv "encoding"
    title "Völlig irrelevant - Der Podcast"
    link ! rel "stylesheet" ! href (textValue $ staticLoc <> "/styles.css")
    link ! rel "preconnect" ! href "https://fonts.gstatic.com"
    link ! href "https://fonts.googleapis.com/css2?family=Abel&display=swap" ! rel "stylesheet"
    script
      ! type_ "text/javascript"
      ! src (textValue schnackUrl)
      ! dataAttribute "schnack-target-class" ".comments"
      ! dataAttribute "schnack-target" ".comments"
      ! dataAttribute "schnack-slug" "rubm-luke"
      ! dataAttribute "schnack-partial-sign-in-via" "Zum Kommentieren anmelden"
      ! dataAttribute "schnack-partial-login-status" "Angemeldet als <strong>%USER%</strong> (<a class='schnack-signout' href='#'>abmelden</a>)."
      ! dataAttribute "schnack-partial-or" " "
      ! dataAttribute "schnack-partial-edit" "Bearbeiten"
      ! dataAttribute "schnack-partial-preview" "Vorschau"
      ! dataAttribute "schnack-partial-cancel" "Abbrechen"
      ! dataAttribute "schnack-partial-reply" "Antworten"
      ! dataAttribute "schnack-partial-send-comment" "Absenden"
      ! dataAttribute "schnack-partial-post-comment" "Dein Kommentar. Markdown **ist** erlaubt."
      ! dataAttribute "schnack-partial-mute" "mute"
      ! dataAttribute "schnack-partial-unmute" "unmute"
      ! dataAttribute "schnack-partial-admin-approval" "Waiting for approval"
      ! dataAttribute "schnack-partial-waiting-for-approval" "Dein Kommentar wird in Kürze freigegeben."
      $ ""
  body $ do
    div ! id "header" $ do
      div ! id "title" $ do
        a ! id "toHome" ! href "/" ! A.title "back to list of all episodes" $
          "‹ all episodes"
        h2 $ text $ "#" <> episodeCustomIndex <> " " <> toLower episodeTitle
        span ! id "episodeDate" $ string $ formatTime defaultTimeLocale "%F" episodePubdate
      div ! id "gradient" $ ""
    div ! id "body" $
      div ! id "contents" $ do
        div ! id "left" $
          if null episodeVideoUrl
          then span ! id "noVideoUrl" $ "No associated video found"
          else "embedded video goes here"
        div ! id "right" $ do
          div ! id "audio" $ do
            audio ! id "single" ! controls "controls" ! preload "none" $
              source ! src (textValue $ mkFileUrl staticLoc episodeFtExtension episodeSlug)
            span ! class_ "download" $ do
              "("
              a ! href (textValue $ mkFileUrl staticLoc episodeFtExtension episodeSlug)
                ! A.title (textValue episodeFtExtension)
                ! class_ "download"
                $ "download"
              ")"
          div ! id "description" $ text episodeDescriptionLong
    div ! class_ "comments"
        ! id (textValue $ "comments-div-" <> episodeSlug)
        $ ""
