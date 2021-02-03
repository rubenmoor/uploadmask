{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Html
  ( uploadForm
  , homepage
  , episode
  , formatDuration
  , SortBy (..)
  , Order (..)
  ) where

import           Text.Printf                   (printf)
import           Control.Monad               (forM_)
import           Data.Text                   (Text, toLower, null, length, take)
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

import           Prelude                     ((<), Maybe (..), (*), mod, Int, ($), (<>), (-))

-- TODO
-- -[ ] custom player to allow css
-- -[ ] add schnack comments
-- -[ ] prerequisite: page per episode
-- -[ ] embed video
-- -[ ] extend basic styling to be presentable

data Order = OrderDescending | OrderAscending
data SortBy = SortByDate Order

mkShortTitle :: Episode -> Text
mkShortTitle Episode{..} =
  let title = toLower $ "#" <> episodeCustomIndex <> " " <> episodeTitle
  in  if length title < 18
      then title
      else take 17 title <> "..."

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
      htmlHead staticLoc
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

platformLinks =
  div ! id "platformlinks" $ do
    a ! href "https://t.me/fullserendipity"
      ! A.title "Join our telegram channel" $
      i ! class_ "fab fa-telegram" $ ""
    a ! href "https://open.spotify.com/show/74AvChSw5sZM9kOIpWt040?si=L8TwbVb8SQOPcFxRVgNqBQ"
      ! A.title "Listen on Spotify" $
      i ! class_ "fab fa-spotify" $ ""
    a ! href "/feed.xml"
      ! A.title "Get the rss-feed" $
      i ! class_ "fas fa-rss" $ ""

homepage :: Text -> [Episode] -> SortBy -> Html
homepage staticLoc episodes sortBy = docTypeHtml $ do
  head $ htmlHead staticLoc
  body $ do
    div ! id "header" $ do
      div ! id "title" $ do
        h2 ! id "allEpisodes" $ text "Alle Folgen"
        div ! id "serendipityworks" $ text "serendipity.works"
      div ! id "gradient" $ ""
    platformLinks
    div ! id "content" $ do
      div ! id "searchparameters" $
        case sortBy of
          SortByDate order -> case order of
            OrderAscending -> do
              a ! href (textValue "/?sortby=date&order=desc") ! A.title "Nach absteigendem Datum sortieren" $
                text "Neueste zuerst"
              " | "
              strong "Älteste zuerst"
            OrderDescending -> do
              strong "Neueste zuerst"
              " | "
              a ! href (textValue "/?sortby=date&order=asc") ! A.title "Nach aufsteigendem Datum sortieren" $
                text "Älteste zuerst"
      forM_ episodes $ \Episode{..} ->
        div ! class_ "episode" $ do
          h3 $ a ! href (textValue $ "/" <> episodeSlug) ! A.title "episode details" $
            text $ "#" <> episodeCustomIndex <> " " <> toLower episodeTitle
          span ! class_ "pubdate" $ string $ formatTime defaultTimeLocale "%F" episodePubdate
          div ! class_ "belowTitle" $ ""
          audio ! class_ "list" ! controls "controls" ! preload "none" $
            source ! src (textValue $ mkFileUrl staticLoc episodeFtExtension episodeSlug)
          div ! class_ "duration" $ text $ "Duration: " <> formatDuration episodeDuration
          br ! A.style "clear:both"
          div ! class_ "description" $ text episodeDescriptionLong
          div ! class_ "belowDescription" $ ""
          div ! class_ "footer" $
            span ! class_ "download" $ do
              "("
              a ! href (textValue $ mkFileUrl staticLoc episodeFtExtension episodeSlug)
                ! A.title (textValue episodeFtExtension)
                ! class_ "download"
                ! customAttribute "download" (textValue episodeSlug)
                $ "download"
              ")"

episode :: Text -> Episode -> Maybe Episode -> Maybe Episode -> Html
episode staticLoc e prev next = docTypeHtml $ do
  head $ htmlHead staticLoc
  body $ do
    div ! id "header" $ do
      div ! id "title" $ do
        div ! id "episodeDate" $ string $ formatTime defaultTimeLocale "%b %d, '%y" (episodePubdate e)
        h2 ! id "episodeTitle" $ text $ "#" <> episodeCustomIndex e <> " " <> toLower (episodeTitle e)
        div ! id "serendipityworks" $ text "serendipity.works"
      div ! id "gradient" $ ""
    platformLinks
    div ! id "navigation" $ do
      div ! id "leftOfPrevious" $ ""
      div ! id "previous" $
        case prev of
          Just prevE ->
            a ! class_ "pnlink"
              ! href (textValue $ "/" <> episodeSlug prevE)
              ! A.title (textValue $ episodeTitle prevE) $ do
              span ! class_ "pncaption" $ "Vorige Folge"
              br
              span ! class_ "pnpubdate" $
                string $ formatTime defaultTimeLocale "%b %d, '%y" (episodePubdate prevE)
              br
              span ! class_ "pntitle" $ text $ mkShortTitle prevE
          Nothing -> div ! class_ "deactivated" $ "Keine älteren Folgen"
      div ! id "toHome" $
        a ! href "/" ! A.title "Zurück zur Liste aller veröffentlichten Folgen" $
          "Alle Folgen"
      div ! id "next"  $
        case next of
          Just nextE -> do
            a ! class_ "pnlink"
              ! href (textValue $ "/" <> episodeSlug nextE)
              ! A.title (textValue $ episodeTitle nextE) $ do
              span ! class_ "pncaption" $ "Nächste Folge"
              br
              span ! class_ "pnpubdate" $
                string $ formatTime defaultTimeLocale "%b %d, '%y" (episodePubdate nextE)
              br
              span ! class_ "pntitle" $ text $ mkShortTitle nextE
          Nothing -> div ! class_ "deactivated" $ "Keine neueren Folgen"
    div ! id "body" $
      div ! id "contents" $ do
        div ! id "left" $
          if null (episodeVideoUrl e)
          then div ! class_ "deactivated" $ "No associated video found"
          else iframe ! width "580"
                      ! height "420"
                      ! src (textValue $ episodeVideoUrl e)
                      ! customAttribute "allow" "fullscreen"
                      $ ""
        div ! id "right" $ do
          div ! id "audio" $ do
            audio ! id "single" ! controls "controls" ! preload "none" $
              source ! src (textValue $ mkFileUrl staticLoc (episodeFtExtension e) (episodeSlug e))
            div ! class_ "duration" $ text $ "Duration: " <> formatDuration (episodeDuration e)
            span ! class_ "download" $ do
              "("
              a ! href (textValue $ mkFileUrl staticLoc (episodeFtExtension e) (episodeSlug e))
                ! A.title (textValue $ episodeFtExtension e)
                ! class_ "download"
                ! customAttribute "download" (textValue $ episodeSlug e)
                $ "download"
              ")"
          div ! id "description" $ text $ episodeDescriptionLong e
    div ! class_ "comments"
        ! id (textValue $ "comments-div-" <> (episodeSlug e))
        $ ""
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

htmlHead :: Text -> Html
htmlHead staticLoc = do
  meta ! content "text/html;charset=utf-8" ! httpEquiv "content-type"
  meta ! content "utf-8" ! httpEquiv "encoding"
  title "serendipity works - Der Podcast"
  link ! rel "stylesheet" ! href (textValue $ staticLoc <> "/styles.css")
  link ! rel "preconnect" ! href "https://fonts.gstatic.com"
  link ! href "https://fonts.googleapis.com/css2?family=Abel&display=swap" ! rel "stylesheet"
  link ! rel "shortcut icon" ! href "https://podcast-static.serendipity.works/favicon.ico"
  -- Font Awesome 5.13 free content -->
  link ! rel "stylesheet" ! href (textValue $ staticLoc <> "/FontAwesome/css/all.min.css")
