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

textarea#description {
  width: 400px;
}

div#submit {
  text-align: right;
}
|]

uploadForm :: Html
uploadForm = docTypeHtml $ do
  head $ do
    title "Upload new episode"
    style myStyle
  body $
    form ! action "" ! method "post" ! enctype "multipart/form-data" $ do
      div $ do
        label ! for "title" $ "Title: "
        br
        input ! type_ "text" ! name "title" ! id "title"
      div $ do
        label ! for "audioFile" $ "Audio file: "
        br
        input ! type_ "file" ! name "audioFile"
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
