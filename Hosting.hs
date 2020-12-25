{-# LANGUAGE OverloadedStrings #-}

module Hosting
  ( module Hosting
  ) where

import Data.Text (Text)

mediaDir :: FilePath
mediaDir = "/root/static/media"

protocol :: Text
protocol = "https://"

podcastLink :: Text
podcastLink = "podcast.rubenmoor.net"

staticLink :: Text
staticLink = "podcast-static.rubenmoor.net"

mediaLink :: Text
mediaLink = staticLink <> "/media"

-- https://dts.podtrac.com/redirect.m4a/podcast-static.rubenmoor.net/media/2020-11-15_BANANE.m4a
mkFileUrl :: Text -> Text -> Text
mkFileUrl filetypeExtension slug =
     protocol
  <> "dts.podtrac.com/redirect"
  <> filetypeExtension
  <> "/" <> mediaLink <> "/"
  <> slug <> filetypeExtension
