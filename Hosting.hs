{-# LANGUAGE OverloadedStrings #-}

module Hosting
  ( module Hosting
  ) where

import Data.Text (Text)

mediaDir :: FilePath
mediaDir = "/root/media"

protocol :: Text
protocol = "https://"

podcastLink :: Text
podcastLink = "podcast.rubenmoor.net"

mediaLink :: Text
mediaLink = "podcast-media.rubenmoor.net"

-- https://dts.podtrac.com/redirect.m4a/podcast-media.rubenmoor.net/2020-11-15_BANANE.m4a
mkFileUrl :: Text -> Text -> Text
mkFileUrl filetypeExtension slug =
     protocol
  <> "dts.podtrac.com/redirect"
  <> filetypeExtension
  <> "/" <> mediaLink <> "/"
  <> slug <> filetypeExtension
