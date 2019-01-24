{-# LANGUAGE OverloadedStrings #-}

module LucidExtensions where

import Data.Text (Text)

import Lucid.Base

aria_describedby_ :: Text -> Attribute
aria_describedby_ = makeAttribute "aria-describedby"