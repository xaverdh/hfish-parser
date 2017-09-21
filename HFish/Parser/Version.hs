{-# language TemplateHaskell #-}
module HFish.Parser.Version where

import Development.GitRev

version :: String
version = $(gitHash)
