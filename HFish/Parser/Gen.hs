{-# language RankNTypes #-}
module HFish.Parser.Gen where

import HFish.Parser.Common

import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Char hiding (space,spaces)
import Data.Maybe (catMaybes)
import Control.Applicative

strGen :: P m 
  => m Char
  -> m Char
  -> m Char
  -> m Char
  -> Bool
  -> m String

strGen allowed escPass escSwallow escIgnore allowEmpty =
  catMaybes <$> (if allowEmpty then many else some) charGen
  where
    charGen = (Just <$> allowed) <|> try escaped
    escaped = char '\\' *> ( ignore <|> pass <|> swallow )
    pass = Just <$> escPass
    swallow = escSwallow *> return Nothing
    ignore = Just <$> ( lookAhead escIgnore *> return '\\' )




