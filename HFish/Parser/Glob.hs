module HFish.Parser.Glob where

import HFish.Parser.Common
import HFish.Lang.Lang

import Text.Parser.Combinators
import Text.Parser.Char hiding (space,spaces)
import Data.Functor
import Control.Applicative
import Control.Monad

glob :: P m => m Glob
glob = qmark <|> star <?> "glob-pattern"
  where
    qmark = char '?' $> QMarkGl
    star =
      char '*'
      *> option StarGl (char '*' $> DiStarGl)

