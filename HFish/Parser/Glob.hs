module HFish.Parser.Glob where

import HFish.Parser.Common
import Fish.Lang

import Text.Parser.Combinators
import Text.Parser.Char hiding (space,spaces)
import Data.Functor
import Control.Applicative
import Control.Monad

glob :: P p m => m Glob
glob = stars <?> "glob-pattern"
  where
    stars =
      char '*'
      *> option StarGl (char '*' $> DiStarGl)

