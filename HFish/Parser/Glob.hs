module Fish.Parser.Glob where

import Fish.Parser.Common
import Fish.Lang.Lang

import Text.Parser.Combinators
import Text.Parser.Char hiding (space,spaces)
import Data.Functor
import Control.Applicative
import Control.Monad

glob :: PC m => P m Glob
glob = qmark <|> star <?> "glob-pattern"
  where
    qmark = char '?' $> QMarkGl
    star =
      char '*'
      *> option StarGl (char '*' $> DiStarGl)
