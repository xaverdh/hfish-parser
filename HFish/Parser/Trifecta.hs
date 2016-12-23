module HFish.Parser.Trifecta where

import qualified HFish.Parser.Parser as P
import qualified HFish.Parser.Common as C
import Fish.Lang
import qualified Data.Text as T
import Text.Trifecta.Parser

program :: Parser (Prog T.Text ())
program = P.program

