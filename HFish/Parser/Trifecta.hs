module HFish.Parser.Trifecta where

import qualified HFish.Parser.Parser as P
import qualified HFish.Parser.Common as C
import Fish.Lang
import Text.Trifecta.Parser

program :: Parser (Prog ())
program = P.program

