module HFish.Parser.Attoparsec where

import qualified HFish.Parser.Parser as P
import Fish.Lang
import Data.Attoparsec.Text

program :: Parser (Prog ())
program = P.program

