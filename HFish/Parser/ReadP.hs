module HFish.Parser.ReadP where

import qualified HFish.Parser.Parser as P
import HFish.Lang.Lang
import Text.ParserCombinators.ReadP

program :: ReadP (Prog ())
program = P.program
