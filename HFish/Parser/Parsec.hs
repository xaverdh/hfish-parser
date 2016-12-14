{-# language FlexibleContexts #-}
module HFish.Parser.Parsec where

import qualified HFish.Parser.Parser as P
import Fish.Lang
import Text.Parsec
import Data.Functor.Identity

program :: Stream s Identity Char => Parsec s () (Prog ())
program = P.program
