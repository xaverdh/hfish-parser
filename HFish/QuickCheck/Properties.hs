{-# language OverloadedStrings #-}
module HFish.QuickCheck.Properties where

import Fish.Lang

import HFish.Parser.Parser
import HFish.Parser.Common
import HFish.Parser.Glob

import HFish.UnParser.UnParser

import HFish.QuickCheck.Arbitrary

import Data.Semigroup
import qualified Data.Text as T
import Data.Attoparsec.Text
import Test.QuickCheck hiding (Args)

attoParse p = parseOnly (p <* endOfInput)

parseUnparse p x = 
  attoParse p (unparse x)
  == Right x

parseUnparse' p x = 
  attoParse p (unparse x <> " ")
  == Right x

checkParseUnparse = do
  -- quickCheck
  -- quickCheck
  -- quickCheck
  quickCheck prop_parse_unparse_glob
  quickCheck prop_parse_unparse_varIdent
  quickCheck prop_parse_unparse_funIdent
  quickCheck prop_parse_unparse_cmdIdent
  -- quickCheck prop_parse_unparse_program

prop_parse_unparse_glob :: Glob -> Bool
prop_parse_unparse_glob =
  parseUnparse glob

prop_parse_unparse_varIdent :: VarIdent T.Text () -> Bool
prop_parse_unparse_varIdent = 
  parseUnparse varIdent

prop_parse_unparse_funIdent :: FunIdent T.Text () -> Bool
prop_parse_unparse_funIdent = 
  parseUnparse funIdent

prop_parse_unparse_cmdIdent :: CmdIdent T.Text () -> Bool
prop_parse_unparse_cmdIdent =
  parseUnparse cmdIdent

prop_parse_unparse_program :: Prog T.Text () -> Bool
prop_parse_unparse_program =
  parseUnparse program


