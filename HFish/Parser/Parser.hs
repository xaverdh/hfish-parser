{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module HFish.Parser.Parser where

import qualified HFish.Parser.Redirect as Redirect
import HFish.Parser.Common
import HFish.Parser.Gen
import HFish.Parser.Glob
import Fish.Lang
import Fish.Lang.Unit

import Text.Parser.Permutation
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Char hiding (space,spaces)
import Data.Functor
import Data.Bool
import Data.Semigroup hiding (option)
import Data.NText
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.List.NonEmpty as N
import Control.Applicative
import Control.Monad
import Control.Monad.State

program :: P m => m (Prog T.Text ())
program = prog <* eof

prog :: P m => m (Prog T.Text ())
prog = stmtSep *>
  ( Prog ()
    <$> (compStmt `sepEndBy` stmtSep1) )
  <?> "code-block"

progN :: P m => m (Prog T.Text ())
progN = stmtSep1 *>
  ( Prog ()
    <$> (compStmt `sepEndBy` stmtSep1) )
  <?> "code-block"

exprs :: P m => m (Exprs T.Text ())
exprs = Exprs ()
  <$> (expr `sepEndBy` spaces1)
  <?> "expressions"

compStmt :: P m => m (CompStmt T.Text ())
compStmt = do
  st <- stmt
  option (Simple () st)
    ( spaces *>
      ( piped st <|> forked st ) )
  where
    piped st = Piped ()
      <$> ( sym "|" $> Fd1
            <|> sym "1>|" $> Fd1
            <|> sym "2>|" $> Fd2 )
      <*> return st
      <*> compStmt
      <?> "pipe"

    forked st =
      sym "&"
      $> Forked () st
      <?> "fork-symbol"

stmt :: P m => m (Stmt T.Text ())
stmt = do
  st <- plain
  option st $ try (RedirectedSt () st <$> redirects)
  <?> "statement"
  where
    redirects = 
      N.fromList
      <$> some redirect
      <?> "redirections"
    
    plain = choice [
        commentSt
        ,funSt
        ,whileSt
        ,forSt
        ,ifSt
        ,switchSt
        ,beginSt
        ,andSt
        ,orSt
        ,notSt
        ,cmdSt
      ]

commentSt :: P m => m (Stmt T.Text ())
commentSt = (CommentSt () . T.pack)
  <$> (char '#' *> many (notChar '\n'))
  <?> "comment-statement"

cmdSt :: P m => m (Stmt T.Text ())
cmdSt = CmdSt () <$> (expr `sepEndByNonEmpty` spaces1)
  <?> "command-statement"

funSt :: P m => m (Stmt T.Text ())
funSt = sym1 "function" *> (
    FunctionSt ()
    <$> lexemeN funIdent
    <*> exprs
    <*> progN
  ) <* symN "end"
  <?> "function-statement"

whileSt :: P m => m (Stmt T.Text ())
whileSt = sym1 "while" *>
  (WhileSt ()
    <$> stmt
    <*> progN <* symN "end")
  <?> "while-statement"

forSt :: P m => m (Stmt T.Text ())
forSt = sym1 "for" *>
  ( ForSt ()
    <$> (lexeme1 varIdent <* sym1 "in")
    <*> exprs
    <*> progN ) <* symN "end"
  <?> "for-statement"

ifSt :: P m => m (Stmt T.Text ())
ifSt =
  ( IfSt () . N.fromList
    <$> ( (:) <$> ifblock <*> many elif )
    <*> optional el ) <* symN "end"
  <?> "if-statement"
  where
    ifblock = sym1 "if" *>
      ((,) <$> stmt <*> progN)
    elif =
      lexeme1 (string "else" *> spaces1 *> string "if")
      *> ((,) <$> stmt <*> progN)
    el = symN "else" *> progN

switchSt :: P m => m (Stmt T.Text ())
switchSt = sym1 "switch" *>
  ( SwitchSt ()
    <$> lexemeN expr
    <*> ( stmtSep1 *>
          ( N.fromList <$> some switchCase ) ) )
  <* symN "end"
  <?> "switch-statement"
  where
    switchCase = sym1 "case" *>
      ( (,) <$> lexemeN expr <*> progN )

beginSt :: P m => m (Stmt T.Text ())
beginSt = symN "begin"
  *> ( BeginSt () <$> progN )
  <* symN "end"


andSt :: P m => m (Stmt T.Text ())
andSt = sym1 "and"
  *> (AndSt () <$> stmt)
  <?> "and-statement"

orSt :: P m => m (Stmt T.Text ())
orSt = sym1 "or"
  *> (OrSt () <$> stmt)
  <?> "or-statement"

notSt :: P m => m (Stmt T.Text ())
notSt = sym1 "not"
  *> (NotSt () <$> stmt)
  <?> "not-statement"

expr :: P m => m (Expr T.Text ())
expr = do
  s <- exprSimple
  option s (ConcatE () s <$> expr)
  <?> "expression"
  where
    exprSimple = choice
      [ strlike
        ,varRefE
        ,cmdSubstE
        ,bracesE
        ,globE
        ,homeDirE ]

varRefE :: P m => m (Expr T.Text ())
varRefE =
  ( VarRefE () False <$> varRef False )
  <|> ( VarRefE () True <$> varRef True )
  <?> "variable-reference"

bracesE :: P m => m (Expr T.Text ())
bracesE = BracesE () <$> (
    start *> body <* end
  ) <?> "braces-substitution"
  where
    start = char '{'
    end = char '}' <?> "end of braces-substitution"
    body = expr `sepBy` char ','

cmdSubstE :: P m => m (Expr T.Text ())
cmdSubstE = CmdSubstE ()
  <$> cmdRef
  <?> "command-substitution"

globE :: P m => m (Expr T.Text ())
globE = GlobE ()
  <$> glob
  <?> "glob-pattern"

homeDirE :: P m => m (Expr T.Text ())
homeDirE = char '~' $> HomeDirE () <?> "~"

redirect :: P m => m (Redirect (Expr T.Text ()))
redirect = Redirect.redirect (lexemeN expr)

ref :: P m => m i -> m (Ref i)
ref q =
  optional (start *> body <* end)
  <?> "array-reference"
  where
    start = sym "[" <?> "index-range"
    end = char ']' <?> "end of index-range"
    body = range `sepEndBy` spaces1
    range = do
      i <- q
      option (Index i) ( Range i <$> (sym "..." *> q) )

cmdRef :: P m => m (CmdRef T.Text ())
cmdRef = 
  CmdRef ()
  <$> body
  <*> ref expr
  where
    body = start *> prog <* end
    start = sym "(" <?> "command-substitution"
    end = char ')' <?> "end of command-substitution"

varRef :: P m => Bool -> m (VarRef T.Text ())
varRef q = VarRef ()
  <$> try name
  <*> ref expr
  <?> "variable-reference"
  where
    name = char (if q then '&' else '$') *> 
      parseEither (varRef q) varIdent

strlike :: P m => m (Expr T.Text ())
strlike = 
  strSQ
  <|> strDQ
  <|> strNQ
  <?> "string"

strSQ :: P m => m (Expr T.Text ())
strSQ = (StringE () . pack)
  <$> ( start *>
        strGen allowed escPass escSwallow escIgnore allowEmpty
        <* end )
  where
    start = char '\''
    end = char '\'' <?> "end of string"
    allowed = noneOf' "'\\"
    escPass = oneOf' "'\\"
    escSwallow = mzero
    escIgnore = noneOf' "'\\"
    allowEmpty = True

strDQ :: P m => m (Expr T.Text ())
strDQ = (StringE () . pack)
  <$> ( start *>
        strGen allowed escPass escSwallow escIgnore allowEmpty  
        <* end )
  where
    start = char '"'
    end = char '"' <?> "end of string"
    allowed = noneOf' "\"\\\n"
    escPass = oneOf' "\"\\"
    escSwallow = char '\n'
    escIgnore = noneOf' "\n\"\\"
    allowEmpty = True

strNQ :: P m => m (Expr T.Text ())
strNQ = do
  (StringE () . pack)
  <$> strGen allowed escPass escSwallow escIgnore allowEmpty
  where
    invalid = 
      "\n\t $&\\*?~%#(){}[]<>^;,\"\'|012."
    allowed =
      noneOf' invalid
      <|> try ( oneOf' "012" <* notFollowedBy (oneOf' "><") )
      <|> try ( char '.' <* notFollowedBy (string "..") )
    escPass =
      (escapeSequence <$> oneOf' "ntfrvabe")
      <|> try (char 'c' *> anyChar >>= controlSequence)
      <|> notChar '\n'
    escSwallow = char '\n'
    escIgnore = mzero
    allowEmpty = False

    escapeSequence :: Char -> Char
    escapeSequence = \case
      'n' -> '\n'
      't' -> '\t'
      'f' -> '\f'
      'r' -> '\r'
      'v' -> '\v'
      'a' -> '\a'
      'b' -> '\b'
      'e' -> C.chr 0x1B
    
    controlSequence :: MonadPlus m => Char -> m Char
    controlSequence = \case
      '@'  -> return (C.chr 0)
      '['  -> return (C.chr 0x1B)
      '\\' -> return (C.chr 0x1C)
      ']'  -> return (C.chr 0x1D)
      '^'  -> return (C.chr 0x1E)
      '_'  -> return (C.chr 0x1F)
      '?'  -> return (C.chr 0x7F)
      c -> 
        let i = C.ord c - C.ord 'a'
         in if i >= 0 && i <= C.ord 'z'
            then return (C.chr i)
            else mzero

varIdent :: P m => m (VarIdent T.Text ())
varIdent = (VarIdent () . mkNText . T.pack)
  <$> ((:) <$> letter <*> many (alphaNum <|> char '_'))
  <?> "variable-identifier"

funIdent :: P m => m (FunIdent T.Text ())
funIdent = (FunIdent () . mkNText . T.pack)
  <$> some ( alphaNum <|> oneOf "_-" )
  <?> "function-identifier"

