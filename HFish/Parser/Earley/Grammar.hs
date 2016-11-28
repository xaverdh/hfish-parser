{-# language RecursiveDo, OverloadedStrings #-}
module Fish.Parser.Earley.Grammar where

import Text.Earley
import Fish.Lang.Lang

import qualified Data.Text as T
import Data.Maybe

tkFunc = namedToken "function"

mkPiped = flip (Piped ())
option d p = fromMaybe d <$> optional p
gram :: Grammar r (Prod r T.Text T.Text (Prog ()))
gram = mdo
  prog <- rule $ Prog () <$> stmtSep *> many (compStmt <* stmtSep)
          <?> "code-block"
  
  compStmt <- rule $ simple <|> piped <|> forked
  simple <- rule $ Simple () <$> stmt
  piped <- rule $ mkPiped
    <$> option StdOutFd (outFd <* namedToken '>') 
        <* namedToken '|' <* spaces
    <*> stmt <*> compStmt
  forked <- rule $ Forked () <$> stmt <* namedToken '&' <* spaces
  
  stmt <- rule $ plain <|> redirected
  plain <- rule $ choice
    [ commentSt ]
  
  redirected <- RedirectedSt () <$> plain <*> redirections
  
  redirections <- rule $
    some (spaces *> redirect)
    <?> "redirection"
  
  redirect <- Redirect () <$> choice [] <?> "redirection"
  
  commentSt <- CommentSt () . T.pack <$> namedToken "#" *> many (satisfy (/="\n"))
  
  return prog


