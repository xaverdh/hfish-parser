{-# language LambdaCase, TupleSections #-}
module HFish.Parser.Redirect where

import HFish.Parser.Common
import HFish.Lang.Lang

import Text.Parser.Combinators
import Text.Parser.Char hiding (space,spaces)
import Data.Functor
import Data.Maybe
import Control.Applicative
import Control.Monad

redirect :: P m => m (Expr t) -> m (Redirect t)
redirect exp = do
  (fd,tk) <- redirectL
  redirectR exp fd tk
  <?> "redirection"

redirectL :: P m => m (Fd,RedirTk)
redirectL = try $ do
  mfd <- optional parseFd
  tk <- redirTk
  (,tk) <$> case tk of
    TkIn -> return $ fromMaybe Fd0 mfd
    TkOut _ -> return $ fromMaybe Fd1 mfd
    TkErr _ -> maybe (return Fd2) (const mzero) mfd

redirectR :: P m
  => m (Expr t) -> Fd -> RedirTk -> m (Redirect t)
redirectR exp fd tk = 
  parseEither fdR (spaces *> exp) >>= \case
    Left mfdr -> case mfdr of
      Nothing -> rClose
      Just fdr -> case tk of
        TkIn -> rIn (Left fdr)
        _ -> rOut (Left fdr)
    Right e -> case tk of
      TkIn -> rIn (Right e)
      TkErr mode -> rOut (Right (mode,e))
      TkOut mode -> rOut (Right (mode,e))
  where
    rIn = return . RedirectIn fd
    rOut = return . RedirectOut fd
    rClose = return $ RedirectClose fd
  
parseFd :: P m => m Fd
parseFd = (toEnum . read . pure) <$> digit

fdR :: P m => m (Maybe Fd)
fdR = lexeme $ char '&' *>
  ( ( Just <$> parseFd )
    <|> ( char '-' $> Nothing ) )

data RedirTk = 
  TkIn | TkOut FileMode | TkErr FileMode
  deriving (Eq,Ord,Show)

redirTk :: P m => m RedirTk
redirTk = choice
  [ char '<' $> TkIn
    ,char '>' *>
      option (TkOut FModeWrite)
      ( char '>' $> TkOut FModeApp
        <|> char '?' $> TkOut FModeNoClob )
    ,char '^' *>
      option (TkErr FModeWrite)
      ( char '^' $> TkErr FModeApp
        <|> char '?' $> TkErr FModeNoClob ) ]

