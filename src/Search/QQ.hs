{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Search.QQ where

import Control.Monad
import Data.Char
import Data.Either
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import ParseHoogle
import Query
import Search.FromHoogle
import Text.Show.Pretty

deriving instance Lift Tables

deriving instance Lift ParseHoogle.Name

deriving instance Lift Typ

deriving instance Lift Sym

deriving instance Lift Symb

deriving instance Lift TVar

deriving instance Lift NameQ

deriving instance Lift Query.Name

deriving instance Lift PM

tracing = True

query :: QuasiQuoter
query = QuasiQuoter {quoteExp = quoteQuery}

quoteQuery str =
  [|
    do
      let Ok a = parseNameQS str
      when tracing do
        putStr $ "[query|" ++ str ++ "|] ===> "
        pPrint a
      return a
    |]

tables :: QuasiQuoter
tables = QuasiQuoter {quoteExp = \s -> warnFst s . partitionEithers . parseHoogleString $ stripLeadingSpace s}

stripLeadingSpace :: String -> String
stripLeadingSpace = unlines . lines

warnFst :: String -> ([(String, String)], [Decs]) -> ExpQ
warnFst orig (warnings, oks) = do
  unless (null warnings) $ reportWarning (show warnings)
  [|
    do
      let orig2 = unlines (map (dropWhile isSpace) (lines orig))
          tab = $(lift (makeTables (map Right oks)))
      when tracing $ putStrLn ("\n [tables|" ++ orig2 ++ "|] ===> " ++ ppShow tab)
      return tab
    |]
