{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Search.QQ where

import Language.Haskell.TH.Quote
import Search.FromHoogle
import ParseHoogle
import Data.Either
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Debug.Trace

deriving instance Lift Tables
deriving instance Lift ParseHoogle.Name
deriving instance Lift Typ
deriving instance Lift Sym
deriving instance Lift Symb
deriving instance Lift TVar

tables :: QuasiQuoter
tables = QuasiQuoter { quoteExp = warnFst . partitionEithers . parseHoogleString . stripLeadingSpace }

stripLeadingSpace :: String -> String
stripLeadingSpace = unlines . lines

warnFst :: ([(String, String)], [Decs]) -> ExpQ
warnFst (warnings, oks) = do
  unless (null warnings) $ reportWarning (show warnings)
  lift (makeTables (map Right oks))
