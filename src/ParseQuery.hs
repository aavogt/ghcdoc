{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- specifically I want to get rid of the warning about tail from lbnf
{-# OPTIONS_GHC -Wno-extra #-}

module ParseQuery (parseNameQS, NameQ (.., NameQ), pol, name, pres, Name (..), PM (..), ParseMonad (..)) where

import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import Language.LBNF

bnfc
  [lbnf|
  token Name (letter | digit | '_' | '\'' | '.')+;

  token PM ["!◇+-"];
  separator PM "";
  NameQ_.NameQ ::= [PM] Name [PM];
  
  entrypoints NameQ |]

-- |
--
-- Queries are type Name together with a PMT for their variance as well as presence.
-- The first +- specifies variance. For `f :: A -> B` `A -A B +B` all match, while
-- `-B +A` do not match.
--
-- The second +- is for adding (default) or removing from the result set. In other
-- words, for `f :: A -> B`, `A- B- -A- +B-` will remove it, but `+A- -B-` will
-- have no effect as they do not match.
--
-- A trailing `!` (equivalently `◇` for necessary) changes the treatment of sum
-- types (or multiple occurences of a type).
--
-- > f :: Either A B -> C  -- A! does not match, because f may take a Right B
-- > g :: A -> Either B C  -- A! matches
--
-- f should be reconsidered. Possibility in negative position is not a problem,
-- because the caller of the function can always choose to pass a Left A. Whereas
-- possibility in the positive position means that you could call the function and
-- get a Left B instead of the desired Right A.
parseNameQS :: String -> ParseMonad [NameQ]
parseNameQS = mapM (pNameQ . myLexer) . words

splitNec :: [PM] -> Maybe (Bool, Set Bool)
splitNec pms = case ss of
  "" -> Nothing
  _ -> Just (any (`elem` ss) "◇!", S.fromList $ [c == '+' | c <- ss, c `elem` "+-"])
  where
    ss = concat [pm | PM pm <- pms]

-- |
--
-- Pretend that bnfc's easy-to-parse
--
-- > data NameQ = NameQ_ [PM] Name [PM]
--
-- was defined as:
--
-- > data NameQ  = NameQ (Bool, Set Bool) Name (Bool, Set Bool)
--
-- where (Bool, Set Bool) is the result of
--
-- "!+-" into (True, {False, True})
-- "+" into (False, {True})
--
-- The first tuple is for variance/ +/- position, the second for presence.
pattern NameQ {pol, name, pres} <- NameQ_ (splitNec -> pol) (Name name) (splitNec -> pres)
