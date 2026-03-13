{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- | convert the "ParseHoogle" AST into 'Tables'
module Search.FromHoogle where

import ParseHoogle
import Data.Set (Set)
import Control.Lens
import Data.Map (Map)
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Char
import Control.Lens.Extras
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Generics.SYB hiding (Generic)
import Search.LensExtra

data Tables = Tables {dataTable :: DataTable, instanceTable :: InstanceTable, vpdMap :: VpdMap}

makeTables :: [Either a Decs] -> Tables
makeTables parses = Tables (makeDataTable d) (concatMap makeInstanceTable d) (vpdMap1s c)
  where
    d = concatMap (\(Decs x) -> x) c
    c = cleanLeadingHasType $ rights parses

-- | `[(String, [Name], [(String, Typ)])]` from 'dataTable'
--
-- >  , [ ( "Either"
-- >     , [ Name "a" , Name "b" ]
-- >     , [ ( "Left"
-- >         , Sym
-- >             (Iden (Name "a"))
-- >             (NormalSym (Symb "->"))
-- >             (App
-- >                (App (Iden (Name "Either")) (Iden (Name "a"))) (Iden (Name "b")))
-- >         )
-- >       , ( "Right"
-- >         , Sym
-- >             (Iden (Name "b"))
-- >             (NormalSym (Symb "->"))
-- >             (App
-- >                (App (Iden (Name "Either")) (Iden (Name "a"))) (Iden (Name "b")))
-- >         )
-- >       ]
--
-- Somehow this is going to be useful for deciding what is available or required
-- when there is an Either in a function type signature.
type DataTable = [(String, [Name], [(String, Typ)])]

-- | `[([(String, [Typ])], Typ, String, Int)]` from 'instanceTable'
type InstanceTable = [([(String, [Typ])], Typ, String, Int)]

vpdMap1s :: [Decs] -> VpdMap
vpdMap1s = M.unionsWith (M.unionWith (M.unionWith S.union)) . concatMap vpdMap1

-- > e :: D a => Int -> T b -> a
--
-- produces a
--
-- >    [ App (Iden (Name "D")) (Iden (Name "a")) ]
-- >      , fromList
-- >          [ ( Left "Int"
-- >            , fromList [ ( fromList [ False ] , fromList [ "e" ] ) ]
-- >            )
-- >          , ( Left "a"
-- >            , fromList [ ( fromList [ True ] , fromList [ "e" ] ) ]
-- >            )
-- >          , ( Right (App (Iden (Name "T")) (Iden (Name "b")))
-- >            , fromList [ ( fromList [ False ] , fromList [ "e" ] ) ]
-- >            )
-- >          ]
-- >      )
--
-- clearly Int and T b have nothing to do with the context
-- `T a` or `a` do.
vpdMap1 :: Decs -> [VpdMap]
vpdMap1 x@(Decs xs) =
  [ M.singleton (shrinkCxt v cxt) $
      M.singleton v $
        M.singleton polarities $
          S.singleton d
    | x <- xs,
      (Just d, vp@(VarPol (_, _, cxt))) <- [getVarPolDec x],
      (v, polarities) <- vp ^.. vpMap . collectSnd
  ]

-- | shrink the context excessively. That is,
--
-- > f :: (C a b, b ~ B) => a -> b
--
-- > shrinkCxt (Left "a") [C a b, b ~ B] ==> [C a b]
-- > shrinkCxt (Left "b") [C a b, b ~ B] ==> [C a b, b ~ B]
--
-- so eventually `f` may end up coming up in a search result
-- for a concrete A but there is no `C A B`. This is not wrong
-- maybe it prompts the user to write `instance C A B`
--
-- Type level strings are not treated properly, but I have to
-- add them to the parser first.
shrinkCxt :: Either String Typ -> Set Typ -> Set Typ
shrinkCxt (Left n) = S.filter (has (template . filtered (== n)))
shrinkCxt (Right t) = S.filter (has (template . filtered (`elem` tn)))
  where
    tn :: [String]
    tn = t ^.. template


makeDataTable :: [Dec] -> DataTable
makeDataTable (DatE (Name n : ps) : (spanExps n -> (es, rest))) = (n, ps, es) : makeDataTable rest
makeDataTable (Dat (Name n : ps) (NormalCons cs) : rest) = (n, ps, map f cs) : makeDataTable rest
  where
    f :: DataCon -> (String, Typ)
    f (Con (Name nc) ts) = (nc, foldr (\x xs -> Sym x (NormalSym (Symb "->")) xs) conT ts)
    conT = foldl App (Iden (Name n)) (map Iden ps)
makeDataTable (Fam _ r@(splitInstHead -> (n, pst)) z : rest) = (n, ps, [e]) : makeDataTable rest
  where
    ps :: [Name]
    ps =
      pst
        ^.. template
          . filtered
            ( \case
                Name (x : _) -> isLower x
                _ -> False
            )
    e :: (String, Typ)
    e = case splitInstHead z of
      (x, xs) -> (x, arrT xs r)

    -- arrT [a,b,c] d = a -> b -> c -> d
    arrT :: [Typ] -> Typ -> Typ
    arrT = flip $ foldr (\x xs -> Sym x (NormalSym (Symb "->")) xs)
makeDataTable (x:xs) = makeDataTable xs
makeDataTable [] = []

-- | helper for 'makeDataTable'
spanExps :: String -> [Dec] -> ([(String, Typ)], [Dec])
spanExps c (Exp (Fun [Name n] bo) : xs) | all isUpper (take 1 n) = spanExps c xs & _1 %~ ((n, bo) :)
spanExps c (Exp (FunI (Symb a) bo) : xs) | all (== ':') (take 1 a) = spanExps c xs & _1 %~ ((a, bo) :)
spanExps _ xs = ([], xs)


-- | like dataCons except for classes
--
-- > instance (Num a, Num b) => Num (a,b)
--
-- becomes
--
-- [ ( [ ( "Num" , [ Iden (Name "b") ] )
--      , ( "Num" , [ Iden (Name "a") ] )
--      ]
--    , Tup (Iden (Name "a")) (Iden (Name "b"))
--    , "Num"
--    , 1
--    )
--  ]
--
-- ie. the result type could become a data
--
-- > [([(String, [Typ])], -- context
-- >         Typ, -- "pattern"
-- >         String, -- class name
-- >         Int)] -- which pattern (2 for the 2nd parameter in a MultiParamTypeClasses)
--
-- > instance C Int
-- > instance C a => D a
-- > f :: D a => a
--
-- gives
--
-- [ [ ( [] , Iden (Name "Int") , "C" , 1 ) ]
-- , [ ( [ ( "C" , [ Iden (Name "a") ] ) ]
--    , Iden (Name "a")
--    , "D"
--    , 1
--    )
--  ]
--
-- now I want to find `f` with the `Int+` query
-- from Int+ I get the first instance, from the "C" 1 I get the next instance. But I also need
-- to unify Iden (Name "a") with Iden (Name "Int"). Find the subsitution that turns "a" into "Int".
--
-- It is unclear whether I store D Int
--
-- am I wrong in treating MPTCS as an int-indexed class? That is C a b is no
-- different than C1 a and C2 b. This introduces errors that expand the list of
-- found instances, since `C a b`
-- becomes `C1 a` `C2 b` and will confabulate the existence of `C A B` given only
-- `C A A` and `C B B` exist. Still, the extra imagined instances probably may not
-- be too much of an annoyance.
--
-- ( [] , Iden (Name "Int") , "C" , 1 )
makeInstanceTable :: Dec -> InstanceTable
makeInstanceTable (Inst (Sym cxt (NormalSym (Symb "=>")) x)) =
  makeInstanceTable (Inst x)
    <&> _1 .~ map splitInstHead (split cxt)
  where
    split (Tup a b) = b : split a
    split x = [x]
makeInstanceTable (Inst (splitInstHead -> (n, ts))) = [([], t, n, i) | (t, i) <- zip ts [1 ..]]
-- moved to dataTable
-- instanceTable (Fam FamTy (splitInstHead -> (n, ts)) rhs)
--   = [ ([("xxx", [rhs])], t, n, i) | (t, i) <- zip ts [1 .. ] ]
-- instanceTable (FamF _ (splitInstHead -> (n, ts))) = [ ([], t, n, i) | (t, i) <- zip ts [1 .. ] ]
-- instanceTable (Newty x) = ...
-- instanceTable (Ty x) = ...
-- instanceTable (Dat x) = ...
-- instanceTable (Fam _ (splitInstHead -> (n, ts)) rhs) = [ ([], t, n, i) | (t, i) <- zip ts [1 .. ] ]
-- type InstanceTable = [([(String, [Typ])], Typ, String, Int)]
-- type instance F a b = rhs
-- n = F
-- ts = [a,b]
-- instance Cxt => C a b
-- somehow rhs will go into the _1? Or do I have it backwards and instead lhs
-- goes into the _1 and rhs goes into the t,n,i? I know they are different, but
-- still each construction leads to a kind of edge in a graph.
makeInstanceTable _ = []

vpMap :: Traversal' VarPol (Either (String, Set Bool) (Typ, Set Bool))
vpMap =
  _Wrapped
    . ((_1 . itraversed) `iadjoinish` (_2 . itraversed))
    . traversed

-- | VarPol is the core of the search. 'getVarPolDec' or 'getVarPolTyp' produce it.
--
-- the traversal 'g' collects this state. True and False are + and - polarities.
-- Set Typ holds class constraints which were anywhere in the signature.
newtype VarPol = VarPol (Map String (Set Bool), Map Typ (Set Bool), Set Typ)
  deriving (Show, Monoid, Semigroup, Generic)

instance Wrapped VarPol

instance (t ~ VarPol) => Rewrapped VarPol t

type VpdMap =
  Map
    (Set Typ) -- context (class constraints) for type variables in Right
    ( Map
        (Either String Typ) -- Left "Int"
        -- or Right (App (Iden (Name "T")) (Iden (Name "b")))
        ( Map
            (Set Bool) -- polarity
            (Set String) -- function names
        )
    )

-- | `getVarPolDec "f :: A -> B"` produces (Just "f", vp)
--
-- | ghci renames shadowed variables. This replaces
-- `forall x y z. e` with just `e`, no matter where it occurs.
-- There seems to be no need to care about RankNTypes when searching.
removeForalls :: Typ -> Typ
removeForalls = rewriteOf uniplate \case
  App x y | has forallBinding x -> Just y
  _ -> Nothing

-- | convert between `forall a b c.` and `["a","b","c"]` (actually reversed?)
forallBinding :: Prism' Typ [Name]
forallBinding = prism' inj ej
  where
    ej (App x Dot) = ej1 x
    ej _ = Nothing
    ej1 (Iden (Name "forall")) = Just []
    ej1 (App x (Iden n)) = (n :) <$> ej1 x
    ej1 _ = Nothing

    inj xs = foldl (\e x -> App e (Iden x)) (Iden (Name "forall")) xs `App` Dot

-- where vp makes it easy to find "f" given A- or B+
getVarPolDec :: Dec -> (Maybe String, VarPol)
getVarPolDec dec =
  case dec of
    (Exp (Fun [Name n] ty)) -> do
      g $ removeForalls ty
      return (Just n)
    _ -> return Nothing
    `runReaderT` True
    `runState` mempty

getVarPolTyp :: Typ -> VarPol
getVarPolTyp typ = g (removeForalls typ) `runReaderT` True `execState` mempty

-- | classifies `Iden` polarities and also collects class constraints
-- perhaps two traversals would have been simpler
-- I am going to skip the -XNoFlexibleContexts version, so the constraints are
-- not collected in the right format
g :: Typ -> ReaderT Bool (State VarPol) ()
g (Iden (Name na)) = do
  p <- ask
  _Wrapped . _1 . at na <>= Just (S.singleton p)
g (ListOf typ') = g typ'
g (App (Sym1 (Symb "->")) typ2) = local not $ g typ2
-- g (App typ' typ2) = g typ' >> g typ2 -- original
g axy@App {} = do
  p <- ask
  _Wrapped . _2 . at axy <>= Just (S.singleton p)
g (Tup typ' typ2) = g typ' >> g typ2
g (Sym typ' (NormalSym (Symb "->")) typ2) = local not (g typ') >> g typ2
g (Sym cxts (NormalSym (Symb "=>")) typ2) = do
  sequence_
    [ _Wrapped . _3 <>= S.singleton cxt
      | cxt <- tupListRev cxts
    ]
  g typ2
g (Sym typ' op typ2) = g typ' >> g typ2
g (Sym1 sy) = return ()
g Unit = return ()
g EList = return ()
g (Bang typ') = g typ'
g Dot = return ()
g HasType = return ()
g Unpack = return ()
g (LitT s) = return ()
g (LitTN n) = return ()

appsListRev :: Typ -> [Typ]
appsListRev (App x y) = y : appsListRev x
appsListRev x = [x]

tupListRev :: Typ -> [Typ]
tupListRev (Tup x y) = y : tupListRev x
tupListRev x = [x]

splitInstHead :: Typ -> (String, [Typ])
splitInstHead = go []
  where
    go accum (App a b) = go (b : accum) a
    go accum (Iden (Name x)) = (x, accum)
    go accum (Sym a (SymBT (Name x)) b) = (x, a : b : accum)
    go x y = error $ show (x, y)

cleanLeadingHasType input =
  everywhere
    ( mkT \case
        App HasType x -> x
        x -> x
    )
    input
