{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
-- | unification-fd adapter
module Search.UTerm (module U, module Search.UTerm) where
import Control.Lens
import Control.Lens.Unsound (adjoin, lensProduct)
import Control.Monad.Logic
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.State.Lazy (execState)
import Control.Monad.Trans.Writer
import Control.Unification as U
import Control.Unification.IntVar as U
import Control.Unification.Types as U
import Data.Char
import Data.Data
import Data.Data.Lens
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Generics hiding (Generic)
import Data.IntMap (IntMap)
import Debug.Trace
import GHC.Generics hiding (to)
import Language.LBNF (ParseMonad (..))
import ParseHoogle
import System.Process (system)
import Text.Show.Pretty (pPrint)

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IntMap as IM

-- continues https://ro-che.info/articles/2017-06-17-generic-unification
-- runM :: Int -> M a -> (Either (UFailure TypF IntVar) a, IntMap String)
runM n = observeMany n . runWriterT . evalIntBindingT . runExceptT



makeBaseFunctor ''Typ

-- where do I put LogicT
type M = ExceptT (UFailure TypF IntVar) (IntBindingT TypF (WriterT (IntMap String) Logic))

deriving instance Data IntVar

deriving instance Data (UTerm TypF IntVar)

deriving instance Data (TypF (UTerm TypF IntVar))

deriving instance Generic1 TypF

instance Unifiable TypF

instance Unifiable []

-- | Convert 'Typ' from BNFC-meta into one suitable for unification-fd.
-- All type variables with the same name are represented by 'freeVar'.
--
-- > do
-- > (a,b,c) <- toUTerm (a,b,c)
-- > Identity x <- toUTerm (Identity x)
--
-- But is this right? The `M` keeps state so that freeVar added with `x` will
-- not overlap those with `(a,b,c)`
--
-- By preapplying the fromUTermEnv, I lose the opportunity to save the old variable names.
-- Why not store it in M? Where do I put the WriterT (IntMap String). It ought to commute with
-- intvarbindingT.
--
-- I am converting type signatures parsed by bnfc-meta into a form for
-- unification-fd. I have a `toUTerm :: Each s t Typ (UTerm TypF IntVar) => s
-- -> M t`. I have success with `do (a,b) <- toUTerm (a,b); c <- toUTerm c;
-- ...`, but can it also be made to work if instead the input `b` becomes `bs
-- :: [Typ]`? If I had `tinplate :: (Data a, Typeable a) => Traversal' s a`
-- except as a `Traversal s t a b` I would use that instead of `each`. There is
-- a `Map String IntVar` that is new for every `toUTerm`. If I store that Map
-- in M, I will end up with `do a <- toUTerm a; bs <- toUTerm bs; resetVarMap;
-- c <- c; ...` . But `do ~(a:bs) <- toUTerm (a:bs); c <- toUTerm c` looks the
-- best so far.
--
-- Assumes type variables have been renamed to avoid shadowing
toUTerm :: (Each s t Typ (UTerm TypF IntVar)) => s -> M t
toUTerm typs = do
  (s', m) <- each toUTerm1 typs `runStateT` (mempty :: Map String IntVar)
  lift $ lift $ tell $ toIM m
  return s'
  where
    toIM :: Map String IntVar -> IntMap String
    toIM m = IM.fromList [(i, s) | (s, IntVar i) <- M.toList m]

    toUTerm1 = \case
      Iden (Name name@(x : _)) | isLower x -> do
        let newN = do
              v <- lift $ lift freeVar
              at name ?= v
              return v
        n <- maybe newN return =<< use (at name)
        return $ UVar n
      x -> UTerm <$> traverse toUTerm1 (project x)

fromUTermEnv ::
  -- | original names
  IntMap String ->
  UTerm TypF IntVar ->
  Typ
fromUTermEnv originalNames typ = fromUTerm1 typ
  where
    fromUTerm1 = \case
      UVar n -> Iden (Name (uniquePrefix ++ show n))
      UTerm f -> undefined f

    -- "a" or "z0_", whatever doesn't occur in the whole type
    uniquePrefix =
      let allNames = typ ^.. uniplate . nameF <&> \(Name x) -> x
          candidateNames =
            map (: []) ['a' .. 'z']
              ++ [c : show n ++ "_" | c <- ['a' .. 'z'], n <- [0 ..]]
       in head $ filter (`notElem` allNames) candidateNames

    nameF :: Traversal' (UTerm TypF IntVar) Name
    nameF = biplate

