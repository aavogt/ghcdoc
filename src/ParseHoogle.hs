{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module ParseHoogle
  ( parseHoogleFile,
    parseHoogleString,
    module ParseHoogle,
  )
where

import Control.DeepSeq
import Control.Exception
import Control.Lens
import Data.Char
import Data.Data
import Data.Data.Lens
import Data.Either
import Data.Generics
import Data.List
import Data.Maybe
import Language.LBNF
import Text.Show.Pretty (pPrint)

bnfc
  [lbnf|
comment "--";

token Name (letter | '_' | '.')*(letter | digit | '_' | ((letter | '_')'\''))+;
token Symb (["~.=-!#$%^&*+|<>/?:"]+); 


SymBT.Sym ::= "`" Name "`";
NormalSym.Sym ::= Symb;


separator nonempty Name "";
Fun.Exp ::= [Name] Typ;
FunI.Exp ::= "(" Symb ")" Typ; 
FunL.Exp ::= "[" Name "]" Typ; 


Decs.Decs ::= [Dec];
separator Dec "";

Clas.Dec ::= "class" Typ ATS;
Pkg.Dec ::= "@package" Name;
Ver.Dec ::= "@version" Name;
-- SigD.Dec ::= Name "::" Typ;
Exp.Dec ::= Exp;
Mod.Dec ::= "module" Name;
Dat.Dec ::= "data" [Name] "=" DataRhs;
DatE.Dec ::= "data" [Name];
DatI.Dec ::= "data" Name Symb Name;
Inst.Dec ::= "instance" Typ;
Newty.Dec ::= "newtype" Typ;
Ty.Dec ::= "type" Name [Name] "=" Typ;
TyK.Dec ::= "type" Name Typ;
Fam.Dec ::= Fam "instance" Typ "=" Typ;
FamK.Dec ::= Fam "family" Typ;
TRole.Dec ::= "type" "role" [Name];

NormalCons.DataRhs ::= [DataCon];
EllipsisCons.DataRhs ::= "...";
Con.DataCon ::= Name [Typ];
separator DataCon "|";
separator Typ "";


FamTy.Fam ::= "type";
FamDat.Fam ::= "data";
FamNewty.Fam ::= "newtype";

separator Integer ".";

ATS.ATS ::= "where" "{" [AT] "}";
NoATS.ATS ::= ;

AT.AT ::= "type" Exp KS;
Ellipsis.AT ::= "...";


NoKS.KS ::= ;
KS.KS ::= "::" Typ;

terminator nonempty AT ";";


Star.Typ5 ::= "*";
Iden.Typ5 ::= Name;
_.Typ4 ::= "(" Typ ")";
ListOf.Typ4 ::= "[" Typ "]";
App.Typ3 ::= Typ3 Typ4;
Tup.Typ2 ::= Typ2 "," Typ3;
Sym.Typ ::= Typ1 Sym Typ;
LitT.Typ5 ::= String;
LitTN.Typ5 ::= Integer;
Sym1.Typ ::= Symb;
Unit.Typ5 ::= "()";
EList.Typ5 ::= "[]";
EListP.Typ5 ::= "'[]";
ConsP.Typ5 ::= "':";
Bang.Typ5 ::= "!" Typ;
Dot.Typ5 ::= ".";
EllipsisT.Typ5 ::= "...";
HasType.Typ5 ::= "::";
Unpack.Typ5 ::= "{-# UNPACK #-}";
Foralls.Typ6 ::= "forall" [TVar] Typ;

Inferred.TVar ::= "{" Name "}";
Specified.TVar ::= Name;
TVarKS.TVar ::= "(" Name "::" Typ ")";


-- supposed to be equivalent to terminator TVar '."
-- type Proxy :: forall {k} x. k -> *
-- is not parsed by it though.
[].[TVar] ::= ".";
(:).[TVar] ::= TVar [TVar];




coercions Typ 6;

entrypoints Symb, Decs, Dec, Typ
|]

test = do
  let f = "hl.txt"
  pPrint . myLexer =<< readFile f
  pPrint . groupBy (const $ all isSpace . listToMaybe) . lines =<< readFile f
  mapM_
    ( \x -> do
        case x of
          Left e -> pPrint e
          Right (Decs e) -> do
            mapM_ (\e -> pPrint e) e
    )
    =<< parseHoogleFile f

parseHoogleString =
  map
    ( \x -> case pDecs (myLexer x) of
        Bad y -> Left (y, x)
        Ok r -> Right r
    )
    . braces
    . lines

-- does this lines/unlines and catch belong in parseHoogleString?
parseHoogleFile f =
  fmap concat
    . mapM
      ( \s ->
          let r = parseHoogleString (concat s)
           in seq (rnf (r ^.. template :: [String])) (return r) `catch` \(SomeException e) -> return [Left (show e, unlines s)]
      )
    . groupBy (\_ -> leadingSpace)
    . lines
    =<< readFile f
  where
    leadingSpace = all isSpace . listToMaybe

-- somehow this fails for type Size (v :: Type -> Type) :: Nat;"
braces (x : xs) | "{" `isSuffixOf` x = case break (== "}") xs of
  (bo, close : rest) -> unlines (x : bo ++ [close]) : braces rest
braces (x : xs) = x : braces xs
braces [] = []

deriving instance Data Dec

deriving instance Data Decs

deriving instance Data AT

deriving instance Data KS

deriving instance Data Exp

deriving instance Data Typ

deriving instance Data Sym

deriving instance Data Fam

deriving instance Data ATS

deriving instance Data Name

deriving instance Data Symb

deriving instance Data TVar

deriving instance Data DataCon

deriving instance Data DataRhs

deriving instance (Data a) => Data (ParseMonad a)
