{-# LANGUAGE ApplicativeDo #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Server (module Server) where

import Codec.Compression.Zstd
import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.Foldable (Foldable (fold, toList), traverse_)
import qualified Data.Foldable.Levenshtein as F
import Data.List
import Data.List.Split as LS
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Data.Traversable (for)
import qualified Data.Trie as Trie
import Debug.Trace (trace, traceShow, traceShowId)
import Distribution.InstalledPackageInfo
import Distribution.ModuleName (ModuleName)
import Distribution.Pretty
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import GHC.Generics (Generic)
import Happstack.Server
import qualified Happstack.Server as Happs
import Happstack.Server.Internal.Monads (withRequest)
import HttpClient
import qualified Paths_ghcdoc
import System.Console.CmdArgs
import System.Directory
import System.FilePath hiding ((<.>))
import System.FilePath.Glob
import System.IO (IOMode (WriteMode), withFile)
import System.Info
import System.Posix.Files
import System.Process
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.EditDistance as T
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Read (readMaybe)
import Text.Regex.Applicative
import qualified Text.Regex.Applicative as Regex

data Ghcdoc = Ghcdoc
  { envFile :: Maybe String,
    packageQuery :: [String],
    browser :: String,
    port :: Int,
    originalInstances :: Bool,
    noOpen :: Bool
  }
  deriving (Data, Eq, Generic)

instance Binary Ghcdoc

data Pkgs = Pkgs
  { _haddockPath :: M.Map String [FilePath],
    _modules :: M.Map String (Set String)
  }
  deriving (Generic, Data)

makeLenses ''Pkgs

instance Binary Pkgs

instance Semigroup Pkgs where
  Pkgs a b <> Pkgs a' b' = Pkgs (M.unionWith (++) a a') (M.unionWith Set.union b b')

instance Monoid Pkgs where
  mempty = Pkgs mempty mempty

-- | instances take up too much space
--
-- Haddock produces the following
--
-- > Instances
-- > (Eq r, Arity d, KnownNat d) => Eq (Split d r)
-- >
-- > (Show r, Arity d, KnownNat d) => Show (Split d r)
--
-- I prefer
--  instances class Eq, class Show
--
-- and clicking on Eq and Show goes to what source usually does
-- class goes to the class haddock
--
--
-- This should also apply to classes? Or not? Anyways it is a class if within <div class="top"> we have a
--
-- <span class="keyword">class</span>
--
-- rather than data, newtype etc.
bpe' :: (Eq t) => (Tag t -> Bool) -> Iso' [Tag t] ([Tag t], [TagTree t])
bpe' p = iso (\xs -> break p xs & _2 %~ tagTree) (\(a, b) -> a ++ flattenTree b)

breakPaired ::
  (Show t, Eq t) =>
  -- | which opening tag to break on
  (Tag t -> Bool) ->
  [Tag t] ->
  -- | before, inside, after
  Maybe ([Tag t], (t, [Attribute t]), [Tag t], [Tag t])
breakPaired p xs = do
  let (a, bs) = break p xs
  TagBranch b attrs cs : ds <- Just (tagTree bs)
  Just (a, (b, attrs), flattenTree cs, flattenTree ds)

breakPairedEq e = breakPaired (== e)

-- | TagOpen is applied by this function, since I always seem to be matching on an opening tag
bpt :: (Show t, Eq t) => t -> [Attribute t] -> Prism' [Tag t] ([Tag t], (t, [Attribute t]), [Tag t], [Tag t])
bpt e as =
  prism'
    (\(a, (b, ba), c, d) -> a ++ [TagOpen b ba] ++ c ++ [TagClose b] ++ d)
    (breakPairedEq (TagOpen e as))

-- | breaks a list of tags into those before, inside and after a given one
bpe ::
  (Show t, Eq t) =>
  -- | opening tag
  (Tag t -> Bool) ->
  Prism' [Tag t] ([Tag t], (t, [Attribute t]), [Tag t], [Tag t])
bpe p = prism' (\(a, (b, ba), c, d) -> a ++ [TagOpen b ba] ++ c ++ [TagClose b] ++ d) (breakPaired p)

-- | inputs define a list
traverseTargets ::
  forall f s t a b x y.
  (Applicative f) =>
  ALens x y (a, s) (b, t) ->
  APrism s t x y ->
  LensLike f s t a b
traverseTargets lensXY pstxy = withPrism pstxy $
  withLens lensXY $
    \getAS putAS put get focus s ->
      let go s = case get s of
            Left t -> pure t
            Right x@(getAS -> (a, s')) -> curry (put . putAS x) <$> focus a <*> go s'
       in go s

-- what about done as a foldl / (more?) tail recursive?

traverseTargets34 p = traverseTargets (lensProduct _3 _4 :: ALens' (a, b, c, d) (c, d)) p

bptWithins e as = traverseTargets34 (bpt e as)

simplifyInstances_ :: [Tag TL.Text] -> [Tag TL.Text]
simplifyInstances_ =
  bptWithins "div" [("class", "top")] %~ \b ->
    let isClass = case b ^. bpt "span" [("class", "keyword")] of
          (_, _, [TagText "class"], _) -> True
          _ -> False
     in b
          & bpt "div" [("class", "subs instances")] . _3 %~ \tt ->
            TagText (if isClass then "class instances " else "instances")
              : intercalate [TagText ","] (simplifyInstances2 isClass tt)

simplifyInstances :: [Tag TL.Text] -> [Tag TL.Text]
simplifyInstances haddockTags =
  let (a, _, b, c) = haddockTags ^. bpt "div" [("class", "top")]

      isClass = case b ^. bpt "span" [("class", "keyword")] of
        (_, _, [TagText "class"], _) -> True
        _ -> False

      subsInstances =
        bpt "div" [("class", "subs instances")] . _3 %~ \tt ->
          TagOpen "div" [("class", "subs instances")]
            : TagText "instances "
            : intercalate [TagText ","] (simplifyInstances2 isClass tt)
            ++ [TagClose "div"]
   in a ++ subsInstances b ++ simplifyInstances c

-- | not right if I have multiple identical packages, but that hasn't happened yet
-- if I have multiple hgeometry-1.0.0.0-*,
-- it should probably be a prefix of the hash 60d0e11
testUniquePrefixes =
  map concat $
    uniquePrefixes $
      drop 3
        . splitOn "-"
        . lsuffix "hgeometry"
        <$> words
          "/home/aavogt/.cabal/store/ghc-9.6.6/hgeometry-1.0.0.0-60d0e11a422806c4e7a540303b7fd84c9bbc5b34c16ded481347b8e58081048b \
          \/home/aavogt/.cabal/store/ghc-9.6.6/hgeometry-1.0.0.0-l-kernel-7022a1ccad256d898a6b5ae2f45fbe3afa31aefdd1a6f411e6f51ad7f932d3b9 \
          \/home/aavogt/.cabal/store/ghc-9.6.6/hgeometry-1.0.0.0-l-point-2504875e1b4fcd7c8d1e883b0082b2a22faa874a6ced693e231819f9ba006f0e \
          \/home/aavogt/.cabal/store/ghc-9.6.6/hgeometry-1.0.0.0-l-vector-86c34ce0d499a8f2cb576db7ab1b43041fdedd8aa9b0f737a3a46fd655ef331a"

-- | if it is a class I need to list the Data not class
p1, p2, p3 :: TL.Text
p1 = "<td class=\"src clearfix\"><span class=\"inst-left\"><span class=\"instance details-toggle-control details-toggle expander\" data-details-id=\"i:ic:Plated:Plated:1\"></span> <a href=\"Control-Lens-Plated.html#t:Plated\" title=\"Control.Lens.Plated\">Plated</a> <a href=\"/package/template-haskell-2.16.0.0/docs/Language-Haskell-TH-Syntax.html#t:Exp\" title=\"Language.Haskell.TH.Syntax\">Exp</a></span> <a href=\"src/Control.Lens.Plated.html#line-245\" class=\"link\">Source</a> <a href=\"#t:Plated\" class=\"selflink\">#</a></td>"
-- Exp
p2 = "<td class=\"src clearfix\"><span class=\"inst-left\"><span class=\"instance details-toggle-control details-toggle expander\" data-details-id=\"i:ic:Plated:Plated:11\"></span> <a href=\"Control-Lens-Traversal.html#t:Traversable\" title=\"Control.Lens.Traversal\">Traversable</a> f =&gt; <a href=\"Control-Lens-Plated.html#t:Plated\" title=\"Control.Lens.Plated\">Plated</a> (<a href=\"/package/free-5.1.7/docs/Control-Monad-Free.html#t:Free\" title=\"Control.Monad.Free\">Free</a> f a)</span> <a href=\"src/Control.Lens.Plated.html#line-220\" class=\"link\">Source</a> <a href=\"#t:Plated\" class=\"selflink\">#</a></td>"
-- Free f a
p3 = "<td class=\"src clearfix\"><span class=\"inst-left\"><span class=\"instance details-toggle-control details-toggle expander\" data-details-id=\"i:id:Exp:Eq:1\"></span> <a href=\"/package/base-4.14.0.0/docs/Data-Eq.html#t:Eq\" title=\"Data.Eq\">Eq</a> <a href=\"Language-Haskell-TH-Syntax.html#t:Exp\" title=\"Language.Haskell.TH.Syntax\">Exp</a></span> <a href=\"src/Language.Haskell.TH.Syntax.html#line-1972\" class=\"link\">Source</a> <a href=\"#t:Exp\" class=\"selflink\">#</a></td>"

-- Eq
p4 = "<span class=\"inst-left\"><span class=\"instance details-toggle-control details-toggle expander\" data-details-id=\"i:if:HList:HMapUnboxF:44\"></span> <a href=\"Data-HList-CommonMain.html#t:HMapUnboxF\" title=\"Data.HList.CommonMain\">HMapUnboxF</a> xs us =&gt; <a href=\"Data-HList-CommonMain.html#t:HMapUnboxF\" title=\"Data.HList.CommonMain\">HMapUnboxF</a> (<a href=\"Data-HList-CommonMain.html#t:HList\" title=\"Data.HList.CommonMain\">HList</a> x ': xs) (<a href=\"Data-HList-CommonMain.html#t:RecordU\" title=\"Data.HList.CommonMain\">RecordU</a> x ': us)</span> <a href=\"src/Data.HList.RecordU.html#line-67\" class=\"link\">Source</a> <a href=\"#t:HMapUnboxF\" class=\"selflink\">#</a>"

p5 = "<td class=\"src clearfix\"><span class=\"inst-left\"><span class=\"instance details-toggle-control details-toggle expander\" data-details-id=\"i:ic:HTails:HTails:2\"></span> <a href=\"Data-HList-CommonMain.html#t:HTails\" title=\"Data.HList.CommonMain\">HTails</a> xs ys =&gt; <a href=\"Data-HList-CommonMain.html#t:HTails\" title=\"Data.HList.CommonMain\">HTails</a> (x ': xs) (<a href=\"Data-HList-CommonMain.html#t:HList\" title=\"Data.HList.CommonMain\">HList</a> (x ': xs) ': ys)</span> <a href=\"src/Data.HList.HList.html#line-1453\" class=\"link\">Source</a> <a href=\"#t:HTails\" class=\"selflink\">#</a></td>"

p1_ans = ("Plated", ["Exp"])

p2_ans = ("Plated", ["Free"])

p3_ans = ("Eq", ["Exp"])

p4_ans = ("HMapUnboxF", ["HList", "RecordU"])

p5_ans = ("HTails", ["HList"])

{- and also there should be hyperlinks accompanying the data -}

-- this one is empty because the span tag is empty
-- within each TagOpen "td" ("class", "src clearfix")

f0 text = parseTags text ^.. traverseTargets34 outerTD . to extract
  where
    outerTD = bpe \case
      TagOpen "td" [("class", "src clearfix")] -> True
      _ -> False
    extract tags =
      [ (t, link)
        | TagOpen "a" (("href", link) : c) : TagText t : _ <- tails tags,
          c /= [("class", "selflink")]
      ]

-- | splits `A => C rest` into (A,C,rest)
-- splitInstLink :: [Tag TL.Text] -> ([Tag TL.Text], [Tag TL.Text], [Tag TL.Text])
-- handle the case where there is no hyperlink?
splitInstLink xs = do
  let (a, b) =
        break
          ( \case
              TagText str -> "=>" `TL.isInfixOf` str
              _ -> False
          )
          xs
  -- what if the class name has no hyperlink?
  -- I want to get the "a" tag
  -- so I need a bpe that gives the attributes of that tag?
  (c, TagBranch "a" classLinkAttr className : rest) <-
    drop 1 b
      ^? bpe'
        ( \case
            TagOpen "a" _ -> True
            _ -> False
        )

  let sourceLink (TagBranch "a" attrs [TagLeaf (TagText "Source")]) = Just attrs
      sourceLink _ = Nothing
  (instHeadMinusClass, (sourceLink -> Just sl) : _) <- Just $ span (isNothing . sourceLink) rest
  Just (a ++ take 1 b, classLinkAttr, className, flattenTree instHeadMinusClass, sl)

-- rendered = iso (TL.unpack . renderTags) (parseTags .

--
-- I want to replace the link to the constructor with the link to the source instance?
-- let replaceLink = \ case
--                       TagOpen "a" _ -> TagOpen "a" sl
--                      x -> x
simplifyInstances2 :: Bool -> [Tag TL.Text] -> [[Tag TL.Text]]
simplifyInstances2
  isClass
  ( TagOpen "td" [("class", "src clearfix")]
      : (break (== TagClose "td") -> (clearFixBody, _ : xs))
    ) =
    processed : simplifyInstances2 isClass xs
    where
      stringToText = map (fmap TL.pack)
      processed =
        maybe clearFixBody stringToText $
          Regex.match re $
            TL.unpack $
              renderTags clearFixBody
      re = do
        many anySym
        "=&gt;"
        below <- many anySym
        return $
          let classNameAndSrcLink (TagOpen "a" (("href", classLink) : _) : TagText t : _ : s) =
                (classLink,t,) <$> g s
              classNameAndSrcLink (_ : xs) = classNameAndSrcLink xs
              classNameAndSrcLink xs = Nothing
              ignoreWhitespace = unwords . words
              g
                ( TagOpen "a" (("href", link) : _)
                    : TagText (ignoreWhitespace -> "Source")
                    : TagClose "a"
                    : _
                  ) = Just link
              g (_ : xs) = g xs
              g [] = Nothing
           in case classNameAndSrcLink $ parseTags below of
                Just (clink, name, link) ->
                  [ TagOpen "a" [("href", clink)],
                    TagText "class",
                    TagClose "a",
                    TagText " ",
                    TagOpen "a" [("href", link)],
                    TagText name,
                    TagClose "a"
                  ]
                Nothing -> []
simplifyInstances2 b (_ : xs) = simplifyInstances2 b xs
simplifyInstances2 _ [] = []

globMt pat = do
  fs <- glob pat
  ts <- mapM (fmap modificationTime . getFileStatus) fs
  return $ zip ts fs

globMtGhcEnv = globMt (".ghc.environment." ++ arch ++ "-" ++ os ++ "-*")

newestDotGhcEnv :: IO (Maybe (Bool, FilePath)) -- (newest cabal file is older than the .ghc.environment,
--  ".ghc.environment.arch-os-8.10.4")
newestDotGhcEnv = do
  (cabalOldest, _) : _ <- sortOn (negate . fst) <$> globMt "*.cabal"
  fmap (_1 %~ (cabalOldest <)) . listToMaybe . sortOn (negate . fst) <$> globMtGhcEnv

-- I should use shake, rattle instead?
-- it takes 0.8s to run ghc-pkg etc.
cacheResult :: (Eq a, Binary a, Binary b) => FilePath -> (a -> IO b) -> a -> IO b
cacheResult file f a =
  let redo = do
        b <- f a
        BS.writeFile file $ compress maxCLevel $ BL.toStrict $ encode (a, b)
        return b
   in (maybe redo return =<<) $ runMaybeT $ do
        True <- lift $ doesFileExist file
        Decompress x <- decompress <$> lift (BS.readFile file)
        MaybeT $ return $ decodeOrFail (BL.fromStrict x) ^? _Right . _3 . filtered ((== a) . fst) . _2

-- here we have to unset GHC_PACKAGE_PATH
mkGhcDotEnv = system "cabal configure --write-ghc-environment-files=always"

initialGhcdoc = do
  do
    x <- globMtGhcEnv
    when (null x) $ void mkGhcDotEnv

  -- get the .ghc.environment file with largest modifiction time
  envFileNewest <-
    maybe
      ( do
          mkGhcDotEnv
          fmap snd <$> newestDotGhcEnv
      )
      ( \(ok, f) -> do
          unless ok (void mkGhcDotEnv)
          return (Just f)
      )
      =<< newestDotGhcEnv

  return
    Ghcdoc
      { envFile = envFileNewest,
        packageQuery = [] &= args,
        browser = "xdg-open" &= help "default xdg-open",
        port = 8000,
        originalInstances =
          False
            &= help
              "by default instances look like:\n\
              \  instances class Eq, class Ord\n\
              \where class links to the class and Eq links to the instance source\n\
              \with this flag, they are spread over many lines like\n\
              \  instance Eq a => Eq (Maybe a)\n\
              \  instance Ord a => Ord (Maybe a)\n",
        noOpen =
          False
            &= help
              "suppress the default opening of the package\
              \index page when no packages are specified"
      }

main = mainWith =<< cmdArgs =<< initialGhcdoc

-- Defined in ‘Graphics.Gloss.Internals.Interface.Backend.Types’ (gloss-1.13.2.2)
-- Documentation: file:///home/aavogt/.cabal/store/ghc-9.8.2/gloss-1.13.2.2-9e0321f160e33ec6d3a09fb8ffb1d448969e822aa4a5383e073dfa5cc6bfc33a/share/doc/html/Graphics-Gloss.html#v:EventKey
-- the link doesn't have that anchor
-- but can ghcdoc still fix it?
-- how do I find the link outside of hls so that I can open a haddock bug?

mainWith ghcdoc@Ghcdoc {..} = do
  envFileMtime <- fmap show <$> traverse getModificationTime envFile
  -- cache based on the envFile filename&mtime and ghcdoc version
  (core, cabal) <-
    cacheResult
      ".ghc.environmentCache"
      (getPkgs . view _1)
      (envFile, envFileMtime, Paths_ghcdoc.version)

  let Pkgs pkgs modules =
        core <> cabal
          & haddockPath . traverse %~ nub

  -- call ghci
  -- :browse! (M.keys modules)
  -- why are haskeline, terminfo, ghc-compat, integer-gmp, libiserv hidden?
  -- more generally I should be able to avoid asking for these?
  --
  -- writeFile "hl5.txt" =<< readProcess "ghci" [] (unlines [":browse! " ++ m | (m, pkg) <- M.toList modules, Set.size pkg == 1])
  -- parseHoogleFile "hl5.txt"

  let openLinks ps
        | noOpen = return ()
        | otherwise = case ps of
            [] -> open ["http://localhost:" ++ show port]
            _ -> mapM_ open [["http://localhost:" ++ show port </> p </> "index.html"] | p <- ps]
        where
          open = void . createProcess . proc browser
  -- not sure if we want to wait like with `callProcess`

  -- make a better table of contents
  -- https://www.haskell.org/haddock/doc/html/invoking.html
  -- haddock --gen-contents -- "interface files"
  --
  -- in other cases packageQuery could be an module name, or an (qualified) identifier
  case packageQuery of
    [] -> openLinks []
    qs -> case partitionEithers
      [ maybe
          (Left q)
          (\bs -> Right (q, bs))
          (M.lookup q pkgs)
        | q <-
            map
              ( \q ->
                  fromMaybe q $
                    listToMaybe . toList -- or is there a preferred module? Or open both?
                      =<< M.lookup q modules
              )
              qs
      ] of
      -- I could open the particular modules rather than the package
      (qs_, good) -> do
        let addIndex (a, bs) = zipWith (\i _ -> a ++ show i) [0 ..] bs
        openLinks $ concatMap addIndex $ nub $ catMaybes [lookupED q fst (M.toList pkgs) | q <- qs_] ++ good

  let haddockPages =
        msum $
          [ dir (d ++ show i) $ serveDirectory EnableBrowsing ["index.html"] p | (d, ps) <- M.toList pkgs, (p, i) <- ps `zip` [0 ..]
          ]

  -- pkgs backwards, map from split path to package(s)
  let sgkp :: M.Map [FilePath] [String]
      sgkp = M.fromListWith (<>) [(splitDirectories p, [d]) | (d, ps) <- M.toList pkgs, p <- ps]
  -- it almost works
  -- perhaps it's failing because there's an empty FilePath?
  let redirectPages :: ServerPartT IO Response
      redirectPages =
        msum
          [ dirs p $ uriRest \p -> seeOther ("/" ++ d ++ show i ++ p) (toResponse ())
            | (d, ps) <- M.toList pkgs,
              (p, i) <- ps `zip` [0 ..]
          ]
          <> do
            Request {rqPaths = p} <- askRq
            guardRq (\_ -> length p > 3)
            dist@((_, d : _) : _) <-
              return $
                sortOn
                  fst
                  [ ( F.genericLevenshteinDistance (\_ -> 1) (\_ -> 0) (\_ _ -> 1) p' p :: Int,
                      ds
                    )
                    | (p', ds) <- M.toList sgkp
                  ]
            seeOther
              ( fromMaybe "/" $ do
                  Just ("/" <> d <> "0/" <> last p)
              )
              (toResponse ())

  -- like haddockPages, I can make a list of hoogle files
  -- /home/aavogt/.cabal/store/ghc-9.2.5/linear-1.21.10-e9a821f3feaa4061074418b16aacbf33ed480f5cb0d28053dbc82248b0382095/share/doc/html/linear.txt
  -- I could call hoogle to consolidate the database
  -- I could call hoogle the library to make the queries into the database
  -- ocaml's merlin has a polarity search https://github.com/ocaml/merlin/blob/master/doc/features.md
  -- I could implement that as a follow up to hoogle?
  -- or do I implement my own hoogle using the hoogle file?

  pwd <- getCurrentDirectory

  -- ghcdoc: Network.Socket.bind: resource busy (Address already in use)
  -- depending on the content of the page (it will include the path in which ghcdoc was run)
  -- either pick the next port (trying again)
  -- or call openLinks
  -- this doesn't work properly
  newPort <- getNewPort port

  -- For example:
  -- docdirPackage = fromList [ ("/home/aavogt/.ghcup/ghc/8.10.6/share/doc/ghc-8.10.6/html/libraries/base-4.14.3.0","base"), ... ]
  -- Many links look like: "../file:///home/aavogt/.ghcup/ghc/8.10.6/share/doc/ghc-8.10.6/html/libraries/base-4.14.3.0/src"
  -- using docdirPackage the link can be transformed into "/base0/src"
  let docdirPackage :: Trie.Trie T.Text
      docdirPackage =
        Trie.fromList
          [ (T.encodeUtf8 (T.pack v), dest)
            | (k, vs) <- M.toList pkgs,
              (v, i) <- vs `zip` [0 ..],
              let dest = T.pack $ k <> show i,
              v <- [v, takeFileName v]
          ]

      -- in addition to simplify instances I would also like to replace cloudflare's MathJax
      -- with a local copy
      validator = Just $ fixFileLinks pp docdirPackage
        where
          pp
            | originalInstances = id
            | otherwise = simplifyInstances_

  case newPort of
    Just port ->
      simpleHTTP
        nullConf
          { Happs.port = port,
            Happs.validator = validator
          }
        $ msum
          [ haddockPages,
            redirectPages,
            ok (toResponse (mkRootPage pkgs pwd))
          ]
    Nothing -> return ()

fixFileLinks postProcess docdirPackage sf@SendFile {}
  | Just ["text/html"] <- rsHeaders sf ^? ix "content-type" . to hValue = do
      let cleanUrl trie =
            fmt . Trie.matches trie . T.encodeUtf8 . TL.toStrict
              <=< stripFirstPrefix ["../file://", "file://", "../"]
            where
              fmt matches =
                listToMaybe
                  [ TL.fromStrict $ "/" <> lib <> rest
                    | (_, lib, T.decodeUtf8' -> Right rest) <- matches
                  ]

              stripFirstPrefix prefixes p = prefixes ^? folded . to (`TL.stripPrefix` p) . _Just

      let cleanHref = iso M.fromList M.toList . ix "href" %%~ cleanUrl docdirPackage
      let cleanAnchor (TagOpen "a" (cleanHref -> Just attrs)) = TagOpen "a" attrs
          cleanAnchor x = x

      f <- TL.readFile (sfFilePath sf)
      return $
        Response
          { rsCode = rsCode sf,
            rsHeaders = rsHeaders sf,
            rsFlags = rsFlags sf,
            rsBody = encodeUtf8 $ renderTags $ postProcess $ map cleanAnchor $ parseTags f,
            rsValidator = rsValidator sf
          }
fixFileLinks _ _ res = return res

-- but before proceeding I should get the list of identifiers ie. reimplement hoogle?
-- qualified vs unqualified identifiers

mkRootPage pkgs pwd = H.head (H.title (fromString pwd)) <> H.body (H.h1 (fromString "ghcdoc packages") <> rootLinkList)
  where
    rootLinkList =
      H.div H.! A.style (fromString "columns: 100px 4") $
        H.ul $
          mconcat
            [ H.li $ H.a H.! A.href (fromString href) $ H.toHtml label
              | (n, hrefs) <- M.toList pkgs,
                let suffs = uniquePrefixes $ drop 3 . splitOn "-" . lsuffix n <$> hrefs,
                (href, label) <- zipWith (\i s -> (n ++ show i, n ++ leadingColon s)) [0 ..] suffs
            ]

    leadingColon xs = case concat xs of
      [] -> []
      a -> ':' : a

uniquePrefixes :: (Eq a) => [[a]] -> [[a]]
uniquePrefixes = go []
  where
    go accum (xs : xss) = go (fromMaybe xs (find (\x -> not (null x) && x `notElem` accum) (inits xs)) : accum) xss
    go accum [] = accum

-- | from the .ghc.environment.* file, set GHC_PACKAGE_PATH and return
-- a function to check that the `compatPackageKey` was required
processEnvFile :: Maybe FilePath -> IO ([FilePath], String -> Bool)
processEnvFile (Just envFile) = do
  ev <-
    dropWhile (\x -> not $ "package" `isPrefixOf` x) . lines
      <$> readFile envFile
  let dbs = [f | e <- ev, Just f <- [stripPrefix "package-db " e]]
      pkgids = S.fromList [f | e <- ev, Just f <- [stripPrefix "package-id " e]]

  return (dbs, (`S.member` pkgids))
processEnvFile Nothing = return (mempty, const True)

-- TODO replace this with (the same operations) of ~/bin/ghc-exposed.sh
-- which includes libraries installed "globally" with cabal install foo --lib
-- and it will work when there is no cabal package
--
-- TODO: present a subset of all libraries installed that are not linked but might be desired?
-- when there are many versions
-- Then offer a way to add them to package.yaml when there's a single `dependencies:` list
getPkgs ::
  Maybe FilePath ->
  -- | (all libraries shipped with ghc, all libraries required by the package)
  IO (Pkgs, Pkgs)
getPkgs envFile = do
  let toPkgs :: InstalledPackageInfo -> Pkgs
      toPkgs x =
        Pkgs
          ( M.singleton
              (unPackageName (pkgName (sourcePackageId x)))
              (haddockHTMLs x)
          )
          ( M.fromListWith
              Set.union
              [ (prettyShow $ exposedName m, Set.singleton $ unPackageName (pkgName (sourcePackageId x)))
                | m <- exposedModules x
              ]
          )

  let parseDump :: (String -> Bool) -> FilePath -> IO Pkgs
      parseDump keep db = do
        confs <- filter (keep . takeBaseName) <$> globDir1 (compile "*.conf") db
        flip foldMap confs $ \f ->
          B8.readFile f
            <&> parseInstalledPackageInfo
            <&> view (_Right . _2 . to toPkgs)

  (dbs, requested) <- processEnvFile envFile

  libdirs <- ghcPkgDb
  core <- parseDump (const True) `foldMap` libdirs
  coreCabal <- parseDump requested `foldMap` (libdirs ++ dbs)

  -- instead of head, make all of them, and report the one that exists
  let substPkgroot =
        each . haddockPath . traverse . traverse %~ \path -> case splitOn "/" path of
          "${pkgroot}" : xs -> intercalate "/" $ dropFileName (head libdirs) : xs
          _ -> path

  return $ substPkgroot (core, coreCabal)

lookupED :: String -> (a -> String) -> [a] -> Maybe a
lookupED q f xs = fmap snd $ listToMaybe $ sortOn fst $ map (\x -> (T.levenshteinDistance defaultEditCosts q (f x), x)) xs

ghcPkgDb :: IO [String]
ghcPkgDb =
  -- TODO https://stackoverflow.com/questions/58173386
  withFile "/dev/null" WriteMode $ \nullHandle ->
    getDbStack <$> readCreateProcess ((shell "ghc-pkg -v2 latest base") {std_err = UseHandle nullHandle}) ""

getDbStack :: String -> [String]
getDbStack = concat . mapMaybe (readMaybe <=< stripPrefix "db stack: ") . lines

-- | library suffix. Give suffix of the path after the package name.
--
-- > lsuffix "text-iso8601" "/home/aavogt/.cabal/store/ghc-9.6.6/text-iso8601-0.1.1-4c9247b4750dba517f02488860270def19b49695a5ea4021f7b43250f1f52734/share/doc/html"
-- > -0.1.1-4c9247b4750dba517f02488860270def19b49695a5ea4021f7b43250f1f52734/share/doc/html
--
-- used to disambiguate hyperlinks (ie. hgeometry-1.0.0.0-l-vector, hgeometry-1.0.0.0-l-point otherwise appear the same)
--
-- https://hackage.haskell.org/package/Cabal-syntax-3.14.0.0/docs/Distribution-Types-MungedPackageId.html
-- might be better since this will break if a package is called home
lsuffix :: String -> String -> String
lsuffix pkgname path = concat $ take 1 $ mapMaybe (stripPrefix pkgname) (reverse (tails path))
