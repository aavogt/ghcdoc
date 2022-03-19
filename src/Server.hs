{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards, TupleSections, ViewPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
module Server where

import qualified Data.Trie as Trie
import Data.String (fromString)
import Codec.Compression.Zstd
import Control.Lens
import Control.Monad
import Data.Binary
import Data.Char
import Data.Either
import Data.List
import Data.List.Split as LS
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Version
import Distribution.InstalledPackageInfo
import Distribution.Pretty
import Distribution.System
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.ComponentId
import Distribution.Types.Module
import Distribution.Types.UnitId
import Distribution.ModuleName (ModuleName)
import Distribution.Backpack
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.FilePath hiding ((<.>))
import System.FilePath.Glob
import System.Info
import System.Posix.Files
import System.Process
import System.Process.ByteString as BS
import Text.EditDistance

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server.FileServe
import Happstack.Server

import qualified Paths_ghcdoc
import HttpClient
import Text.HTML.TagSoup
import GHC.Generics (Generic)
import Data.Foldable (Foldable(toList, fold), traverse_)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Monoid
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Debug.Trace (traceShow, trace, traceShowId)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Traversable (for)
import Text.Read (readMaybe)
import System.IO (IOMode (WriteMode), withFile)
import Text.Regex.Applicative
import qualified Text.Regex.Applicative as Regex
import Text.HTML.TagSoup.Tree
import qualified Happstack.Server as Happs

data Ghcdoc = Ghcdoc { envFile :: Maybe String,
                     packageQuery :: [String],
                     browser :: String,
                     port :: Int,
                     originalInstances :: Bool,
                     noOpen :: Bool } deriving (Data, Eq, Generic)

instance Binary Ghcdoc

data Pkgs = Pkgs { _haddockPath :: M.Map String [FilePath],
        _modules :: M.Map String String }
        deriving (Generic)

makeLenses ''Pkgs
instance Binary Pkgs

instance Semigroup Pkgs where
        Pkgs a b <> Pkgs a' b' = Pkgs (M.unionWith (++) a a') (M.unionWith (++) b b')
instance Monoid Pkgs where
        mempty = Pkgs mempty mempty


{- | instances take up too much space

Haddock produces the following

> Instances
> (Eq r, Arity d, KnownNat d) => Eq (Split d r)
>
> (Show r, Arity d, KnownNat d) => Show (Split d r)

I prefer
  instances class Eq, class Show

and clicking on Eq and Show goes to what source usually does
class goes to the class haddock



-}
simplifyInstances :: [Tag TL.Text] -> [Tag TL.Text]
simplifyInstances divtags @ (TagOpen "div" [("class", "subs instances")] : _) = case tagTree divtags of
  TagBranch "div" _ tt : xs -> TagOpen "div" [("class", "subs instances")]
                                : TagText "instances "
                                : intercalate [TagText ","] (simplifyInstances1 (flattenTree tt))
                                ++ TagClose "div" : flattenTree xs
  _ -> error "simplifyInstances: tagTree behaves unexpectedly"
simplifyInstances (x : xs) = x : simplifyInstances xs
simplifyInstances [] = []

simplifyInstances1 :: [Tag TL.Text] -> [[Tag TL.Text]]
simplifyInstances1 (TagOpen "td" [("class","src clearfix")] :
                        (break (==TagClose "td") -> (clearFixBody, _ : xs)))
                = processed : simplifyInstances1 xs
  where
    processed = parseTags
                $ TL.pack
                $ renderTags
                $ fromMaybe []
                $ Regex.match re
                $ TL.unpack
                $ renderTags clearFixBody
    re = do
     many anySym
     "=&gt;"
     below <- many anySym
     return $ let classNameAndSrcLink (TagOpen "a" (("href",classLink):_) : TagText t : _ : s)
                                = (classLink,t,) <$> g s
                  classNameAndSrcLink (_ : xs) = classNameAndSrcLink xs
                  classNameAndSrcLink xs = Nothing
                  ignoreWhitespace = unwords . words
                  g (TagOpen "a" (("href", link) : _)
                        : TagText (ignoreWhitespace -> "Source")
                        : TagClose "a"
                        : _) = Just link
                  g (_ : xs) = g xs
                  g [] = Nothing
        in case classNameAndSrcLink $ parseTags below of
                Just (clink, name, link) ->
                       [TagOpen "a" [("href",clink)], TagText "class", TagClose "a", TagText " ",
                        TagOpen "a" [("href",link)], TagText name, TagClose "a"]
                Nothing -> []

simplifyInstances1 (_:xs) = simplifyInstances1 xs
simplifyInstances1 [] = []


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
             BS.writeFile file $ compress maxCLevel $ BL.toStrict $ encode (a,b)
             return b
  in (maybe redo return =<<) $ runMaybeT $ do
        True <- lift $ doesFileExist file
        Decompress x <- decompress <$> lift (BS.readFile file)
        MaybeT $ return $ decodeOrFail (BL.fromStrict x) ^? _Right . _3 . filtered ((==a) . fst) . _2


-- here we have to unset GHC_PACKAGE_PATH
mkGhcDotEnv = system "cabal configure --write-ghc-environment-files=always"

initialGhcdoc = do
  do x<- globMtGhcEnv
     when (null x) $ void mkGhcDotEnv

  -- get the .ghc.environment file with largest modifiction time
  envFileNewest <-
          maybe (do mkGhcDotEnv
                    fmap snd <$> newestDotGhcEnv)
                (\ (ok, f) -> do
                        unless ok (void mkGhcDotEnv)
                        return (Just f)) =<< newestDotGhcEnv

  return Ghcdoc{
                envFile = envFileNewest,
                packageQuery = [] &= args,
                browser = "xdg-open" &= help "default xdg-open",
                port = 8000,
                originalInstances = False &= help "by default instances look like:\n\
                        \  instances class Eq, class Ord\n\
                        \where class links to the class and Eq links to the instance source\n\
                        \with this flag, they are spread over many lines like\n\
                        \  instance Eq a => Eq (Maybe a)\n\
                        \  instance Ord a => Ord (Maybe a)\n",
                noOpen = False &= help "suppress the default opening of the package\
                                        \index page when no packages are specified" }

main = mainWith =<< cmdArgs =<< initialGhcdoc

mainWith ghcdoc@Ghcdoc{..} = do
  envFileMtime <- fmap show <$> traverse getModificationTime envFile
  -- cache based on the envFile filename&mtime and ghcdoc version
  (Pkgs corePkgs coreModules, Pkgs pkgs  modules) <- cacheResult ".ghc.environmentCache" (getPkgs . view _1)
        (envFile, envFileMtime, Paths_ghcdoc.version)

  -- why is pkgs empty when running under ghcid?
  -- the GHC_PACKAGE_PATH is identical
  -- perhaps this is the wrong approach, there should be something in the dump
  -- that will allow me to distinguish between the two types of package

  let openLinks ps | noOpen = return ()
                | otherwise = case ps of
        [] -> open ["http://localhost:" ++ show port]
        _ -> mapM_ open [ ["http://localhost:" ++ show port </> p </> "index.html"] | p <- ps]
        where open = void . createProcess . proc browser
              -- not sure if we want to wait like with `callProcess`

  -- make a better table of contents
  -- https://www.haskell.org/haddock/doc/html/invoking.html
  -- haddock --gen-contents -- "interface files"
  --
  -- in other cases packageQuery could be an module name, or an (qualified) identifier
  case packageQuery of
    [] -> openLinks []
    qs -> case partitionEithers [ maybe (Left q) (\bs -> Right (q,bs))
                                                (M.lookup q pkgs) | q <- map (\q -> fromMaybe q $ M.lookup q modules) qs ] of
                                                -- I could open the particular modules rather than the package
            (qs_, good) ->do
                    let addIndex (a,bs) = zipWith (\i _ -> a ++ show i) [0 .. ] bs
                    -- TODO a page "did you mean" with a timed redirect
                    openLinks $ concatMap addIndex $ nub $ catMaybes [ lookupED q fst (M.toList pkgs)  | q <- qs_ ] ++ good

  let haddockPages = msum $ [ dir (d ++ show i) $ serveDirectory EnableBrowsing ["index.html"] p |  (d,ps) <- M.toList pkgs,
                              (p,i) <- ps `zip` [0 .. ]]

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
      docdirPackage = Trie.fromList $ (mapped . _1 %~ T.encodeUtf8 . T.pack)  $ concat [ [ (v, dest),
                                                 (takeFileName v, dest) ] |
              (k,vs) <- M.toList corePkgs ++ M.toList pkgs,
              (v, i) <- vs `zip` [0 .. ],
              let dest = T.pack $ k <> show i]


      validator = Just $ fixFileLinks pp docdirPackage where
              pp
                | originalInstances = id
                | otherwise = simplifyInstances

  case newPort of
    Just port ->
          simpleHTTP nullConf{Happs.port=port,
                             Happs.validator = validator }
                $ msum [
                  haddockPages ,
                  ok (toResponse (mkRootPage pkgs pwd)) ]
    Nothing -> return ()


fixFileLinks postProcess docdirPackage sf@SendFile {} |
  Just ["text/html"] <- rsHeaders sf ^? ix "content-type" . to hValue = do

    let cleanUrl trie = fmt . Trie.matches trie . T.encodeUtf8 . TL.toStrict
                               <=< stripFirstPrefix ["../file://", "file://", "../"]
         where
         fmt matches = listToMaybe [  TL.fromStrict $ "/" <> lib <> rest |
                                               (_, lib, T.decodeUtf8' -> Right rest) <- matches ]

         stripFirstPrefix prefixes p = prefixes ^? folded . to (`TL.stripPrefix` p) . _Just

    let cleanHref = iso M.fromList M.toList . ix "href" %%~ cleanUrl docdirPackage
    let cleanAnchor (TagOpen "a" (cleanHref -> Just attrs)) = TagOpen "a" attrs
        cleanAnchor x = x

    f <- TL.readFile (sfFilePath sf)
    return $ Response { rsCode = rsCode sf,
            rsHeaders = rsHeaders sf,
            rsFlags = rsFlags sf,
            rsBody = encodeUtf8 $ renderTags $ postProcess $ map cleanAnchor $ parseTags f,
            rsValidator = rsValidator sf }

fixFileLinks _ _ res = return res



-- but before proceeding I should get the list of identifiers ie. reimplement hoogle?
-- qualified vs unqualified identifiers

mkRootPage pkgs pwd = H.head (H.title (fromString pwd)) <> H.body (H.h1 (fromString "ghcdoc packages") <> rootLinkList)
  where
  rootLinkList = H.div H.! A.style (fromString "columns: 100px 4") $ H.ul $ mconcat
         [ H.li $ H.a H.! A.href (fromString n') $ H.toHtml n  | (n,hrefs) <- M.toList pkgs,
                                                let nhref = length hrefs,
                                                (href, i) <- hrefs `zip` [0 .. ],
                                                let n' = n ++ show i ]

-- | from the .ghc.environment.* file, set GHC_PACKAGE_PATH and return
-- a function to check that the `compatPackageKey` was required
processEnvFile :: Maybe FilePath -> IO ([FilePath], String -> Bool)
processEnvFile (Just envFile) = do
  ev <- dropWhile (\ x -> not $ "package" `isPrefixOf` x) . lines
                <$> readFile envFile
  let dbs = [ f | e <- ev, Just f <- [stripPrefix "package-db " e] ]
      pkgids = S.fromList [ f| e <- ev, Just f <- [stripPrefix "package-id " e] ]

  return (dbs, (`S.member` pkgids))

processEnvFile Nothing = return (mempty, const True)

-- TODO: present a subset of all libraries installed that are not linked but might be desired?
-- when there are many versions
getPkgs :: Maybe FilePath -> IO (Pkgs, Pkgs) -- ^ (all libraries shipped with ghc, all libraries required by the package)
getPkgs envFile = do
  let toPkgs :: InstalledPackageInfo -> Pkgs
      toPkgs x = Pkgs
        (M.singleton (unPackageName (pkgName (sourcePackageId x)))
                        (haddockHTMLs x))
        (M.fromListWith (++) [ (prettyShow $ exposedName m, unPackageName (pkgName (sourcePackageId x)))
                   | m <- exposedModules x ])

  let parseDump :: (String -> Bool) -> FilePath -> IO Pkgs
      parseDump keep db = do
              confs <- filter (keep . takeBaseName) <$> globDir1 (compile "*.conf") db
              flip foldMap confs $ \f ->
                      B8.readFile f <&>
                      parseInstalledPackageInfo <&>
                      view (_Right . _2 . to toPkgs)

  (dbs, requested) <- processEnvFile envFile

  libdirs <- ghcPkgDb
  core <- parseDump (const True) `foldMap` libdirs
  coreCabal <- parseDump requested `foldMap` (libdirs ++ dbs)

  return (core, coreCabal)

lookupED :: String -> (a -> String) -> [a] -> Maybe a
lookupED q f xs = fmap snd $ listToMaybe $ sortOn fst $ map (\x -> (levenshteinDistance defaultEditCosts q (f x), x)) xs

ghcPkgDb :: IO [String]
ghcPkgDb =
        -- https://stackoverflow.com/questions/58173386
        withFile "\\.\NUL" WriteMode $ \ nullHandle ->
        getDbStack <$> readCreateProcess ((shell "ghc-pkg -v2 latest base") { std_err = UseHandle nullHandle } ) ""

getDbStack :: String -> [String]
getDbStack = concat . mapMaybe (readMaybe <=< stripPrefix "db stack: ") . lines
