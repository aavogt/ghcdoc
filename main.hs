{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards, TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
module Main where

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
import Distribution.System
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.FilePath
import System.FilePath.Glob
import System.Info
import System.Posix.Files
import System.Process
import System.Process.ByteString as BS
import Text.EditDistance

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Happstack.Server.FileServe
import Happstack.Server
import qualified Paths_ghcdoc

data Ghcdoc = Ghcdoc { envFile :: String,
                     packageQuery :: [String],
                     browser :: String,
                     port :: Int } deriving (Data, Eq, Generic)

instance Binary Ghcdoc


globMt pat = do
        fs <- glob pat
        ts <- mapM (fmap modificationTime . getFileStatus) fs
        return $ zip ts fs

globMtGhcEnv = globMt (".ghc.environment." ++ arch ++ "-" ++ os ++ "-*")

getGhcEnvFiles :: IO (Maybe (Bool, FilePath)) -- (newest cabal file is older than the .ghc.environment,
                                              --  ".ghc.environment.arch-os-8.10.4")
getGhcEnvFiles = do
  (cabalFst , _) : _ <- sortOn (negate . fst) <$> globMt "*.cabal"
  fmap (_1 %~ (cabalFst <)) . listToMaybe . sortOn (negate . fst) <$> globMtGhcEnv

-- I should use shake?
-- it takes 0.8s to run ghc-pkg etc.
-- catch errors from decode and then just run (f a) and cache again
cacheResult :: (Eq a, Binary a, Binary b) => FilePath -> (a -> IO b) -> a -> IO b
cacheResult file f a = do
        let redo = do
                   b <- f a
                   BS.writeFile file $ compress maxCLevel $ BL.toStrict $ encode (a,b)
                   return b

        e <- doesFileExist file
        if e then do
                (a', b') <- (\ (Decompress x) -> decode (BL.fromStrict x) ) . decompress <$> BS.readFile file
                if a' == a then return b'
                           else redo
           else redo

generateEnvFile = system "cabal configure --write-ghc-environment-files=always"

main = do
  do x<- globMtGhcEnv
     when (null x) $ void generateEnvFile

  -- get the .ghc.environment file with largest modifiction time
  Just envFileNewest <- do
          maybe (do generateEnvFile
                    fmap snd <$> getGhcEnvFiles)
                (\ (ok, f) ->
                        if ok then return (Just f)
                              else do
                                      generateEnvFile
                                      return (Just f)) =<< getGhcEnvFiles

  ghcdoc@ Ghcdoc{..} <- cmdArgs $ Ghcdoc {
                envFile = envFileNewest,
                packageQuery = [] &= args,
                browser = "firefox",
                port = 8000 }

  envFileMtime <- show <$> getModificationTime envFileNewest
  -- cache based on the envFile filename&mtime...but not ghcdoc version
  pkgs <- cacheResult ".ghc.environmentCache" (getPkgs . view _1) (envFile, envFileMtime, Paths_ghcdoc.version)

  let openLinks ps = case ps of
        [] -> open ["http://localhost:" ++ show port]
        _ -> open [ "http://localhost:" ++ show port </> p </> "index.html" | p <- ps]
        where open = void . createProcess . proc browser
              -- not sure if we want to wait like with `callProcess`

  -- make a better table of contents
  -- https://www.haskell.org/haddock/doc/html/invoking.html
  -- haddock --gen-contents -- "interface files"
  case packageQuery of
    [] -> openLinks []
    qs -> case partitionEithers [ maybe (Left q) (\bs -> Right (q,bs))
                                                (M.lookup q pkgs) | q <- qs ] of
            (qs_, good) ->do
                    let addIndex (a,bs) = zipWith (\i _ -> a ++ show i) [1 .. ] bs
                    openLinks $ concatMap addIndex $ [ lookupED q fst (M.toList pkgs)  | q <- qs_ ] ++ good
                    return ()

  let rootLinkList = H.ul $ mconcat
         [ H.li (H.a H.! H.href (fromString n') $ H.toHtml n)  | (n,hrefs) <- M.toList pkgs,
                                                let nhref = length hrefs,
                                                (href, i) <- hrefs `zip` [1 .. ],
                                                let n' = n ++ show i ]
      rootPage = H.body (H.h1 (fromString "ghcdoc packages") <> rootLinkList)

        -- doc-index.json is missing even though I changed the options
      haddockPages = msum $ [ dir (d ++ show i) $ serveDirectory EnableBrowsing ["index.html"] p |  (d,ps) <- M.toList pkgs,
                              (p,i) <- ps `zip` [1 .. ]]

  simpleHTTP nullConf{Happstack.Server.port=port} $ msum [
          haddockPages ,
          ok (toResponse rootPage) ]

getPkgs envFile = do
  ev <- dropWhile (\ x -> not $ "package" `isPrefixOf` x) . lines
                <$> readFile envFile
  let dbs = [ f | e <- ev, Just f <- [stripPrefix "package-db " e] ]
      pkgids = S.fromList [ f| e <- ev, Just f <- [stripPrefix "package-id " e] ]

      setDbEnv = setEnv "GHC_PACKAGE_PATH" (intercalate ":" dbs ++ ":")
  setDbEnv
  (_, out, _) <- BS.readCreateProcessWithExitCode (proc "ghc-pkg" ["dump"]) mempty

  let pkgsAndErrs = map (fmap (fmap getPackageInfoFields) . parseInstalledPackageInfo . B8.unlines) $ LS.splitOn [B8.pack "---"] $ B8.lines out
  return $ M.fromListWith (++) [ (pn, haddock) | Right (_, (pn, compat, haddock) ) <- pkgsAndErrs,
                   compat `S.member` pkgids ]

lookupED :: String -> (a -> String) -> [a] -> a
lookupED q f xs = snd $ head $ sortOn fst $ map (\x -> (levenshteinDistance defaultEditCosts q (f x), x)) xs

getPackageInfoFields x = ( unPackageName (pkgName (sourcePackageId x)),
               compatPackageKey x,
               haddockHTMLs x)
