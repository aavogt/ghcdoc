{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec.Golden
import Test.Hspec.WebDriver
import System.Process
import Server hiding (main)
import ParseHoogle
import ParseQuery
import System.Environment
import Control.Concurrent
import Control.Exception
import Data.Either
import Control.Lens
import Test.WebDriver.Commands
import qualified Data.Map as M
import Text.Show.Pretty

import qualified Test.Hspec as H
import qualified ParseQuery as PQ


-- instance Example (Golden String)
-- except I have an IO to run first?

{-
specsUnit = do
    linear <- runIO $ parseHoogleFile "linear.txt"
    describe "linear" $ do
        it "linear" $ defaultGolden "linear" (ppShow $ cleanLeadingHasType linear)

    describe "dataCons" $ do
        let p str = case fmap (concatMap (\(Decs x) -> x)) . partitionEithers . cleanLeadingHasType . parseHoogleString $ str of
                ([], r) -> r
                (errs, _) -> error $ show errs
            pg n f s = defaultGolden n (ppShow (f (p s))) -- or store the unpretty printed, and store the input?
        it "data constructors no parameters" $
                let s = "g :: Ty\ndata Ty\nCon1 :: Int -> Ty\nCon2 :: Char -> Ty\nf :: Ty"
                in pg "con1" dataCons s
        it "type constructor with infix constructor" $
                pg "con2" dataCons "data Complex a\n(:+) :: !a -> !a -> Complex a"
        it "hgeometry Ext" $
                pg "con3" dataCons "data core :+ extra\n(:+) :: core -> extra -> (:+) core extra"
        it "ignores trailing infix functions" $
                pg "con4" dataCons "data core :+ extra\n(:+) :: core -> extra -> (:+) core extra\n(+) :: Num a => a -> a -> a"
        
        -}



clickElem linkText = click =<< findElem (ByLinkText linkText)

specsInteg = do
    describe "ghcdoc hyperlinks" $ startingGhcdoc $ do
      session "HTTP" $ using [chromeCaps] $ do
        it "index page to HTTP to base" $ runWD $ do
                openPage "http://localhost:8000"
                mapM_ clickElem ["HTTP", "Network.Browser", "Show"]
                getTitle `shouldReturn` "Text.Show"
        it "lens to base to base" $ runWD $ do
                openPage "http://localhost:8000/lens0/Control-Lens-Indexed.html"
                mapM_ clickElem ["Monad", "Applicative"]
                getTitle `shouldReturn` "Control.Applicative"
        -- broken
        it "lens source to base" $ runWD $ do
                openPage "http://localhost:8000/lens0/Control-Lens-Indexed.html"
                mapM_ clickElem ["Source", "Applicative"]
                getCurrentURL `shouldReturn` "http://localhost:8000/base0/src/Control-Applicative.html"

startingGhcdoc specs = H.beforeAll (do
  conf <- initialGhcdoc <&> \x -> x{ noOpen = True }
  threadId <- forkIO $ mainWith conf
  threadDelay 100000 -- Server.mainWith could release a lock (fill an MVar with newPort)
  return threadId
  ) $ H.afterAll killThread 
        $ H.aroundWith (. const ()) -- H.ignoreSubject if hspec >= 2.8.2
          specs

specsPQ = do
  describe "ParseQuery" $ do
    it "concrete" $
      parseNameQS "+X -Y" `H.shouldBe` Ok [NameQ (PM1 [PM "+"]) (PQ.Name "X") NoPM, NameQ (PM1 [PM "-"]) (PQ.Name "Y") NoPM]


main = do
  -- before this you need to run
  -- java -jar selenium-server-standalone-*.jar
  -- but really webdriver should have an option to block/pol etc.
  -- hspec specsInteg
  -- hspec specsUnit
  hspec specsPQ
  Text.Show.Pretty.pPrint =<< parseHoogleFile "hl3.txt"
