{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec.WebDriver
import System.Process
import Server hiding (main)
import System.Environment
import Control.Concurrent
import Control.Exception
import Control.Lens
import Test.WebDriver.Commands
import qualified Data.Map as M

import qualified Test.Hspec as H

-- success, now to 
--
--
-- separate file no I can pass --match to hspec
-- https://hspec.github.io/match.html
--
-- but --match gets passed on to webdriver
-- which doesn't make sense to me because --match should be
-- preventing the "ghcdoc hyperlinks" section from running
--
-- but starting initialGhcdoc...
              
pk0 = M.fromList 
          [("base", ["/home/aavogt/.ghcup/ghc/8.10.5/share/doc/ghc-8.10.5/html/libraries/base-4.14.2.0"]),
           ("distributive",["/home/aavogt/.cabal/store/ghc-8.10.5/distributive-0.6.2.1-5eac67187065aeb1d4e6e7768849fdde1a7a8bb57b77bc2ea3ab23d264873d59/share/doc/html"]) 
          ]


clickElem linkText = click =<< findElem (ByLinkText linkText)
specs = do
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
  conf <- initialGhcdoc <&> \x -> x{ openIndex = False }
  threadId <- forkIO $ mainWith conf
  threadDelay 100000 -- Server.mainWith could release a lock (fill an MVar with newPort)
  return threadId
  ) $ H.afterAll killThread $ H.ignoreSubject specs

main = do
  -- before this you need to run
  -- java -jar selenium-server-standalone-*.jar
  -- but really webdriver should have an option to block/pol etc.
  hspec specs
