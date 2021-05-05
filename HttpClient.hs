{-# LANGUAGE OverloadedStrings #-}

module HttpClient (getNewPort, runningPwdEq) where

import Network.HTTP
import Network.URI
import Text.HTML.TagSoup
import System.Directory
import Data.Maybe

import Control.Exception

getNewPort port =
   let loop p = do
           r <- runningPwdEq p `catch` \(SomeException a) -> return Nothing
           -- change it to the actual exception thrown which looks like
           -- Exception: user error (openTCPConnection: failed to connect to 127.0.0.1:8000: Network.Socket.connect: <socket: 24>: does not exist (Connection refused))
           case r of
             Just False -> loop (p+1)
             Nothing -> return $ Just p
             Just True -> return Nothing
   in loop port

-- | runninPwdEq port looks at
-- http:://localhost:port for <title>x</title>
-- (before <body>), and compares this to pwd
runningPwdEq :: Int -> IO (Maybe Bool)
runningPwdEq port = do
        c <- Network.HTTP.simpleHTTP $ mkRequest Network.HTTP.GET $
                fromJust $ parseURI $ "http://localhost:"++show port
        case c of
          Left {} -> return Nothing
          Right Network.HTTP.Response{ rspBody = d } -> do
                  pwd <- getCurrentDirectory
                  return $ fmap (==pwd) $ grabTitle $ parseTags d

-- grabTitle :: [Tag B8.ByteString] -> Maybe B8.ByteString 
grabTitle (TagOpen "title" _ : TagText t : _) = Just t
grabTitle (TagOpen "body" _ : _ ) = Nothing
grabTitle (_:xs) = grabTitle xs
grabTitle [] = Nothing

