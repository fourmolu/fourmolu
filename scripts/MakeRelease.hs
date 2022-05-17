#!/usr/bin/env stack
{- stack runghc
    --package aeson
    --package bytestring
    --package directory
    --package http-client
    --package http-client-tls
    --package http-types
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_, unless)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.ByteString.Lazy.Char8 as Char8L
import Data.List (isPrefixOf)
import Network.HTTP.Client
  ( Manager,
    Request (..),
    httpLbs,
    parseUrlThrow,
    responseBody,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)
import System.Directory (listDirectory)
import System.Environment (getEnv)

main :: IO ()
main = do
  token <- getEnv "token"
  version <- getEnv "version"
  repo <- getEnv "GITHUB_REPOSITORY"
  sha <- getEnv "GITHUB_SHA"

  changelogLines <- lines <$> readFile "CHANGELOG.md"

  unless (take 1 changelogLines == ["## Fourmolu " ++ version]) $
    error "CHANGELOG doesn't look updated"

  let versionName = "v" ++ version
      versionChanges = unlines . takeWhile (not . ("## Fourmolu" `isPrefixOf`)) . drop 1 $ changelogLines

  exes <- filter ("fourmolu-" `isPrefixOf`) <$> listDirectory "./bin/"
  putStrLn $ ">>>>> Creating release with files: " ++ show exes

  manager <- newTlsManager
  let mkGitHubURL url = do
        req <- parseUrlThrow url
        return
          req
            { requestHeaders =
                [ (hAccept, "application/vnd.github.v3+json"),
                  (hAuthorization, "token " <> Char8.pack token),
                  (hUserAgent, Char8.pack repo)
                ]
            }

  createReq <- mkGitHubURL $ "POST https://api.github.com/repos/" ++ repo ++ "/releases"
  createResp <-
    httpSend manager createReq $
      Aeson.encode . Aeson.object $
        [ "tag_name" .= versionName,
          "target_commitish" .= sha,
          "name" .= versionName,
          "body" .= versionChanges
        ]

  -- uploadUrl is in the format: "https://...{?name,label}"
  let uploadUrlBase = takeWhile (/= '{') (uploadUrl createResp)
  forM_ exes $ \exeName -> do
    uploadReq <- mkGitHubURL $ "POST " ++ uploadUrlBase ++ "?name=" ++ exeName
    exeContent <- ByteStringL.readFile exeName
    httpSend manager uploadReq exeContent :: IO Aeson.Value

  putStrLn $ ">>>>> Released fourmolu " ++ versionName ++ "!"

httpSend :: Aeson.FromJSON a => Manager -> Request -> ByteStringL.ByteString -> IO a
httpSend manager req body = do
  putStrLn $ ">>>>> Sending request: " ++ show req
  Char8L.putStrLn body
  resp <- httpLbs req manager
  putStrLn $ ">>>>> Got response: " ++ show resp
  either fail return . Aeson.eitherDecode . responseBody $ resp

data CreateReleaseResponse = CreateReleaseResponse
  { uploadUrl :: String
  }
  deriving (Show)

instance Aeson.FromJSON CreateReleaseResponse where
  parseJSON = Aeson.withObject "CreateReleaseResponse" $ \o ->
    CreateReleaseResponse
      <$> (o .: "upload_url")
