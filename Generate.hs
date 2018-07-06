#!/usr/bin/env stack
-- stack script --resolver nightly-2018-04-14

{-# OPTIONS -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.String (fromString)
import Data.String.Conversions (cs)
import Distribution.ModuleName (ModuleName, components, fromComponents)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, readGenericPackageDescription, runParseResult)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Types.CondTree (condTreeData)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, condLibrary)
import Distribution.Types.Library (exposedModules, reexportedModules)
import Distribution.Types.ModuleReexport (ModuleReexport (..))
import Distribution.Types.PackageName (PackageName)
import Distribution.Verbosity (silent)
import Network.HTTP.Simple (getResponseBody, getResponseHeader, getResponseStatusCode, httpLBS)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified Data.ByteString.Lazy.Char8 as L8

getExposedModules :: GenericPackageDescription -> [ModuleName]
getExposedModules pd =
  case condLibrary pd of
    Just x -> exposedModules (condTreeData x)
    Nothing -> error "No library"

downloadFile :: String -> FilePath -> IO [ModuleName]
downloadFile from to = do
  ex <- doesFileExist to
  bs <- if ex
  then do
    putStrLn $ "Re-using existing " ++ to
    L8.readFile to
  else do
    putStrLn $ "Downloading: " ++ from
    response <- httpLBS $ fromString from
    putStrLn $ "Status: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    let body = getResponseBody response
    L8.writeFile to body
    pure body
  case runParseResult (parseGenericPackageDescription (cs bs)) of
    (_, Right res) -> do
      putStrLn $ "Parsed " ++ to
      pure $ getExposedModules res
    err -> error $ "parse error: " ++ show err

setReexportedModules :: GenericPackageDescription -> [ModuleReexport] -> GenericPackageDescription
setReexportedModules pd mods =
  pd
    { condLibrary =
      case condLibrary pd of
        Just cn -> Just cn { condTreeData = (condTreeData cn) { reexportedModules = mods } }
        Nothing -> Nothing
    }

mkReexport :: PackageName -> ModuleName -> ModuleReexport
mkReexport pkg m = ModuleReexport (Just pkg) m m

cabalFileUrl :: String -> String -> String
cabalFileUrl name version
  =  "http://raw.githubusercontent.com/commercialhaskell/all-cabal-files/hackage/"
  ++ name
  ++ "/" ++ version
  ++ "/" ++ name ++ ".cabal"

migratePath :: FilePath
migratePath = "base-compat-migrate.cabal"

baseVersion :: String
baseVersion = "4.11.0.0"

baseCompatVersion :: String
baseCompatVersion = "0.10.4"

main :: IO ()
main = do
  ex <- doesFileExist migratePath
  unless ex $
    error $ "Couldn't find " ++ migratePath ++ ", are you in the repository root?"
  createDirectoryIfMissing False "tmp"
  allBaseModules                <- downloadFile
    (cabalFileUrl "base" baseVersion)
    ("tmp/base-" ++ baseVersion ++ ".cabal")
  allBaseCompatModules          <- downloadFile
    (cabalFileUrl "base-compat" baseCompatVersion)
    ("tmp/base-compat-" ++ baseCompatVersion ++ ".cabal")
  let compatModules              = filter ((/= "Repl") . last . components) $ allBaseCompatModules
  let compatModulesWithoutCompat = fromComponents . init . components <$> compatModules
  let baseModules                = filter (`notElem` compatModulesWithoutCompat) allBaseModules
  pd <- readGenericPackageDescription silent migratePath
  let pd' = setReexportedModules pd $ map (mkReexport "base") baseModules ++ map (mkReexport "base-compat") compatModules
  writeGenericPackageDescription migratePath pd'
