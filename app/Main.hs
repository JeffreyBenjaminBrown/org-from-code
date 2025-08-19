module Main where

import System.Directory (getCurrentDirectory)
import System.FilePath  ((</>))
import Control.Monad    (forM)
import Data.List        (isSuffixOf)

import Util
import qualified Elisp as E
import qualified Rust as R


-- | Main for quick testing: writes all-code.org in the CWD.
main :: IO ()
main = do
  org <- generateOrgFromCurrentDir
  writeFile "all-code.org" org

-- | Entry points
generateOrgFromCurrentDir :: IO String
generateOrgFromCurrentDir = do
  cwd  <- getCurrentDirectory
  tree <- buildFileTree cwd
  fileTreeToOrg tree

generateOrgFromFileTree :: FilePath -> IO String
generateOrgFromFileTree rootPath = do
  tree <- buildFileTree rootPath
  fileTreeToOrg tree

-- | Emit Org for the whole tree.
fileTreeToOrg :: FileTree -> IO String
fileTreeToOrg t = do
  base <- getCurrentDirectory
  unlines <$> fileTreeToOrgLines base t 1

fileTreeToOrgLines :: FilePath -> FileTree -> Int -> IO [String]
fileTreeToOrgLines base (Directory abspath children) depth = do
  let prefix  = if "/" `isSuffixOf` base then base else base ++ "/"
      relpath = if take (length prefix) abspath == prefix
                   then drop (length prefix) abspath else abspath
      shown   = if null relpath then "." else relpath
      here    = stars depth ++ " " ++ "[[./" ++ (shown ++ "/") ++ "]]"
  below <- let f c = fileTreeToOrgLines base c $ depth + 1
           in concat <$> mapM f children
  pure $ here : below
fileTreeToOrgLines base (File abspath) depth = do
  let prefix  = if "/" `isSuffixOf` base then base else base ++ "/"
      relpath = if take (length prefix) abspath == prefix
                 then drop (length prefix) abspath else abspath
      shown   = if null relpath then "." else relpath
      here    = stars depth ++ " " ++ "[[./" ++ shown ++ "]]"
  subs <- emitSubtreeForFile abspath $ depth + 1
  pure $ here : subs

emitSubtreeForFile :: FilePath -> Int -> IO [String]
emitSubtreeForFile path fileDepth
  | ".rs" `isSuffixOf` path = do
      src <- readFile path
      let items = R.extractRustTopLevel src
      pure (concatMap (R.rustItemToOrg $ fileDepth + 1) items)
  | ".el" `isSuffixOf` path = do
      src <- readFile path
      let items = E.extractElispTopLevel src
      pure (concatMap (E.elItemToOrg $ fileDepth + 1) items)
  | otherwise = pure []

addSlash :: FilePath -> FilePath
addSlash p | null p    = p
           | otherwise = p ++ "/"
