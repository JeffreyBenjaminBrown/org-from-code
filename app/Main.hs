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
fileTreeToOrg t = unlines <$> fileTreeToOrgLines t 1

fileTreeToOrgLines :: FileTree -> Int -> IO [String]
fileTreeToOrgLines (Directory path children) depth = do
  let here = stars depth ++ " " ++ addSlash path
  below <- concat <$> mapM (\c -> fileTreeToOrgLines c (depth + 1)) children
  pure (here : below)
fileTreeToOrgLines (File path) depth = do
  let here = stars depth ++ " " ++ path
  subs <- emitSubtreeForFile path depth
  pure (here : subs)

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
