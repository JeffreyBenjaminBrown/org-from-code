module Util
  ( FileTree(..)
  , buildFileTree
  , isIgnoredDir
  , isValidFile
  , isValidNode
  , stars
  ) where

import System.Directory
import System.FilePath
import Data.List (sort, isSuffixOf)

-- | Basic tree structure (directories may be empty when filtered).
data FileTree = File FilePath | Directory FilePath [FileTree]
  deriving (Eq, Ord, Show)

-- | Build the file tree (filters common junk dirs).
buildFileTree :: FilePath -> IO FileTree
buildFileTree path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      contents <- listDirectory path
      let filtered = filter (not . isIgnoredDir) contents
      children <- mapM (buildFileTree . (path </>)) filtered
      let valid = filter isValidNode (sort children)
      if null valid then pure (Directory path []) else pure (Directory path valid)
    else do
      if isValidFile path then pure (File path) else pure (Directory path [])

-- | Ignore typical build/config directories.
isIgnoredDir :: FilePath -> Bool
isIgnoredDir d = d `elem`
  [ ".config", "target", "temp", ".git", ".stack-work", ".direnv", ".cache", ".vscode", ".idea" ]

-- | We currently only care about Rust and (for headings only) elisp files.
isValidFile :: FilePath -> Bool
isValidFile p = any (`isSuffixOf` p) [".rs", ".el"]

-- | Keep non-empty directories; files always valid.
isValidNode :: FileTree -> Bool
isValidNode (File _)        = True
isValidNode (Directory _ l) = not (null l)

-- | Heading stars helper.
stars :: Int -> String
stars n = replicate (max 1 n) '*'
