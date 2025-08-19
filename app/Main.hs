{-# LANGUAGE LambdaCase #-}
-- org_from_tree.hs
-- Build an Org document listing files and, for Rust files, top-level functions and impl blocks.
-- Function nodes have sub-branches: "comment", "type signature", "code".
-- Nested functions/methods stay inside the parent's "code" branch (no nested headings).

module Main where

import System.Directory
import System.FilePath
import Control.Monad
import Data.Char (isSpace)
import Data.List (sort, isSuffixOf)
import qualified Data.Vector as V

-- File tree
data FileTree = File FilePath | Directory FilePath [FileTree]
  deriving (Eq, Ord, Show)

-- Main for quick testing: writes all-code.org in the CWD
main :: IO ()
main = do
  org <- generateOrgFromCurrentDir
  writeFile "all-code.org" org

-- Entry points
generateOrgFromCurrentDir :: IO String
generateOrgFromCurrentDir = do
  cwd  <- getCurrentDirectory
  tree <- buildFileTree cwd
  fileTreeToOrg tree

generateOrgFromFileTree :: FilePath -> IO String
generateOrgFromFileTree rootPath = do
  tree <- buildFileTree rootPath
  fileTreeToOrg tree

-- Build tree (skip some dirs; include .rs and .el as files)
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

isIgnoredDir :: FilePath -> Bool
isIgnoredDir d = d `elem` [
  ".cache",
  ".config",
  ".direnv",
  ".git",
  ".idea",
  ".stack-work",
  ".vscode",
  "dist-newstyle",
  "target",
  "temp" ]

isValidFile :: FilePath -> Bool
isValidFile p = any (`isSuffixOf` p) [".rs", ".el"]

isValidNode :: FileTree -> Bool
isValidNode (File _)        = True
isValidNode (Directory _ l) = not (null l)

-- Emit Org for the whole tree
fileTreeToOrg :: FileTree -> IO String
fileTreeToOrg t = unlines <$> go t 1
  where
    go :: FileTree -> Int -> IO [String]
    go (Directory p cs) depth = do
      let here = stars depth ++ " " ++ addSlash p
      rest <- concat <$> mapM (\c -> go c (depth+1)) cs
      pure (here:rest)
    go (File p) depth = do
      let here = stars depth ++ " " ++ p
      if ".rs" `isSuffixOf` p
        then do
          src <- readFile p
          let items = extractRustTopLevel src
          pure (here : concatMap (rustItemToOrg depth) items)
        else pure [here]
    addSlash x = if null x then x else x ++ "/"

stars :: Int -> String
stars n = replicate (max 1 n) '*'

--------------------------------------------------------------------------------
-- Rust extraction
--------------------------------------------------------------------------------

data RustItem
  = ItemFn   { headline :: String, commentBlock :: String, sigBlock :: String, codeBlock :: String }
  | ItemImpl { implHead :: String, implCode     :: String }
  deriving (Show)

rustItemToOrg :: Int -> RustItem -> [String]
rustItemToOrg depth (ItemFn h c s b) =
  [ stars (depth+1) ++ " " ++ h
  , stars (depth+2) ++ " comment"
  ] ++ block c ++
  [ stars (depth+2) ++ " type signature"
  ] ++ block s ++
  [ stars (depth+2) ++ " code"
  ] ++ block b
  where block t = if null t then [] else lines t
rustItemToOrg depth (ItemImpl h body) =
  [ stars (depth+1) ++ " " ++ h
  , stars (depth+2) ++ " code"
  ] ++ (if null body then [] else lines body)

-- Top-level scanner
extractRustTopLevel :: String -> [RustItem]
extractRustTopLevel src =
  let s      = src
      ls     = V.fromList (lines s)
      offs   = lineOffsets s
      idx2ln = indexToLine offs
  in scan s ls offs idx2ln

-- Vector of absolute offsets for line starts
lineOffsets :: String -> V.Vector Int
lineOffsets s = V.fromList (0 : go 0 s)
  where
    go _ [] = []
    go i (c:cs)
      | c == '\n' = (i+1) : go (i+1) cs
      | otherwise = go (i+1) cs

indexToLine :: V.Vector Int -> Int -> Int
indexToLine offs i =
  let n = V.length offs
      go lo hi
        | hi - lo <= 1 = lo
        | otherwise =
            let mid = (lo + hi) `div` 2
            in if offs V.! mid <= i then go mid hi else go lo mid
  in max 0 (go 0 (n-1))

-- Core scanner: comments/strings/brace depth; only detect atTop (braceDepth==0)
scan :: String -> V.Vector String -> V.Vector Int -> (Int -> Int) -> [RustItem]
scan s ls offs idx2ln = go 0 False 0 False False False 0 []
  where
    n = length s
    go :: Int -> Bool -> Int -> Bool -> Bool -> Bool -> Int -> [RustItem] -> [RustItem]
    go i inLine blockDepth inStr rawStr inChar braceDepth acc
      | i >= n = reverse acc
      | otherwise =
          let c  = s !! i
              c1 = if i+1 < n then Just (s !! (i+1)) else Nothing
              atTop = braceDepth == 0 && blockDepth == 0 && not inStr && not inChar && not inLine && not rawStr
              startsWord w =
                let m = length w
                    okStart = i == 0 || not (isIdentChar (s !! (i-1)))
                    okEnd   = i+m >= n || not (isIdentChar (s !! (i+m)))
                in take m (drop i s) == w && okStart && okEnd
          in
          -- end of // comment
          if inLine && c == '\n' then go (i+1) False blockDepth inStr rawStr inChar braceDepth acc
          -- end of /*...*/
          else if blockDepth > 0 && c == '*' && c1 == Just '/'
               then go (i+2) inLine (blockDepth-1) inStr rawStr inChar braceDepth acc
          else if blockDepth > 0
               then go (i+1) inLine blockDepth inStr rawStr inChar braceDepth acc
          -- start of //
          else if not inStr && not inChar && c == '/' && c1 == Just '/'
               then go (i+2) True blockDepth inStr rawStr inChar braceDepth acc
          -- start of /*
          else if not inStr && not inChar && c == '/' && c1 == Just '*'
               then go (i+2) inLine (blockDepth+1) inStr rawStr inChar braceDepth acc
          -- strings (basic; raw strings not fully supported)
          else if not inChar && c == '"' && not inStr
               then go (i+1) inLine blockDepth True rawStr inChar braceDepth acc
          else if inStr && c == '"'
               then go (i+1) inLine blockDepth False rawStr inChar braceDepth acc
          -- char literal
          else if not inStr && c == '\'' && not inChar
               then go (i+1) inLine blockDepth inStr rawStr True braceDepth acc
          else if inChar && c == '\''
               then go (i+1) inLine blockDepth inStr rawStr False braceDepth acc
          -- braces
          else if c == '{'
               then go (i+1) inLine blockDepth inStr rawStr inChar (braceDepth+1) acc
          else if c == '}'
               then go (i+1) inLine blockDepth inStr rawStr inChar (max 0 (braceDepth-1)) acc

          -- top-level items
          else if atTop && startsWord "impl"
               then
                 let lineStart = offs V.! idx2ln i
                     headLine  = takeWhile (/= '\n') (drop lineStart s)
                     (inside, jEnd) = grabBraceBlock i
                 in go jEnd inLine blockDepth inStr rawStr inChar braceDepth (ItemImpl headLine inside : acc)

          else if atTop && startsWord "fn"
               then
                 let fnLineStart = offs V.! idx2ln i
                     fnHeadLine  = takeWhile (/= '\n') (drop fnLineStart s)
                     (preSig, signature, postLead, body, jEnd) = sliceFn fnLineStart i
                     item = ItemFn fnHeadLine (preSig <> postLead) signature body
                 in go jEnd inLine blockDepth inStr rawStr inChar braceDepth (item:acc)

          else go (i+1) inLine blockDepth inStr rawStr inChar braceDepth acc

    isIdentChar ch = ch == '_' || ch == '\'' || ch == '#'
                     || (ch >= '0' && ch <= '9')
                     || (ch >= 'A' && ch <= 'Z')
                     || (ch >= 'a' && ch <= 'z')

    -- Find a { ... } block starting from position k (at 'i' of "impl" or 'f' of "fn")
    -- Find a { ... } block starting from position k (at 'i' of "impl" or 'f' of "fn")
    grabBraceBlock :: Int -> (String, Int)
    grabBraceBlock k =
      let (bracePos, _) = seekOpenBrace k
          (inside, j2)  = collectUntilMatch (bracePos+1) 1
      in (inside, j2)
      where
        seekOpenBrace p
          | p >= n = (p,p)
          | otherwise =
              case s !! p of
                '/' | p+1 < n && s !! (p+1) == '/' -> seekOpenBrace (skipToNL (p+2))
                '/' | p+1 < n && s !! (p+1) == '*' -> seekOpenBrace (skipBlock (p+2) 1)
                '"'  -> seekOpenBrace (skipString (p+1))
                '\'' -> seekOpenBrace (skipChar (p+1))
                '{'  -> (p, p+1)
                _    -> seekOpenBrace (p+1)

        -- FIXED: carry both p and q so we can slice s[p..q)
        collectUntilMatch :: Int -> Int -> (String, Int)
        collectUntilMatch p depth
          | p >= n    = ("", n)
          | depth==0  = ("", p)
          | otherwise =
              case s !! p of
                '/' | p+1 < n && s !! (p+1) == '/' ->
                     let q = skipToNL (p+2)
                     in addSeg p q (collectUntilMatch q depth)
                '/' | p+1 < n && s !! (p+1) == '*' ->
                     let q = skipBlock (p+2) 1
                     in addSeg p q (collectUntilMatch q depth)
                '"'  -> let q = skipString (p+1)
                        in addSeg p q (collectUntilMatch q depth)
                '\'' -> let q = skipChar (p+1)
                        in addSeg p q (collectUntilMatch q depth)
                '{'  -> let (rest, r) = collectUntilMatch (p+1) (depth+1)
                        in (take 1 (drop p s) <> rest, r)
                '}'  -> let (_   , r) = collectUntilMatch (p+1) (depth-1)
                        in ("", r)
                _    -> collectUntilMatch (p+1) depth

        addSeg :: Int -> Int -> (String, Int) -> (String, Int)
        addSeg p q (rest, r) = (take (q - p) (drop p s) <> rest, r)

        skipToNL q | q >= n = n | otherwise =
          let rest = drop q s
              k    = length (takeWhile (/= '\n') rest)
          in q + k + (if k < length rest then 1 else 0)

        skipBlock q d
          | q >= n = n
          | otherwise =
              case s !! q of
                '/' | q+1 < n && s !! (q+1) == '*' -> skipBlock (q+2) (d+1)
                '*' | q+1 < n && s !! (q+1) == '/' -> if d == 1 then q+2 else skipBlock (q+2) (d-1)
                '"'  -> skipBlock (skipString (q+1)) d
                '\'' -> skipBlock (skipChar (q+1)) d
                _    -> skipBlock (q+1) d

        skipString q
          | q >= n        = n
          | s !! q == '"'  = q+1
          | s !! q == '\\' = skipString (q+2)
          | otherwise      = skipString (q+1)

        skipChar q
          | q >= n         = n
          | s !! q == '\'' = q+1
          | s !! q == '\\' = skipChar (q+2)
          | otherwise      = skipChar (q+1)

    -- Slice function into (preDoc/attrs, signature, postBraceLeadingComments, body, indexAfter)
    sliceFn :: Int -> Int -> (String,String,String,String,Int)
    sliceFn fnLineStart fnTokIdx =
      let (bracePos, _)          = seekOpenBrace fnTokIdx
          sigStart               = fnLineStart
          sigEnd                 = bracePos
          signature              = take (sigEnd - sigStart) (drop sigStart s)
          (postLead, bodyStart)  = takeLeadingComments (bracePos + 1)
          (_, j2)                = collectUntilMatch (bracePos+1) 1
          bodyFull               = take (j2 - bodyStart - 1) (drop bodyStart s)
          preComments            = gatherPreComments (indexToLine offs) fnLineStart
      in (preComments, signature, postLead, bodyFull, j2)
      where
        (seekOpenBrace, collectUntilMatch, skipToNL, skipBlock, skipString, skipChar) = grabbers
        grabbers =
          ( seekOpenBrace', collectUntilMatch', skipToNL', skipBlock', skipString', skipChar' )
          where
            seekOpenBrace' p
              | p >= n = (p,p)
              | otherwise =
                  case s !! p of
                    '/' | p+1 < n && s !! (p+1) == '/' -> seekOpenBrace' (skipToNL' (p+2))
                    '/' | p+1 < n && s !! (p+1) == '*' -> seekOpenBrace' (skipBlock' (p+2) 1)
                    '"' -> seekOpenBrace' (skipString' (p+1))
                    '\'' -> seekOpenBrace' (skipChar' (p+1))
                    '{' -> (p, p+1)
                    _   -> seekOpenBrace' (p+1)
            collectUntilMatch' p depth
              | p >= n    = ("", n)
              | depth==0  = ("", p)
              | otherwise =
                  case s !! p of
                    '/' | p+1 < n && s !! (p+1) == '/' -> let q = skipToNL' (p+2) in second' p q (collectUntilMatch' q depth)
                    '/' | p+1 < n && s !! (p+1) == '*' -> let q = skipBlock' (p+2) 1 in second' p q (collectUntilMatch' q depth)
                    '"'  -> let q = skipString' (p+1) in second' p q (collectUntilMatch' q depth)
                    '\'' -> let q = skipChar' (p+1)   in second' p q (collectUntilMatch' q depth)
                    '{'  -> let (rest, r) = collectUntilMatch' (p+1) (depth+1) in (take 1 (drop p s) <> rest, r)
                    '}'  -> let (_   , r) = collectUntilMatch' (p+1) (depth-1) in ("", r)
                    _    -> collectUntilMatch' (p+1) depth
            second' p q (rest, r) = (take (q - p) (drop p s) <> rest, r)
            skipToNL' q | q >= n = n | otherwise =
              let rest = drop q s
                  k    = length (takeWhile (/= '\n') rest)
              in q + k + (if k < length rest then 1 else 0)
            skipBlock' q d
              | q >= n = n
              | otherwise =
                  case s !! q of
                    '/' | q+1 < n && s !! (q+1) == '*' -> skipBlock' (q+2) (d+1)
                    '*' | q+1 < n && s !! (q+1) == '/' -> if d == 1 then q+2 else skipBlock' (q+2) (d-1)
                    '"'  -> skipBlock' (skipString' (q+1)) d
                    '\'' -> skipBlock' (skipChar' (q+1)) d
                    _    -> skipBlock' (q+1) d
            skipString' q
              | q >= n        = n
              | s !! q == '"'  = q+1
              | s !! q == '\\' = skipString' (q+2)
              | otherwise      = skipString' (q+1)
            skipChar' q
              | q >= n         = n
              | s !! q == '\'' = q+1
              | s !! q == '\\' = skipChar' (q+2)
              | otherwise      = skipChar' (q+1)

        -- take whitespace + leading comments after '{'
        takeLeadingComments p0 =
          let (p1, c1) = eatWS p0 ""
          in case look2 p1 of
               ('/','/') -> let p2 = skipToNL (p1+2)
                                txt = take (p2 - p1) (drop p1 s)
                                (t, p3) = takeLeadingComments p2
                            in (c1 <> txt <> t, p3)
               ('/','*') -> let p2 = skipBlock (p1+2) 1
                                txt = take (p2 - p1) (drop p1 s)
                                (t, p3) = takeLeadingComments p2
                            in (c1 <> txt <> t, p3)
               _         -> (c1, p1)
          where
            look2 k | k+1 < n = (s !! k, s !! (k+1))
                    | otherwise = ('\0','\0')
            eatWS p acc
              | p >= n = (p, acc)
              | let ch = s !! p
              , ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
              = eatWS (p+1) (acc ++ [ch])
              | otherwise = (p, acc)

        -- gather comments/attributes immediately above fn line; block comments may contain blank lines
        gatherPreComments i2l fnLineStart =
          let fnLn = i2l fnLineStart
              go ln acc
                | ln <= 0   = acc
                | otherwise =
                    let t = dropWhile isSpace (ls V.! (ln-1))
                    in if isLineAttr t || isLineComment t
                          then go (ln-1) (ls V.! (ln-1) : acc)
                       else if endsBlockComment (ln-1)
                          then let (ln', chunk) = climbBlock (ln-1) []
                               in go ln' (chunk ++ acc)
                       else if all isSpace (ls V.! (ln-1))
                          then acc  -- blank line (outside block) breaks association
                       else acc
              endsBlockComment ln =
                let line = ls V.! ln
                in "*/" `isSuffixOf` dropWhile isSpace line || ("*/" `elem` words line)
              climbBlock ln acc
                | ln < 0 = (0, acc)
                | otherwise =
                    let line = ls V.! ln
                    in if "/*" `elem` words line
                          then (ln, line:acc)
                          else climbBlock (ln-1) (line:acc)
              isLineComment t = take 2 t == "//"
              isLineAttr    t = take 1 t == "#"
          in unlines (go fnLn [])
