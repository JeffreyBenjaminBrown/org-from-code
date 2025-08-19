module Rust
  ( RustItem(..)
  , extractRustTopLevel
  , rustItemToOrg
  ) where

import Data.Char (isSpace)
import Data.List (isSuffixOf)
import qualified Data.Vector as V

-- | Org rendering helpers (simple, no styling).
stars :: Int -> String
stars n = replicate (max 1 n) '*'

-- | Rust items we extract.
data RustItem
  = ItemFn   { headline :: String, commentBlock :: String, sigBlock :: String, codeBlock :: String }
  | ItemImpl { implHead :: String, implCode     :: String }
  deriving (Show)

-- | Convert a RustItem into Org lines. Caller passes desired heading depth.
rustItemToOrg :: Int -> RustItem -> [String]
rustItemToOrg depth (ItemFn h c s b) =
  [ stars depth ++ " " ++ h
  , stars (depth+1) ++ " comment"
  ] ++ block c ++
  [ stars (depth+1) ++ " type signature"
  ] ++ block s ++
  [ stars (depth+1) ++ " code"
  ] ++ block b
  where block t = if null t then [] else lines t
rustItemToOrg depth (ItemImpl h body) =
  [ stars depth ++ " " ++ h
  , stars (depth+1) ++ " code"
  ] ++ (if null body then [] else lines body)

-- | Public entry: extract top-level functions and impl blocks.
extractRustTopLevel :: String -> [RustItem]
extractRustTopLevel src =
  let ls     = V.fromList (lines src)
      offs   = lineOffsets src
      idx2ln = indexToLine offs
  in scanTopLevel src ls offs idx2ln

--------------------------------------------------------------------------------
-- Scanning top-level Rust
--------------------------------------------------------------------------------

scanTopLevel :: String -> V.Vector String -> V.Vector Int -> (Int -> Int) -> [RustItem]
scanTopLevel s ls offs idx2ln = go 0 False 0 False False False 0 []
  where
    n = length s
    go :: Int -> Bool -> Int -> Bool -> Bool -> Bool -> Int -> [RustItem] -> [RustItem]
    go i inLine blockDepth inStr rawStr inChar braceDepth acc
      | i >= n = reverse acc
      | otherwise =
          let c  = s !! i
              c1 = if i+1 < n then Just (s !! (i+1)) else Nothing
              atTop = braceDepth == 0 && blockDepth == 0 && not inStr && not inChar && not inLine && not rawStr
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
          -- strings (basic; raw strings not handled)
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
          -- top-level detections
          else if atTop && startsWord s i "impl"
               then
                 let lineStart     = offs V.! idx2ln i
                     headLine      = takeWhile (/= '\n') (drop lineStart s)
                     (inside, jEnd)= grabBraceBlock s i
                 in go jEnd inLine blockDepth inStr rawStr inChar braceDepth (ItemImpl headLine inside : acc)

          else if atTop && startsWord s i "fn"
               then
                 let fnLineStart = offs V.! idx2ln i
                     fnHeadLine  = takeWhile (/= '\n') (drop fnLineStart s)
                     (preSig, signature, postLead, body, jEnd) = sliceFn s ls offs idx2ln fnLineStart i
                     item = ItemFn fnHeadLine (preSig <> postLead) signature body
                 in go jEnd inLine blockDepth inStr rawStr inChar braceDepth (item:acc)

          else go (i+1) inLine blockDepth inStr rawStr inChar braceDepth acc

--------------------------------------------------------------------------------
-- Helpers (pure, top-level)
--------------------------------------------------------------------------------

-- | Check if word w begins at position i and is bounded by non-ident chars.
startsWord :: String -> Int -> String -> Bool
startsWord s i w =
  let m = length w
      okStart = i == 0 || not (isIdentChar (s !! (i-1)))
      okEnd   = i+m >= length s || not (isIdentChar (s !! (i+m)))
  in take m (drop i s) == w && okStart && okEnd

isIdentChar :: Char -> Bool
isIdentChar ch = ch == '_' || ch == '\'' || ch == '#'
              || ('0' <= ch && ch <= '9')
              || ('A' <= ch && ch <= 'Z')
              || ('a' <= ch && ch <= 'z')

-- | Offsets: start index of each line (0-based).
lineOffsets :: String -> V.Vector Int
lineOffsets s = V.fromList (0 : go 0 s)
  where
    go _ [] = []
    go i (c:cs)
      | c == '\n' = (i+1) : go (i+1) cs
      | otherwise = go (i+1) cs

-- | Map an index to a line number (binary search over offsets).
indexToLine :: V.Vector Int -> Int -> Int
indexToLine offs i =
  let n = V.length offs
      go lo hi
        | hi - lo <= 1 = lo
        | otherwise =
            let mid = (lo + hi) `div` 2
            in if offs V.! mid <= i then go mid hi else go lo mid
  in max 0 (go 0 (n-1))

-- | Skip to just after the end of the current line.
skipToNL :: String -> Int -> Int
skipToNL s q | q >= length s = length s
skipToNL s q =
  let rest = drop q s
      k    = length (takeWhile (/= '\n') rest)
  in q + k + (if k < length rest then 1 else 0)

-- | Skip a nested block comment starting right after '/*'.
skipBlock :: String -> Int -> Int -> Int
skipBlock s q d
  | q >= length s = length s
  | otherwise =
      case s !! q of
        '/' | q+1 < length s && s !! (q+1) == '*' -> skipBlock s (q+2) (d+1)
        '*' | q+1 < length s && s !! (q+1) == '/' -> if d == 1 then q+2 else skipBlock s (q+2) (d-1)
        '"'  -> skipBlock s (skipString s (q+1)) d
        '\'' -> skipBlock s (skipChar   s (q+1)) d
        _    -> skipBlock s (q+1) d

-- | Skip a string literal (handles escapes) starting after opening quote.
skipString :: String -> Int -> Int
skipString s q
  | q >= length s     = length s
  | s !! q == '"'     = q+1
  | s !! q == '\\'    = skipString s (q+2)
  | otherwise         = skipString s (q+1)

-- | Skip a char literal (handles escapes) starting after opening quote.
skipChar :: String -> Int -> Int
skipChar s q
  | q >= length s     = length s
  | s !! q == '\''    = q+1
  | s !! q == '\\'    = skipChar s (q+2)
  | otherwise         = skipChar s (q+1)

-- | From a starting index (at 'i' of \"impl\" or 'f' of \"fn\"), find the opening '{' while skipping strings/comments.
seekOpenBrace :: String -> Int -> Int
seekOpenBrace s p
  | p >= length s = p
  | otherwise =
      case s !! p of
        '/' | p+1 < length s && s !! (p+1) == '/' -> seekOpenBrace s (skipToNL s (p+2))
        '/' | p+1 < length s && s !! (p+1) == '*' -> seekOpenBrace s (skipBlock s (p+2) 1)
        '"'  -> seekOpenBrace s (skipString s (p+1))
        '\'' -> seekOpenBrace s (skipChar   s (p+1))
        '{'  -> p
        _    -> seekOpenBrace s (p+1)

-- | Collect text until the matching closing '}', starting inside the body.
-- Returns (collectedText, indexAfterClosingBrace).
collectUntilMatch :: String -> Int -> Int -> (String, Int)
collectUntilMatch s p depth
  | p >= length s = ("", length s)
  | depth == 0    = ("", p)
  | otherwise =
      case s !! p of
        '/' | p+1 < length s && s !! (p+1) == '/' ->
              let q = skipToNL s (p+2) in addSeg s p q (collectUntilMatch s q depth)
        '/' | p+1 < length s && s !! (p+1) == '*' ->
              let q = skipBlock s (p+2) 1 in addSeg s p q (collectUntilMatch s q depth)
        '"'  -> let q = skipString s (p+1) in addSeg s p q (collectUntilMatch s q depth)
        '\'' -> let q = skipChar   s (p+1) in addSeg s p q (collectUntilMatch s q depth)
        '{'  -> let (rest, r) = collectUntilMatch s (p+1) (depth+1)
                in (take 1 (drop p s) <> rest, r)
        '}'  -> let (_   , r) = collectUntilMatch s (p+1) (depth-1)
                in ("", r)
        _    -> collectUntilMatch s (p+1) depth

addSeg :: String -> Int -> Int -> (String, Int) -> (String, Int)
addSeg s p q (rest, r) = (take (q - p) (drop p s) <> rest, r)

-- | Grab the text inside the next brace-balanced block and the index after it.
grabBraceBlock :: String -> Int -> (String, Int)
grabBraceBlock s k =
  let bracePos  = seekOpenBrace s k
      (inside, j2) = collectUntilMatch s (bracePos+1) 1
  in (inside, j2)

-- | Gather comments/attributes immediately above the fn line.
-- Block comments may span multiple lines and contain blank lines.
gatherPreComments :: String -> V.Vector String -> (Int -> Int) -> Int -> String
gatherPreComments s lvec i2l fnLineStart =
  let fnLn = i2l fnLineStart
      go ln acc
        | ln <= 0   = acc
        | otherwise =
            let t = dropWhile isSpace (lvec V.! (ln-1))
            in if isLineAttr t || isLineComment t
                  then go (ln-1) (lvec V.! (ln-1) : acc)
               else if endsBlockComment (ln-1)
                  then let (ln', chunk) = climbBlock (ln-1) []
                       in go ln' (chunk ++ acc)
               else if all isSpace (lvec V.! (ln-1))
                  then acc  -- blank line breaks association (outside block)
               else acc
      endsBlockComment ln =
        let line = lvec V.! ln
        in "*/" `suffixOfTrim` line || ("*/" `elem` words line)
      climbBlock ln acc
        | ln < 0 = (0, acc)
        | otherwise =
            let line = lvec V.! ln
            in if "/*" `elem` words line
                  then (ln, line:acc)
                  else climbBlock (ln-1) (line:acc)
      isLineComment t = take 2 t == "//"
      isLineAttr    t = take 1 t == "#"
  in unlines (go fnLn [])

suffixOfTrim :: String -> String -> Bool
suffixOfTrim suf s = suf `isSuffixOf` dropWhile isSpace s

-- | After the opening '{', take whitespace + leading comments; return (text, indexAfter).
takeLeadingComments :: String -> Int -> (String, Int)
takeLeadingComments s p0 =
  let (p1, c1) = eatWS s p0 ""
  in case look2 s p1 of
       ('/','/') -> let p2 = skipToNL s (p1+2)
                        txt = take (p2 - p1) (drop p1 s)
                        (t, p3) = takeLeadingComments s p2
                    in (c1 <> txt <> t, p3)
       ('/','*') -> let p2 = skipBlock s (p1+2) 1
                        txt = take (p2 - p1) (drop p1 s)
                        (t, p3) = takeLeadingComments s p2
                    in (c1 <> txt <> t, p3)
       _         -> (c1, p1)

eatWS :: String -> Int -> String -> (Int, String)
eatWS s p acc
  | p >= length s = (p, acc)
  | let ch = s !! p
  , ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
  = eatWS s (p+1) (acc ++ [ch])
  | otherwise = (p, acc)

look2 :: String -> Int -> (Char, Char)
look2 s k | k+1 < length s = (s !! k, s !! (k+1))
          | otherwise      = ('\0','\0')

-- | Slice one function into its three parts plus end index.
-- Returns (preComments, signature, postBraceLeadingComments, body, indexAfter).
sliceFn :: String
        -> V.Vector String
        -> V.Vector Int
        -> (Int -> Int)
        -> Int  -- ^ fn line start (absolute index of line start)
        -> Int  -- ^ index of the 'f' in "fn"
        -> (String, String, String, String, Int)
sliceFn s lvec offs i2l fnLineStart fnTokIdx =
  let bracePos               = seekOpenBrace s fnTokIdx
      sigStart               = fnLineStart
      sigEnd                 = bracePos
      signature              = take (sigEnd - sigStart) (drop sigStart s)
      (postLead, bodyStart)  = takeLeadingComments s (bracePos + 1)
      (_, j2)                = collectUntilMatch s (bracePos+1) 1
      bodyFull               = take (j2 - bodyStart - 1) (drop bodyStart s) -- exclude final '}'
      preComments            = gatherPreComments s lvec i2l fnLineStart
  in (preComments, signature, postLead, bodyFull, j2)
