module Elisp
  ( ElItem(..)
  , extractElispTopLevel
  , elItemToOrg
  ) where

import Data.Char (isSpace)
import qualified Data.Vector as V
import Util (stars)


-- Items we emit for Org
data ElItem
  = ElDefun  { headline :: String, commentBlock :: String, sigBlock :: String, codeBlock :: String }
  | ElDefvar { headline :: String, commentBlock :: String, sigBlock :: String, codeBlock :: String }
  deriving (Show)

-- Render an Elisp item as Org lines under a given depth
elItemToOrg :: Int -> ElItem -> [String]
elItemToOrg depth (ElDefun h c s b) =
  [ stars depth ++ " " ++ h
  , stars (depth+1) ++ " comment"
  ] ++ block c ++
  [ stars (depth+1) ++ " type signature"
  ] ++ block s ++
  [ stars (depth+1) ++ " code"
  ] ++ block b
  where block t = if null t then [] else lines t
elItemToOrg depth (ElDefvar h c s b) =
  [ stars depth ++ " " ++ h
  , stars (depth+1) ++ " comment"
  ] ++ block c ++
  [ stars (depth+1) ++ " type signature"
  ] ++ block s ++
  [ stars (depth+1) ++ " code"
  ] ++ block b
  where block t = if null t then [] else lines t

-- Public entry: scan a buffer of Emacs Lisp source for top-level defun/defvar
extractElispTopLevel :: String -> [ElItem]
extractElispTopLevel src =
  let ls     = V.fromList (lines src)
      offs   = lineOffsets src
      idx2ln = indexToLine offs
      n      = length src
  in scanTop src ls offs idx2ln n 0 False 0 False 0 0 []

--------------------------------------------------------------------------------
-- Scanner (top-level only; nested fns stay in code, not as headings)
--------------------------------------------------------------------------------

scanTop
  :: String            -- ^ source
  -> V.Vector String   -- ^ lines
  -> V.Vector Int      -- ^ line start offsets
  -> (Int -> Int)      -- ^ index -> line number
  -> Int               -- ^ n
  -> Int               -- ^ i
  -> Bool              -- ^ in line comment ';'
  -> Int               -- ^ block depth for #| ... |#
  -> Bool              -- ^ in string
  -> Int               -- ^ paren depth
  -> Int               -- ^ comment nest depth for block (#| nesting)
  -> [ElItem]          -- ^ acc (reversed)
  -> [ElItem]
scanTop s ls offs idx2ln n i inLine blockCDepth inStr parenDepth commentNest acc
  | i >= n = reverse acc
  | otherwise =
      let c  = s !! i
          c1 = if i+1 < n then Just (s !! (i+1)) else Nothing
          top = parenDepth == 0 && not inStr && not inLine && blockCDepth == 0
      in case () of
        _ | inLine && c == '\n'
            -> scanTop s ls offs idx2ln n (i+1) False blockCDepth inStr parenDepth commentNest acc

          -- end of #| ... |# (possibly nested)
          | blockCDepth > 0 && c == '|' && c1 == Just '#'
            -> scanTop s ls offs idx2ln n (i+2) inLine (blockCDepth-1) inStr parenDepth commentNest acc

          | blockCDepth > 0
            -> scanTop s ls offs idx2ln n (i+1) inLine blockCDepth inStr parenDepth commentNest acc

          -- start of ; line comment
          | not inStr && c == ';'
            -> scanTop s ls offs idx2ln n (skipToNL s (i+1)) False blockCDepth inStr parenDepth commentNest acc

          -- start of #| block comment
          | not inStr && c == '#' && c1 == Just '|'
            -> scanTop s ls offs idx2ln n (i+2) inLine (blockCDepth+1) inStr parenDepth commentNest acc

          -- strings (simple escapes)
          | c == '"' && not inStr
            -> scanTop s ls offs idx2ln n (i+1) inLine blockCDepth True parenDepth commentNest acc
          | c == '"' && inStr && not (escaped s i)
            -> scanTop s ls offs idx2ln n (i+1) inLine blockCDepth False parenDepth commentNest acc

          -- parens
          | c == '(' && not inStr
            -> if top && startsSymbol s (i+1) "defun"
                 then let defStart    = i
                          lineStart   = offs V.! idx2ln i
                          headLine    = takeWhile (/= '\n') (drop lineStart s)
                          preCmt      = gatherPreElispComments ls (idx2ln i)
                          (sigEndEx, postCmt, codeStart) = defunSignature s defStart
                          formEndEx   = endOfTopForm s defStart
                          sigTxt      = take (sigEndEx - defStart) (drop defStart s)
                          codeTxt     = take (formEndEx - codeStart - 1) (drop codeStart s) -- exclude final ')'
                          item        = ElDefun headLine (preCmt <> postCmt) sigTxt codeTxt
                      in scanTop s ls offs idx2ln n formEndEx inLine blockCDepth inStr parenDepth commentNest (item:acc)
                 else if top && startsSymbol s (i+1) "defvar"
                 then let defStart    = i
                          lineStart   = offs V.! idx2ln i
                          headLine    = takeWhile (/= '\n') (drop lineStart s)
                          preCmt      = gatherPreElispComments ls (idx2ln i)
                          (sigEndEx, postCmt, codeStart) = defvarSignature s defStart
                          formEndEx   = endOfTopForm s defStart
                          sigTxt      = take (sigEndEx - defStart) (drop defStart s)
                          codeTxt     = take (formEndEx - codeStart - 1) (drop codeStart s)
                          item        = ElDefvar headLine (preCmt <> postCmt) sigTxt codeTxt
                      in scanTop s ls offs idx2ln n formEndEx inLine blockCDepth inStr parenDepth commentNest (item:acc)
                 else scanTop s ls offs idx2ln n (i+1) inLine blockCDepth inStr (parenDepth+1) commentNest acc

          | c == ')' && not inStr
            -> scanTop s ls offs idx2ln n (i+1) inLine blockCDepth inStr (max 0 (parenDepth-1)) commentNest acc

          | otherwise
            -> scanTop s ls offs idx2ln n (i+1) inLine blockCDepth inStr parenDepth commentNest acc

--------------------------------------------------------------------------------
-- Signatures and slicing
--------------------------------------------------------------------------------

-- defun: signature ends after the close-paren of the arglist.
-- Returns (sigEndExclusive, postSignatureComments, codeStart)
defunSignature :: String -> Int -> (Int, String, Int)
defunSignature s defStart =
  let p0         = skipSpacesComments s (defStart + 1 + length "defun") -- after "(defun"
      pNameEnd   = skipNameOrList s p0
      p1         = skipSpacesComments s pNameEnd
      -- expect arglist beginning '('
      argOpen    = p1
      argCloseEx = skipBalanced s argOpen
      sigEndEx   = argCloseEx
      (postC, codeStart) = takeLeadingElispComments s sigEndEx
  in (sigEndEx, postC, codeStart)

-- defvar: "signature" ends just after the variable name (or after "(defvar VAR)").
-- We then collect any comments immediately after that, as post-signature comments.
defvarSignature :: String -> Int -> (Int, String, Int)
defvarSignature s defStart =
  let p0       = skipSpacesComments s (defStart + 1 + length "defvar")
      nameEnd  = skipNameOrList s p0
      p1       = skipSpacesComments s nameEnd
      sigEndEx = if p1 < length s && s !! p1 == ')' then p1 + 1 else nameEnd
      (postC, codeStart) = takeLeadingElispComments s sigEndEx
  in (sigEndEx, postC, codeStart)

-- Find the index just after the final ')' that closes the top-level form at defStart.
endOfTopForm :: String -> Int -> Int
endOfTopForm s defStart =
  let (_, jEx) = collectUntilMatch s (defStart+1) 1
  in jEx

--------------------------------------------------------------------------------
-- Preceding comments (lines above), including #| ... |# blocks (may contain blank lines)
--------------------------------------------------------------------------------

gatherPreElispComments :: V.Vector String -> Int -> String
gatherPreElispComments lvec defLine =
  unlines (go (defLine-1) [])
  where
    go ln acc
      | ln < 0 = acc
      | otherwise =
          let line = lvec V.! ln
              t    = dropWhile isSpace line
          in if isLineComment t
                then go (ln-1) (line:acc)
             else if endsBlockComment t
                then let (ln', chunk) = climbBlock (ln-1) [line]
                     in go ln' (chunk ++ acc)
             else if all isSpace line
                then acc   -- blank line (outside block) breaks association
             else acc

    isLineComment t = not (null t) && head t == ';'

    endsBlockComment t = "|#" `suffixOfTrim` t || ("|#" `elem` words t)

    climbBlock ln acc
      | ln < 0 = (0, acc)
      | otherwise =
          let line = lvec V.! ln
          in if startsBlockComment (dropWhile isSpace line)
                then (ln, line:acc)
                else climbBlock (ln-1) (line:acc)

    startsBlockComment t = take 2 t == "#|"

    suffixOfTrim suf s' = suf `elem` tails (dropWhile isSpace s')
    tails xs = case xs of [] -> [[]]; _ -> xs : tails (tail xs)

--------------------------------------------------------------------------------
-- Token/lex helpers (strings, comments, parens)
--------------------------------------------------------------------------------

escaped :: String -> Int -> Bool
escaped s i =
  let backslashes = length (takeWhile (== '\\') (reverse (takeWhile (== '\\') (take i s))))
  in odd backslashes

skipSpaces :: String -> Int -> Int
skipSpaces s p
  | p >= length s = p
  | isSpace (s !! p) = skipSpaces s (p+1)
  | otherwise        = p

-- Skip immediate comment(s) (#|...|# blocks and ; line comments) and whitespace.
skipSpacesComments :: String -> Int -> Int
skipSpacesComments s p =
  let p1 = skipSpaces s p
  in if p1+1 < length s && s !! p1 == '#' && s !! (p1+1) == '|'
        then skipSpacesComments s (skipBlockComment s (p1+2) 1)
     else if p1 < length s && s !! p1 == ';'
        then skipSpacesComments s (skipToNL s (p1+1))
     else p1

-- Take and return the text of leading comments (and whitespace) at 'p'; return (text, indexAfter)
takeLeadingElispComments :: String -> Int -> (String, Int)
takeLeadingElispComments s p0 =
  let (p1, ws) = takeSpaces s p0
  in case () of
       _ | p1+1 < length s && s !! p1 == '#' && s !! (p1+1) == '|' ->
            let q  = skipBlockComment s (p1+2) 1
                txt = take (q - p1) (drop p1 s)
                (rest, p2) = takeLeadingElispComments s q
            in (ws <> txt <> rest, p2)
         | p1 < length s && s !! p1 == ';' ->
            let q  = skipToNL s (p1+1)
                txt = take (q - p1) (drop p1 s)
                (rest, p2) = takeLeadingElispComments s q
            in (ws <> txt <> rest, p2)
         | otherwise -> (ws, p1)

takeSpaces :: String -> Int -> (Int, String)
takeSpaces s p
  | p >= length s = (p, "")
  | isSpace (s !! p) =
      let (q, acc) = takeSpaces s (p+1)
      in (q, (s !! p) : acc)
  | otherwise = (p, "")

skipToNL :: String -> Int -> Int
skipToNL s q
  | q >= length s = length s
  | s !! q == '\n' = q+1
  | otherwise      = skipToNL s (q+1)

-- #| ... |# block comments (nested)
skipBlockComment :: String -> Int -> Int -> Int
skipBlockComment s p depth
  | p >= length s = length s
  | otherwise =
      case s !! p of
        '#' | p+1 < length s && s !! (p+1) == '|'
              -> skipBlockComment s (p+2) (depth+1)
        '|' | p+1 < length s && s !! (p+1) == '#'
              -> if depth == 1 then p+2 else skipBlockComment s (p+2) (depth-1)
        '"'  -> skipBlockComment s (skipString s (p+1)) depth
        _    -> skipBlockComment s (p+1) depth

skipString :: String -> Int -> Int
skipString s p
  | p >= length s   = length s
  | s !! p == '"'   = p+1
  | s !! p == '\\'  = skipString s (p+2)
  | otherwise       = skipString s (p+1)

-- Skip a symbol name or a parenthesized list (handles (setf foo))
skipNameOrList :: String -> Int -> Int
skipNameOrList s p =
  let p1 = skipSpacesComments s p
  in if p1 < length s && s !! p1 == '('
        then skipBalanced s p1
     else skipSymbol s p1

skipSymbol :: String -> Int -> Int
skipSymbol s p =
  let isSymChar ch =
        not (isSpace ch) && ch /= '(' && ch /= ')' && ch /= ';' && ch /= '"' && ch /= '#'
  in if p >= length s then p else
       let q = p + length (takeWhile isSymChar (drop p s))
       in q

-- Skip a balanced list starting at '('; return index just after matching ')'
skipBalanced :: String -> Int -> Int
skipBalanced s p
  | p >= length s || s !! p /= '(' = p
  | otherwise =
      let (_, jEx) = collectUntilMatch s (p+1) 1
      in jEx

-- Collect until matching ')'; returns (accumulated text, indexAfter). Used for bodies and blocks.
collectUntilMatch :: String -> Int -> Int -> (String, Int)
collectUntilMatch s p depth
  | p >= length s = ("", length s)
  | depth == 0    = ("", p)
  | otherwise =
      case s !! p of
        ';' -> let q = skipToNL s (p+1)
               in addSeg s p q (collectUntilMatch s q depth)
        '#' | p+1 < length s && s !! (p+1) == '|'
               -> let q = skipBlockComment s (p+2) 1
                  in addSeg s p q (collectUntilMatch s q depth)
        '"' -> let q = skipString s (p+1)
               in addSeg s p q (collectUntilMatch s q depth)
        '(' -> let (rest, r) = collectUntilMatch s (p+1) (depth+1)
               in (take 1 (drop p s) <> rest, r)
        ')' -> let (_   , r) = collectUntilMatch s (p+1) (depth-1)
               in ("", r)
        _   -> collectUntilMatch s (p+1) depth

addSeg :: String -> Int -> Int -> (String, Int) -> (String, Int)
addSeg s p q (rest, r) = (take (q - p) (drop p s) <> rest, r)

--------------------------------------------------------------------------------
-- Line/offset mapping (for headlines & pre-comments)
--------------------------------------------------------------------------------

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
      bsearch lo hi
        | hi - lo <= 1 = lo
        | otherwise =
            let mid = (lo + hi) `div` 2
            in if offs V.! mid <= i then bsearch mid hi else bsearch lo mid
  in max 0 (bsearch 0 (n-1))

-- Does s at pos begin with given symbol name, with a non-symbol boundary?
startsSymbol :: String -> Int -> String -> Bool
startsSymbol s pos w =
  let m = length w
      okStart = pos == 0 || delim (s !! (pos-1))
      okEnd   = pos + m >= length s || delim (s !! (pos+m))
      delim ch = isSpace ch || ch == '(' || ch == ')' || ch == '"' || ch == ';'
  in take m (drop pos s) == w && okStart && okEnd
