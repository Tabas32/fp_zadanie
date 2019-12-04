module Scanner where

import Data.Char

-- skipWhiteSpaces -------------------------------------------------------------

skipWhiteSpaces :: String -> String
skipWhiteSpaces (c:cs) | isSpace c   = skipWhiteSpaces cs
skipWhiteSpaces cs                   = cs 
-- nextWordAndRest -------------------------------------------------------------

nextWordAndRest :: String -> (String, String)
nextWordAndRest text = nextWordAndRest' (skipWhiteSpaces text) ""
    where
        nextWordAndRest' :: String -> String -> (String, String)
        nextWordAndRest' []           word              = (reverse word, [])
        nextWordAndRest' txt@('(':_)  word              = (reverse word, txt)
        nextWordAndRest' txt@(')':_)  word              = (reverse word, txt)
        nextWordAndRest' txt@('"':_)  word              = (reverse word, txt)
        nextWordAndRest' txt@(':':_)  word              = (reverse word, txt)
        nextWordAndRest' txt@('{':_)  word              = (reverse word, txt)
        nextWordAndRest' txt@('}':_)  word              = (reverse word, txt)
        nextWordAndRest' txt@('[':_)  word              = (reverse word, txt)
        nextWordAndRest' txt@(']':_)  word              = (reverse word, txt)
        nextWordAndRest' (s:rest)     word | isSpace s  = (reverse word, rest)
        nextWordAndRest' (c:rest)     word              = nextWordAndRest' rest (c:word)

-- tokenize  -------------------------------------------------------------------

{--
  This is a function to split a string by either spaces or '(' or ')'
  whereas parantheses will become tokens
--}
tokenize :: String -> [String]
tokenize text =
   case skipWhiteSpaces text of
     ""         -> []
     '(' : rest  -> "(" : tokenize rest
     ')' : rest  -> ")" : tokenize rest
     '"' : rest  -> "\"" : tokenize rest
     ':' : rest  -> ":" : tokenize rest
     '{' : rest  -> "{" : tokenize rest
     '}' : rest  -> "}" : tokenize rest
     '[' : rest  -> "[" : tokenize rest
     ']' : rest  -> "]" : tokenize rest
     txt        ->
       let
         (tkn, rest) = nextWordAndRest txt
        in
          tkn : tokenize (skipWhiteSpaces rest)

--------------------------------------------------------------------------------
