module Json2 where

import Data.Char
import Data.List

import Scanner
import Parser

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)


jsonVal :: Parser JsonValue
jsonVal = jNul `orElse` jBool `orElse` jNumber `orElse` jString
          `orElse` jArray `orElse` jObj

jNul :: Parser JsonValue
jNul = (token "null") `mapTo` \n -> JsonNull

jBool :: Parser JsonValue
jBool = ((token "true") `orElse` (token "false"))
            `mapTo`  \b -> if (b == "true") then (JsonBool True) else (JsonBool False)

jNumber :: Parser JsonValue
jNumber = (word `filterBy` (all isDigit)) `mapTo` \w -> (JsonNumber (read w::Double))

jString :: Parser JsonValue
jString = (token "\"") `andThen` \_ -> multiple (notToken "\"")
          `andThen` \s -> (token "\"") 
          `mapTo` \_ -> JsonString (unwords s)

jArray :: Parser JsonValue
jArray = (token "[") `andThen` \_ -> multipleSep jsonVal (token ",")
                    `andThen` \a -> (token "]") 
                        `mapTo` \_ -> JsonArray a


jObj :: Parser JsonValue
jObj = (token "{") `andThen` \_ -> multipleSep (jString
            `andThen` \(JsonString n) -> (token ":") 
            `andThen` \_ -> jsonVal  
            `mapTo` \v -> (n, v)) (token ",")
            `andThen` \a -> (token "}") 
            `mapTo`\_ -> JsonObject a


-- ====================================================================
-- ====================================================================

