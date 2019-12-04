import Json2
import Scanner
import Data.Maybe
import System.IO

main = do
    input <- readFile "example.json"
    let log = jArray (tokenize input)
    --log <- parseFile "example.json" jsonValue
    --putStrLn $ show $ getJsonArray $ fromJust log
    --putStrLn $ show $ getId $ head $ getJsonArray $ fromJust log
    --putStrLn $ show $ getById "c8948c063943d511a56d9f79a7ad3de90a81468f" (fromJust log)
    --putStrLn $ show $ getRoots $ fromJust log
    --putStrLn $ show $ getMessage $ head $ getJsonArray $ fromJust log
    --putStrLn $ show $ drawLinesFromNode (fromJust log) (head $ getJsonArray $ fromJust log)
    --putStrLn $ show $ drawDigraph (fromJust log)
    --putStrLn $ show $ (tokenize input)
    writeFile "log.gv" (drawDigraph (fromJust $ fst log))


-- F-cie na pracu s JsonValue ----------------------------------------
getJsonArray :: JsonValue -> [JsonValue]
getJsonArray (JsonArray xs) = xs
getJsonArray _ = []

getJsonString :: JsonValue -> String
getJsonString (JsonString s) = s
getJsonString _ = ""

-- F-cie na pracu s JsonObject ---------------------------------------

compareId :: String -> JsonValue -> Bool
compareId id (JsonObject xs) = id == (getJsonString $ snd $ head $ filter (\a -> (fst a) == "id") xs)

getId :: JsonValue -> JsonValue
getId (JsonObject xs) = snd $ head $ filter (\a -> (fst a) == "id") xs

getParentIds :: JsonValue -> [JsonValue]
getParentIds (JsonObject xs) = getJsonArray $ snd $ head $ filter (\a -> (fst a) == "parent_ids") xs

getMessage :: JsonValue -> String
getMessage (JsonObject xs) = getJsonString $ snd $ head $ filter(\a -> (fst a) == "message") xs

-- F-cie pre pracu si JsonArray---------------------------------------

getById :: String -> JsonValue -> JsonValue
getById id jArray = head $ filter (compareId id) (getJsonArray jArray)

getRoots :: JsonValue -> [JsonValue]
getRoots jArray = filter (\a -> (length (getParentIds a)) == 0) (getJsonArray jArray)

drawLinesFromNode :: JsonValue -> JsonValue -> [String]
drawLinesFromNode jArray obj = map (\a -> (drawMessage obj) ++ " -> " ++ (drawMessage $ getById (getJsonString a) jArray) ++ ";") (getParentIds obj)

drawMessage :: JsonValue -> String
drawMessage obj = show $ getMessage obj

drawAllLines :: JsonValue -> [String]
drawAllLines jArray = foldr (++) [] (map (drawLinesFromNode jArray) (getJsonArray jArray))

drawDigraph :: JsonValue -> String
drawDigraph jArray = "digraph {\n" ++ (unlines $ drawAllLines jArray) ++ "}"
