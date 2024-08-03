module Jq.Json where

-- import Data.Scientific (Scientific)
import Data.List (intercalate)
import Data.Char (ord)
import Numeric (showHex)

data JSON = JNothing | JNull
    | JString String
    | JNumber Double
    | JBool Bool
    | JObject [(String, JSON)]
    | JArray [JSON]
  deriving (Eq)

instance Show JSON where

  show json = renderJSON json 1 where
      insertTabs :: Int -> String
      insertTabs t = replicate (2*t) ' '

      intercalateTabs :: Int -> String 
      intercalateTabs t = ",\n" ++ insertTabs t

      renderTabs :: Int -> String -> String
      renderTabs t v 
        | v == "" = v
        | otherwise = "\n" ++ insertTabs t ++ v ++ "\n" ++ insertTabs (t-1)

      renderJSON :: JSON -> Int -> String
      renderJSON JNothing _ = ""
      
      renderJSON JNull _ = "null"

      renderJSON (JBool b) _
          | b = "true"
          | otherwise = "false"

      renderJSON (JNumber x) _
        | isInt x = show (truncate x :: Integer)
        | otherwise = show x
        where
          isInt :: Double -> Bool
          isInt y = y == fromIntegral (round y :: Integer)

      renderJSON (JString s) _ = '\"' : escapeString s ++ "\"" where
        escapeString :: String -> String
        escapeString = concatMap escapeChar

        escapeChar :: Char -> String
        escapeChar c
          | c `elem` ['\"', '\\'] = "\\" ++ [c]
          | c == '\r' = "\\r"
          | c == '\n' = "\\n"
          | c == '\t' = "\\t"
          | c == '\b' = "\\b"
          | c == '\f' = "\\f"
          | c `elem` ['\x00'..'\x1F'] = "\\u" ++ padTo4 (showHex (ord c) "")
          | otherwise = [c]

        padTo4 :: String -> String
        padTo4 st
          | length st < 4 = replicate (4 - length st) '0' ++ st
          | otherwise = st

      renderJSON (JObject o) t = "{" ++ renderTabs t pairs ++ "}" where
          pairs = intercalate (intercalateTabs t) (map (\(x, y) -> renderJSON (JString x) (t+1) ++ ": " ++ renderJSON y (t+1)) o)

      renderJSON (JArray l) t = "[" ++ renderTabs t vals ++ "]" where
          vals = intercalate (intercalateTabs t) (map (\el -> renderJSON el (t+1)) l )

-- instance Eq JSON where
--   JNull == JNull = True
--   _ == _ = undefined

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to JSON datatype
-- For the tests to succeed fill them in with functions that return correct JSON values
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC x = JNumber (fromIntegral x)

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject
