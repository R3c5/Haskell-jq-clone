module Jq.JParser where

import Parsing.Parsing
import Jq.Json

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJBoolTrue :: Parser JSON
parseJBoolTrue = do
    _ <- string "true"
    return (JBool True)

parseJBoolFalse :: Parser JSON
parseJBoolFalse = do
    _ <- string "false"
    return (JBool False)

parseJString :: Parser JSON
parseJString = do
    _ <- char '"'
    x <- many notQuote
    _ <- char '"'
    return (JString x)

-- negJNum :: Parser JSON
-- negJNum = do
--         _ <- char '-'
--         n <- readDoubleNumber
--         return (JNumber (-n))

-- posJNum :: Parser JSON
-- posJNum = JNumber <$> readDoubleNumber

parseJNumber :: Parser JSON
parseJNumber = JNumber <$> readDoubleNumber

parseJArray :: Parser JSON
parseJArray = do
        _ <- token . char $ '['
        _ <- space
        el <- many parseEl
        _ <- token . char $ ']'
        return (JArray el)

parseJObject :: Parser JSON
parseJObject = do
        _ <- token . char $ '{'
        el <- many parseKeyVal
        _ <- token . char $ '}'
        return (JObject el)

parseEl :: Parser JSON
parseEl = do
    _ <- space
    x <- parseOrder
    _ <- space
    _ <- many (sat (== ','))
    _ <- space
    return x

parseKeyVal :: Parser (String, JSON) 
parseKeyVal = do 
    _ <- token . char $ '"'
    key <- many notQuote
    _ <- token . char $ '"'
    _ <- token . char $ ':'
    val <- parseEl
    return (key, val)


parseOrder :: Parser JSON
parseOrder =  parseJObject <|> parseJArray <|> parseJNumber <|> parseJString <|> parseJBoolTrue <|> parseJBoolFalse <|> parseJNull

parseJSON :: Parser JSON
parseJSON = token parseOrder
