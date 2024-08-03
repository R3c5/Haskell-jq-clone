module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.Json

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseOptional :: Parser Bool
parseOptional = (True <$ some (symbol "?")) <|> pure False


parseFindex :: Parser Filter
parseFindex = do
  _ <- symbol "["
  ind <- readDoubleNumber
  _ <- symbol "]"
  FIndex ind <$> parseOptional


parseSliceLeftEmpty :: Parser Filter
parseSliceLeftEmpty = do
  _ <- symbol "[" *> symbol ":"
  right <- readDoubleNumber
  _ <- symbol "]"
  FSlice Nothing (pure right) <$> parseOptional

parseSliceRightEmpty :: Parser Filter
parseSliceRightEmpty = do
  _ <- symbol "["
  left <- readDoubleNumber
  _ <- symbol ":"
  _ <- symbol "]"
  FSlice (pure left) Nothing <$> parseOptional

parseSliceBoth :: Parser Filter
parseSliceBoth = do
  _ <- symbol "["
  left <- readDoubleNumber
  _ <- symbol ":"
  right <- readDoubleNumber
  _ <- symbol "]"
  FSlice (pure left) (pure right) <$> parseOptional


parseListEl :: Parser Double
parseListEl = do
  _ <- space
  x <- readDoubleNumber
  _ <- space
  _ <- many (sat (== ','))
  _ <- space
  return x

parseFIterator :: Parser Filter
parseFIterator = do
  _ <- symbol "["
  ls <- many parseListEl
  _ <- symbol "]"
  FIterator ls <$> parseOptional

parseFSlice :: Parser Filter
parseFSlice = parseSliceBoth <|> parseSliceLeftEmpty <|> parseSliceRightEmpty
 

identStr :: Parser String
identStr = do
  c0 <- letter
  rest <- many (alphanum <|> char '_')
  return (c0 : rest)

parseIdentifString :: Parser Filter
parseIdentifString = do
  _ <- space
  _ <- char '.'
  str <- identStr
  FStringInd str <$> parseOptional

parseQuotesInd :: Parser Filter
parseQuotesInd = do
  _ <- symbol "."
  _ <- char '"'
  str <- many notQuote
  _ <- char '"'
  FStringInd str <$> parseOptional

parseGenericInd :: Parser Filter
parseGenericInd = do
  _ <-  symbol "["
  _ <- char '"'
  str <- many notQuote
  _ <- char '"'
  _ <- symbol "]"
  FStringInd str <$> parseOptional

parseFStringIdent :: Parser Filter
parseFStringIdent = parseQuotesInd <|> parseIdentifString

parseDotted :: Parser Filter
parseDotted = do
  _ <- symbol "."
  parseFindex <|> parseFSlice <|> parseFIterator <|> parseGenericInd

parseUndotted :: Parser Filter
parseUndotted = parseFindex <|> parseFSlice <|> parseFIterator <|> parseGenericInd

parseFParan :: Parser Filter
parseFParan = do
  _ <- symbol "("
  filt <- parseFilter
  _ <- symbol ")"
  return (FParan filt)

parseFDescOp :: Parser Filter
parseFDescOp = do
  _ <- symbol ".."
  return FDescOp


parseFPipe :: Parser Filter
parseFPipe = do
  left <- token parseFilter1
  _ <- symbol "|"
  FPipe left <$> parseFilter

parseFComma :: Parser Filter
parseFComma = do
  left <- token parseFilter1
  _ <- symbol ","
  FComma left <$> parseFilter

parseImplicitPipe :: Parser Filter
parseImplicitPipe = do
  left <- parseFilter1
  (parseUndotted <|> parseFilter) >>= parseImplicitCase left
    
parseImplicitCase :: Monad m => Filter -> Filter -> m Filter
parseImplicitCase left (FPipe _ f2) = return $ FPipe left f2
parseImplicitCase left (FComma f1 f2) = return $ FComma (FPipe left f1) f2
parseImplicitCase left f = return $ FPipe left f

parseLitNull :: Parser Filter
parseLitNull = do
  _ <- string "null"
  return $ Literal JNull

parseLitBoolTrue :: Parser Filter
parseLitBoolTrue = do
  _ <- string "true"
  return $ Literal (JBool True)

parseLitBoolFalse :: Parser Filter
parseLitBoolFalse = do
  _ <- string "false"
  return $ Literal (JBool False)

parseLitString :: Parser Filter
parseLitString = do 
    _ <- space
    _ <- char '"'
    x <- many notQuote
    _ <- char '"'
    _ <- space
    return $ Literal (JString x)

parseLitNumber :: Parser Filter
parseLitNumber = Literal . JNumber <$> readDoubleNumber

parseEmptyArr :: Parser Filter
parseEmptyArr = do
        _ <-symbol "["
        _ <- symbol "]"
        return (LitArray [])


parseFilledArr :: Parser Filter
parseFilledArr = do
        _ <-symbol "["
        x <- token parseFilter
        xs <- many (do { _ <- symbol ","; token parseFilter })
        _ <- symbol "]"
        return (LitArray (x : xs))

parseLitArray :: Parser Filter
parseLitArray = parseEmptyArr <|> parseFilledArr 


parseEmptyObj :: Parser Filter
parseEmptyObj = do
        _ <-symbol "{"
        _ <- symbol "}"
        return (LitObj [])


parseFilledObj :: Parser Filter
parseFilledObj = do
        _ <-symbol "{"
        x <- token parseObjEl
        xs <- many (do { _ <- symbol ","; token parseObjEl})
        _ <- symbol "}"
        return (LitObj (x : xs))

parseLitObj :: Parser Filter
parseLitObj = parseEmptyObj <|> parseFilledObj

parseObjEl :: Parser (Filter, Filter)
parseObjEl = parseKV <|> parseKNoV <|> parseKFilV

parseKNoV :: Parser (Filter, Filter) 
parseKNoV = do 
    str <- inQuotesString <|> identifier
    return (Literal (JString str), FStringInd str False)

parseKV :: Parser (Filter, Filter) 
parseKV = do 
    strkey <- inQuotesString <|>  identifier
    _ <- token $ symbol ":"
    val <- token parseFilter1
    return (Literal (JString strkey), val)

parseKFilV :: Parser (Filter, Filter)
parseKFilV = do
   key <- parseFParan
   _ <- token $ symbol ":"
   val <- token parseFilter1
   return (key, val)

parseTryCatch :: Parser Filter
parseTryCatch = do
  _ <- symbol "try"
  f1 <- parseFilter0
  _ <- symbol "catch"
  TryCatch f1 <$> parseFilter

parseAdd :: Parser Filter
parseAdd = do
  left <- token parseFilter1
  _ <- symbol "+"
  FAdd left <$> parseFilter  

parseSub :: Parser Filter
parseSub = do
  left <- token parseFilter1
  _ <- symbol "-"
  FSub left <$> parseFilter  

parseDiv :: Parser Filter
parseDiv = do
  left <- token parseFilter1
  _ <- symbol "/"
  FDiv left <$> parseFilter  

parseMul :: Parser Filter
parseMul = do
  left <- token parseFilter1
  _ <- symbol "*"
  FMul left <$> parseFilter 


parseEq :: Parser Filter
parseEq = do
  left <- token parseFilter1
  _ <- symbol "=="
  FEq left <$> parseFilter 

parseNEq :: Parser Filter
parseNEq = do
  left <- token parseFilter1
  _ <- symbol "/="
  FNEq left <$> parseFilter 

parseLT :: Parser Filter
parseLT = do
  left <- token parseFilter1
  _ <- symbol "<"
  FLT left <$> parseFilter 

parseGT :: Parser Filter
parseGT = do
  left <- token parseFilter1
  _ <- symbol ">"
  FGT left <$> parseFilter 


parseLTE :: Parser Filter
parseLTE = do
  left <- token parseFilter1
  _ <- symbol "<="
  FLTE left <$> parseFilter 

parseGTE :: Parser Filter
parseGTE = do
  left <- token parseFilter1
  _ <- symbol ">="
  FGTE left <$> parseFilter 

parseIF :: Parser Filter
parseIF = do
  _ <- symbol "if"
  cond <- parseFilter
  _ <- symbol "then"
  b1 <- parseFilter
  _ <- symbol "else"
  b2 <- parseFilter
  _ <- symbol "end"  
  return $ FIF cond b1 b2

parseComparisons :: Parser Filter
parseComparisons = parseEq <|> parseNEq <|> parseLT <|> parseLTE <|> parseGT <|> parseGTE

parseArith :: Parser Filter
parseArith =  parseAdd <|> parseSub <|> parseMul <|> parseDiv

parseLiteral :: Parser Filter
parseLiteral = parseLitObj <|> parseLitArray <|> parseLitString <|> parseLitNumber <|> parseLitBoolTrue <|> parseLitBoolFalse <|> parseLitNull

parseFilter1 :: Parser Filter
parseFilter1 = parseFParan <|> parseFDescOp <|> parseFStringIdent <|> parseDotted <|> parseIdentity <|> parseLiteral

parseFilter0 :: Parser Filter
parseFilter0 =  parseFPipe <|>  parseImplicitPipe <|> parseFComma <|> parseArith <|> parseComparisons <|> parseFilter1

parseFilter :: Parser Filter
parseFilter =  parseTryCatch <|> parseIF <|> parseFilter0



parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
