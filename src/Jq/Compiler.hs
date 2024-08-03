module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.List

import Data.Ord

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]

compile Identity inp = return [inp]
compile (TryCatch t c) inp = 
        let ct = compile t inp
        in case ct of
            Left _  -> compile c inp
            Right r -> return r

compile FDescOp inp = return $ decomposeJS inp
compile (Literal js) _ = return [js]
compile (LitArray js) inp = do 
    mapped <- mapM (`compile` inp) js
    return [JArray (concat mapped)]

compile (LitObj js) inp = do 
    mapped <- mapM (compileKVs inp) js
    kvs <- mapM  (\(k,v) -> do { vk <- verifyKeys k; return (vk, v) }) mapped
    let cbs =  traverse allCombsStrJson kvs
    return $ map (JObject . sortBy (comparing fst)) cbs

compile (FParan f) inp = compile f inp

compile (FStringInd {}) JNull = return [JNull]
compile (FStringInd s _) (JObject ls) = findStringInd s ls
compile (FStringInd _ opt) _
    | opt = return []
    | otherwise = Left "Can only apply string index filter on Objects!"

compile (FIndex {}) JNull = return [JNull]
compile (FIndex i _) (JArray xs) = let ind = boundInd (takeDec i) (length xs)  
                                    in return [findInd xs ind]
compile (FIndex _ opt) _ 
    | opt = return []
    | otherwise = Left "Can not take index of non array"

compile (FSlice {}) JNull = return [JNull]
compile (FSlice Nothing Nothing _) (JString _) = Left "Can not perform slice of empty bounds"
compile (FSlice Nothing Nothing _) (JArray _) = Left "Can not perform slice of empty bounds"
compile (FSlice i j _) (JString s) = return  [JString (computeSlice i j s)]
compile (FSlice i j _) (JArray xs) = return  [JArray $ computeSlice i j xs]
compile (FSlice _ _ opt) _
    | opt = return []
    | otherwise = Left "Can not take slice of non array"

compile (FIterator ls _) JNull = return $ replicate (length ls) JNull
compile (FIterator [] _) (JArray xs) = return xs
compile (FIterator ls _) (JArray xs) = return $ map (\i -> findInd xs (boundInd (takeDec i) (length xs))) ls
compile (FIterator [] _) (JObject els) = return $ map snd els
compile (FIterator _ opt) _
    | opt = return []
    | otherwise = Left "Can not iterate over non array"

compile (FComma f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    return (r1 ++ r2)

compile (FPipe f1 f2) inp = do
        res <- compile f1 inp
        mapped <- mapM (compile f2) res
        return (concat mapped)


compile (FAdd f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry addition) combs

compile (FMul f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry multiplication) combs

compile (FSub f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry subtraction) combs

compile (FDiv f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry division) combs

compile (FEq f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry equality) combs

compile (FNEq f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry inequality) combs

compile (FLT f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry compareLT) combs

compile (FLTE f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry compareLTE) combs

compile (FGT f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry compareGT) combs

compile (FGTE f1 f2) inp = do
    r1 <- compile f1 inp
    r2 <- compile f2 inp
    let combs = [(e1, e2) | e1 <- r1, e2 <- r2]
    mapM (uncurry compareGTE) combs   

compile (FIF c b1 b2) inp = do
    rc <- compile c inp
    lc <- mapM (conditional inp b1 b2) rc
    return $ concat lc

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p = p

retNothing :: Either a [JSON]
retNothing = Right [JNothing]

findStringInd :: String -> [(String, JSON)] -> Either String [JSON]
findStringInd s ls =
    let res = map snd $ filter (\(key, _) -> key == s) ls
    in case res of
        [] -> return [JNull]
        xs -> return [last xs]

boundInd :: Int -> Int -> Int
boundInd x l
    | x < 0 = l + x
    | otherwise =  x


takeDec :: Double -> Int
takeDec x
    | x < 0 = ceiling x
    | otherwise = floor x

boundifyIndLeft :: Double -> Int -> Int
boundifyIndLeft x l
    | x < 0 = max (floor (fromIntegral l + x)) 0
    | otherwise = floor x

boundifyIndRight :: Double -> Int -> Int
boundifyIndRight x l
    | x < 0 = max (ceiling (fromIntegral l + x)) 0
    | otherwise = ceiling x

takeSlice :: Int -> Int -> [a] -> [a]
takeSlice _ _ [] = []
takeSlice i j xs
    | i >= j = []
    | otherwise = take (j - i) $ drop i xs


findInd :: [JSON] -> Int -> JSON
findInd [] _= JNull 
findInd (x : _) 0 = x
findInd (_ : xs) i = findInd xs (i-1)

decomposeJS :: JSON -> [JSON]
decomposeJS (JArray xs) = JArray xs : concatMap decomposeJS xs
decomposeJS (JObject ls) = JObject ls : concatMap (decomposeJS . snd) ls
decomposeJS js = [js]


computeSlice :: Maybe Double -> Maybe Double -> [a] -> [a]
computeSlice Nothing Nothing _ = []
computeSlice (Just i) Nothing xs = let sind = boundifyIndLeft i (length xs)   
                                            in takeSlice sind (length xs) xs
computeSlice Nothing (Just j) xs = let eind = boundifyIndRight j (length xs)   
                                            in takeSlice 0 eind xs
computeSlice (Just i) (Just j) xs = 
    let sind = boundifyIndLeft i (length xs)
        eind = boundifyIndRight j (length xs)   
    in takeSlice sind eind xs


compileKVs :: JSON -> (Filter, Filter) -> Either String ([JSON], [JSON])
compileKVs inp (k,v) = do
    ck <- compile k inp
    cv <- compile v inp
    return (ck,cv)


verifyKeys :: [JSON] -> Either String [String]
verifyKeys [JString s] = return [s]
verifyKeys ((JString s) : xs) = do
    ls <- verifyKeys xs 
    return (s : ls)
verifyKeys _ = Left "Can not construct non string key"


allCombsStrJson :: ([a], [b]) -> [(a, b)]
allCombsStrJson ([], _) = []
allCombsStrJson (_, []) = []
allCombsStrJson (st : strs, jss) = zip (repeat st) jss ++ allCombsStrJson (strs, jss) 

addition :: JSON -> JSON -> Either String JSON
addition JNull x = return x
addition x JNull = return x
addition (JNumber x) (JNumber y) = return $ JNumber (x + y)
addition (JString x) (JString y) = return $ JString (x ++ y)
addition (JArray xs) (JArray ys) = return $ JArray (xs++ys)
addition (JObject xs) (JObject ys) =
                    let combs = filter (\(k, _) -> k `notElem` map fst ys) xs ++ ys
                    in return $ JObject combs
addition _ _ = Left "can only add together numbers, arrays, strings, and objects"


subtraction :: JSON -> JSON -> Either String JSON
subtraction (JNumber x) (JNumber y) = return $ JNumber (x - y)
subtraction (JArray xs) (JArray ys) = return $ JArray  (filter (`notElem` ys) xs)
subtraction _ _ = Left "Can only subtract numbers from numbers, and arrays from arrays"


division :: JSON -> JSON -> Either String JSON
division (JNumber x) (JNumber y)
    | y == 0 = Left "Can not divide by 0"
    | otherwise = return $ JNumber (x / y)
division (JString x) (JString y) = return $ JArray (map JString (splitOn x y))
division _ _ = Left "Be careful with division!"

splitOn :: String -> String -> [String]
splitOn [] _ = [""]
splitOn str delim= go str
  where
    go [] = [""]
    go (c:cs)
      | delim `isPrefixOf` (c:cs) = "" : splitOn (drop (length delim) (c:cs)) delim
      | otherwise = (c : head rest) : tail rest
      where
        rest = go cs


multiplication :: JSON -> JSON -> Either String JSON
multiplication (JNumber x) (JNumber y) = return $ JNumber (x * y)
multiplication (JNumber x) (JString y) = return $ multiplyStrings x y
multiplication (JString y) (JNumber x) = return $ multiplyStrings x y
multiplication (JObject x) (JObject y) = return $ JObject (mergeObjects x y)
multiplication _ _ = Left " Can not multiply non numbers or non obj"


mergeObjects :: [(String, JSON)] -> [(String, JSON)] -> [(String, JSON)]
mergeObjects [] ys = ys
mergeObjects xs [] = xs
mergeObjects ((k1, v1) : xs) ys =
  case lookup k1 ys of
    Nothing -> (k1, v1) : mergeObjects xs ys
    Just v2 -> (k1, mergeJSON v1 v2) : mergeObjects xs (delete (k1, v2) ys)

mergeJSON :: JSON -> JSON -> JSON
mergeJSON (JObject xs) (JObject ys) = JObject $ mergeObjects xs ys
mergeJSON _ y = y

multiplyStrings :: Double -> String -> JSON
multiplyStrings x y
    | x < 0 = JNull
    | otherwise = JString $ concat $ replicate (floor x) y

equality :: JSON -> JSON -> Either String JSON
equality x y = return $ JBool (x == y)

inequality :: JSON -> JSON -> Either String JSON
inequality x y = return $ JBool (x /= y)

compareLT :: JSON -> JSON -> Either String JSON
compareLT (JNumber x) (JNumber y) = return $ JBool (x < y)
compareLT _ _ = Left "Can not compare non numbers"

compareLTE :: JSON -> JSON -> Either String JSON
compareLTE (JNumber x) (JNumber y) = return $ JBool (x <= y)
compareLTE _ _ = Left "Can not compare non numbers"

compareGT :: JSON -> JSON -> Either String JSON
compareGT (JNumber x) (JNumber y) = return $ JBool (x > y)
compareGT _ _ = Left "Can not compare non numbers"

compareGTE :: JSON -> JSON -> Either String JSON
compareGTE (JNumber x) (JNumber y) = return $ JBool (x >= y)
compareGTE _ _ = Left "Can not compare non numbers"


conditional :: JSON -> Filter -> Filter -> JSON -> Either String [JSON]
conditional inp b1 _ (JBool True) = compile b1 inp
conditional inp _ b2 (JBool False) = compile b2 inp
conditional _ _ _ _= Left "Conditional must be boolean"