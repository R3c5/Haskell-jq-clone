module Jq.Filters where
import           Jq.Json

data Filter = Identity 
    | FDescOp
    | FStringInd String Bool
    | FIndex Double Bool
    | FSlice (Maybe Double) (Maybe Double) Bool
    | FIterator [Double] Bool
    | FPipe Filter Filter 
    | FComma Filter Filter
    | FParan Filter
    | Literal JSON
    | LitArray [Filter]
    | LitObj [(Filter, Filter)]
    | TryCatch Filter Filter
    | FAdd Filter Filter
    | FSub Filter Filter
    | FDiv Filter Filter
    | FMul Filter Filter
    | FEq Filter Filter
    | FNEq Filter Filter
    | FLT Filter Filter
    | FGT Filter Filter
    | FLTE Filter Filter
    | FGTE Filter Filter
    | FIF Filter Filter Filter
    deriving (Show, Eq)



data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC s = FStringInd s False

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = FPipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = FComma
