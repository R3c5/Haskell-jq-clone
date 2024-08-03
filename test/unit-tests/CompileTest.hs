module CompileTest (compileTests) where

import Jq.CParser (parseFilter)
import Jq.Compiler (compile, run)
import Jq.Filters (Filter (..))
import Jq.Json (JSON (..))
import Parsing.Parsing (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Prelude hiding (fail)

compileTests :: TestTree
compileTests = testGroup "Compile tests" [
    testCase "identityNullTest" $ (Identity, JNull) `compileTo` [JNull],
 
    testCase "stringIndObj" $ (FStringInd "a" False, JObject [("a", JNumber 2), ("b", JNull)]) `compileTo` [JNumber 2],
    testCase "stringIndOther" $ fail (FStringInd "a" False) (JArray [JNull]),
    testCase "stringIndNotFound" $ (FStringInd "c" True, JObject [("a", JNumber 2), ("b", JNull)]) `compileTo` [JNull],

    testCase "optStringIndObj" $ (FStringInd "a" True, JObject [("a", JNumber 2), ("b", JNull)]) `compileTo` [JNumber 2],
    testCase "optStringIndOther" $ (FStringInd "a" True, JArray [JNull]) `compileTo` [],


    testCase "index1" $ (FIndex 1 False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 2],
    testCase "index1" $ (FIndex 1.1 False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 2],
    testCase "index1" $ (FIndex 2 False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 3],
    testCase "index1" $ (FIndex 0 False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 1],
    testCase "index2neg" $ (FIndex (-0.1) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 1],
    testCase "index2neg" $ (FIndex (-1.1) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 3],
    testCase "index2pos" $ (FIndex 0.1 False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 1],
    testCase "index3" $ (FIndex 4 False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNull],
    testCase "index3" $ (FIndex (-4) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNull],
    testCase "index4" $ (FIndex (-1) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 3],
    testCase "index5" $ (FIndex (-3) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 1],
    testCase "index5" $ (FIndex (-3.99) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 1],
    testCase "stringIndfail" $ fail (FIndex 2 False) (JString "abcd"),


    testCase "iter1" $ (FIterator [1,4,2,0] False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 2, JNull, JNumber 3, JNumber 1],
    testCase "iter2" $ (FIterator [-1, 2] False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 3, JNumber 3],
    testCase "iter" $ (FIterator [] False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JNumber 1, JNumber 2, JNumber 3],
    testCase "iterObj" $ (FIterator [] False, JObject [("a" , JNumber 1), ("b" , JNumber 2)]) `compileTo` [JNumber 1, JNumber 2],

    testCase "slices1" $ (FSlice (Just 1) (Just 2) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo`  [JArray [JNumber 2]],
    testCase "mixSlice" $ (FSlice (Just 0) (Just 2) False, JArray [JNumber 1, JBool True, JString "ab"]) `compileTo` [JArray[JNumber 1, JBool True]],
    testCase "slices2" $ (FSlice Nothing (Just 2) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 1, JNumber 2]],
    testCase "slices3" $ (FSlice (Just 1) Nothing False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 2, JNumber 3]],
    testCase "slicesoutOFBounds1" $ (FSlice (Just (-4)) (Just 4) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 1, JNumber 2, JNumber 3]],
    testCase "slicesoutOFBoundsBothNeg" $ (FSlice (Just (-4)) (Just (-2)) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 1]],
    testCase "slicesoutOFBoundsDec" $ (FSlice (Just 0.5) (Just 0.1) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 1]],
    testCase "slicesoutOFBoundsDec" $ (FSlice (Just 1.5) (Just (-1.1)) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 2]],
    testCase "slicesoutOFBoundsDec" $ (FSlice (Just (-1.5)) (Just (-1.1)) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 2]],
    testCase "slicesoutOFBoundsDec" $ (FSlice (Just (-1.5)) (Just 1.1) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 2]],
    testCase "slicesoutOFBounds2" $ (FSlice (Just (-2)) (Just 4) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 2, JNumber 3]],
    testCase "slicesoutOFBounds2" $ (FSlice (Just (-2)) Nothing False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 2, JNumber 3]],
    testCase "slicesoutOFBounds3" $ (FSlice (Just 0) (Just (-2)) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 1]],
    testCase "slicesoutOFBounds4" $ (FSlice (Just 0) (Just (-0.1)) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 1, JNumber 2, JNumber 3]],
    testCase "slicesoutOFBounds5" $ (FSlice (Just (-0.1)) Nothing False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 3]],
    testCase "slicesoutOFBounds5" $ (FSlice (Just 0.1) (Just 1.1) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[JNumber 1, JNumber 2]],
    testCase "slicesoutOFBoundsString" $ (FSlice (Just 1) (Just 3) False, JString "abcd") `compileTo` [JString "bc"],
    testCase "slicesoutOFBoundsString" $ (FSlice (Just 7) (Just 3) False, JString "abcd") `compileTo` [JString ""],
    testCase "slicesoutOFBoundsString" $ (FSlice (Just 0) (Just 3) False, JString "ab\n\tcd") `compileTo` [JString "ab\n"],
    testCase "slicesoutOFBoundsString" $ (FSlice (Just 0) (Just 2) False, JString "") `compileTo` [JString ""],
    testCase "slicesoutOFBounds6" $ (FSlice (Just 4) (Just 6) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[]],
    testCase "slicesoutOFBounds6" $ (FSlice (Just 0) (Just 1) False, JArray []) `compileTo` [JArray[]],
    testCase "slicesIndexInverted" $ (FSlice (Just 2) (Just 1) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[]],
    testCase "slicesIndexInverted" $ (FSlice (Just 1) (Just 1) False, JArray [JNumber 1, JNumber 2, JNumber 3]) `compileTo` [JArray[]],


    testCase "pipe" $ (FPipe (FStringInd "a" False) (FStringInd "b" False), JObject [("a", JObject [("a", JNumber 2), ("b", JNumber 3)]), ("b", JNull)]) `compileTo` [JNumber 3],
    testCase "pipefail" $ fail (FPipe (FIndex 2 False) (FStringInd "b" False)) (JObject [("a", JObject [("a", JNumber 2), ("b", JNumber 3)]), ("b", JNull)]),

    testCase "comma" $ (FComma (FStringInd "a" False) (FStringInd "b" False), JObject [("a", JObject [("a", JNumber 2), ("b", JNumber 3)]), ("b", JNull)]) `compileTo` [JObject [("a", JNumber 2), ("b", JNumber 3)], JNull],
    testCase "comma2" $ (FComma (FStringInd "user" False) (FPipe (FStringInd "projects" False) (FIterator [] False)), JObject [("user", JString "stedolan"), ("projects", JArray [JString "jq", JString "wikiflow"])]) `compileTo` [JString "stedolan", JString "jq", JString "wikiflow"],
    testCase "commafail" $ fail (FComma (FIndex 2 False) (FStringInd "b" False)) (JObject [("a", JObject [("a", JNumber 2), ("b", JNumber 3)]), ("b", JNull)]),


    testCase "sub" $ (FSub (Literal $ JNumber 4) (FStringInd "a" False), JObject [("a", JNumber 3), ("b", JNull)]) `compileTo` [JNumber 1],


    testCase "paranCommaPipe" $ (FComma (FPipe (FStringInd "a" False) (FStringInd "b" False)) (FStringInd "b" False), JObject [("a", JObject [("a", JNumber 2), ("b", JNumber 3)]), ("b", JBool True)]) `compileTo` [JNumber 3, JBool True],
    testCase "litArray" $ (LitArray [FStringInd "user" False, FPipe (FStringInd "projects" False) (FIterator [] False)] , JObject [("user",JString "stedolan"), ("projects", JArray [JString "jq", JString "wikiflow"])]) `compileTo` [JArray[JString "stedolan", JString "jq", JString "wikiflow"]],
    testCase "litObjKindOfComplex" $ (LitObj [(Literal (JString "a"), FStringInd "a" False),
                                (Literal (JString "b"), FStringInd "b" False),
                                (Literal (JString "d"), FComma (FStringInd "a" False) (FStringInd "b" False))],
                                JObject [("a",JNumber 1), ("b",JNumber 2), ("c",JNumber 3)]) `compileTo` 
                                [JObject [("a",JNumber 1), ("b",JNumber 2), ("d", JNumber 1)],
                                 JObject [("a",JNumber 1), ("b",JNumber 2), ("d", JNumber 2)]],
    testCase "litObjComplex" $ (LitObj [(Literal (JString "a"), FStringInd "a" False),
                                (Literal (JString "b"), FStringInd "b" False),
                                (FParan (FComma (FPipe (FStringInd "d" False) (FIndex 0 False)) (FPipe (FStringInd "d" False) (FIndex 1 False))), FStringInd "a" False),
                                (Literal (JString "e"), FStringInd "b" False)],
                                JObject [("a",JNumber 1), ("b",JNumber 2), ("c",JNumber 3), ("d", JArray[JString "c", JString "f"] )]) `compileTo` 
                                [JObject [("a",JNumber 1), ("b",JNumber 2), ("c", JNumber 1), ("e", JNumber 2)],
                                 JObject [("a",JNumber 1), ("b",JNumber 2), ("f", JNumber 1), ("e", JNumber 2)]],
    testCase "ParanNullTest" $ (FParan Identity, JNull) `compileTo` [JNull]]

compileTo :: (Filter, JSON) -> [JSON] -> Assertion
compileTo (f, j) o = case run (compile f) j of
  Left s -> assertFailure $ "Compilation failed with:\n" ++ s
  Right v -> assertEqual ("Expected:\n" ++ show o ++ "\ngot:\n" ++ show v) o v

fail :: Filter -> JSON -> Assertion
fail f j = case run (compile f) j of
  Right v -> assertFailure $ "Compilation should fail but succeeded with:\n" ++ show v
  _ -> return ()
