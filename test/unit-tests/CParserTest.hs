module CParserTest (cParserTests) where

import Jq.CParser (parseFilter)
import Jq.Filters (Filter (..))
import Parsing.Parsing (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Prelude hiding (fail)
import Jq.Json

cParserTests :: TestTree
cParserTests = testGroup "CParser tests" [
    testCase "identityTest" $ "." `parseTo` Identity,
    testCase "indTest" $ ".[2]" `parseTo` FIndex 2 False,
    testCase "indTest" $ ".[1.0e-1]" `parseTo` FIndex 1.0e-1 False,
    testCase "indTest" $ ".[-1.1]" `parseTo` FIndex (-1.1) False,
    testCase "indOptTest" $ ".[2]?" `parseTo` FIndex 2 True,
    testCase "sliceTest" $ ".[2:4]" `parseTo` FSlice (Just 2) (Just 4) False,
    testCase "sliceDecimalTest" $ ".[:1.1]" `parseTo` FSlice Nothing (Just 1.1) False,
    testCase "sliceDecimalTest" $ ".[:-1.1]" `parseTo` FSlice Nothing (Just (-1.1)) False,
    testCase "sliceLeftEmptyTest" $ ". [ :4]" `parseTo` FSlice Nothing (Just 4) False,
    testCase "sliceRightEmptyTest" $ ".[2:]" `parseTo` FSlice (Just 2) Nothing False,
    testCase "sliceRightEmptyTest" $ ".[-0.9:]" `parseTo` FSlice (Just (-0.9)) Nothing False,
    testCase "iteratorEmptyTest" $ ".[]" `parseTo` FIterator [] False,
    testCase "iteratorEmptyOptTest" $ ".[]? ?? ?" `parseTo` FIterator [] True,
    testCase "iteratorTest" $ ".[1, 3, 2]" `parseTo` FIterator [1, 3, 2] False,
    testCase "identifierStringTest" $ ".a2_c" `parseTo` FStringInd "a2_c" False,
    testCase "identifierStringTest" $ ".[\"a2_c\"]?" `parseTo` FStringInd "a2_c" True,
    -- testCase "?before]" $ ".[\"a2_c\"?]" `parseTo` FStringInd "a2_c" True,
    testCase "identifierStringTest" $ ".[\"a2_c\"]?? ?? ?" `parseTo` FStringInd "a2_c" True,
    testCase "identifierStringTest" $ ".\"a2_c\"?" `parseTo` FStringInd "a2_c" True,
    testCase "identifierStringTest" $ ".\"1a2_c\"?" `parseTo` FStringInd "1a2_c" True,
    testCase "identifierStringNoFirstNumTest" $ fail ".1a2_c" ,
    testCase "identifierStringNoFirstNumTest" $ fail ".1a2_c?" ,
    testCase "identifierStringNoSpaceTest" $ fail ". a2_c?" ,
    testCase "identifierQuotesStringTest" $ ". \"a 2.._c\"" `parseTo` FStringInd "a 2.._c" False,
    testCase "identifierGenericStringTest" $ ". [  \"a 2.._c\" ]" `parseTo` FStringInd "a 2.._c" False,
    testCase "pipeTest" $ ".|.a" `parseTo` FPipe Identity (FStringInd "a" False),
    testCase "implicitpipeTest" $ ".a.b" `parseTo` FPipe (FStringInd "a" False) (FStringInd "b" False),
    testCase "desc" $ ".." `parseTo` FDescOp,
    testCase "commaStrAndIntIndex" $ ".d.[0],.d.[1]" `parseTo` FComma (FPipe (FStringInd "d" False) (FIndex 0 False)) (FPipe (FStringInd "d" False) (FIndex 1 False)),
    testCase "commaDesc" $ ".,.." `parseTo` FComma Identity FDescOp,
    testCase "commaDesc" $ ".user, .projects[]" `parseTo` FComma (FStringInd "user" False) (FPipe (FStringInd "projects" False) (FIterator [] False)),
    testCase "objConsCompl" $ "{a,b,(.d.[0],.d.[1]):.a,e:.b}" `parseTo` LitObj [(Literal (JString "a"), FStringInd "a" False),
                                (Literal (JString "b"), FStringInd "b" False),
                                (FParan (FComma (FPipe (FStringInd "d" False) (FIndex 0 False)) (FPipe (FStringInd "d" False) (FIndex 1 False))), FStringInd "a" False),
                                (Literal (JString "e"), FStringInd "b" False)],
    testCase "objConsSimple" $ "{\"a\",b,\"a$2\"}" `parseTo` LitObj [(Literal (JString "a"), FStringInd "a" False), (Literal (JString "b"), FStringInd "b" False), (Literal (JString "a$2"), FStringInd "a$2" False)],
    testCase "subTest" $ "4 - .a" `parseTo` FSub (Literal $ JNumber 4) (FStringInd "a" False),

    testCase "singlObjs" $ "{\"k\": {\"a\": 1, \"b\": 2}}" `parseTo` LitObj [(Literal $ JString "k", LitObj [(Literal $ JString "a", Literal $ JNumber 1), (Literal $ JString "b", Literal $ JNumber 2)])],
    testCase "multObjs" $ "{\"k\": {\"a\": 1, \"b\": 2}} * {\"k\": {\"a\": 0,\"c\": 3}}" `parseTo` FMul (LitObj [(Literal $ JString "k", LitObj [(Literal $ JString "a", Literal $ JNumber 1), (Literal $ JString "b", Literal $ JNumber 2)])]) (LitObj [(Literal $ JString "k", LitObj [(Literal $ JString "a", Literal $ JNumber 0), (Literal $ JString "c", Literal $ JNumber 3)])]),
    -- testCase "fourDotFail" $ fail "....",
    testCase "failure" $ fail ""]

parseTo :: String -> Filter -> Assertion
parseTo s j = case parse parseFilter s of
  [(v, "")] -> assertEqual ("Expected:\n" ++ show j ++ "\ngot:\n" ++ show v) j v
  [(v, s)] -> assertFailure $ "Parsed:\n" ++ show v ++ "\nwith remainder:\n" ++ show s
  _ -> assertFailure "Parsing failed"

fail :: String -> Assertion
fail s = case parse parseFilter s of
  [(v, "")] -> assertFailure $ "Parsing should fail but succeeded with:\n" ++ show v
  _ -> return ()

