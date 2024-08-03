module JParserTest (jParserTests) where

import Jq.JParser (parseJSON)
import Jq.Json (JSON (..))
import Parsing.Parsing (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Prelude hiding (fail)

jParserTests :: TestTree
jParserTests = testGroup"JParser tests"[
    testCase "nullTest" $ "null" `parseTo` JNull,
    testCase "nullrnTest" $ "\"null\r\n\"" `parseTo` JString "null\r\n",
    testCase "stringTest" $ "\"a\na\\u0041b\"" `parseTo` JString "a\naAb", 
    testCase "stringTest" $ "\"#$()\"" `parseTo` JString "#$()", 
    testCase "failingString" $ "\"Aa\r\n\t\b\f\"" `parseTo` JString "Aa\r\n\t\b\f", 
    testCase "intTest" $ "123" `parseTo` JNumber 123, 
    testCase "fracTest" $ "-0.002" `parseTo` JNumber (-0.002), 
    testCase "scientificTest" $ "1e+3" `parseTo` JNumber 1000, 
    testCase "listEmptylist" $ "[]" `parseTo` JArray [], 
    testCase "simpleList" $ "[1, 2, 3]" `parseTo` JArray [JNumber 1, JNumber 2, JNumber 3], 
    testCase "composedList" $ "[[1, 2], 3]" `parseTo` JArray [JArray [JNumber 1, JNumber 2], JNumber 3], 
    testCase "simpleObj" $ "{\"a$2\" : 1, \"b\" : 2}" `parseTo` JObject [("a$2", JNumber 1), ("b", JNumber 2)], 
    testCase "complObj" $ "{\"a\" : 1, \"b\" : [1, 2]}" `parseTo` JObject [("a", JNumber 1), ("b", JArray [JNumber 1, JNumber 2])], 
    -- testCase "scientificTest2" $ "1.16E-1" `parseTo` JNumber 1.16E-1, 
    testCase "failure" $ fail "tnull"]

parseTo :: String -> JSON -> Assertion
parseTo s j = case parse parseJSON s of
    [(v, "")] -> assertEqual ("Expected:\n" ++ show j ++ "\ngot:\n" ++ show v) j v
    [(v, s)] -> assertFailure $ "Parsed:\n" ++ show v ++ "\nwith remainder:\n" ++ show s
    x -> assertFailure $ "Parsing failed: " ++ show x

fail :: String -> Assertion
fail s = case parse parseJSON s of
    [(v, "")] -> assertFailure $ "Parsing should fail but succeeded with:\n" ++ show v
    _ -> return ()
