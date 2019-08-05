{-# LANGUAGE OverloadedStrings #-}
{-
Compile with:
    ghc --make CrossSumMain

To see minimal puzzle:
    ./CrossSumMain

To solve arbitrary 9x9 puzzle:
    cat puzzle.json | ./CrossSumMain -

    where puzzle.json resembles:

    {
      "rows": [
        {"x": 0, "y": 0, "length": 2, "sum": 3}
      ],
      
      "columns": [
        {"x": 0, "y": 1, "length": 2, "sum": 4}
      ]
    } 
-}

module Main where

import           Console
import           Control.Applicative
import           CrossSum
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import           System.Environment

main = do
    args <- getArgs
    if null args
    then printCrossSum minimumSums
    else if (head args) == "-"
         then LBS.getContents >>= printCrossSum . decodePuzzleSpec
         else error $ "unrecognized command line argument: " ++ (head args)

decodePuzzleSpec json = 
    case (eitherDecode json) of
        Right spec -> spec
        Left x     -> error $ "couldn't decode json: "
                            ++ x ++ ": " ++ LBS.unpack json

parsePuzzleSpec :: Value -> Parser PuzzleSpec
parsePuzzleSpec = withObject "puzzle spec" $ \v ->
    PuzzleSpec <$> v .: "rows" <*> v .: "columns"

instance (FromJSON PuzzleSpec) where
    parseJSON = parsePuzzleSpec

parseCrossSum :: Value -> Parser CrossSum
parseCrossSum = withObject "cross sum" $ \v -> do
    x <- v .: "x"
    y <- v .: "y"
    l <- v .: "length"
    s <- v .: "sum"
    return $ CrossSum (x, y) l s 

instance (FromJSON CrossSum) where
    parseJSON = parseCrossSum

minsum_json :: LBS.ByteString
minsum_json = "{\"rows\":[{\"x\":0, \"y\":0, \"length\":2, \"sum\": 3}],\"columns\":[{\"x\":0,\"y\":1,\"length\":2,\"sum\":4}]}"

minsum_decoded :: Either String PuzzleSpec
minsum_decoded = eitherDecode minsum_json

decodedOk = minsum_decoded == Right minimumSums

minimumSums :: PuzzleSpec
minimumSums = PuzzleSpec {
  rows = [ CrossSum (0, 0) 2 3 ],
  cols = [ CrossSum (0, 1) 2 4 ]
}

