module RollDice.Parser where

import Text.ParserCombinators.Parsec

data RollStatement = RollStatement {
                        mRollCount :: Maybe Int
                      , mDiceCount :: Maybe Int
                      , sideCount :: Int
                      , mDropCount :: Maybe Int
                      , modFuns :: [Int -> Int] }

type parseResult = Either ParseError RollStatement

parseRollStatement :: String -> Either ParseError [RollStatement]
parseRollStatement = parse rollstatement "(unknown)"

