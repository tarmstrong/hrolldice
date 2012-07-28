{- Parsing code for hrolldice. -}
module RollDice.Parser
  (
      RollStatement (..)
    , parseRollStatement
    , maybeParseRollStatement
    , ParseError (..)
  ) where

import Text.ParserCombinators.Parsec

-- | RollStatement is a command that tells the program how many dice to
-- | roll, etc. Has Maybes for values that are optional.
data RollStatement = RollStatement {
                        rollCount :: Maybe Int
                      , diceCount :: Maybe Int
                      , sideCount :: Int
                      , dropCount :: Maybe Int
                      , modFuns :: [Int -> Int] }

-- | Take a string and attempt to parse it as a RollStatement.
parseRollStatement :: String -> Either ParseError RollStatement
parseRollStatement = parse rollstatement "(unknown)"

-- | parseRollStatement, but wrap the result in Maybe.
maybeParseRollStatement :: String -> Maybe RollStatement
maybeParseRollStatement statement =
  case parseRollStatement statement of
    Right rollstatement -> Just rollstatement
    Left  _             -> Nothing

{- Parser for a RollStatement. Examples include:
 
 * d6 : roll a six-sided die.
 * 4d6 : roll four six-sided die and sum the result.
 * 2x4d6 : same as 4d6, but perform it twice and show each result separately.
 * 4d6s1 : same as 4d6 but drop the lowest number rolled.
 * d6*2+1 : same as d6 but multiply it by 2 and then add 1
 * d6+1*2 : same as d6 but add 1 and then multiply the result by 2.
 
 In other words, the "modifier functions" at the end are processed in order from left to right. "+1*2-3" can be thought of as "\x -> (((x+1)*2)-3)".
-}
rollstatement :: Parser RollStatement
rollstatement = do
  nrolls <- optionMaybe . try $ rollcount 
  ndice <- optionMaybe . try $ dicecount 
  char 'd'
  nsides <- many1 digit
  ndrops <- optionMaybe . try $ dropcount
  modfuns <- many modfun
  return RollStatement {
                      rollCount = nrolls
                    , diceCount = ndice
                    , sideCount = read nsides
                    , dropCount = ndrops
                    , modFuns = modfuns }

-- | Parser for a roll count.
-- | "2x1d6" means roll one six-sided die two times
-- | and show the results separately. This parser
-- | parses the "2x".
rollcount :: Parser Int
rollcount = do
  count <- many1 digit
  char 'x'
  return . read $ count

-- | Parser for dice count.
-- | "4d6" means roll four six-sided dice. This parser parses
-- | the "4".
dicecount :: Parser Int
dicecount = do
  digs <- many1 digit
  return . read $ digs

-- | "s2" means drop the two lowest rolls.
dropcount :: Parser Int
dropcount = do
  char 's'
  many1 digit >>= return . read

-- | Parser for roll modifiers.
-- | "d6*2" means roll one six-sided die and double its result.
-- | "d6-2" means roll one six-sided die and subtract 2 from its result.
modfun :: Parser (Int -> Int)
modfun = do
  op <- oneOf "*+-"
  operand <- many1 digit
  let fun = case op of
        '*' -> (*)
        '+' -> (+)
        '-' -> (-)
  return (fun  (read operand))


-- dummyRollStatement = RollStatement {
--                       rollCount = Just 2
--                     , diceCount = Just 4
--                     , sideCount = 6
--                     , dropCount = Just 1
--                     , modFuns = [(*2), (+1)] }
