import Test.QuickCheck
import RollDice.Parser (RollStatement, maybeParseRollStatement)

import Data.Maybe (isJust)

goodRollStatements = [
  "d1", "d2", "d6", "d8", "d20", "d100",
  "1d6", "1d1", "2d6", "5d8", "82d3", "20d20",
  "1x4d6", "2x4d6", "6x4d6", "20x20d543", "2xd6",
  "3x4d6s1", "4d6s3",
  "4x4d6s1*2", "4x4d6s1*100",
  "4x4d6s2*2+3+4-2*4" ]

badRollStatements = [
  "10", "2", "0", "2x", "2x3", "+1", "*3", "2x*2", "x", "d",
  "4x4d-6s1", "2s4x1d6"
  ]

prop_acceptAllGoodRolls = forAll (elements goodRollStatements) (\s -> (isJust . maybeParseRollStatement) s == True)

prop_acceptNoBadRolls = forAll (elements badRollStatements) (\s -> (isJust . maybeParseRollStatement) s == False)

main = do
  quickCheck prop_acceptAllGoodRolls
  quickCheck prop_acceptNoBadRolls
