import Test.QuickCheck
import RollDice.Parser (RollStatement, parseRollStatement)

goodRollStatements = [
  "d1", "d2", "d6", "d8", "d20", "d100",
  "1d6", "1d1", "2d6", "5d8", "82d3", "20d20",
  "1x4d6", "2x4d6", "6x4d6", "20x20d543", "2xd6",
  "3x4d6s1", "4d6s3",
  "4x4d6s1*2", "4x4d6s1*100",
  "4x4d6s2*2+3+4-2*4"
]

successfulParse :: parseResult -> Bool
successfulParse (Right rollstatement) = True
successfulParse _                     = False

prop_acceptAllGoodRolls = forAll (elements goodRollStatements) (\s -> (successfulParse . parseRollStatement) s == True)
