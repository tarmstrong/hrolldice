{- QuickCheck tests for hrolldice. -}
import Test.QuickCheck
import System.Random (mkStdGen)
import RollDice.Parser (RollStatement, maybeParseRollStatement)
import RollDice.Roller

import Data.Maybe (isJust)

main = do
  quickCheck prop_acceptAllGoodRolls
  quickCheck prop_acceptNoBadRolls
  quickCheck prop_rollsFollowRules
  quickCheck prop_goodRollsFollowRules
  quickCheck prop_applyModsWork

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

prop_applyModsWork i =
    forAll (elements modExamples)
      (\(str, result) ->
        case rollRoll (mkStdGen i) str of
          Left e -> False
          Right result' -> [result] == result')
  where modExamples = [
          ("1d1*2", 2),
          ("1d1*10", 10),
          ("1d1*2+3*2", 10),
          ("1d1+2+3+1*3", 21),
          ("1d1-10", -9) ]


-- | For a given roll with ndrops > ndice, it should
-- | be within certain bounds. Verify this.
-- | Remove modFuns for easier testing.
prop_rollsFollowRules r i = ndice > ndrops ==> verifyResult r i n
  where n = roll1Roll (mkStdGen i) (removeMF r)
        ndice = diceCount r
        ndrops = dropCount r

-- | Make sure all the good roll strings end up with valid results.
prop_goodRollsFollowRules i = forAll (elements goodRollStatements) goodvalues
  where goodvalues str = do
          case maybeParseRollStatement str of
            Nothing -> False
            Just rs -> do
              let cleanRoll = (removeMF . rollStatement2Roll) rs
                  result = rollRoll' (mkStdGen i) cleanRoll
              all id $ map (verifyResult cleanRoll i) result

verifyResult r i n = all id rules
  where nsides = sideCount r
        ndice = diceCount r
        ndrops = dropCount r
        modfuns = modFuns r
        rules :: [Bool]
        rules = [
          (ndice-ndrops) <= n && n <= (ndice-ndrops)*nsides,
          n /= 0]

-- | Remove modFuns from a Roll.
removeMF :: Roll -> Roll
removeMF roll = Roll {
                rollCount = rollCount roll
              , diceCount = diceCount roll
              , sideCount = sideCount roll
              , dropCount = dropCount roll
              , modFuns = [] }

instance Arbitrary Roll where
  arbitrary = do
    rc <- choose (1, 20)
    dc <- choose (1, 20)
    sc <- choose (1, 20)
    drc <- choose (1, 20)
    mf <- do
      operand <- choose (1, 5)
      operation <- elements [(*), (+), (-)]
      return (operation operand)
    return Roll {
                rollCount = rc
              , diceCount = dc
              , sideCount = sc
              , dropCount = drc
              , modFuns = [mf] }
