{- Functions and data types for rolling dice (with random number
   generation) and processing user input. -}
module RollDice.Roller (runRoll, rollRoll, Roll (..), rollRoll', roll1Roll, runRollMaybe ) where

import qualified RollDice.Parser as P -- (parseRollStatement, RollStatement)
import System.Random
import Data.List

data Roll = Roll {
                rollCount :: Int
              , diceCount :: Int
              , sideCount :: Int
              , dropCount :: Int
              , modFuns :: [Int -> Int] }

instance Show Roll where
  show (Roll rc dc sc drc mf) = "Roll (" ++ stats ++ ")"
    where stats = intercalate ", " ["rollCount: " ++ show rc,
                                    "diceCount: " ++ show dc,
                                    "sideCount: " ++ show sc,
                                    "dropCount: " ++ show drc]



-- | Take a roll statement as a String and perform the roll.
runRoll :: String -> IO (Either String [Int])
runRoll str = do
  g <- newStdGen
  let rollResult = rollRoll g str
  case rollResult of
    Left e -> do
      return (Left e)
    Right ints -> do
      return (Right ints)

-- | Like runRoll, but it returns a Maybe instead of an Either.
runRollMaybe :: String -> IO (Maybe [Int])
runRollMaybe str = do
  rollResult <- runRoll str
  return $ case rollResult of
    Left e -> Nothing
    Right results -> Just results

-- | Rolls a single roll with a String as input.
rollRoll :: RandomGen g => g -> String -> Either String [Int]
rollRoll gen str = do
  let parseResult = P.parseRollStatement str
  case parseResult of
    Left e ->
      Left . show $ e
    Right rollstmt ->
      let roll = rollStatement2Roll rollstmt
      in Right $ rollRoll' gen roll

-- | Like rollRoll but accepts a Roll as an argument.
rollRoll' :: RandomGen g => g -> Roll -> [Int]
rollRoll' gen roll = do
  [1..(rollCount roll)]
  return $ roll1Roll gen roll

-- | Roll a single roll. This ignores only the 'rollCount' component
-- | of the Roll.
roll1Roll :: RandomGen g => g -> Roll -> Int
roll1Roll gen roll = sum . (drop ndrops) . sort . (applyMods (modFuns roll)) $ take ndice $ randomWithSides gen nsides
  where ndrops = dropCount roll
        nsides = sideCount roll
        ndice = diceCount roll

-- | Generate large random numbers and then modulate them to the range of the die.
randomWithSides gen nsides = map (\x -> (x `mod` nsides) + 1) (randoms gen)

-- | Apply a list of modifier functions like (*2) and (+4) to
-- | each of a list of integers.
applyMods :: [(Int -> Int)] -> [Int] -> [Int]
applyMods mods nums = [(foldl (\acc f -> f acc) i mods) | i <- nums]

-- | Take a RollStatement as parsed by RollDice.Parser and convert
-- | it to a Roll by filling in missing default values (if any).
rollStatement2Roll :: P.RollStatement -> Roll
rollStatement2Roll stmt = Roll {
                rollCount = maybe 1 id (P.rollCount stmt)
              , diceCount = maybe 1 id (P.diceCount stmt)
              , sideCount = P.sideCount stmt
              , dropCount = maybe 1 id (P.dropCount stmt)
              , modFuns =  P.modFuns stmt}

