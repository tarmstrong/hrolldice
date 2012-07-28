{-# LANGUAGE DeriveDataTypeable #-}
{- Main hrolldice executable -}
import System.Console.CmdArgs
import RollDice.Roller
import Data.List

data RollStatements = RollStatements {rolls :: [String]}
  deriving (Show, Data, Typeable)

rollstatements = RollStatements {rolls = def &= args  } &= summary "hRollDice, a dice rolling application modeled after 'rolldice' by Stevie Strickland." 

outputResult :: Either String [Int] -> IO ()
outputResult (Left e) = fail e
outputResult (Right nums) = do
  putStrLn $ intercalate " " $ map show nums
  

main :: IO ()
main = do
  argrs <- cmdArgs rollstatements
  let statements = rolls argrs
  rollResults <- mapM runRoll statements
  mapM_ outputResult rollResults
