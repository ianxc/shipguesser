{-
File    : FindInitialGuess.hs
Purpose : Helper logic to find the best initial guess
          for a particular implemented strategy.
-}

module FindInitialGuess where

import           Data.Char   (intToDigit)
import           Data.List   (sort, tails)
import           Data.Maybe  (fromJust)
import           ShipGuesser (GameState, Location, feedback, fromLocation,
                              initialGuess, nextGuess, toLocation)
import           System.Exit ()

{- | Results with fewest guesses given target [C3, D4, F3]:
(count, initialGuess)
(2,     [A2,B1,F3])
(3,     [A1,D4,G2])
(3,     [A2,B1,H2])
(3,     [A2,E1,H4])
(3,     [A2,F1,H4]) <- chosen
(3,     [A2,F3,G3])
(3,     [A3,B1,H3])
(4,     [A1,A2,G4])
(4,     [A1,A3,D4])
(4,     [A1,A3,F3])
-}
main :: IO ()
main = do
  let x = initialState !! 3500  --  C3, D4, F3
  let y = take 100 initialState -- To save time, even if it skews results
  let target1initialAll = testAllInitial x y
  mapM_ print $ sort target1initialAll

  -- return ()

-- | Fast combinations generator that doesn't take 10+ lines of code.
combs :: (Eq t, Num t) => t -> [a] -> [[a]]
combs 0 _  = [[]]
combs r ns = [x : ys | (x : xs) <- tails ns, ys <- combs (r - 1) xs]

-- | All triplet combinations of locations.
initialState :: [[Location]]
initialState = combs 3 [ fromJust $ toLocation [c, intToDigit r]
                       | c <- ['A' .. 'H'], r <- [1 .. 4]
                       ]

-- | Tests a single target against several initial guesses.
testAllInitial :: [Location] -> [[Location]] -> [(Int, [Location])]
testAllInitial target = map (\i -> (loopStart target i, i))

-- avg :: [Int] -> Float
-- avg xs = fromIntegral sum xs / fromIntegral length xs

-- | The driver for the initial guess.
loopStart :: [Location] -> [Location] -> Int
loopStart target init = do
  loop target init initialState 1

{- | Continue guessing until triplet guessed correctly, short circuit quicker
on reaching count > 5 since we are aiming for low counts. (Although this
won't be consistent between other targets, oh well!)
-}
loop :: [Location] -> [Location] -> GameState -> Int -> Int
loop target guess other count = do
  let answer = feedback target guess

  if answer == (3, 0, 0)
    then count
    else do
      if count > 10
        then 999
        else do
          let (guess', other') = nextGuess (guess, other) answer
          loop target guess' other' (count + 1)
