{-  ShipGuesser

File    : ShipGuesser.hs
Author  : Ian Chen
Purpose : To pinpoint the exact positions of a an arbitrary triplet of
          battleships on a (4x8) grid, making as few guesses as possible.

This aims to solve a battleship-like game that has a 4 row, 8 column grid,
in which the coordinates of 3 ships, each in their own square on the grid,
must be guessed together in 1 group in as few guesses as possible using
some limited feedback about the number of guesses that spotted ships within
0, 1 or 2 squares. To achieve this, firstly a data type representing a
location was devised to represent the 2 dimensions, followed by a gamestate.
To avoid array indexing of a grid, we generate all possible combinations
of correct triplets of locations, and filter them down with the following
strategy: Firstly, an initial guess is provided, which was determined
using some supplementary code on a couple sample targets and just picking
one that had a low turn count (<6) (it seemed to take too long to compute
all). This chose first guess provides a good starting point for eliminating
more 'inconsistent' squares, as described below. We then apply this guess
and get feedback about the number of guesses which are 0, 1, or 2 units'
distance away from the closest of the three ships. To reduce the search
space, for future guesses, we consider only those which, as targets togther
with the old guess, would have produced the same feedback triple i.e. are
'consistent'. Then from these 'future guesses', we select the optimal guess
by computing the expected/avg number of remaining candidates, if we were to
choose them, and selecting the guess with the lowest expected remaining
number. This continues until we find that there are 3 locations in the guess
that match the target triplet i.e. the entire guess matches.

Exports functions and data types to be used externally, in the testing code.
-}
module ShipGuesser
  (
    GameState,
    Location,
    feedback,
    fromLocation,
    initialGuess,
    nextGuess,
    toLocation,
  )
where

import           Data.Char (digitToInt)
import           Data.List (delete, group, sort, tails)

{- | The GameState stores a list of lists (triplets) of locations which are the
remaining valid targets which can be part of the next guess. Initially
begins with (8x4)c3 = 4960 triplets/elements.
-}
type GameState = [[Location]]

{- | A location cell represents a coordinate on the board in col, row format.
Although the row can store an arbitrary integer, for now, conversion from
string to a Location object only tages in single-digit row numbers.
-}
data Location = Cell Char Int
  deriving (Ord, Eq)

instance Show Location where
  show = fromLocation

-- | Converts the coordinate info for a cell into a visually more compact form.
fromLocation :: Location -> String
fromLocation (Cell col row) = col : show row

{- | A smart constructor which takes a 2-character string and splits it into
column and row components, rejecting any values which lie outside the range
of the board. Assumes that row numbers are only single digits, but this can
be modified in the future through the pattern guard.
-}
toLocation :: String -> Maybe Location
toLocation xs
  | [col, row] <- xs, inBounds col row = Just $ Cell col (digitToInt row)
  | otherwise = Nothing
  where
    inBounds col row = and (zipWith elem [col, row] [['A' .. 'H'], ['1' .. '4']])

{- Takes in a target, and a guess and returns a triplet (c0, c1, c2) where
c0 is the number of correct guesses, c1 is the number of guesses exactly 1
space away from a target, c2 is the number of guesses exactly 2 spaces away
from a target. Thus, feedback is _not_ commutative. Each individual guess
may only count for up to 1 target ship, so we take the minimum distance for
a given guess to all the targets, which means 0 <= c0 + c1 + c2 <= 3.
-}
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback _ [] = (0, 0, 0)
feedback targets (guess : guesses) =
  case minimum $ map (distance guess) targets of
    0 -> (c0 + 1, c1, c2)
    1 -> (c0, c1 + 1, c2)
    2 -> (c0, c1, c2 + 1)
    _ -> (c0, c1, c2)
  where
    (c0, c1, c2) = feedback targets guesses

{- | Takes two locations and returns the distance between them. Since we include
diagonals and distance is calculated per dimension, we define overall
'distance' as the maximum of the 1-dimensional deltas for the 2 dimensions.
-}
distance :: Location -> Location -> Int
distance (Cell col row) (Cell col' row') =
  max (dimDistance col col') (dimDistance row row')
  where
    dimDistance t t' = abs (fromEnum t - fromEnum t')

{- | Provides the initial guess of 3 locations for the simulation, and populates
the gamestate with all combinations of 3 locations to serve as the starting
point for all possible next guesses, that will be pared down in nextGuess.
The initial guess is deleted to prevent re-using it. The initial guess
A2, F1, H4 was found by enumerating the first 1000 triplets in initialState
against a sample point C3, D4, F3 and choosing a low guess (with 3 guesses)
Even if an individual guess is not slow, running 4960 together takes a while
hence only the first 1000 were run: this is OK as there is board symmetry.
(count, initialGuess)
(2,[A2,B1,F3])
(3,[A1,D4,G2])
(3,[A2,B1,H2])
(3,[A2,E1,H4])
(3,[A2,F1,H4]) ****
(3,[A2,F3,G3])
(3,[A3,B1,H3])
(4,[A1,A2,G4])
(4,[A1,A3,D4])
(4,[A1,A3,F3])
Please see the attached Test.hs for the exact code used to run this.
-}
initialGuess :: ([Location], GameState)
initialGuess = (initialCell, initialState')
  where
    initialCell = [Cell 'A' 2, Cell 'F' 1, Cell 'H' 4]
    initialState = combs 3 [Cell col row | col <- ['A' .. 'H'], row <- [1 .. 4]]
    initialState' = delete initialCell initialState

{- | Generate unique and ordered combinations of size r from a list ns by
recursively constructing smaller lists, then prepending elements back on.
-}
combs :: (Eq t, Num t) => t -> [a] -> [[a]]
combs 0 _  = [[]]
combs r ns = [x : ys | (x : xs) <- tails ns, ys <- combs (r - 1) xs]

{- | Given a guess triplet of locations, a GameState, as well as the feedback
obtained using said guess, generate an optimal new guess and an updated
GameState. To get the optimal guess, firstly we keep only consistent
guesses - which means that if they were targets of the old guess, they would
produce the same feedback value. These candidate guesses are stored in
preState'. Secondly, we take the candidate guesses from preState', and
attempt to exploit the ability to remove symmetry: the set of guesses that
yield the same feedback will leave you with the same candidate guess set/
space. Hence we store the expected # of remaning candidates for each
candidate guess (should it go ahead), along with the guess itself for easy
retrieval. Then, make the candidate guess with the lowest expected # cands
the new guess, and update the gameState. Note that using `delete` rather
than an extra condition in the list comprehension seems to run a bit faster.
-}
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess (guess, state) result = (guess', state')
  where
    preState' = [target | target <- state, feedback target guess == result]
    expCands =
      [ (mean, target)
        | target <- preState',
          let mean = expCand $ map (feedback target) (delete target preState')
      ]
    guess' = snd $ minimum expCands
    state' = delete guess' preState'

{- | Given a list of feedback tuples, group them into buckets of the same
   feedback, tally them up and compute the expected value of the number of
   remaining candidates, given as sum(count(f)^2)/(total # of feedback) as
 suggested by hint 6.
-}
expCand :: [(Int, Int, Int)] -> Float
expCand feedbacks = sum (map (\f -> fromIntegral (length f ^ 2)) g) / t
  where
    g = group (sort feedbacks)
    t = fromIntegral (length feedbacks)

-- Feel free to view Test.hs for the code for determining initial guess!
