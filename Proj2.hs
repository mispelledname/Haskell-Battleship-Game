{- |
File     : Proj2.hs
Author   : Zi Ng <zyng@student.unimelb.edu.au>
Id       : zyng
Purpose  : Implements two-player game by guessing where 3 ships are located
           on a 4x8 grid and providing feedback on the accuracy of guesses.

Given target of three ship locations on grid, program makes an initial guess of
where the ships are located and enumerates all possible targets. Feedback on 
the accuracy of the guess is returned in the form of a triple:
    ( Number of correct guess locations
    , Number of guess locations exactly one space away from a target location,
    , Number of guess locations exactly two spaces away from a target location
    )
The program uses this feedback to eliminate targets from the list of possible 
targets, and calculates the expected number of remaining possible targets for 
each potential guess. The guess with the smallest expected number of remaining 
possible targets is taken as the next guess. The program repeats until the 
guess matches the target exactly. 

To run this program in Grok, load the files and run the game loop using the
following ghci commands,
> :l Main
> main
-}


module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char(digitToInt)
import Data.List(minimumBy, nub)
import Data.Maybe(fromJust, isJust, fromMaybe)
import Data.Ord(comparing)
import Text.Read(readMaybe)

-- | Smallest and largest row number.
minrow = 1
maxrow = 4
-- | Distance between guess and closest target in score triple.
dist0  = 0
dist1  = 1
dist2  = 2
-- | Initial guess which was pre-determined to have minimum number of expected
-- remaining possible targets at start of game. 
guess  = "A1 A3 H1"

-- | Represents the horizontal position on 4x8 grid.
data Column = A | B | C | D | E | F | G | H
    deriving (Show, Eq, Ord, Enum, Read, Bounded)

-- | Location on 4x8 grid represented by a Column (A-H) and Row (1-4) value.
type Location = (Column, Int)

-- | GameState stores list of possible guesses where each guess consists of a 
-- list of 3 locations.
type GameState = [[Location]]

-- | Counts frequency of element in list
count :: Eq a => a -> [a] -> Int
count x xs = length (filter (x==) xs)

-- | Calculates distance between two Columns, takes two column values as input.
columnDistance :: Column -> Column -> Int
columnDistance col1 col2 = abs ((fromEnum col1) - (fromEnum col2))

-- | Determines Chebyshev distance between two locations on grid, i.e. maximum 
-- distance along row and column axes.
distance :: Location -> Location -> Int
distance (col1, row1) (col2, row2)
    = max colDist rowDist
    where colDist = columnDistance col1 col2
          rowDist = abs (row1 - row2)

-- | Converts string to Maybe Location if string is a valid location, 
-- otherwise returns Nothing.
toLocation :: String -> Maybe Location
toLocation (colStr:rowStr:[])
    | validRow && validCol = Just (col, row)
    | otherwise            = Nothing
    where row      = digitToInt rowStr
          validRow = row >= minrow && row <= maxrow
          maybeCol = readMaybe [colStr] :: Maybe Column
          validCol = isJust maybeCol
          col      = fromJust maybeCol
toLocation _ = Nothing

-- | Converts Location to 2-character string representation of location.
fromLocation :: Location -> String
fromLocation (col, row) = show col ++ show row

-- | Given target locations and guess locations, returns feedback on guess 
-- accuracy in the form of a scores triple. 
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback target guess = 
    ( count dist0 distances
    , count dist1 distances
    , count dist2 distances
    )
    where distances = [closestDistance loc target | loc <- guess]

-- | Given a guess location and target, returns the closest distance between
-- said guess location and any target location. 
closestDistance :: Location -> [Location] -> Int
closestDistance guess target = minimum (map (distance guess) target)

-- | Makes hardcoded initial guess of target locations, returns a tuple of 
-- initial guess and game state which records a list of valid guesses. 
initialGuess :: ([Location],GameState)
initialGuess = (firstGuess, gameState)
    where firstGuess = fromJust (mapM (toLocation) $ words guess)
          squares    = [(col, row) | 
              col <- [(minBound::Column)..(maxBound::Column)], 
              row <- [minrow..maxrow]]
          gameState  = [[loc1,loc2,loc3] | loc1 <- squares, loc2 <- squares, 
              loc3 <- squares, loc1 < loc2, loc2 < loc3]

-- | Given tuple of previous guess and game state which stores possible 
-- targets, as well as feedback for previous guess, returns a tuple of next 
-- guess and new list of possible targets. 
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (guess, gameState) feedback = (nextGuess, targets)
    where targets   = filter (possibleTarget guess feedback) gameState
          nextGuess = minimumBy (comparing (targetsRemaining targets)) targets

-- | Determines if a target is possible, given a guess, feedback received 
-- for said guess and said target. 
possibleTarget :: [Location] -> (Int,Int,Int) -> [Location] -> Bool
possibleTarget guess scores target
    = feedback target guess == scores

-- | Calculates average number of remaining possible targets after a guess, 
-- given list of possible targets and said guess. 
targetsRemaining :: [[Location]] -> [Location] -> Double 
targetsRemaining targets guess
    = sum [(fromIntegral (count score scores))^2 | score <- scoreSet]
    where scores   = [feedback target guess | target <- targets]
          scoreSet = nub scores
