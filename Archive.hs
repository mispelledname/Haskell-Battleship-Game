{- |
File     : Archive.hs
Author   : Zi Ng <zyng@student.unimelb.edu.au>
Id       : zyng
Purpose  : Archive of old implementation code.
 
This file is intended to store discarded and obsolete implementation code for 
Project 2. 
-}


-- | Determines if a location is still a possible target location based
-- on feedback on a guess, specifically number of ships exactly located. 
filterScore0 :: (Int,Int,Int) -> [Location] -> [Location] -> Bool
filterScore0 (score0,score1,score2) guess target
    = length (intersect guess target) == score0

-- | Determines if a location is still a possible target location based
-- on feedback on a guess, specifically number of guesses exactly one space 
-- away from ship.
filterScore1 :: (Int,Int,Int) -> [Location] -> [[Location]] -> [[Location]]
filterScore1 (score0,score1,score2) guess targets
    -- all guesses still valid
    | score0 /= 0 && score1 == 0 = targets
    -- rings are all invalid guesses
    | score0 == 0 && score1 == 0 = [target | target <- targets, 
        length (intersect ring target) == 0]
    -- exactly one guess is next to a target
    | score1 == 1 = [target | target <- targets, 
        length (intersect ring target) == 1]
    -- at least one guess is next to a target
    | otherwise = [target | target <- targets,
        length (intersect ring target) >= 1]
    where ring = [loc | location <- guess, loc <- generateRing dist1 location]

-- | Filters impossible locations from list of possible target locations based
-- on number of guesses two squares away from any target.
filterScore2 :: (Int,Int,Int) -> [Location] -> [[Location]] -> [[Location]]
filterScore2 (score0,score1,score2) guess targets
    -- all guesses still valid
    | (score0 /= 0 || score1 /= 0) && score2 == 0 = targets
    -- 2-rings are all invalid guesses
    | score0 == 0 && score1 == 0 && score2 == 0 = [target | target <- targets, 
        length (intersect ring target) == 0]
    -- at least one guess is two squares away from a target
    | otherwise = [target | target <- targets,
        length (intersect ring target) >= 1]
    where ring = [loc | location <- guess, loc <- generateRing dist2 location]

-- | Generates a list of locations which are N distance away from a given
-- Location, taking as input the distance and centre location. 
generateRing :: Int -> Location -> [Location]
generateRing n location
    = filter (location/=) surroundings
    where Location col row = location
          surroundings = [
              ringLocation | r <- [row-n..row+n], 
              elem r [minrow..maxrow], 
              cNum <- [(fromEnum col)-n..(fromEnum col)+n],
              elem cNum [mincol..maxcol],
              let c = (toEnum cNum :: Column),
              let ringLocation = Location c r,
              distance ringLocation location == n
              ]

-- | Helper function to recursively return feedback given ship locations and
-- guesses in one pass. 
feedback targets guesses = feedbackHelper targets guesses (0,0,0)
feedbackHelper :: [Location] -> [Location] -> (Int,Int,Int) -> (Int,Int,Int)
feedbackHelper _ [] scores = scores
feedbackHelper targets (guess:guesses) (score0,score1,score2)
    = feedbackHelper targets guesses newScores
    where distance = closestDistance guess targets
          newScores 
              | distance == dist0 = (score0+1, score1, score2)
              | distance == dist1 = (score0, score1+1, score2)
              | distance == dist2 = (score0, score1, score2+1)
              | otherwise         = (score0, score1, score2)
