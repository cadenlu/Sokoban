module Solve ( solve, solveRev ) where

import Data.List

import Game


-- the dynamic part of the game state, only concerning player and box locations
-- for caching, box locations should be sorted
type DynamicState = (Coord, [Coord])
-- a state and directions to reach it
type ReachedState = (DynamicState, [MoveDirection])

-- shortest solution to a level, if found
solve :: GameState -> [MoveDirection]
solve = reverse . solveRev

-- shortest solution to a level, if found, with the first element being the last move
-- faster than solve if viable
solveRev :: GameState -> [MoveDirection]
solveRev startingState = solve' [] [(dynStateOf startingState, [])]
  where
    -- the dynamic parts of a given game state: player location and box locations
    dynStateOf :: GameState -> DynamicState
    dynStateOf state = (playerLocation state, boxLocations state)

    -- states reachable from a given state. may contain duplicates
    nextStates :: ReachedState -> [ReachedState]
    nextStates (dynState, dirs) = let
      state = stateOf dynState
      Just upState = act (Move MUp) state
      Just downState = act (Move MDown) state
      Just leftState = act (Move MLeft) state
      Just rightState = act (Move MRight) state
      in [(dynStateOf upState, MUp : dirs),
          (dynStateOf downState, MDown : dirs),
          (dynStateOf leftState, MLeft : dirs),
          (dynStateOf rightState, MRight : dirs)]

    -- remove states which appear elsewhere in the list
    -- this does not preserve order
    deduplicateStates :: [ReachedState] -> [ReachedState]
    deduplicateStates = foldl (\acc state -> if fst state `elem` map fst acc then acc else state : acc) []

    -- given cached states and states needing to be checked,
    -- make new cached states and states needing to be checked
    newCachedStates :: [ReachedState] -> [ReachedState] -> ([ReachedState], [ReachedState])
    newCachedStates cached toCheck = let
      reached = toCheck >>= nextStates
      sortedReached = map (\ ((player, boxes), dirs) -> ((player, sort boxes), dirs)) reached
      dedupReached = deduplicateStates sortedReached
      newCached = toCheck ++ cached
      newlyReached = filter (not . (`elem` (map fst newCached)) . fst) dedupReached
      in (newCached, newlyReached)

    -- a state formed by combining the given dynamic state with the static state of the game
    stateOf :: DynamicState -> GameState
    stateOf (playerLocation, boxLocations) =
      startingState { playerLocation = playerLocation, boxLocations = boxLocations }

    -- takes cached states and states to check, returning moves of the shortest solution
    solve' :: [ReachedState] -> [ReachedState] -> [MoveDirection]
    solve' cached toCheck = case filter (won . stateOf . fst) toCheck of
      [] -> let (newCached, newToCheck) = newCachedStates cached toCheck
            in solve' newCached newToCheck
      ((_, soln) : _) -> soln
