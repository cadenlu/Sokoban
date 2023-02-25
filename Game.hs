module Game where


-- a coordinate on the (x, y) game plane
-- positive x is rightward, and positive y is downward
type Coord = (Int, Int)
-- a tile of a game map
data Tile = Empty -- nothing here
          | Wall -- a wall which cannot be walked or pushed through
          | Storage -- a location where a box should go
  deriving (Eq)


-- LEVEL MAP --

-- A level map. cells is guaranteed to be a height × width sized list
data Map = Map
  { tiles :: [[Tile]] -- tiles !! x !! y is the tile in the map at (x, y)
  , width :: Int -- the width of the map, length tiles, stored for convenience
  , height :: Int -- the height of the map, length (head tiles), stored for convenience
  }

-- takes list of tiles indexed first by y then by x
-- and returns a map filled with empty squares to a constant width and height
mapFromDisplayOrderedTiles :: [[Tile]] -> Map
mapFromDisplayOrderedTiles tiles = let
    height = length tiles
    filledTiles = fillTiles tiles
    width = length filledTiles
  in Map
    { tiles = filledTiles
    , width = width
    , height = height
    }
  where
    -- head and rest of given tile list
    -- if the list is empty, give Empty tile as the head
    nextTile [] = (Empty, [])
    nextTile (t:ts) = (t, ts)
    -- transpose and fill out of bounds tiles with empty
    -- e.g.
    --   [ [ Empty, Wall ],
    --     [ Wall,  Wall, Wall, Wall ] ]
    -- becomes
    --   [ [ Empty, Wall ],
    --     [ Wall,  Wall ],
    --     [ Empty, Wall ],
    --     [ Empty, Wall ] ]
    fillTiles tiles
      | all null tiles = []
      | otherwise = let
          nextTiles = map nextTile tiles
          firstTiles = map fst nextTiles
          restTiles = map snd nextTiles
        in firstTiles : fillTiles restTiles

-- whether the given location is in bounds for the map
-- (you don't have to check this for reading a tile:
--  tileAt handles it for you with correct out-of-bounds behavior!)
inBounds :: Map -> Coord -> Bool
inBounds map (x, y) =
  0 <= x && x < width map &&
  0 <= y && y < height map

-- tile at given coordinate in map,
-- with those out of bounds interpretted as walls
tileAt :: Map -> Coord -> Tile
tileAt map coord@(x,y)
  | inBounds map coord = tiles map !! x !! y
  | otherwise = Wall


-- the state of the sokoban game
data GameState = GameState
  { playerLocation :: Coord
  , boxLocations :: [Coord]
  , levelMap :: Map
  }

data MoveDirection = MLeft | MRight | MUp | MDown
data Action = Move MoveDirection | Quit


-- processes actions given by user and updates the GameState
act :: Action -> GameState -> Maybe GameState
act (Move dir) state =
  let player = playerLocation state             -- gets playerLocation
      boxes = boxLocations state                -- gets boxLocations
      newPos = moveInDirection player (Move dir)-- gets new position after moving
      newTile = tileAt (levelMap state) newPos  -- gets tile at the new position
      (boxMoved, newBoxes) = moveBoxIfPossible state boxes newPos (Move dir) -- if boxMoved, and updated boxLocations
  in case newTile of
    Wall -> Just state
    _ -> -- either Empty or Storage
      if boxMoved
        then Just state { playerLocation = newPos, boxLocations = newBoxes } -- update both player and box locations
        else Just state { playerLocation = newPos }                          -- not update boxes, but move player
act Quit _ = Nothing


-- moves box if the tile after the box is not a wall or another box, returns if our boxMoved, and our newBoxes locations
moveBoxIfPossible :: GameState -> [Coord] -> Coord -> Action -> (Bool, [Coord])
moveBoxIfPossible state boxes boxPos (Move dir)
  | not (boxPos `elem` boxes) = (False, boxes)  
  | tileNext == Wall || boxPosNext `elem` boxes = (False, boxes)
  | otherwise = (True, newBoxes)
    where
      tileNext = tileAt (levelMap state) boxPosNext
      boxPosNext = moveInDirection boxPos (Move dir)
      newBoxPos = moveInDirection boxPosNext (Move dir)
      newBoxes = replace boxes boxPos newBoxPos


-- helper for moveBoxIfPossible (replaces boxPos with newBoxPos)
replace :: Eq a => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) old new
  | x == old = new:replace xs old new
  | otherwise = x:replace xs old new


-- returns new coordinate after movement action (helper for act function)
moveInDirection :: Coord -> Action -> Coord
moveInDirection (x,y) (Move MUp)    = (x, y-1)
moveInDirection (x,y) (Move MDown)  = (x, y+1)
moveInDirection (x,y) (Move MLeft)  = (x-1, y)
moveInDirection (x,y) (Move MRight) = (x+1, y)


-- true if all boxLocations are onStorage
won :: GameState -> Bool
won state = all (`onStorage` state) (boxLocations state)

-- check if a given box location is on a Storage tile in the game state (helper for won)
onStorage :: Coord -> GameState -> Bool
onStorage loc state = tileAt (levelMap state) loc == Storage


class GameInterface i where
  -- render the current state of the game
  render :: i -> GameState -> IO ()
  -- await next player input relevent to the game
  getInput :: i -> IO Action
