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


act :: Action -> GameState -> Maybe GameState
act (Move dir) state = error "not yet implemented!" -- TODO
act Quit _ = Nothing

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
