module Load where

import Game



type InProgressLevel =
  ( Maybe Coord -- player location, if seen
  , [Coord] -- box locations
  , [[Tile]] -- finished lines
  , [Tile] -- line currently in progress
  )

-- load level from ASCII-art grid
-- [String] in results is list of warnings
loadLevelFromString :: String -> ([String], GameState)
loadLevelFromString str = do -- ([String], a) accumulating warnings
  (maybePlayerLocation, boxLocations, tiles, line) <-
    readStr str (0,0)

  playerLocation <- case maybePlayerLocation of
    Just loc -> ([], loc)
    Nothing -> (["No player location given, defaulting to (0,0)"], (0, 0))

  return GameState
    { playerLocation = playerLocation
    , boxLocations = boxLocations
    , levelMap = mapFromDisplayOrderedTiles (line : tiles)
    }
  where
    -- read new cell with tiles and entities into level being built
    -- [String] in output accumulates warnings
    -- e.g. unrecognized characters, multiple player start locations
    -- this is read from the input string back to front
    -- so we can add new tiles to the beginning of the list
    readCellChar :: Coord -> Char -> InProgressLevel -> ([String], InProgressLevel)
    readCellChar coord ch (player, boxes, tiles, line) = case ch of
      ' ' -> ([], (player, boxes, tiles, Empty   : line))
      '#' -> ([], (player, boxes, tiles, Wall    : line))
      '.' -> ([], (player, boxes, tiles, Storage : line))
      '@' -> (case player of
          Nothing -> []
          Just prevPos -> ["Duplicate player position at " ++ show coord ++
                           ", was previously " ++ show prevPos],
        (Just coord, boxes, tiles, Empty : line))
      '+' -> (case player of
        Nothing -> []
        Just prevPos -> ["Duplicate player position at " ++ show coord ++
                         ", was previously " ++ show prevPos],
        (Just coord, boxes, tiles, Storage : line))
      '$' -> ([], (player, coord : boxes, tiles, Empty : line))
      '*' -> ([], (player, coord : boxes, tiles, Storage : line))
      '\n' -> if null tiles && null line -- ignore trailing newlines
              then ([], (player, boxes, [], []))
              else ([], (player, boxes, line : tiles, []))
      _ -> (["Unrecognized character at " ++ show coord],
                  (player, boxes, tiles, Empty   : line))

    nextCoord :: Coord -> Char -> Coord
    nextCoord (x, y) '\n' = (0, y + 1)
    nextCoord (x, y) _    = (x + 1, y)

    readStr :: String -> Coord -> ([String], InProgressLevel)
    readStr [] _  = ([], (Nothing, [], [], []))
    readStr (c:cs) loc = readStr cs (nextCoord loc c) >>= readCellChar loc c

