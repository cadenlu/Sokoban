module Main where

import Game
import Terminal


-- TODO read from a file
testLevelTiles :: [[Tile]]
testLevelTiles =
  [ [ Wall, Wall,  Wall,  Wall,    Wall ]
  , [ Wall, Empty, Empty, Empty,   Wall ]
  , [ Wall, Empty, Empty, Storage, Wall ]
  , [ Wall, Empty, Empty, Empty,   Wall ]
  , [ Wall, Wall,  Wall,  Wall,    Wall ]
  ]
testLevel :: GameState
testLevel = GameState
  { playerLocation = (1, 2)
  , boxLocations = [(2, 2)]
  , levelMap = mapFromDisplayOrderedTiles testLevelTiles
  }

main :: IO ()
main = do
  -- any setup?
  gameLoop TerminalInterface testLevel

gameLoop :: GameInterface i => i -> GameState -> IO ()
gameLoop interface state = do
  render interface state
  if won state then return ()
  else do
    action <- getInput interface
    let newState = act action state
    case newState of
      -- continue game
      Just state' -> gameLoop interface state
      -- quit game
      Nothing -> putStrLn ""
