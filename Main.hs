module Main where

import Control.Monad
import System.Environment

import Game
import Load
import Terminal


main :: IO ()
main = do
  levelPaths <- getArgs
  interface <- setupTerminalInterface
  foldl (>>) (return ()) $ map (loadLevelFromFile >=> gameLoop interface) levelPaths

gameLoop :: GameInterface i => i -> GameState -> IO ()
gameLoop interface state = do
  render interface state
  if won state then putStrLn "level completed!"
  else do
    action <- getInput interface
    let newState = act action state
    case newState of
      -- continue game
      Just state' -> gameLoop interface state'
      -- quit game
      Nothing -> putStrLn ""
