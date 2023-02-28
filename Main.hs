module Main where

import Control.Monad
import System.Environment

import Game
import Load
import Solve
import Terminal


-- a command line argument
data Arg = Option String | Level FilePath
-- determine whether a command line argument is an option or a file
parseArg :: String -> Arg
parseArg ('-' : '-' : option) = Option option
parseArg path = Level path

-- filter array to Just entries of a function into Maybe
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f [] = []
filterMap f (x:xs) = case f x of
  Just y -> y : filterMap f xs
  Nothing -> filterMap f xs

-- takes paths to levels as command line arguments
-- and presents them to the user to play, one after another
main :: IO ()
main = do
  argsStrs <- getArgs
  let args = map parseArg argsStrs
  let levelPaths = filterMap (\a -> case a of Level path -> Just path ; _ -> Nothing) args
  let options = filterMap (\a -> case a of Option opt -> Just opt ; _ -> Nothing) args

  if "solve" `elem` options
  then
    foldl (>>) (return ()) $ map showSolution levelPaths
  else do
    interface <- setupTerminalInterface
    foldl (>>) (return ()) $ map (loadLevelFromFile >=> gameLoop interface) levelPaths

-- find and display a solution for a level
showSolution :: FilePath -> IO ()
showSolution path = do
  state <- loadLevelFromFile path
  let solnRev = solveRev state
  putStrLn $ path ++ ": " ++ foldl (\str dir -> directionArrow dir : ' ' : str) [] solnRev
  where
    -- directional arrow from move direction, for displaying level solutions
    directionArrow :: MoveDirection -> Char
    directionArrow MUp = '↑'
    directionArrow MDown = '↓'
    directionArrow MLeft = '←'
    directionArrow MRight = '→'

-- the main game loop, playing a single level
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
