module Terminal where

import Game
import System.IO


-- an interface with terminal input/output
data TerminalInterface = TerminalInterface

setupTerminalInterface :: IO TerminalInterface
setupTerminalInterface = do
  hSetBuffering stdin NoBuffering
  return TerminalInterface

displayCharAtCoord :: GameState -> Coord -> Char
displayCharAtCoord state coord
  | playerLocation state == coord =
      if tile == Storage then '+' else '@'
  | any (== coord) (boxLocations state) =
      if tile == Storage then '*' else '$'
  | otherwise = case tile of
      Empty -> ' '
      Wall -> '#'
      Storage -> '.'
  where tile = tileAt (levelMap state) coord

instance GameInterface TerminalInterface where
  render _ state = do
    putStrLn ""
    -- 2D list of (x, y) coordinates for each tile
    let coords = [ [ (x, y)
                   | x <- [0 .. width (levelMap state) - 1] ]
                 | y <- [0 .. height (levelMap state) - 1] ]
    let displayText = map (map (displayCharAtCoord state)) coords
    -- write each line
    foldl (>>) (return ()) (map putStrLn displayText)

  getInput _ = getChar >>= handleInput
    where
      -- getInput where we have already acquired a character
      -- used to leave arrow escape sequences early
      -- if an unexpected character occurs
      handleInput :: Char -> IO Action
      handleInput 'w' = return (Move MUp)
      handleInput 'a' = return (Move MLeft)
      handleInput 's' = return (Move MDown)
      handleInput 'd' = return (Move MRight)
      handleInput 'q' = return Quit
      -- the escape sequences of arrow keys are \ESC[A through \ESC[D
      handleInput '\ESC' = do
        ch1 <- getChar
        if ch1 /= '[' then handleInput ch1
        else do
          ch2 <- getChar
          case ch2 of
            'A' -> return (Move MUp)
            'B' -> return (Move MDown)
            'C' -> return (Move MRight)
            'D' -> return (Move MLeft)
            _   -> handleInput ch2
      -- CTRL+D
      handleInput '\EOT' = return Quit
      -- if its another character then just ignore
      handleInput _ = getInput TerminalInterface
