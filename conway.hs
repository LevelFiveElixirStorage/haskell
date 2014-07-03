module Main (main) where

type Board = [[Cell]]
data Cell = Alive | Dead

main :: IO()
main = do s <- getContents
          mapM_ putStrLn(take 10 $  (iterate tick s))

tick :: String -> String
tick input = showBoard $ update $ readBoard input

-- Update cells in the board
update :: Board -> Board
update b = mapBoard (updateCell b) b

-- Update a cell based on the rules of conways game of life
-- 1. Living Cells with less than 2 neighbors die (under population)
-- 2. Living Cells with more than 3 neighbors die (over population)
updateCell :: Board -> (Int, Int, Cell) -> Cell
updateCell b (x, y, Alive)
  | c < 2     = Dead
  | c > 3     = Dead
  | otherwise = Alive
  where c = nearbyPopulation b x y

-- 3. Dead Cells with 3 neighbors become alive (reporoduction)
updateCell b (x, y, Dead)
  | c == 3    = Alive
  | otherwise = Dead
  where c = nearbyPopulation b x y

-- Number of living cells in this moore neighborhood
nearbyPopulation :: Board -> Int -> Int -> Int
nearbyPopulation b x y = length (livingNeighbors) where
    isAlive Alive = True
    isAlive Dead = False
    livingNeighbors = filter isAlive $ neighbors b x y

-- Returns the moore neighborhood at x, y (excludes the center)
neighbors :: Board -> Int -> Int -> [Cell]
neighbors b x y = map (cell b) points where
  points = [(x',y') | x' <- [x-1..x+1],
                      y' <- [y-1..y+1],
                      x' /= x || y' /= y,
                      x' >= 0, x' < length b,
                      y' >= 0, y' < length b  ]

-- Gets the value of the cell x, y from the board.
cell :: Board -> (Int, Int) -> Cell
cell b (x, y) = b !! x !! y

showBoard :: Board -> String
showBoard b = unlines $ map (map showCell) b

showCell :: Cell -> Char
showCell Alive = 'X'
showCell Dead  = ' '

readBoard :: String -> Board
readBoard s = map (map readCell) (lines s)

readCell :: Char -> Cell
readCell 'X' = Alive
readCell  _  = Dead

-- Applies function f to each cell in the board
-- f is passed a tuple of (x, y, cell)
mapBoard :: ((Int, Int, Cell) -> b) -> Board -> [[b]]
mapBoard f b = map (map f) $ boxZipIndex b

-- Funky way to add x, y cordinates to each cell in the board
boxZipIndex :: Board -> [[(Int, Int, Cell)]]
boxZipIndex b =  map xIndexBoard (zipIndex b) where
  -- Adds x indexes to each cell in the board
  xIndexBoard (x, column) = map (yIndexRow x) (zipIndex column)
  -- Adds y indexes to each cell in the row
  yIndexRow x (y, cell) = (x, y, cell)


-- Pairs each element in the list with it's index,
-- returning a list of tuples
zipIndex :: [a] -> [(Int, a)]
zipIndex xs = zip  indexes xs where
  indexes = [x | x <- [0..(length xs)]]
