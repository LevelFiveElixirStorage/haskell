module Main (main) where


type Board = [[Cell]]
data Cell = Alive | Dead

main :: IO()
main = do s <- getContents
          mapM_ putStrLn(take 10 $  (iterate tick s))

tick :: String -> String
tick input = showBoard $ update $ readBoard input

update :: Board -> Board
update b = mapCells (updateCell b) b

updateCell :: Board -> (Int, Int, Cell) -> Cell
updateCell b (x, y, Alive)
  | c < 2     = Dead
  | c > 3     = Dead
  | otherwise = Alive
  where c = length (livingNeighbors b x y)

updateCell b (x, y, Dead)
  | c == 3    = Alive
  | otherwise = Dead
  where c = length (livingNeighbors b x y)

livingNeighbors :: Board -> Int -> Int -> [Cell]
livingNeighbors b x y = filter isAlive neighbors where
  isAlive Alive = True
  isAlive Dead = False
  neighbors = map (cell b) points
  points = [(x',y') | x' <- [x-1..x+1],
                      y' <- [y-1..y+1],
                      x' /= x || y' /= y,
                      x' >= 0, x' < length b,
                      y' >= 0, y' < length b  ]

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

mapCells :: ((Int, Int, Cell) -> b) -> [[Cell]] -> [[b]]
mapCells f b = map (map f) $ map (\ (x, column) -> map (\ (y, cell) -> (x, y, cell)) (zipIndex column)) (zipIndex b)

zipIndex :: [a] -> [(Int, a)]
zipIndex xs = zip  indexes xs where
  indexes = [x | x <- [0..(length xs)]]
