module Main (main) where


type Board = [[Cell]]
data Cell = Alive | Dead

main :: IO()
main = do s <- getContents
          mapM_ putStrLn(take 3 $  (iterate tick s))

tick :: String -> String
tick input = showBoard $ update $ readBoard input

update :: Board -> Board
update b = b

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
