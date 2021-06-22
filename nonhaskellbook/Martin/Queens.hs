module Queens where

type Board = [Int]
queens :: Int -> [Board]
queens n = loop [[]] 0 n

loop :: [Board] -> Int -> Int -> [Board]
loop boards counter n
  | counter == n = boards
  | otherwise = loop (concatMap (expand n) boards)
    (counter+1) n

expand :: Int -> Board -> [Board]
expand n board = [x : board | x <- [1..n],
  safe x board 1]

safe :: Int -> Board -> Int -> Bool
safe x [] _ = True
safe x (c:y) n' = and [x /= c, x /= c + n',
  x /= c - n', safe x y (n'+1)]

-- -- queens 4 = loop [[]] 0 4 = []
test01 = loop (concatMap (expand 4) [[]]) 1 4
test02 = loop (concat [[x:[]| x <- [1,2,3,4],
  safe x [] 1]]) 1 4
test03a = safe 1 [] 1 == True
test03b = safe 2 [] 1 == True
test03c = safe 3 [] 1 == True
test03d = safe 4 [] 1 == True
test04 = loop ([[4,3,2,1]]) 1 4
test05 = loop (concatMap (expand 4) [[4,3,2,1]]) 2 4

-- -- queens 2 = loop [[]] 0 2 = []
-- test01 = loop (concatMap (expand 2) [[]]) 1 2
-- test02 = loop (concat (map (expand 2) [[]])) 1 2
-- test03 = loop (concat [[x:[]| x <- [1,2], safe x [] 1]]) 1 2
-- test04 = safe 1 [] 1 == True
-- test05 = safe 2 [] 1 == False
-- test06 = loop ([[2,1]]) 1 2
-- test07 = loop (concatMap (expand 2) [[2,1]]) 2 2
-- test08 = loop (concat [[x:[2,1]| x <- [1,2],
--   safe x [2,1] 1]]) 2 2
-- test09 = safe 1 [2,1] 1 == False
-- test10 = safe 2 [2,1] 1 == False
-- test11 = loop ([]) 2 2 == []
-- -- [] because `x:[2,1]` is one whole list
-- -- which is lost if `safe x [2,1] 1` outputs False

-- -- queens 1 = loop [[]] 0 1 = [[1]]
-- test01 = loop (concatMap (expand 1) [[]]) 1 1
-- test02 = loop (concat (map (expand 1) [[]])) 1 1
-- test03 = loop (concat [[[x]| x <- [1], safe x [] 1]]) 1 1
-- test04 = safe 1 [] 1 == True
-- test05 = loop ([[1]]) 1 1 == [[1]]

