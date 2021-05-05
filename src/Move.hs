module Move where
import Test.QuickCheck
import Board

type Move = (Int, Int)
type Solution = [Move]


make :: Move -> Board -> Board
make move (T (empty, tileAtPos)) = T (move, newTileAtPos)
  where newTileAtPos = tileAtPos . swap move empty

makeMoves :: Board -> Solution -> Board
makeMoves = foldr make

-- Generates a board that can be solved with n moves
genBoard :: Gen Board
genBoard = makeRandomMoves goalBoard 10 []

-- Makes n random moves to a board without repeatitions
makeRandomMoves :: Board -> Int -> [Board] -> Gen Board
makeRandomMoves board 0 _ = return board
makeRandomMoves board n seenBoards = do
    newBoard <- elements (nextBoards board seenBoards)
    makeRandomMoves newBoard (n-1) (board : seenBoards)

-- This function is used to compose changes to the goal's tileAtPos method
swap :: Position -> Position -> Position -> Position
swap pos2swap1 pos2swap2 pos
    | pos == pos2swap1 = pos2swap2
    | pos == pos2swap2 = pos2swap1
    | otherwise = pos

whatMove :: Board -> Board -> Move
whatMove b1 b2 = head (filter resultsInB2 (moves b1))
  where resultsInB2 = \move -> make move b1 == b2

moves :: Board -> [Move]
moves (T ((y, x), _)) = filter (\move -> move /= (y, x)) allMoves
  where allMoves = [(y, minX), (y, maxX), (minY, x), (maxY, x)]
        minX = maximum [x-1, 1]
        maxX = minimum [x+1, size]
        minY = maximum [y-1, 1]
        maxY = minimum [y+1, size]

-- Gives a list of all the boards that are one move away
-- from the given board and that aren't in the seen-boards list
nextBoards :: Board -> [Board] -> [Board]
nextBoards board seenBoards = filter (`notElem` seenBoards) allBoards
  where allBoards = map (`make` board) (moves board)
