module Search where
import Board
import Move

solvable :: Board -> Bool
solvable board = not (null (solutions board))

solutions :: Board -> [Solution]
solutions board = dfs board [] [] 0

isSolution :: Board -> Solution -> Bool
isSolution board solution = makeMoves board solution == goalBoard

dfs :: Board -> [Board] -> Solution -> Int -> [Solution]
dfs board seen solution nMoves
    | board == goalBoard = [solution]
    | nMoves == 15 = []
    | otherwise = concatMap (\b -> dfs b newSeen (whatMove board b : solution) (nMoves + 1)) boards
        where boards = nextBoards board seen
              newSeen = board : seen