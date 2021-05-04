module Tests where
import Search
import Move
import Board


solvableTiles :: [[Tile]]
solvableTiles = [
  [Just 1,Just 2,Just 3,Just 4,Nothing,Just 8,Just 7,Just 6,Just 5],
  [Just 5,Just 3,Just 6,Just 2,Just 1,Just 8,Nothing,Just 4,Just 7],
  [Just 1,Just 2,Just 3,Just 5,Nothing,Just 7,Just 8,Just 6,Just 4],
  [Nothing,Just 3,Just 5,Just 1,Just 7,Just 2,Just 8,Just 4,Just 6],
  [Just 3,Just 8,Just 5,Just 1,Just 2,Just 6,Just 4,Just 7,Nothing],
  [Just 2,Just 5,Just 3,Just 1,Nothing,Just 6,Just 7,Just 8,Just 4],
  [Just 1,Just 3,Just 8,Just 7,Just 4,Just 5,Nothing,Just 2,Just 6],
  [Just 4,Just 1,Just 2,Just 7,Nothing,Just 6,Just 8,Just 3,Just 5],
  [Just 1,Just 2,Just 6,Just 4,Just 5,Just 8,Nothing,Just 7,Just 3],
  [Just 7,Just 4,Just 1,Just 2,Nothing,Just 3,Just 8,Just 6,Just 5],
  [Just 7,Just 1,Just 2,Just 8,Nothing,Just 3,Just 4,Just 5,Just 6]]

solvableBoards :: [Board]
solvableBoards = map tiles2Board solvableTiles

solvableBoardsTest :: Bool
solvableBoardsTest = all solvable solvableBoards


unsolvableTiles :: [[Tile]]
unsolvableTiles = [
    [Just 1,Just 2,Just 4,Just 3,Nothing,Just 8,Just 7,Just 6,Just 5],
    [Just 2,Just 6,Just 3,Just 1,Nothing,Just 8,Just 7,Just 4,Just 5],
    [Nothing,Just 1,Just 2,Just 7,Just 4,Just 6,Just 5,Just 8,Just 3],
    [Just 3,Just 5,Just 6,Just 2,Just 1,Just 8,Nothing,Just 4,Just 7],
    [Just 8,Just 2,Nothing,Just 1,Just 3,Just 6,Just 4,Just 7,Just 5],
    [Nothing,Just 3,Just 2,Just 4,Just 8,Just 1,Just 5,Just 7,Just 6],
    [Nothing,Just 2,Just 4,Just 5,Just 3,Just 6,Just 1,Just 7,Just 8],
    [Just 4,Just 7,Just 1,Just 5,Just 2,Just 3,Nothing,Just 8,Just 6],
    [Just 2,Just 1,Just 3,Just 5,Nothing,Just 7,Just 8,Just 6,Just 4],
    [Nothing,Just 5,Just 3,Just 1,Just 7,Just 2,Just 8,Just 4,Just 6]]

unsolvableBoards :: [Board]
unsolvableBoards = map tiles2Board unsolvableTiles

unsolvableBoardsTest :: Bool
unsolvableBoardsTest = not (any solvable unsolvableBoards)


tilesWithSolutions :: [([Tile], Solution)]
tilesWithSolutions = [
    ([Just 1,Just 2,Just 3,Just 4,Just 5,Just 6,Just 7,Nothing,Just 8], [(3,3)]),
    ([Just 1,Just 2,Just 3,Just 4,Just 5,Just 6,Nothing,Just 7,Just 8], [(3,3), (3,2)])]

boardsWithSolutions :: [(Board, Solution)]
boardsWithSolutions = map (\(t, s) -> (tiles2Board t, s)) tilesWithSolutions

isSolutionTest :: Bool
isSolutionTest = all (uncurry isSolution) boardsWithSolutions &&
                 all (\board -> all (isSolution board) (solutions board)) solvableBoards

solutionsTest :: Bool
solutionsTest = all (\(b, s) -> s `elem` solutions b) boardsWithSolutions