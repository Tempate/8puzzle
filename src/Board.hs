module Board where
import Data.List

newtype Board = T (Position, Position -> Tile)

type Position = (Int, Int)
type Tile = Maybe Int

size = 3

positions :: [Position]
positions = [(y, x) | y <- [1..size], x <- [1..size]]

posOfTile :: Board -> Tile -> Position
posOfTile board tile = head (filter hasTile positions)
  where hasTile = \pos -> tileAtPos board pos == tile

tileAtPos :: Board -> Position -> Tile
tileAtPos (T (_, f)) = f

-- Two boards are equal if their tileAtPos method is equal
instance Eq Board
  where b1 == b2 = all (\pos -> tileAtPos b1 pos == tileAtPos b2 pos) positions

-- Pretty-print a board
instance Show Board
  where show board = "\n" ++ unlines (map (intercalate " | ") tilesByRow)
          where positionsByRow = groupBy (\p1 p2 -> fst p1 == fst p2) positions
                tilesByRow = map (map (maybe " " show . tileAtPos board)) positionsByRow

-- By default the goal board are tiles ordered left-to-right and top-to-bottom.
goalBoard :: Board
goalBoard = T ((size, size), tileAtPosGB)

tileAtPosGB :: Position -> Tile
tileAtPosGB (y, x)
    | x == size && y == size = Nothing
    | otherwise = Just ((y - 1) * size + x)

-- Convert between boards and tiles
tiles2Board :: [Tile] -> Board
tiles2Board tiles = T (posOfTile auxBoard Nothing, tileAtPos)
  where auxBoard = T ((0, 0), tileAtPos)
        tileAtPos = \pos -> snd (head (filter ((== pos) . fst) tilesByPos))
        tilesByPos = zip positions tiles

board2Tiles :: Board -> [Tile]
board2Tiles board = map (tileAtPos board) positions
