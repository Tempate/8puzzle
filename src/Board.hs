module Board where
import Data.List

newtype Board = T (Position, Position -> Tile)

type Position = (Int, Int)
type Tile = Maybe Int

size = 3

positions :: [Position]
positions = [(m, n) | m <- [1..size], n <- [1..size]]

posOfTile :: Board -> Tile -> Position
posOfTile board tile = head (filter hasTile positions)
  where hasTile = \pos -> tileAtPos board pos == tile

tileAtPos :: Board -> Position -> Tile
tileAtPos (T (_, f)) = f


instance Eq Board
  where b1 == b2 = all (\pos -> tileAtPos b1 pos == tileAtPos b2 pos) positions

instance Show Board
  where show board = "\n" ++ unlines (map (intercalate " | ") tilesByRow)
          where positionsByRow = groupBy (\p1 p2 -> fst p1 == fst p2) positions
                tilesByRow = map (map (maybe " " show . tileAtPos board)) positionsByRow


goalBoard :: Board
goalBoard = T ((size, size), tileAtPosGB)

tileAtPosGB :: Position -> Tile
tileAtPosGB (y, x)
    | x == size && y == size = Nothing
    | otherwise = Just ((y - 1) * size + x)


tiles2Board :: [Tile] -> Board
tiles2Board tiles = T (posOfTile auxBoard Nothing, tileAtPos)
  where auxBoard = T ((0, 0), tileAtPos)
        tileAtPos = tileAtPosInList tiles

tileAtPosInList :: [Tile] -> Position -> Tile
tileAtPosInList tiles position = snd (head (filter ((== position).fst) tilesByPosition))
  where tilesByPosition = zip positions tiles

board2Tiles :: Board -> [Tile]
board2Tiles board = map (tileAtPos board) positions
