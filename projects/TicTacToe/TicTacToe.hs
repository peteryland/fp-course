data Owner = X | O deriving (Show, Eq)
o2c o =
  case o of
    Nothing -> '.'
    Just O -> 'O'
    Just X -> 'X'

other O = X
other X = O

data Position = A1 | A2 | A3 | B1 | B2 | B3 | C1 | C2 | C3

class BoardLike b where
  playerAt :: b -> Position -> Maybe Owner
  whoWon :: b -> Maybe Owner
  isFinished :: b -> Bool
  emptyBoard :: b

data Board = Board (Maybe Owner) (Maybe Owner) (Maybe Owner) (Maybe Owner) (Maybe Owner)
                   (Maybe Owner) (Maybe Owner) (Maybe Owner) (Maybe Owner) Owner
instance Show Board where
  show b@(Board a1 a2 a3 b1 b2 b3 c1 c2 c3 _) =
    let
      b' = o2c a1:o2c a2:o2c a3:'\n':o2c b1:o2c b2:o2c b3:'\n':o2c c1:o2c c2:o2c c3:"\n"
      winner = if isFinished b
        then case whoWon b of
          Nothing -> "Nobody wins\n"
          Just o -> "Winner is " ++ show o ++ "\n"
        else ""
    in b' ++ winner

instance BoardLike Board where
  playerAt (Board a1 a2 a3 b1 b2 b3 c1 c2 c3 _) i =
    case i of
      A1 -> a1
      A2 -> a2
      A3 -> a3
      B1 -> b1
      B2 -> b2
      B3 -> b3
      C1 -> c1
      C2 -> c2
      C3 -> c3

  whoWon b =
    case b of
      Board (Just X) (Just X) (Just X) _ _ _ _ _ _ _ -> (Just X)
      Board (Just O) (Just O) (Just O) _ _ _ _ _ _ _ -> (Just O)
      Board _ _ _ (Just X) (Just X) (Just X) _ _ _ _ -> (Just X)
      Board _ _ _ (Just O) (Just O) (Just O) _ _ _ _ -> (Just O)
      Board _ _ _ _ _ _ (Just X) (Just X) (Just X) _ -> (Just X)
      Board _ _ _ _ _ _ (Just O) (Just O) (Just O) _ -> (Just O)
      Board (Just X) _ _ (Just X) _ _ (Just X) _ _ _ -> (Just X)
      Board (Just O) _ _ (Just O) _ _ (Just O) _ _ _ -> (Just O)
      Board _ (Just X) _ _ (Just X) _ _ (Just X) _ _ -> (Just X)
      Board _ (Just O) _ _ (Just O) _ _ (Just O) _ _ -> (Just O)
      Board _ _ (Just X) _ _ (Just X) _ _ (Just X) _ -> (Just X)
      Board _ _ (Just O) _ _ (Just O) _ _ (Just O) _ -> (Just O)
      Board (Just X) _ _ _ (Just X) _ _ _ (Just X) _ -> (Just X)
      Board (Just O) _ _ _ (Just O) _ _ _ (Just O) _ -> (Just O)
      Board _ _ (Just X) _ (Just X) _ (Just X) _ _ _ -> (Just X)
      Board _ _ (Just O) _ (Just O) _ (Just O) _ _ _ -> (Just O)
      _ -> Nothing

  isFinished b =
    case whoWon b of
      Just X -> True
      Just O -> True
      _ -> case b of
        Board Nothing _ _ _ _ _ _ _ _ _ -> False
        Board _ Nothing _ _ _ _ _ _ _ _ -> False
        Board _ _ Nothing _ _ _ _ _ _ _ -> False
        Board _ _ _ Nothing _ _ _ _ _ _ -> False
        Board _ _ _ _ Nothing _ _ _ _ _ -> False
        Board _ _ _ _ _ Nothing _ _ _ _ -> False
        Board _ _ _ _ _ _ Nothing _ _ _ -> False
        Board _ _ _ _ _ _ _ Nothing _ _ -> False
        Board _ _ _ _ _ _ _ _ Nothing _ -> False
        _ -> True

  emptyBoard = Board Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing X

data GameBoard = GB [Board]

instance Show GameBoard where
  show (GB []) = show (emptyBoard :: Board)
  show (GB (b:bs)) = show b

instance BoardLike GameBoard where
  playerAt (GB bs) i = case bs of
    [] -> Nothing
    (b:_) -> playerAt b i

  whoWon (GB bs) = case bs of
    [] -> Nothing
    (b:_) -> whoWon b

  isFinished (GB bs) = case bs of
    [] -> False
    (b:_) -> isFinished b

  emptyBoard = GB []

takeBack (GB b) =
  case b of
    [] -> GB []
    (_:bs) -> GB bs

move :: GameBoard -> Position -> GameBoard
move (GB []) i =
  let b = case i of
            A1 -> Board (Just X) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing O
            A2 -> Board Nothing (Just X) Nothing Nothing Nothing Nothing Nothing Nothing Nothing O
            A3 -> Board Nothing Nothing (Just X) Nothing Nothing Nothing Nothing Nothing Nothing O
            B1 -> Board Nothing Nothing Nothing (Just X) Nothing Nothing Nothing Nothing Nothing O
            B2 -> Board Nothing Nothing Nothing Nothing (Just X) Nothing Nothing Nothing Nothing O
            B3 -> Board Nothing Nothing Nothing Nothing Nothing (Just X) Nothing Nothing Nothing O
            C1 -> Board Nothing Nothing Nothing Nothing Nothing Nothing (Just X) Nothing Nothing O
            C2 -> Board Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just X) Nothing O
            C3 -> Board Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just X) O
  in GB [b]
move gb@(GB (b@(Board a1 a2 a3 b1 b2 b3 c1 c2 c3 o):bs)) i =
  if isFinished b then gb
  else
    case playerAt b i of
      Nothing ->
        let o' = other o
            b' = case i of
                   A1 -> Board (Just o) a2 a3 b1 b2 b3 c1 c2 c3 o'
                   A2 -> Board a1 (Just o) a3 b1 b2 b3 c1 c2 c3 o'
                   A3 -> Board a1 a2 (Just o) b1 b2 b3 c1 c2 c3 o'
                   B1 -> Board a1 a2 a3 (Just o) b2 b3 c1 c2 c3 o'
                   B2 -> Board a1 a2 a3 b1 (Just o) b3 c1 c2 c3 o'
                   B3 -> Board a1 a2 a3 b1 b2 (Just o) c1 c2 c3 o'
                   C1 -> Board a1 a2 a3 b1 b2 b3 (Just o) c2 c3 o'
                   C2 -> Board a1 a2 a3 b1 b2 b3 c1 (Just o) c3 o'
                   C3 -> Board a1 a2 a3 b1 b2 b3 c1 c2 (Just o) o'
        in GB (b':b:bs)
      _ -> gb

runGame moveList = foldl move emptyBoard moveList
