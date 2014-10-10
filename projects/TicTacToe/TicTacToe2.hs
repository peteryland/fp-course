{-
TicTacToe
=========

Write an API for the tic-tac-toe game. Do not use variables -- they are not permitted. This includes libraries that expose in-line updates. No exceptions (or non-termination) in exposed functions -- all functions return a consistent value for every element of their domain. The follow API methods should exist:

* `move`: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board. This function can only be called on a board that is empty or in-play. Calling `move` on a game board that is finished is a *compile-time type error*.

* `whoWon`: takes a tic-tac-toe board and returns the player that won the game (or a draw if neither). This function can only be called on a board that is finished. Calling `whoWon` on a game board that is empty or in-play is a *compile-time type error*.

* `takeBack`: takes either a finished board or a board in-play that has had at least one move and returns a board in-play. It is a compile-time type error to call this function on an empty board.

* `playerAt`: takes a tic-tac-toe board and position and returns the (possible) player at a given position. This function works on any type of board.

* Other API functions that you may see fit. These can be determined by also writing an interactive console application that uses the API -- other useful functions are likely to arise.

You should write automated tests for your API. For example, the following universally quantified property holds true:

`forall Board b. forall Position p. such that (not (positionIsOccupied p b)). takeBack(move(p, b)) == b`

You should encode this property in an automated specification test. For Scala, use ScalaCheck. For Haskell, QuickCheck. For Java, consider [Functional Java](http://functionaljava.org/). For .NET, use [FsCheck](https://github.com/fsharp/FsCheck). For other languages, you may need to search around.

Haskell-specific
----------------

If you choose to use Haskell, also take advantage of its superior tooling:

* Build with CABAL
* Include a `.ghci` file for convenience when developing
  * http://haskell.org/ghc/docs/6.12.2/html/users_guide/ghci-dot-files.html
* API documented using Haddock
  * [http://www.haskell.org/haddock/doc/html/index.html](http://haskell.org/ghc/docs/6.12.2/html/users_guide/ghci-dot-files.html)
* Code style examined using hlint
  * `cabal install hlint`
  * Produce a report (`--report`)
  * [http://community.haskell.org/~ndm/darcs/hlint/hlint.htm](http://community.haskell.org/~ndm/darcs/hlint/hlint.htm)
* Use hoogle and hayoo to find library functions
  * [http://haskell.org/hoogle/](http://haskell.org/hoogle/)
  * [http://holumbus.fh-wedel.de/hayoo/hayoo.html](http://holumbus.fh-wedel.de/hayoo/hayoo.html)


Extra-curricular
----------------
* Write an opponent that never loses
* Write an opponent with easy, medium, hard difficulty levels
-}

data Owner = X | O deriving (Show, Eq)

o2c o =
  case o of
    Nothing -> '.'
    Just O -> 'O'
    Just X -> 'X'

other O = X
other X = O

data Position = A1 | A2 | B1 | B2

data Board =
  NNNN | -- Empty
  XNNN | NXNN | NNXN | NNNX | XONN | XNON | XNNO | XONX | XNOX | OXNN | -- In Play
  NXON | NXNO | OXXN | NXXO | ONXN | NOXN | NNXO | ONNX | NONX | NNOX |
  XOXN | XXON | XXNO | XNXO | OXNX | NXOX | ONXX | NOXX | XOOX | OXXO -- Finished

instance Show Board where
  show b =
    let
      b' = case b of
        NNNN -> "..\n..\n"

        XNNN -> "X.\n..\n"
        NXNN -> ".X\n..\n"
        NNXN -> "..\nX.\n"
        NNNX -> "..\n.X\n"
        XONN -> "XO\n..\n"
        XNON -> "X.\nO.\n"
        XNNO -> "X.\n.O\n"
        XONX -> "XO\n.X\n"
        XNOX -> "X.\nOX\n"
        OXNN -> "OX\n..\n"
        NXON -> ".X\nO.\n"
        NXNO -> ".X\n.O\n"
        OXXN -> "OX\nX.\n"
        NXXO -> ".X\nXO\n"
        ONXN -> "O.\nX.\n"
        NOXN -> ".O\nX.\n"
        NNXO -> "..\nXO\n"
        ONNX -> "O.\n.X\n"
        NONX -> ".O\n.X\n"
        NNOX -> "..\nOX\n"

        XOXN -> "XO\nX.\n"
        XXON -> "XX\nO.\n"
        XXNO -> "XX\n.O\n"
        XNXO -> "X.\nXO\n"
        OXNX -> "OX\n.X\n"
        NXOX -> ".X\nOX\n"
        ONXX -> "O.\nXX\n"
        NOXX -> ".O\nXX\n"
        XOOX -> "XO\nOX\n"
        OXXO -> "OX\nXO\n"
      winner = if isFinished b
        then case whoWon b of
          Nothing -> "Nobody wins"
          Just o -> "Winner is " ++ show o
        else ""
    in b' ++ winner

isFinished b = case b of
  XOXN -> True
  XXON -> True
  XXNO -> True
  XNXO -> True
  OXNX -> True
  NXOX -> True
  ONXX -> True
  NOXX -> True
  XOOX -> True
  OXXO -> True
  _ -> False

whoWon b = case b of
  XOXN -> Just X
  XXON -> Just X
  XXNO -> Just X
  XNXO -> Just X
  OXNX -> Just X
  NXOX -> Just X
  ONXX -> Just X
  NOXX -> Just X
  _ -> Nothing

data GameBoard = GB [Board]
emptyBoard :: GameBoard
emptyBoard = GB []

instance Show GameBoard where
  show (GB []) = show NNNN
  show (GB (b:_)) = show b

move :: GameBoard -> Position -> GameBoard
move (GB b) i =
  case move' b i of
    Nothing -> GB b
    Just b' -> GB (b':b)

takeBack (GB b) =
  case b of
    [] -> GB []
    (_:bs) -> GB bs

playerAt :: GameBoard -> Position -> Maybe Owner
playerAt (GB []) _ = Nothing
playerAt (GB (b:_)) i =
  let i' = case i of
             A1 -> 0
             A2 -> 1
             B1 -> 3
             B2 -> 4
  in case show b !! i' of
    'X' -> Just X
    'O' -> Just O
    _ -> Nothing

move' [] i =
  case i of
    A1 -> Just XNNN
    A2 -> Just NXNN
    B1 -> Just NNXN
    B2 -> Just NNNX

move' (b:_) i =
    case (b, i) of
      (XNNN, A2) -> Just XONN
      (XNNN, B1) -> Just XNON
      (XNNN, B2) -> Just XNNO
      (NXNN, A1) -> Just OXNN
      (NXNN, B1) -> Just NXON
      (NXNN, B2) -> Just NXNO
      (NNXN, A1) -> Just ONXN
      (NNXN, A2) -> Just NOXN
      (NNXN, B2) -> Just NNXO
      (NNNX, A1) -> Just ONNX
      (NNNX, A2) -> Just NONX
      (NNNX, B1) -> Just NNOX
      (XONN, B1) -> Just XOXN
      (XONN, B2) -> Just XONX
      (XNON, A2) -> Just XXON
      (XNON, B2) -> Just XNOX
      (XNNO, A2) -> Just XXNO
      (XNNO, B1) -> Just XNXO
      (XONX, B1) -> Just XOOX
      (XNOX, A2) -> Just XOOX
      (OXNN, B1) -> Just OXXN
      (OXNN, B2) -> Just OXNX
      (NXON, A1) -> Just XXON
      (NXON, B2) -> Just NXOX
      (NXNO, A1) -> Just XXNO
      (NXNO, B1) -> Just NXXO
      (OXXN, B2) -> Just OXXO
      (NXXO, A1) -> Just OXXO
      (ONXN, A2) -> Just OXXN
      (ONXN, B2) -> Just ONXX
      (NOXN, A1) -> Just XOXN
      (NOXN, B2) -> Just NOXX
      (NNXO, A1) -> Just XNXO
      (NNXO, A2) -> Just NXXO
      (ONNX, A2) -> Just OXNX
      (ONNX, B1) -> Just ONXX
      (NONX, A1) -> Just XONX
      (NONX, B1) -> Just NOXX
      (NNOX, A1) -> Just XNOX
      (NNOX, A2) -> Just NXOX
      _ -> Nothing
