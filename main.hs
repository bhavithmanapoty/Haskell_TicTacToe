import Data.Bool (bool)
import Data.Primitive (indexArray)
import Data.List ( intercalate )

-- Open position will have integer, Player Piece (X or O)
data Piece
  = Open Int
  | Player Char
  deriving Eq

-- Define show for pieces
instance Show Piece where
  show (Open n) = show n
  show (Player c) = [c]

-- Removes Nth item (index being N-1) from a list
removeNth :: Int -> [a] -> ([a],[a])
removeNth index lst = (left,right)
  where
    (left,ys) = splitAt (index - 1) lst
    right = drop 1 ys

-- Places piece on the board given board, piece and index to place it in
placePiece :: [a] -> a -> Int -> [a]
placePiece board piece index = xs ++ [piece] ++ ys
  where (xs,ys) = removeNth index board

pieceIsOpen :: Piece -> Bool
pieceIsOpen (Open _) = True
pieceIsOpen _        = False

-- Return true if the index in this board is open
openSpace :: [Piece] -> Int -> Bool
openSpace board index
  | length board < i         = False
  | pieceIsOpen $ board !! i = True
  | otherwise                = False
  where i = index -1

-- Get valid position to play piece, given a board
getPiecePos :: [Piece] -> IO Int
getPiecePos board = do
  input <- getChar
  -- If input is a single digit, return as int, otherwise get input again
  if input `elem` ['1' .. '9'] && openSpace board (read [input])
    then return $ read [input]
    else do
      putStrLn "Enter an Open Position (1-9):"
      getPiecePos board

-- Makes a single line of three iotems in a board list
showBoardLine :: [Piece] -> String
showBoardLine (a:b:c:xs) = show a ++ " | " ++ show b ++ " | " ++ show c
showBoardLine _ = error "List must contain at least three elements"

-- Fnx to draw a line to separate the board lines
boardBorder :: String
boardBorder = "\n---------\n"

-- Prints out the board as a String
showBoard :: [Piece] -> String
showBoard board = intercalate boardBorder [top, middle, bottom]
  where
    top = showBoardLine board
    middle = showBoardLine (drop 3 board)
    bottom = showBoardLine (drop 6 board)

-- Swaps the current player
swapPlayers :: Char -> Char
swapPlayers 'X' = 'O'
swapPlayers 'O' = 'X'
swapPlayers _ = error "swapPlayers only accepts the chars 'O' or 'X'"

-- Check vertical win, given board, player piece and position on board
checkVerticalWin :: [Piece] -> Piece -> Int -> Bool
checkVerticalWin board player index = topPos == player && middlePos == player && bottomPos == player
  where
    topPos = board !! index
    middlePos = board !! (index + 3)
    bottomPos = board !! (index + 6)

-- Return true if player wins vertically
playerWonVertically :: [Piece] -> Piece -> Bool
playerWonVertically board player = any (checkVerticalWin board player) [0, 1, 2]

-- Check horizontal win, given board, player piece and position on board
checkHorizontalWin :: [Piece] -> Piece -> Int -> Bool
checkHorizontalWin board player index = leftPos == player && middlePos == player && rightPos == player
  where
    leftPos = board !! index
    middlePos = board !! (index + 1)
    rightPos = board !! (index + 2)

-- Return true if player wins horizontally
playerWonHorizontally :: [Piece] -> Piece -> Bool
playerWonHorizontally board player = any (checkHorizontalWin board player) [0, 3, 6]

-- Check diagonal win, given board, player piece, position on board and step
checkDiagonalWin :: [Piece] -> Piece -> Int -> Int -> Bool
checkDiagonalWin board player index step = firstPos == player && secondPos == player && thirdPos == player
  where
    firstPos = board !! index
    secondPos = board !! (index + step)
    thirdPos = board !! (index + 2*step)

-- Return true if player wins diagonally
playerWonDiagonally :: [Piece] -> Piece -> Bool
playerWonDiagonally board player = wonFirstDiagonal || wonSecondDiagonal
  where
    wonFirstDiagonal = checkDiagonalWin board player 0 4
    wonSecondDiagonal = checkDiagonalWin board player 2 2

-- Return true if player won
playerWon :: [Piece] -> Piece -> Bool
playerWon board player = playerWonDiagonally board player || playerWonHorizontally board player || playerWonVertically board player

-- If game is tied
tieGame :: [Piece] -> Bool
tieGame = not . any pieceIsOpen

-- Checks if anyone won/tied the game
checkBoardState :: [Piece] -> Char -> IO ()
checkBoardState board playerChr
  | tieGame board                  = putStrLn "It's a tie!"
  | playerWon board (Player 'X')   = putStrLn "X won the game!"
  | playerWon board (Player 'O')   = putStrLn "O won the game!"
  | otherwise                      = runTTT board (swapPlayers playerChr)

runTTT :: [Piece] -> Char -> IO ()
runTTT board playerChr = do
  putStrLn $ showBoard board
  rawChoice <- getPiecePos board
  -- Create new board after placeing Player's piece
  let newBoard = placePiece board (Player playerChr) rawChoice
  -- Check if anyone won and 
  checkBoardState newBoard playerChr

main :: IO ()
main = runTTT board 'X'
  where board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]