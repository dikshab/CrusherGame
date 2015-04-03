
-- @author Diksha Bansal

-- All main components (crusher, board evaluator, etc) are labelled within brackets 
-- And three/four new lines are added after each block of code that serves a different purpose


-- Players
white_player='W'
black_player='B'




-- {---------------- Crusher Function ----------------}
-- Board is transformed to a new representation, 
-- wehere each row is a string and the whole board 
-- is consequently a list of strings. 
crusher_n6u8 :: [String] -> Char -> Int -> Int -> [String]
crusher_n6u8 boardList teamSide minimax_depth n=crusher_n6u8' (head newBoardList) newBoardList teamSide minimax_depth
                                     where newBoardList=genNewBoardsList_n6u8 boardList n

crusher_n6u8' :: [String] -> [[String]] -> Char -> Int -> [String]
crusher_n6u8' board pastBoardsList teamSide minimax_depth
  | hasWon_n6u8 board pastBoardsList (otherPlayer_n6u8 teamSide)=map genOldBoard_n6u8 (board:pastBoardsList)
  | otherwise                                          			=map genOldBoard_n6u8 (nextMove:pastBoardsList)
  	where nextMove=createNextMove_n6u8 (genNewMoves_n6u8 board pastBoardsList teamSide) pastBoardsList teamSide minimax_depth




-- {------- Change Board Representation ---------}
-- Get new board representation as a list of strings (each representing a row)
genNewBoardsList_n6u8 :: [String] -> Int -> [[String]]
genNewBoardsList_n6u8 boardList n
  | null boardList=[]
  | otherwise=(genNewBoardList_n6u8 (head boardList) n n):(genNewBoardsList_n6u8 (tail boardList) n)

-- Seperate given board representation to list of strings (where each string represents a row)
genNewBoardList_n6u8 :: String -> Int -> Int -> [String]
genNewBoardList_n6u8 board n n'
  | n' >= (n * 2-1)=(fst split):genNewBoardList_n6u8' (snd split) (n'-1)
  | otherwise        =(fst split):genNewBoardList_n6u8 (snd split) n (n'+1)
  	where split=splitAt n' board

genNewBoardList_n6u8' :: String -> Int -> [String]
genNewBoardList_n6u8' board n
  | null board=[]
  | otherwise =(fst split):genNewBoardList_n6u8' (snd split) (n-1)
  	where split=splitAt n board

-- Concat the lists (rows) to give back original board representation
genOldBoard_n6u8 :: [String] -> String
genOldBoard_n6u8 board=concat board




-- {----------------- Move Generation -----------------}

-- First generate the board states that haven't occurred previously in the game. 
genNewMoves_n6u8 :: [String] -> [[String]] -> Char -> [[String]]
genNewMoves_n6u8 board pastBoardsList teamSide=genNewMoves_n6u8' (genAllMoves_n6u8 board teamSide) pastBoardsList teamSide

genNewMoves_n6u8' :: [[String]] -> [[String]] -> Char -> [[String]]
genNewMoves_n6u8' boardList pastBoardsList teamSide
  | null boardList                                  =[]
  | hasOccurred_n6u8 (head boardList) pastBoardsList=genNewMoves_n6u8' (tail boardList) pastBoardsList teamSide
  | otherwise                                       =(head boardList):genNewMoves_n6u8' (tail boardList) pastBoardsList teamSide

genAllMoves_n6u8 :: [String] -> Char -> [[String]]
genAllMoves_n6u8 board teamSide=filter (/=board) (genAllMoves_n6u8' board (getPlayerCoords_n6u8 board teamSide) teamSide)

genAllMoves_n6u8' :: [String] -> [(Int, Int)] -> Char -> [[String]]
genAllMoves_n6u8' board list_coords teamSide
  | null list_coords=[]
  | otherwise     =concat [(genPawnMovesList_n6u8 board (head list_coords) teamSide), (genAllMoves_n6u8' board (tail list_coords) teamSide)]

-- Concat the list of all possible moves
-- DL, UR mean Down Left, Up Right, etc
-- Only L or R means sliding horizontally across the board
genPawnMovesList_n6u8 :: [String] -> (Int, Int) -> Char -> [[String]]
genPawnMovesList_n6u8 board coordinate teamSide=concat  [[crushR_n6u8 board x y teamSide], [crushL_n6u8 board x y teamSide], 
			                                              [crushUR_n6u8 board x y teamSide], [crushUL_n6u8 board x y teamSide], 
			                                              [crushDR_n6u8 board x y teamSide], [crushDL_n6u8 board x y teamSide],
			                                              [moveR_n6u8 board x y teamSide], [moveL_n6u8 board x y teamSide], 
			                                              [moveUR_n6u8 board x y teamSide], [moveUL_n6u8 board x y teamSide], 
			                                              [moveDR_n6u8 board x y teamSide], [moveDL_n6u8 board x y teamSide]]
						                                              
														                                           where x=(fst coordinate);
														                                                 y=(snd coordinate)



-- All Crushes
crushR_n6u8:: [String] -> Int -> Int -> Char -> [String]
crushR_n6u8 board x y teamSide
  | getPawn_n6u8 board x (y+1) == teamSide && (getPawn_n6u8 board x (y+2) == '-' || getPawn_n6u8 board x (y+2) == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') x (y+2) teamSide
  | otherwise=board

crushL_n6u8 :: [String] -> Int -> Int -> Char -> [String]
crushL_n6u8 board x y teamSide
  | getPawn_n6u8 board x (y-1) == teamSide && (getPawn_n6u8 board x (y-2) == '-' || getPawn_n6u8 board x (y-2) == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') x (y-2) teamSide
  | otherwise=board

crushUR_n6u8 :: [String] -> Int -> Int -> Char -> [String]
crushUR_n6u8 board x y teamSide
  | x <= (n-1) && getPawn_n6u8 board (x-1) y == teamSide && (getPawn_n6u8 board (x-2) y == '-' || getPawn_n6u8 board (x-2) y == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-2) y teamSide
  | x == n && getPawn_n6u8 board (x-1) (y+1) == teamSide && (getPawn_n6u8 board (x-2) (y+1) == '-' || getPawn_n6u8 board (x-2) (y+1) == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-2) (y+1) teamSide
  | x > n && getPawn_n6u8 board (x-1) (y+1) == teamSide && (getPawn_n6u8 board (x-2) (y+2) == '-' || getPawn_n6u8 board (x-2) (y+2) == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-2) (y+2) teamSide
  | otherwise=board
  	where n=length (head board)

crushUL_n6u8 :: [String] -> Int -> Int -> Char -> [String]
crushUL_n6u8 board x y teamSide
  | x <= (n-1) && getPawn_n6u8 board (x-1) (y-1) == teamSide && (getPawn_n6u8 board (x-2) (y-2) == '-' || getPawn_n6u8 board (x-2) (y-2) == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-2) (y-2) teamSide
  | x == n && getPawn_n6u8 board (x-1) y == teamSide && (getPawn_n6u8 board (x-2) (y-1) == '-' || getPawn_n6u8 board (x-2) (y-1) == otherPlayer_n6u8 teamSide)            =genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-2) (y-1) teamSide
  | x > n && getPawn_n6u8 board (x-1) y == teamSide && (getPawn_n6u8 board (x-2) y == '-' || getPawn_n6u8 board (x-2) y == otherPlayer_n6u8 teamSide)                         =genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-2) y teamSide
  | otherwise=board
  	where n=length (head board)

crushDR_n6u8 :: [String] -> Int -> Int -> Char -> [String]
crushDR_n6u8 board x y teamSide
  | x < (n-2) && getPawn_n6u8 board (x+1) (y+1) == teamSide && (getPawn_n6u8 board (x+2) (y+2) == '-' || getPawn_n6u8 board (x+2) (y+2) == otherPlayer_n6u8 teamSide) =genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+2) (y+2) teamSide
  | x == (n-2) && getPawn_n6u8 board (x+1) (y+1) == teamSide && (getPawn_n6u8 board (x+2) (y+1) == '-' || getPawn_n6u8 board (x+2) (y+1) == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+2) (y+1) teamSide
  | x >= (n-1) && getPawn_n6u8 board (x+1) y == teamSide && (getPawn_n6u8 board (x+2) y == '-' || getPawn_n6u8 board (x+2) y == otherPlayer_n6u8 teamSide)                  =genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+2) y teamSide
  | otherwise=board
  	where n=length (head board)

crushDL_n6u8 :: [String] -> Int -> Int -> Char -> [String]
crushDL_n6u8 board x y teamSide
  | x < (n-2) && getPawn_n6u8 board (x+1) y == teamSide && (getPawn_n6u8 board (x+2) y == '-' || getPawn_n6u8 board (x+2) y == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+2) y teamSide
  | x == (n-2) && getPawn_n6u8 board (x+1) y == teamSide && (getPawn_n6u8 board (x+2) (y-1) == '-' || getPawn_n6u8 board (x+2) (y-1) == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+2) (y-1) teamSide
  | x >= (n-1) && getPawn_n6u8 board (x+1) (y-1) == teamSide && (getPawn_n6u8 board (x+2) (y-2) == '-' || getPawn_n6u8 board (x+2) (y-2) == otherPlayer_n6u8 teamSide)=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+2) (y-2) teamSide
  | otherwise=board
  	where n=length (head board)

-- All Moves
moveR_n6u8 :: [String] -> Int -> Int -> Char -> [String]
moveR_n6u8 board x y teamSide
  | getPawn_n6u8 board x (y+1) == '-'=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') x (y+1) teamSide
  | otherwise                          =board

moveL_n6u8 :: [String] -> Int -> Int -> Char -> [String]
moveL_n6u8 board x y teamSide
  | getPawn_n6u8 board x (y-1) == '-'=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') x (y-1) teamSide
  | otherwise                          =board

moveUR_n6u8 :: [String] -> Int -> Int -> Char -> [String]
moveUR_n6u8 board x y teamSide
  | x <= (n-1) && getPawn_n6u8 board (x-1) y == '-'     =genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-1) y teamSide
  | x > (n-1) && getPawn_n6u8 board (x-1) (y+1) == '-'=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-1) (y+1) teamSide
  | otherwise                                               =board
  	where n=length (head board)

moveUL_n6u8 :: [String] -> Int -> Int -> Char -> [String]
moveUL_n6u8 board x y teamSide
  | x <= (n-1) && getPawn_n6u8 board (x-1) (y-1) == '-'=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-1) (y-1) teamSide
  | x > (n-1) && getPawn_n6u8 board (x-1) y == '-'       =genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x-1) y teamSide
  | otherwise                                                =board
  	where n=length (head board)

moveDR_n6u8 :: [String] -> Int -> Int -> Char -> [String]
moveDR_n6u8 board x y teamSide
  | x < (n-1) && getPawn_n6u8 board (x+1) (y+1) == '-'=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+1) (y+1) teamSide
  | x >= (n-1) && getPawn_n6u8 board (x+1) y == '-'     =genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+1) y teamSide
  | otherwise                                               =board
  	where n=length (head board)

moveDL_n6u8 :: [String] -> Int -> Int -> Char -> [String]
moveDL_n6u8 board x y teamSide
  | x < (n-1) && getPawn_n6u8 board (x+1) y == '-'       =genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+1) y teamSide
  | x >= (n-1) && getPawn_n6u8 board (x+1) (y-1) == '-'=genNewBoard_n6u8 (genNewBoard_n6u8 board x y '-') (x+1) (y-1) teamSide
  | otherwise                                                =board
  	where n=length (head board)





-- {---------- New Board Generation -----------}
-- Board is generated row by row
genNewBoard_n6u8 :: [String] -> Int -> Int -> Char -> [String]
genNewBoard_n6u8 board x y elem
  | x == 0	  =(genNewRow_n6u8 (head board) y elem):(tail board)
  | otherwise =(head board):(genNewBoard_n6u8 (tail board) (x-1) y elem)

genNewRow_n6u8 :: String -> Int -> Char -> String
genNewRow_n6u8 x y elem
  | y == 0	 =elem:(tail x)
  | otherwise=(head x):genNewRow_n6u8 (tail x) (y-1) elem





-- {----------- Board Evaluation (for given team) -----------}

-- Heuristic generation -> Max Level
genMaxHeur_n6u8 :: [String] -> [[String]] -> Char -> Int
genMaxHeur_n6u8 board pastBoardsList teamSide
  | hasWon_n6u8 board pastBoardsList (otherPlayer_n6u8 teamSide)= -999
  | otherwise                                          			=genHeur_n6u8 board teamSide

 -- Heuristic generation -> Min Level
genMinHeur_n6u8 :: [String] -> [[String]] -> Char -> Int
genMinHeur_n6u8 board pastBoardsList teamSide
  | hasWon_n6u8 board pastBoardsList teamSide=999
  | otherwise                       		 =genHeur_n6u8 board teamSide

-- Heuristic generation -> Board (winner not decided)
genHeur_n6u8 :: [String] -> Char -> Int
genHeur_n6u8 board teamSide=numPawns teamSide-(numPawns (otherPlayer_n6u8 teamSide))
  where numPawns p=length (getPlayerCoords_n6u8 board p)

-- Team hasWon
hasWon_n6u8 :: [String] -> [[String]] -> Char -> Bool
hasWon_n6u8 board pastBoardsList teamSide
  | (otherPlayerPawns <= (startingPawns_n6u8 n)-n)=True
  | (null oppMoves)                              	=True
  | otherwise                                    	=False
  where n=length (head board);
        otherPlayerPawns=length (getPlayerCoords_n6u8 board (otherPlayer_n6u8 teamSide));
        oppMoves=genNewMoves_n6u8 board pastBoardsList (otherPlayer_n6u8 teamSide)





-- {------------ Minimax Algorithm -------------}

-- Get the next best move (from heuristic results)
createNextMove_n6u8 :: [[String]] -> [[String]] -> Char -> Int -> [String]
createNextMove_n6u8 boardList pastBoardsList teamSide minimax_depth
  | null (tail boardList)=(head boardList)
  | otherwise         	 =getBestBoard_n6u8 (boardScoresList_n6u8 boardList pastBoardsList teamSide minimax_depth [])

-- Return best board (based on Heuristics)
getBestBoard_n6u8 :: [([String],Int)] -> [String]
getBestBoard_n6u8 heuristicPairings
  | null (tail heuristicPairings)                          =fst (head heuristicPairings)
  | snd (head heuristicPairings) < snd (head (tail heuristicPairings))=getBestBoard_n6u8 (tail heuristicPairings)
  | otherwise                                   =getBestBoard_n6u8 ((head heuristicPairings):(tail (tail heuristicPairings)))
	
-- Get list of Boards (B) and their Scores (S) as (B, S) 
boardScoresList_n6u8 :: [[String]] -> [[String]] -> Char -> Int -> [([String], Int)] -> [([String], Int)]
boardScoresList_n6u8 boardList pastBoardsList teamSide minimax_depth heuristicPairings
  | null boardList=heuristicPairings
  | otherwise  =(boardScoresList_n6u8 (tail boardList) pastBoardsList teamSide minimax_depth ((heuristicPairing_n6u8 (head boardList) pastBoardsList teamSide minimax_depth):heuristicPairings))

heuristicPairing_n6u8 :: [String] -> [[String]] -> Char -> Int -> ([String], Int)
heuristicPairing_n6u8 board pastBoardsList teamSide minimax_depth=(board, (minimax_n6u8 board (board:pastBoardsList) teamSide (minimax_depth-1) False))

-- MINIMAX Algorithm
minimax_n6u8 :: [String] -> [[String]] -> Char -> Int -> Bool -> Int
minimax_n6u8 board pastBoardsList teamSide minimax_depth isBest
  | minimax_depth == 0 && isBest                                        =genMaxHeur_n6u8 board pastBoardsList teamSide
  | minimax_depth == 0 && not isBest                                    =genMinHeur_n6u8 board pastBoardsList teamSide
  | isBest && hasWon_n6u8 board pastBoardsList (otherPlayer_n6u8 teamSide)=genMaxHeur_n6u8 board pastBoardsList teamSide
  | not isBest && hasWon_n6u8 board pastBoardsList teamSide               =genMinHeur_n6u8 board pastBoardsList teamSide
  | isBest    =maximum (minimax_n6u8' (genNewMoves_n6u8 board pastBoardsList teamSide) pastBoardsList teamSide (minimax_depth-1) False)
  | otherwise=minimum (minimax_n6u8' (genNewMoves_n6u8 board pastBoardsList (otherPlayer_n6u8 teamSide)) pastBoardsList teamSide (minimax_depth-1) True)

minimax_n6u8' :: [[String]] -> [[String]] -> Char -> Int -> Bool -> [Int]
minimax_n6u8' boardList pastBoardsList teamSide minimax_depth isBest
  | null boardList=[]
  | otherwise  =(minimax_n6u8 (head boardList) ((head boardList):pastBoardsList) teamSide minimax_depth isBest):(minimax_n6u8' (tail boardList) pastBoardsList teamSide minimax_depth isBest)




-- {------- Pawn Position Functions ---------}

-- Return the pawn at (x,y) on the board
-- Asterisk represents value that is not possible
getPawn_n6u8 :: [String] -> Int -> Int -> Char
getPawn_n6u8 board x y
  | x < 0                                            	='*'
  | y < 0                                               ='*'
  | x >= totalRows 										='*'	
  | x <= (n-1) && y >= x+n                              ='*'
  | x > (n-1) && y > abs(x-midRow-(totalRows-n-1))		='*'
  | otherwise                                           =((board !! (x)) !! y)
  where totalRows=length board;
        n=length (head board);
        midRow=2*n-1

-- Return pawn coordinates on the board (x,y) for a given teamSide
getPlayerCoords_n6u8 :: [String] -> Char -> [(Int, Int)]
getPlayerCoords_n6u8 board teamSide=getPlayerCoords_n6u8' board teamSide 0

getPlayerCoords_n6u8' :: [String] -> Char -> Int -> [(Int, Int)]
getPlayerCoords_n6u8' board teamSide rowNum
  | null board=[]
  | otherwise =positionInRow_n6u8 (head board) teamSide rowNum ++ getPlayerCoords_n6u8' (tail board) teamSide (rowNum+1)

positionInRow_n6u8 :: String -> Char -> Int -> [(Int, Int)]
positionInRow_n6u8 row teamSide rowNum=positionInRow_n6u8' row teamSide rowNum 0

positionInRow_n6u8' :: String -> Char -> Int -> Int -> [(Int, Int)]
positionInRow_n6u8' row teamSide rowNum colNum
  | null row            =[]
  | (head row) == teamSide=(rowNum, colNum):positionInRow_n6u8' (tail row) teamSide rowNum (colNum+1)
  | otherwise           =positionInRow_n6u8' (tail row) teamSide rowNum (colNum+1)



-- {------ Miscellenous Functions -------}

-- Find number of starting pawns
startingPawns_n6u8 :: Int -> Int
startingPawns_n6u8 n=(n*2)-1

-- Return other player's (opponent's) team color
otherPlayer_n6u8 :: Char -> Char
otherPlayer_n6u8 teamSide
	| teamSide == 'W'='B'
	| teamSide == 'B'='W'

-- See if Board has occurred in the history of the game
hasOccurred_n6u8 :: [String] -> [[String]] -> Bool
hasOccurred_n6u8 board pastBoardsList
  | null pastBoardsList         =False
  | board == head pastBoardsList=True
  | otherwise            =hasOccurred_n6u8 board (tail pastBoardsList)