import Prelude
import System.Environment ( getArgs )
import Data.List
import Data.Maybe

-- The main method that will be used for testing / command line access
main = do
	args <- getArgs
	pacFile <- readFile (head args)
	map <- mapTuple pacFile
	let
		pac = map
		in yourMain pac

partOneOutput :: (Int, Int, Int, [[Char]]) -> IO ()
partOneOutput pac = do
	print pac

-- Converts a string to a tuple representing the pac-man map
mapTuple :: String -> IO (Int, Int, Int, [[Char]], Char)
mapTuple = readIO

-- YOUR CODE SHOULD COME AFTER THIS POINT

-- yourMain
yourMain pac =
	partOneOutput $ trymove pac
	
trymove :: (Int, Int, Int, [[Char]], Char) -> (Int, Int, Int, [[Char]])
trymove (le, sc, li, ma, mo)
	| nextMove == 'W' = (level, score, lives, map)
	| nextMove == 'B' = (level, score, lives, map)
	| nextMove == '_' = (level, score, lives, pacMap)
	| nextMove == 'F' = (level, score+1, lives, pacMap)
	| nextMove == 'P' = (level, score+10, lives, pacMap)
	| nextMove == 'G' = (level, score, lives-1, map)
	| nextMove == '6' = (level, score, lives-1, map)
	| nextMove == 'R' = (level, score, lives-1, map)
	where
		-- Get the current stats, map layout, and move
		level = le
		score = sc
		lives = li
		map = ma
		move = mo
		
		-- Find the current position of PacMan
		pacPosition = findPac map
		row = fst pacPosition
		column = snd pacPosition
		
		-- Find out which direction PacMan moves
		direction = moveDirection move
		columnChange = fst direction
		rowChange = snd direction
		
		-- The map after PacMan has made his move
		pacMap = pacMove map pacPosition direction

		-- The symbol of the space PacMan will be moving into
		nextMove = (map !! (row+rowChange)) !! (column+columnChange)


			
-- HELPER FUNCTIONS


-- FUNCTIONS TO FIND PACMAN'S LOCATION ON THE MAP
-- findPac determines where PacMan (M) is on the map and returns coordinates
findPac :: [[Char]] -> (Int, Int)
findPac xs = (findPacRow xs 0, findPacCol xs)

-- findPacCol determines what column Pacman (M) is in
findPacCol :: [[Char]] -> Int
findPacCol (x:xs) =
	case ('M' `elemIndex` x) of
		 Just n -> n
		 Nothing -> findPacCol xs

-- findPacRow determines what row Pacman (M) is in
findPacRow :: [[Char]] -> Int -> Int
findPacRow (x:xs) current
	| ('M' `elem` x) = current
	| not ('M' `elem` x) = findPacRow xs (current+1)


-- FUNCTIONS TO DETERMINE MOVE DIRECTION AND EXECUTE MOVE IF POSSIBLE
-- moveDirection returns an ordered pair based on the move direction
moveDirection :: Char -> (Int, Int)
moveDirection c =
	case (c) of
		'U' -> (0, -1)
		'D' -> (0, 1)
		'L' -> (-1, 0)
		'R' -> (1, 0)

-- pacMove moves pacman to his new position and regenerates the map
-- @param map -> pacPosition -> direction -> newMap
pacMove :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
pacMove map pacPosition direction
	| not (((map !! (row + rowChange)) !! (column + columnChange)) `elem` ['W', 'B']) = swap (row + rowChange) swapPac tempMap
	| otherwise = map
	where
		-- PacMan's current location
		row = fst pacPosition
		column = snd pacPosition

		-- The direction PacMan will be moving		
		columnChange = fst direction
		rowChange = snd direction

		swapSpace = swap column '_' (map !! row)
		tempMap = swap row swapSpace map
		swapPac = swap (column + columnChange) 'M' (tempMap !! (row + rowChange))


-- FUNCTIONS FOR OTHER OPERATIONS				
-- swap replaces the index in the list with the desired input
swap :: Int -> a -> [a] -> [a]
swap n input (x:xs)
     | (n == 0) = input:xs
     | otherwise = x:swap (n-1) input xs




		