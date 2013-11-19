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

partFourOutput :: (Int, Int, Int, [[Char]]) -> IO ()
partFourOutput pac = do
	print pac

-- Converts a string to a tuple representing the pac-man map
mapTuple :: String -> IO (Int, Int, Int, [[Char]], [Char])
mapTuple = readIO


-- YOUR CODE SHOULD COME AFTER THIS POINT

-- yourMain
yourMain pac =
	partFourOutput $ makemove pac

-- makemove is from Part 4 and does a series of moves with functionality from Parts 1-3
makemove :: (Int, Int, Int, [[Char]], [Char]) -> (Int, Int, Int, [[Char]])
makemove (le, sc, li, ma, mo)
	| tail moves /= [] = makemove nextMapTuple
	| otherwise = nextMap
	where
		level = le
		score = sc
		lives = li
		map = ma
		moves = mo
		
		nextMap = trymove (level, score, lives, map, (head moves))
		
		nextMapTuple = getNextMapTuple nextMap (tail moves)

-- restart is from Part 3 and restarts the map from the beginning
restart :: [[Char]] -> [[Char]]
restart map = resetPellets (replace map) 0
		
-- replace is from Part 2 and places pacman and the ghosts in default positions
replace :: [[Char]] -> [[Char]]
replace map =
	let
		pacMap = replacePac map
		ghostMap = replaceGhost pacMap
		ghost6Map = replaceGhost6 ghostMap
		ghostRMap = replaceGhostR ghost6Map
	in
		placeCharacters ghostRMap

-- trymove is from Part 1 and moves pacman according to some input
trymove :: (Int, Int, Int, [[Char]], Char) -> (Int, Int, Int, [[Char]])
trymove (le, sc, li, ma, mo)
	| not(pelletsRemaining map 0) = trymove (level+1, score, lives, (restart map), move)
	| nextMove == 'W' = (level, score, lives, pacMap)
	| nextMove == 'B' = (level, score, lives, pacMap)
	| nextMove == '_' = (level, score, lives, pacMap)
	| nextMove == 'F' = (level, score+1, lives, pacMap)
	| nextMove == 'P' = (level, score+10, lives, pacMap)
	| nextMove == 'G' = (level, score, lives-1, (replace map))
	| nextMove == '6' = (level, score, lives-1, (replace map))
	| nextMove == 'R' = (level, score, lives-1, (replace map))
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

		-- The location of PacMan (to be used in ghost logic) after he has moved
		newPacPosition = findPac pacMap

		-- The symbol of the space PacMan will be moving into
		nextMove = (map !! (row+rowChange)) !! (column+columnChange)

			
-- HELPER FUNCTIONS


-- getters for the 5-tuple used in the next map
getNextMapTuple :: (Int, Int, Int, [[Char]]) -> [a] -> (Int, Int, Int, [[Char]], [a])
getNextMapTuple (le, sc, li, ma) mo = (le, sc, li, ma, mo)


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
		'N' -> (0, 0)

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

		
-- FUNCTION TO REPLACE PACMAN (M)
-- replacePac replaces all instances of M with _
replacePac :: [[Char]] -> [[Char]]
replacePac (x:[]) = (x:[])
replacePac (x:xs) =
	case ('M' `elemIndex` x) of
		Just n -> 	let
						pacSwapped = swap n '_' x
					in
						swap 0 pacSwapped (x:xs)
		Nothing -> x:replacePac xs


-- FUNCTIONS TO REPLACE GHOSTS (G,6,R)
-- MULTIPLE GHOSTS NECESSITATE USE OF findIndex OVER elemIndex	
-- replaceGhost replaces all instances of G with _
replaceGhost :: [[Char]] -> [[Char]]
replaceGhost (x:[]) = (x:[])
replaceGhost (x:xs) = 
	case (findIndex (=='G') x) of
		Just n -> 	let 
						ghostSwapped = swap n '_' x
						newGhostRow = swap 0 ghostSwapped (x:xs)
					in
						replaceGhost newGhostRow
		Nothing -> x:replaceGhost xs

-- replaceGhost6 replaces all instances of 6 with F
replaceGhost6 :: [[Char]] -> [[Char]]
replaceGhost6 (x:[]) = (x:[])
replaceGhost6 (x:xs) = 
	case (findIndex (=='6') x) of
		Just n -> 	let 
						ghost6Swapped = swap n 'F' x
						newGhost6Row = swap 0 ghost6Swapped (x:xs)
					in
						replaceGhost6 newGhost6Row
		Nothing -> x:replaceGhost6 xs

-- replaceGhostR replaces all instances of R with P
replaceGhostR :: [[Char]] -> [[Char]]
replaceGhostR (x:[]) = (x:[])
replaceGhostR (x:xs) = 
	case (findIndex (=='R') x) of
		Just n -> 	let 
						ghostRSwapped = swap n 'P' x
						newGhostRRow = swap 0 ghostRSwapped (x:xs)
					in
						replaceGhostR newGhostRRow
		Nothing -> x:replaceGhostR xs

		
-- FUNCTION TO PLACE PACMAN (M) AND GHOSTS (G) IN STARTING POSITIONS
-- places characters in their starting positions, the map is assumed to have:
-- * odd number (>7) of rows and columns
-- * pacman is center column, 2 rows beneath center row
-- * 3 ghosts are 3 center columns, on center row
-- * 1 ghost is center column, 1 row above center row
placeCharacters :: [[Char]] -> [[Char]]
placeCharacters map = 
	let 
		totalRows = length map
		-- Integer division to get the actual middle row
		middleRow = totalRows `div` 2
		-- Square map, so middle column is the same index as middle row
		middleCol = middleRow
		
		-- Row index increases from top to bottom
		pacRow = middleRow + 2		
		-- Rows/columns pertaining to ghost placement
		ghostTopRow = middleRow-1
		ghostLeftCol = middleCol-1
		ghostRightCol = middleCol+1
		
		-- Place PacMan into his location
		pacMan = swap middleCol 'M' (map !! pacRow)
		
		-- Place the ghosts into position
		-- Top ghost is placed independently of other ghosts
		topGhost = swap middleCol 'G' (map !! ghostTopRow)
		
		-- Other three ghosts must be placed in each other's row
		leftGhost = swap ghostLeftCol 'G' (map !! middleRow)
		middleGhost = swap middleCol 'G' leftGhost
		rightGhost = swap ghostRightCol 'G' middleGhost
		
		-- Place the rows in the correct locations within the map
		oneGhostReplaced = swap ghostTopRow topGhost map
		allGhostReplaced = swap middleRow rightGhost oneGhostReplaced
		
	in
		-- Finally add the PacMan row back in and return
		swap pacRow pacMan allGhostReplaced		

--FUNCTIONS PERTAINING TO PELLETS IN THE MAP
-- resetPellets resets _ to P if it's a map corner, F otherwise
resetPellets :: [[Char]] -> Int -> [[Char]]
resetPellets (x:xs) rowCount =
	let 
		-- Return a list of indices for the element '_' in the current row
		spaces = findIndices (`elem` ['_']) ((x:xs) !! rowCount)
	in
		-- If there are no '_' in the current row
		if (spaces == []) then
			-- If the current row has not exceeded the total map rows
			if (rowCount < (length (x:xs)) -1) then
				resetPellets (x:xs) (rowCount+1)
			else
				(x:xs)
		else
			-- If the current cell is a corner, replace '_' with P, otherwise replace with F
			if (checkCorner (x:xs) rowCount (head spaces)) then 
				let
					addPellet = swap (head spaces) 'P' ((x:xs) !! rowCount)
					newPelletAddedMap = swap rowCount addPellet (x:xs)
				in
					resetPellets newPelletAddedMap rowCount
			else
				let
					addPellet = swap (head spaces) 'F' ((x:xs) !! rowCount)
					newPelletAddedMap = swap rowCount addPellet (x:xs)
				in
					resetPellets newPelletAddedMap rowCount 
					
-- pelletsRemaining returns True if there are pellets in the map, false otherwise
pelletsRemaining :: [[Char]] -> Int -> Bool
pelletsRemaining (x:xs) rowCount =
	let 
		-- Return a list of indices for P or F in the current row
		pellets = findIndices (`elem` ['P','F']) ((x:xs) !! rowCount)
	in
		-- If there are no pellets in the current row
		if (pellets == []) then
			-- If the entire map has not been searched
			if (rowCount < (length (x:xs)) -1) then
				pelletsRemaining (x:xs) (rowCount+1)
			else
				False
		else
			True		


-- FUNCTIONS FOR OTHER OPERATIONS				
-- swap replaces the index in the list with the desired input
swap :: Int -> a -> [a] -> [a]
swap n input (x:xs)
     | (n == 0) = input:xs
     | otherwise = x:swap (n-1) input xs
	 
-- checkCorner returns True if the current cell is a corner, False otherwise
-- @param map -> current row -> current column -> Boolean
checkCorner :: [[Char]] -> Int -> Int -> Bool
checkCorner map row col
	-- (Excluding borders) 
	-- | If it's the first row && the first or last column, true corner
	-- | if it's the last row && the first or last column, true corner
	| (row == 1) && (col == 1 || col == (length map) - 2) = True
	| (row == (length map)-2) && (col == 1 || col == (length map)-2) = True
	| otherwise = False