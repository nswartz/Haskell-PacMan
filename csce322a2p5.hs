import Prelude
import System.Environment ( getArgs )
import Data.List
import Data.Maybe
import Data.Function

-- The main method that will be used for testing / command line access
main = do
	args <- getArgs
	pacFile <- readFile (head args)
	map <- mapTuple pacFile
	let
		pac = map
		in yourMain pac

partFiveOutput :: (Int, Int, Int, [[Char]]) -> IO ()
partFiveOutput pac = do
	print pac

-- Converts a string to a tuple representing the pac-man map
mapTuple :: String -> IO (Int, Int, Int, [[Char]], [Char])
mapTuple = readIO




-- YOUR CODE SHOULD COME AFTER THIS POINT

-- yourMain
yourMain pac =
	partFiveOutput $ moveall pac

	
-- moveall simulates an entire game of PacMan with modified logic from Parts 1-4
-- and additional helpers to govern ghost logic
moveall :: (Int, Int, Int, [[Char]], [Char]) -> (Int, Int, Int, [[Char]])
moveall input = makemove input

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
	| nextMove == 'W' = (level, score, lives, ghostMap)
	| nextMove == 'B' = (level, score, lives, ghostMap)
	| nextMove == '_' = (level, score, lives, ghostMap)
	| nextMove == 'F' = (level, score+1, lives, ghostMap)
	| nextMove == 'P' = (level, score+10, lives, ghostMap)
	| nextMove == 'G' = (level, score, lives-1, (replace ghostMap))
	| nextMove == '6' = (level, score, lives-1, (replace ghostMap))
	| nextMove == 'R' = (level, score, lives-1, (replace ghostMap))
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
		
		-- Sort the ghosts right-to-left, top-to-bottom
		ghostPositions = findNextGhost map
		sortedGhostPositions = sortBy sortGhosts ghostPositions
						
		-- The map after PacMan has made his move
		pacMap = pacMove map pacPosition direction

		-- The location of PacMan (to be used in ghost logic) after he has moved
		newPacPosition = findPac pacMap

		-- The symbol of the space PacMan will be moving into
		nextMove = (map !! (row+rowChange)) !! (column+columnChange)

		-- The map after the ghosts have made their moves
		ghostMap = moveGhost pacMap sortedGhostPositions newPacPosition

			
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
moveDirection d =
	case (d) of
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


-- FUNCTIONS TO GOVERN GHOST MOVEMENT
-- findNextGhost finds the next ghost that will move right-to-left, top-to-bottom
findNextGhost :: [[Char]] -> [(Int, Int)]
findNextGhost [] = [(-1, -1)]
findNextGhost (he:ta) = findHelper (he:ta) (0, 0) []

findHelper :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findHelper [] _ list = list
findHelper (he:ta) (r, c) list
		| he == [] = findHelper ta (r+1, 0) list
		| ((head he) `elem` ['G', '6', 'R']) = findHelper ((tail he):ta) (r, c+1) ((r, c):list)
		| otherwise = findHelper ((tail he) : ta) (r, c+1) list

		
-- sortGhosts is a helper for sorting a pair of coordinates by row, then column
sortGhosts (a1, b1) (a2, b2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | a1 == a2 = compare b1 b2	
		
-- moveGhost moves all the ghosts and returns the updated map
moveGhost :: [[Char]] -> [(Int, Int)] -> (Int, Int) -> [[Char]]
moveGhost a [] _ = a
moveGhost map (coord:coordinates) pacPosition 
	| (coord:coordinates) == [] = map
	| otherwise = moveGhost moveChanged coordinates pacPosition
	where				
		-- Find the ghost's distance from PacMan				
		ghostDistance = findDistance pacPosition coord map
			
		-- Chooses the shortest, valid move for the ghost
		bestMove = chooseMove ghostDistance coord map
			
		-- Determines the direction the ghost moves
		ghostDirection = moveDirection bestMove

		-- Places the proper symbol in the space the ghost is moving out of
		previousChanged = ghostPrevChecker map coord
			
		-- Places the proper symbol in the space the ghost is moving into
		moveChanged = ghostMoveChecker previousChanged coord ghostDirection

-- ghostPrevChecker returns a modified map with the ghost's previous location overwritten
ghostPrevChecker :: [[Char]] -> (Int, Int) -> [[Char]]
ghostPrevChecker map coordinates
	| (map !! row) !! column == 'R' = swap row swapP map
	| (map !! row) !! column == '6' = swap row swapF map
	| otherwise = swap row swapSpace map
	where 
		-- Current coordinates of the ghost
		row = fst coordinates
		column = snd coordinates
			
		-- Swaps the corresponding symbol into the ghost's previous row location
		swapP = swap column 'P' (map !! row)
		swapF = swap column 'F' (map !! row)
		swapSpace = swap column '_' (map !! row) 
						
-- ghostMoveChecker returns a modified map with the ghost's new location
ghostMoveChecker :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
ghostMoveChecker map ghostPosition ghostDirection
	| ((map !! (row + rowChange)) !! (column + columnChange) == 'P') = swap (row + rowChange) swapR map
	| ((map !! (row + rowChange)) !! (column + columnChange) == 'F') = swap (row + rowChange) swap6 map
	| ((map !! (row + rowChange)) !! (column + columnChange) == '_') = swap (row + rowChange) swapG map
	| otherwise = replace map
	where
		-- Current coordinates of the ghost
		row = fst ghostPosition
		column = snd ghostPosition
		
		-- The unit of movement the ghost will take
		columnChange = fst ghostDirection
		rowChange = snd ghostDirection
		
		-- Swaps the corresponding symbol into the ghost's new row location
		swapR = swap (column + columnChange) 'R' (map !! (row + rowChange))
		swap6 = swap (column + columnChange) '6' (map !! (row + rowChange))
		swapG = swap (column + columnChange) 'G' (map !! (row + rowChange))
						
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
	
-- findDistance determines the row and column distance between a ghost and PacMan
findDistance :: (Int, Int) -> (Int, Int) -> [[Char]] -> (Int, Int)
findDistance pacPosition ghostPosition map =
	let
		pacRow = fst pacPosition
		pacCol = snd pacPosition
		
		ghostRow = fst ghostPosition
		ghostCol = snd ghostPosition
		
		rowDist = (pacRow - ghostRow)
		colDist = (pacCol - ghostCol)
	in
		(rowDist, colDist)
		
-- chooseMove chooses the actual move to execute based on distance
chooseMove :: (Int, Int) -> (Int, Int) -> [[Char]] -> Char
chooseMove dist pos map =
	let		
		rowDist = fst dist
		colDist = snd dist
		
		ghostRow = fst pos
		ghostCol = snd pos
		
		-- Ghost movement is only considered for a valid move if
		-- the space being moved into is F or P or '_' or M
		-- Walls, borders, and other ghosts are exluded as valid moves in this way
		
		-- Up is current row -1
		up = 	((((map !! (ghostRow-1)) !! ghostCol) == 'F')
				|| (((map !! (ghostRow-1)) !! ghostCol) == 'P') 
				|| (((map !! (ghostRow-1)) !! ghostCol) == '_')
				|| (((map !! (ghostRow-1)) !! ghostCol) == 'M'))
		
		-- Right is current column + 1
		right = ((((map !! ghostRow) !! (ghostCol+1)) == 'F') 
				|| (((map !! ghostRow) !! (ghostCol+1)) == 'P') 
				|| (((map !! ghostRow) !! (ghostCol+1)) == '_')
				|| (((map !! ghostRow) !! (ghostCol+1)) == 'M'))
		
		-- Down is current row + 1
		down = 	((((map !! (ghostRow+1)) !! ghostCol) == 'F') 
				|| (((map !! (ghostRow+1)) !! ghostCol) == 'P') 
				|| (((map !! (ghostRow+1)) !! ghostCol) == '_')
				|| (((map !! (ghostRow+1)) !! ghostCol) == 'M'))
		
		-- Left is current column - 1
		left = 	((((map !! ghostRow) !! (ghostCol-1)) == 'F') 
				|| (((map !! ghostRow) !! (ghostCol-1)) == 'P') 
				|| (((map !! ghostRow) !! (ghostCol-1)) == '_')
				|| (((map !! ghostRow) !! (ghostCol-1)) == 'M'))
	in
		if (rowDist < 0) then
			-- Valid moves are U and L
			if (colDist < 0) then
				if (abs (rowDist) < abs (colDist)) then
					if (left) then
						'L'
					else
						if (up) then
							'U'
						else
							'N'
				else 
					if (abs (rowDist) == abs (colDist)) then
						if (up) then
							'U'
						else 
							if (left) then
								'L'
							else
								'N'
					else
						if (up) then
							'U'
						else 
							if (left) then
								'L'
							else
								'N'
			-- Valid move is U
			else 
				if (colDist == 0) then
					if (up) then
						'U'
					else
						'N'
			-- Valid moves are U and R
				else
					if (abs (rowDist) < abs (colDist)) then
						if (right) then
							'R'
						else
							if (up) then
								'U'
							else
								'N'
					else 
						if (abs (rowDist) == abs (colDist)) then
							if (up) then
								'U'
							else 
								if (right) then
									'R'
								else
									'N'
						else
							if (up) then
								'U'
							else 
								if (right) then
									'R'
								else
									'N'
		else 
			if (rowDist == 0) then
				-- Valid move is L
				if (colDist < 0) then
					if (left) then
						'L'
					else
						'N'
				-- Valid move is N
				else 
					if (colDist == 0) then
						'N'
				-- Valid move is R
					else 
						if (colDist > 0) then
							if (right) then
								'R'
							else
								'N'
						else
							'N'
			else
				-- Valid moves are D and L
				if (colDist < 0) then
					if (abs (rowDist) < abs (colDist)) then
						if (left) then
							'L' 
						else
							if (down) then
								'D'
							else
								'N'
					else 
						if (abs (rowDist) == abs (colDist)) then
							if (down) then
								'D'
							else 
								if (left) then
									'L'
								else
									'N'
						else
							if (down) then
								'D'
							else 
								if (left) then
									'L'
								else
									'N'
				-- Valid move is D
				else 
					if (colDist == 0) then
						if (down) then
							'D'
						else
							'N'
					-- Valid moves are D and R
					else
						if (abs (rowDist) < abs (colDist)) then
							if (right) then
								'R'
							else
								if (down) then
									'D'
								else
									'N'
						else 
							if (abs (rowDist) == abs (colDist)) then
								if (right) then
									'R'
								else 
									if (down) then
										'D'
									else
										'N'
							else
								if (down) then
									'D'
								else
									if (right) then
										'R'
									else
										'N'