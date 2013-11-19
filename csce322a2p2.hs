import Prelude
import System.Environment ( getArgs )
import Data.List
import Data.Maybe

-- The main method that will be used for testing / command line access
main = do
	args <- getArgs
	pacFile <- readFile (head args)
	map <- mapList pacFile
	let
		pac = map
		in yourMain pac

partTwoOutput :: [[Char]] -> IO ()
partTwoOutput pac = do
	print pac

-- Converts a string to a tuple representing the pac-man map
mapList :: String -> IO [[Char]]
mapList = readIO




-- YOUR CODE SHOULD COME AFTER THIS POINT

-- yourMain
yourMain pac =
	partTwoOutput $ replace pac

replace :: [[Char]] -> [[Char]]
replace map =
	let
		pacMap = replacePac map
		ghostMap = replaceGhost pacMap
		ghost6Map = replaceGhost6 ghostMap
		ghostRMap = replaceGhostR ghost6Map
	in
		placeCharacters ghostRMap

		
-- HELPER FUNCTIONS
	
	
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

		
-- FUNCTIONS FOR OTHER OPERATIONS				
-- swap replaces the index in the list with the desired input
swap :: Int -> a -> [a] -> [a]
swap n input (x:xs)
     | (n == 0) = input:xs
     | otherwise = x:swap (n-1) input xs



















