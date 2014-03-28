import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Data.List -- for transpose and reverse
import Data.List.Split -- for chunksOf (Could also use prelude and roll my own)
import System.Random -- for getStdGen, using StdGen objects and random
import Control.Lens -- for set, element

main = 
  do
    g <- getStdGen
    play
      (InWindow "2048 in Haskell by Maia"
         (400,400)
         (20,20)
      )
      black -- background color
      fps -- simulation steps per second
      (initPositions g) -- initial world
      drawWorld -- function to convert world to picture
      handleInputEvents -- TODO add randomness
      stepWorld

fps = 20

-- World is a 4x4 double array of ints
type Row = [Int]
type World = ([[Int]],StdGen) -- also stores random number generator to help with some stuff

--------------------------------------------
-- Utils for world generation and updating
--------------------------------------------

makeNthZeroTwo :: Int -> [Int] -> [Int]
makeNthZeroTwo _ [] = []
makeNthZeroTwo 0 (x:xs) = if x == 0 then 2:xs else x:(makeNthZeroTwo 0 xs)
makeNthZeroTwo n (x:xs) = if x == 0 then x:(makeNthZeroTwo (n-1) xs) 
                                  else x:(makeNthZeroTwo n xs)

addNthZero :: Int -> World -> World
addNthZero n (board,gen) = (chunksOf 4 $ makeNthZeroTwo n $ concat board, gen)

---------------------------------------------
-- Initial world generation
---------------------------------------------

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

-- generates an initial world state with, on average, two 2s to start
initPositions :: StdGen -> World
-- This is a really cool line of code but sadly it is no longer useful.
-- initPositions gen = let board = chunksOf 4 $ map ((*2) . toInt . (== 0) . (`mod` 4)) $ take 16 $ (randoms gen :: [Int]) in (board,gen)
initPositions gen = let numZeros = 16
                        (rand1, gen1) = random gen :: (Int, StdGen)
                        (rand2, gen2) = random gen :: (Int, StdGen)
                        n1 = rand1 `mod` numZeros
                        n2 = rand2 `mod` (numZeros-1)
                        newBoard = chunksOf 4 $ makeNthZeroTwo n2 $ makeNthZeroTwo n1 $ replicate 16  0
                    in (newBoard, gen2)

--------------------------------------------
-- Regeneration of 2s
--------------------------------------------

addTwos :: World -> World
addTwos (board,gen) = let numZeros = (sum . (map toInt) . (map (==0)) . concat) board
                          (rand, newGen) = random gen :: (Int, StdGen)
                          n = if numZeros == 0 then 0 else rand `mod` numZeros
                          newBoard = chunksOf 4 $ makeNthZeroTwo n $ concat board
                      in (newBoard, newGen)

---------------------------------------------
-- Input handling
-- -----------------------------------------

handleInputEvents :: Event -> World -> World
handleInputEvents (EventKey (SpecialKey KeyUp) _ _ _) (board,gen) = addTwos (goUp board, gen)
handleInputEvents (EventKey (SpecialKey KeyDown) _ _ _) (board,gen) = addTwos (goDown board, gen)
handleInputEvents (EventKey (SpecialKey KeyLeft) _ _ _) (board,gen) = addTwos (goLeft board, gen) 
handleInputEvents (EventKey (SpecialKey KeyRight) _ _ _) (board,gen) = addTwos (goRight board, gen)
handleInputEvents  _ x = x

stepWorld :: Float -> World -> World
stepWorld _ x = x

rowHgt = 100

drawWorld :: World -> Picture
drawWorld ([r1, r2, r3, r4],_) = translate (150) (150) (pictures [ drawRow r1, 
                                        translate 0 (-rowHgt) (drawRow r2),
                                        translate 0 (-rowHgt*2) (drawRow r3),
                                        translate 0 (-rowHgt*3) (drawRow r4)
                                      ])

tileS = 90
textScale = 0.2

colorZipper :: [(Int, Color)]
colorZipper = [(2, makeColor8 255 235 235 255),
               (4, makeColor8 255 200 200 255),
               (8, makeColor8 255 170 170 255),
               (16, makeColor8 255 140 140 255),
               (32, makeColor8 255 110 110 255),
               (64, makeColor8 255 80 80 255),
               (128, makeColor8 255 80 255 255)]

getColorUnsafe :: Int -> Maybe Color
getColorUnsafe x = lookup x colorZipper 

convertColor :: Maybe Color -> Color
convertColor (Just c) = c
convertColor Nothing = makeColor8 255 255 255 255

getColor :: Int -> Color
getColor x = convertColor (getColorUnsafe x)

-- Takes x-offset and tile value and draws the tile
drawTile :: Float -> Int -> Picture
drawTile x val = let background = [color (getColor val) (translate x 0 (rectangleSolid tileS tileS))]
                     number = if val > 0 then  -- only display the number if it isn't 0
                           [translate x 0 (scale textScale textScale (text (show val)))]
                           else [] :: [Picture]
                 in pictures (background ++ number)

drawRow :: Row -> Picture
drawRow [i,j,k,l] = translate (-300) 0 (pictures [ drawTile 0 i, 
                                                   drawTile rowHgt j,
                                                   drawTile (rowHgt*2) k,
                                                   drawTile (rowHgt*3) l ])

-- TODO fix drawRow to actually draw integers and corresponding colors
-- then write combination functions
-- then write input handlers
-- then write score tracker

------------------------------
-- Board handling ------------
------------------------------


scootLambda :: Int -> [Int] -> [Int]
scootLambda y [] = [y]
scootLambda y [x] = if x == 0 then [x,y] else [y,x]
scootLambda y (x:xs) = if x == 0 then x:y:xs else y:x:xs

-- Takes a row and scoots all numbers through zeroes *once*
-- Example: [2,0,0,2] -> [0,2,0,2] and [0,2,0,2] -> [0,0,2,2]
scootRowRightOnce :: [Int] -> [Int]
scootRowRightOnce = foldr scootLambda []

-- does scootRight three times
scootRowRight :: [Int] -> [Int]
scootRowRight = scootRowRightOnce . scootRowRightOnce . scootRowRightOnce

-- scoots whole board
scootRight :: [[Int]] -> [[Int]]
scootRight = map scootRowRight

scootLeft = transpose . reverse . transpose . scootRight . transpose . reverse . transpose
scootDown = transpose . scootRight . transpose
scootUp = reverse . transpose . scootRight . transpose . reverse

--------------------------------------------
-- Comboing (TODO: REFACTOR)              --
--------------------------------------------

comboLambda :: Int -> [Int] -> [Int]
comboLambda y [] = [y]
comboLambda y [x] = if x == y then [0,x+y] else [y,x]
comboLambda y (x:xs) = if x == y then 0:x+y:xs else y:x:xs

-- Takes a row and scoots all numbers through zeroes *once*
-- Example: [2,0,0,2] -> [0,2,0,2] and [0,2,0,2] -> [0,0,2,2]
comboRowRightOnce :: [Int] -> [Int]
comboRowRightOnce = foldr comboLambda []

-- does scootRight three times
comboRowRight :: [Int] -> [Int]
comboRowRight = comboRowRightOnce . comboRowRightOnce . comboRowRightOnce

-- scoots whole board
comboRight :: [[Int]] -> [[Int]]
comboRight = map comboRowRight

comboLeft = transpose . reverse . transpose . comboRight . transpose . reverse . transpose
comboDown = transpose . comboRight . transpose
comboUp = reverse . transpose . comboRight . transpose . reverse

-- This could DEFINITELY be more clean.
goRight = scootRight . comboRight . scootRight
goLeft = scootLeft . comboLeft . scootLeft
goDown = scootDown . comboDown . scootDown
goUp = scootUp . comboUp . scootUp

