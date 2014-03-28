import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Data.List (transpose,reverse)
import Data.List.Split (chunksOf)
import System.Random (StdGen,getStdGen,random,randoms)
import Control.Lens (set,element)

--------------------------------------------
--TODO:
--loss detection
--scoring
--animations (major!)
--make 4s spawn sometimes
-------------------------------------------

main = 
  do
    g <- getStdGen
    play
      (InWindow "2048 in Haskell by Maia"
         (410,500)
         (20,20)
      )
      black -- background color
      fps -- simulation steps per second
      (initPositions g) -- initial world
      drawWorld -- function to convert world to picture
      handleInputEvents -- TODO add randomness
      stepWorld

fps = 20

-- type World = ([[Int]],StdGen) -- also stores random number generator to help with some stuff
data Tile = Tile { val :: Int, popInTime :: Float, popOutTime :: Float} deriving (Eq)
type Row = [Tile]
data World = World {board :: [[Tile]], gen :: StdGen}

--------------------------------------------
-- Utils for world generation and updating
--------------------------------------------

tileIntFunction :: (Int -> Int) -> Tile -> Tile
tileIntFunction f t = t {val=f (val t)}

setVal :: Tile -> Int -> Tile
setVal t i = t {val=i}

setVals :: [Tile] -> [Int] -> [Tile]
setVals ts is = zipWith setVal ts is

setValss :: [[Tile]] -> [[Int]] -> [[Tile]]
setValss tss iss = zipWith setVals tss iss

makeNthZeroTwo :: Int -> [Int] -> [Int]
makeNthZeroTwo _ [] = []
makeNthZeroTwo 0 (x:xs) = if x == 0 then 2:xs else x:(makeNthZeroTwo 0 xs)
makeNthZeroTwo n (x:xs) = if x == 0 then x:(makeNthZeroTwo (n-1) xs) 
                                  else x:(makeNthZeroTwo n xs)

addNthZero :: Int -> World -> World
-- only modifies the board, nothing else
addNthZero n world = let concatWorld = concat (board world)
             in world {board = chunksOf 4 $ setVals concatWorld $ makeNthZeroTwo n $ map val $ concatWorld }

---------------------------------------------
-- Initial world generation
---------------------------------------------

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

makeTile :: Int -> Tile
makeTile i = Tile {val=i, popInTime=0.0, popOutTime=0.0}

-- generates an initial world state with, on average, two 2s to start
initPositions :: StdGen -> World
-- This is a really cool line of code but sadly it is no longer useful.
-- initPositions gen = let board = chunksOf 4 $ map ((*2) . toInt . (== 0) . (`mod` 4)) $ take 16 $ (randoms gen :: [Int]) in (board,gen)
initPositions g = let origBoard = chunksOf 4 $ map makeTile $ replicate 16 0
                    in addTwo $ addTwo World {board=origBoard,gen=g}

--------------------------------------------
-- Regeneration of 2s
--------------------------------------------

addTwo :: World -> World
addTwo world = let numZeros = (sum . (map toInt) . (map (==0)) . map val . concat) (board world)
                   (rand, newGen) = random (gen world) :: (Int, StdGen)
                   n = if numZeros == 0 then 0 else rand `mod` numZeros
                   newBoard = chunksOf 4 $ setVals (concat (board world)) $ makeNthZeroTwo n $ map val $ concat (board world)
                in world {board=newBoard, gen=newGen}

---------------------------------------------
-- Input handling
-- -----------------------------------------
--
data Direction = U | D | L | R

keyDir :: Key -> Maybe Direction
keyDir (SpecialKey KeyUp) = Just U
keyDir (SpecialKey KeyDown) = Just D
keyDir (SpecialKey KeyLeft) = Just L
keyDir (SpecialKey KeyRight) = Just R
keyDir _ = Nothing

handleInputEvents :: Event -> World -> World
handleInputEvents (EventKey k Down _ _) world = let dir = keyDir k
                                                    newBoard = go dir (board world)
                                                in if newBoard == (board world) -- NOTE watch out if it's testing equality of popin and popout times??
                                                   then world
                                                   else addTwo world
handleInputEvents  _ x = x

stepWorld :: Float -> World -> World
stepWorld _ x = x

rowHgt = 100

drawWorld :: World -> Picture
drawWorld World {board = [r1, r2, r3, r4]} = translate (150) (150) (pictures [ drawRow r1, 
                                        translate 0 (-rowHgt) (drawRow r2),
                                        translate 0 (-rowHgt*2) (drawRow r3),
                                        translate 0 (-rowHgt*3) (drawRow r4)
                                      ])

tileS = 90
textScale = 0.2

colorZipper :: [(Int, Color)]
colorZipper = [(2,    makeColor8 255 235 235 255),
               (4,    makeColor8 255 200 200 255),
               (8,    makeColor8 255 170 170 255),
               (16,   makeColor8 255 140 140 255),
               (32,   makeColor8 255 110 110 255),
               (64,   makeColor8 255  80  80 255),
               (128,  makeColor8 255 230  90 255),
               (256,  makeColor8 255 230  90 255),
               (512,  makeColor8 255 230  90 255),
               (1024, makeColor8 255 230  90 255),
               (2048, makeColor8 255 235  90 255)]

getColorUnsafe :: Int -> Maybe Color
getColorUnsafe x = lookup x colorZipper 

convertColor :: Maybe Color -> Color
convertColor (Just c) = c
convertColor Nothing = makeColor8 255 255 255 255

getColor :: Int -> Color
getColor x = convertColor (getColorUnsafe x)

-- Takes x-offset and tile value and draws the tile
drawTileBack :: Float -> Int -> Picture
drawTileBack x val = color white (translate x 0 (rectangleSolid tileS tileS))

-- Draws the tile including its animation stuff
drawTile :: Float -> Tile -> Picture
drawTile x tile = let v = val tile
                      background = [color (getColor v) (translate x 0 (rectangleSolid tileS tileS))]
                      number = if v > 0 then  -- only display the number if it isn't 0
                                  [translate x 0 (scale textScale textScale (text (show v)))]
                                 else [] :: [Picture]
                      poppingIn = (popInTime tile) > 0
                      poppingOut = (popOutTime tile) > 0
                      curScale = if poppingIn then (1-popInTime tile) else (1+popOutTime tile)
         in scale curScale curScale (pictures (background ++ number))

drawRow :: Row -> Picture
drawRow [i,j,k,l] = translate (-300) 0 (pictures [ drawTile 0 i, 
                                                   drawTile rowHgt j,
                                                   drawTile (rowHgt*2) k,
                                                   drawTile (rowHgt*3) l ])

------------------------------
-- Board handling ------------
------------------------------



scootLambda :: Int -> [Int] -> [Int]
scootLambda y [] = [y]
scootLambda y [x] = if x == 0 then [x,y] else [y,x]
scootLambda y (x:xs) = if x == 0 then x:y:xs else y:x:xs

-- Takes a row and scoots all numbers through zeroes *once*
-- Example: [2,0,0,2] -> [0,2,0,2] and [0,2,0,2] -> [0,0,2,2] scootRowRightOnce :: [Int] -> [Int]
scootRowRightOnce = foldr scootLambda []

-- does scootRight three times
scootRowRight :: [Int] -> [Int]
scootRowRight = scootRowRightOnce . scootRowRightOnce . scootRowRightOnce

-- scoots whole board
scootRight :: [[Int]] -> [[Int]]
scootRight = map scootRowRight

scoot :: Maybe Direction -> [[Int]] -> [[Int]]
scoot Nothing = id
scoot (Just R) = scootRight
scoot (Just U) = reverse . transpose . scootRight . transpose . reverse
scoot (Just L) = transpose . reverse . transpose . scootRight . transpose . reverse . transpose
scoot (Just D) = transpose . scootRight . transpose

--------------------------------------------
-- Comboing (TODO: REFACTOR)              --
--------------------------------------------

comboLambda :: Int -> [Int] -> [Int]
comboLambda y [] = [y]
comboLambda y [x] = if x == y then [0,x+y] else [y,x]
comboLambda y (x:xs) = if x == y then 0:x+y:xs else y:x:xs

-- Takes a row and scoots all numbers through zeroes *once*
-- Example: [2,0,0,2] -> [0,2,0,2] and [0,2,0,2] -> [0,0,2,2]
comboRowRight :: [Int] -> [Int]
comboRowRight = foldr comboLambda []

-- scoots whole board
comboRight :: [[Int]] -> [[Int]]
comboRight = map comboRowRight

combo :: Maybe Direction -> [[Int]] -> [[Int]]
combo Nothing = id
combo (Just R) = comboRight
combo (Just U) = reverse . transpose . comboRight . transpose . reverse
combo (Just L) = transpose . reverse . transpose . comboRight . transpose . reverse . transpose
combo (Just D) = transpose . comboRight . transpose

-- !! Now actually go in the direction!!

-- todo: make this on Tiles instead of Ints
goInt :: Maybe Direction -> [[Int]] -> [[Int]]
goInt dir = (scoot dir) . (combo dir) . (scoot dir)

go :: Maybe Direction -> [[Tile]] -> [[Tile]]
go dir tss = (setValss tss . (goInt dir) . chunksOf 4 . map val . concat) tss
