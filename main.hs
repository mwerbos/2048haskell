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
data Tile = Tile { val :: Int, popInTime :: Float, popOutTime :: Float} deriving (Eq,Show)
type Row = [Tile]
data World = World {board :: [[Tile]], gen :: StdGen, score :: Int}

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
                    in addTwo $ addTwo World {board=origBoard,gen=g,score=0}

--------------------------------------------
-- Regeneration of 2s
--------------------------------------------

makeNthZeroTwo :: Int -> [Tile] -> [Tile]
makeNthZeroTwo _ [] = []
makeNthZeroTwo 0 (x:xs) = if val x == 0
                          then (Tile {val=2, popInTime = 0.5, popOutTime = 0.0}:xs)
                          else x:(makeNthZeroTwo 0 xs)
makeNthZeroTwo n (x:xs) = if val x == 0 then x:(makeNthZeroTwo (n-1) xs)
                                  else x:(makeNthZeroTwo n xs)

addTwo :: World -> World
addTwo world = let numZeros = (sum . (map toInt) . (map (==0)) . map val . concat) (board world)
                   (rand, newGen) = random (gen world) :: (Int, StdGen)
                   n = if numZeros == 0 then 0 else rand `mod` numZeros
                   newBoard = chunksOf 4 $ makeNthZeroTwo n $ concat (board world)
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
                                       -- NOTE watch out if it's testing equality of popin and popout times??
                                                in if newBoard == (board world)
                                                   then world {board=newBoard}
                                                   else addTwo world {board=newBoard}
handleInputEvents  _ x = x

--------------------------------------------------------
-- Time steps: All they do is update the animations   --
-- -----------------------------------------------------

popInSpeed = 4
popOutSpeed = 0.5

-- changes animation times for each tile
updateTile :: Float -> Tile -> Tile
updateTile dt t = if val t > 0 then
                    t {popInTime = if popInTime t - dt*popInSpeed > 0 then popInTime t - dt*popInSpeed else 0,
                     popOutTime = if popOutTime t - dt*popInSpeed > 0 then popOutTime t - dt*popInSpeed else 0}
                  else t {popInTime = 0, popOutTime = 0}

updateTiles :: Float -> [[Tile]] -> [[Tile]]
updateTiles dt tss = (map (map (updateTile dt))) tss

stepWorld :: Float -> World -> World
stepWorld dt world = world {board=updateTiles dt (board world)}

---------------------------------------------------------
--  Drawing and display
---------------------------------------------------------

rowHgt = 100

drawWorld :: World -> Picture
drawWorld World {board = [r1, r2, r3, r4], score=s} = translate (150) (150) (pictures [ drawRow r1,
                                        translate 0 (-rowHgt) (drawRow r2),
                                        translate 0 (-rowHgt*2) (drawRow r3),
                                        translate 0 (-rowHgt*3) (drawRow r4),
                                        translate (-100) 60 $ scale 0.2 0.2 $ color white $ text $ "Score: " ++ (show s) ])

tileS = 90
textScale = 0.2

-- TODO: also make a zipper to *pictures* to also adjust number position, text color, etc. by number

colorZipper :: [(Int, Color)]
colorZipper = [(2,    makeColor8 238 228 218 255),
               (4,    makeColor8 237 224 200 255),
               (8,    makeColor8 242 177 121 255),
               (16,   makeColor8 245 149  99 255),
               (32,   makeColor8 246 124  95 255),
               (64,   makeColor8 246 102  62 255),
               (128,  makeColor8 238 208 114 255),
               (256,  makeColor8 237 204  97 255),
               (512,  makeColor8 237 200  80 255),
               (1024, makeColor8 237 197  63 255),
               (2048, makeColor8 237 194  46 255)]

getColorUnsafe :: Int -> Maybe Color
getColorUnsafe x = lookup x colorZipper

convertColor :: Maybe Color -> Color
convertColor (Just c) = c
convertColor Nothing = makeColor8 255 255 255 255

getColor :: Int -> Color
getColor x = convertColor (getColorUnsafe x)

-- Takes x-offset and draws the tile background
-- maybe unroll this into drawTile?
drawTileBack :: Float -> Picture
drawTileBack x = color white (translate x 0 (rectangleSolid tileS tileS))

-- Takes x-offset and tile and draws the tile itself
drawTile :: Float -> Tile -> Picture
drawTile x tile = let background = [color (getColor $ val tile) $ rectangleSolid tileS tileS]
                      number = if val tile > 0
                               then [translate (-20) (-10) $ scale textScale textScale $ text $ show $ val tile]
                               else []
                      curScale = if (popInTime tile) > 0
                                 then (1-(popInTime tile))
                                 else (1+(popOutTime tile))
                  in pictures
                     [ drawTileBack x,
                       translate x 0 $ scale curScale curScale $ pictures $ background ++ number]

drawRow :: Row -> Picture
drawRow [i,j,k,l] = translate (-300) 0 (pictures [ drawTile 0 i,
                                                   drawTile rowHgt j,
                                                   drawTile (rowHgt*2) k,
                                                   drawTile (rowHgt*3) l ])

--------------------------------------------
-- Board handling (moving and stuff)      --
--------------------------------------------

scootLambda :: Tile -> [Tile] -> [Tile]
scootLambda y [] = [y]
scootLambda y [x] = if val x == 0 then [x,y] else [y,x] -- TODO add animations
scootLambda y (x:xs) = if val x == 0 then x:y:xs else y:x:xs

-- Takes a row and scoots all numbers through zeroes *once*
-- Example: [2,0,0,2] -> [0,2,0,2] and [0,2,0,2] -> [0,0,2,2] scootRowRightOnce :: [Int] -> [Int]
scootRowRightOnce = foldr scootLambda []

-- does scootRight three times
scootRowRight :: [Tile] -> [Tile]
scootRowRight = scootRowRightOnce . scootRowRightOnce . scootRowRightOnce

-- scoots whole board
scootRight :: [[Tile]] -> [[Tile]]
scootRight = map scootRowRight

scoot :: Maybe Direction -> [[Tile]] -> [[Tile]]
scoot Nothing = id
scoot (Just R) = scootRight
scoot (Just U) = reverse . transpose . scootRight . transpose . reverse
scoot (Just L) = transpose . reverse . transpose . scootRight . transpose . reverse . transpose
scoot (Just D) = transpose . scootRight . transpose

--------------------------------------------
-- Comboing (TODO: REFACTOR)              --
--------------------------------------------

-- Crap, refactoring this to put the score in is going to be annoying.

comboLambda :: Tile -> [Tile] -> [Tile]
comboLambda y [] = [y]
comboLambda y (x:xs) = if val x == val y && val x > 0
                       then (makeTile 0):(Tile {val=val x + val y, popOutTime = 0.1, popInTime = 0}):xs
                       else y:x:xs -- TODO animations
-- comboLambda y (x:xs) = if val x == val y then (makeTile 0):(makeTile $ val x+val y):xs else y:x:xs

-- Takes a row and does combos to the right on all numbers *once*
-- Example: [2,2,0,0] -> [0,4,0,0] and [2,2,2,2] -> [0,4,0,4]
comboRowRight :: [Tile] -> [Tile]
comboRowRight = foldr comboLambda []

-- does combos on whole board
comboRight :: [[Tile]] -> [[Tile]]
comboRight = map comboRowRight

combo :: Maybe Direction -> [[Tile]] -> [[Tile]]
combo Nothing = id
combo (Just R) = comboRight
combo (Just U) = reverse . transpose . comboRight . transpose . reverse
combo (Just L) = transpose . reverse . transpose . comboRight . transpose . reverse . transpose
combo (Just D) = transpose . comboRight . transpose

-- !! Now actually go in the direction!!

go :: Maybe Direction -> [[Tile]] -> [[Tile]]
go dir = (scoot dir) . (combo dir) . (scoot dir)
