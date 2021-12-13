import Data.List
import Data.List.Split


type Point = (Int, Int)
type FoldInstruction = (Char, Int)

foldPaper :: Int -> [String] -> [Point]
foldPaper n cmds = map head . group . sort            -- Distinct points
                 . foldr map points . map foldPoint   -- Apply n fold
                 . reverse $ take n folds             -- instruction
  where [ps, fs] = splitOn [""] cmds

        points = map parsePoint ps
        folds  = map parseFold fs
        
        -- | For Points of input String 
        --
        -- "1141,761"
        --
        parsePoint :: String -> Point
        parsePoint input = (x,y)
          where [x,y] = map toInt $ splitOn "," input

        -- | For Folding instruction input
        --
        -- "fold along x=655"
        --
        parseFold :: String -> FoldInstruction
        parseFold input = (head axis, toInt step)
          where [axis, step] = splitOn "=" 
                             . last 
                             $ splitOn " " input

        toInt :: String -> Int
        toInt x = read x :: Int

        -- | FoldInstruction Points along an axis
        --      
        --      oooooo|ooox        ooxooo|
        --      ↑     ↑   ↑   ==>  ↑ ↑   ↑
        --      0    h=6 x=10      0 x' h=6
        --
        foldPoint :: FoldInstruction -> Point -> Point
        foldPoint ('x', h) (x, y) | h >= x    = (x,     y)
                                  | otherwise = (h-x+h, y)
        foldPoint ('y', v) (x, y) | v >= y    = (x,     y)
                                  | otherwise = (x, v-y+v)

-- | Turn a list of indices into a String of marks
--                   0123456
--    [0,1,2,6] ==> "###   #"
--
showLn :: [Int] -> String
showLn = showLn' (-1)
  where showLn' :: Int -> [Int] -> String
        showLn'    _       []                       = ""
        showLn' prev s@(x:xs) | abs (x - prev) == 1 = '#' : showLn' x xs
                              | otherwise           = ' ' : showLn' (prev + 1) s


-- Example from Task
examplePointList :: [Point]
examplePointList = [ (6,10), (0,14), (9,10), (0, 3), (10, 4), (4,11)
                   , (6, 0), (6,12), (4, 1), (0,13), (10,12), (4, 4)
                   , (3, 0), (8, 4), (1,10), (2,14), ( 8,10), (9, 0) ]

exampleFoldList :: [FoldInstruction]
exampleFoldList = [ ('y', 7), ('x', 5) ]


-- Parts
partOne :: String -> Int
partOne = length . foldPaper 1 . lines
        
partTwo :: String -> String
partTwo = foldr (\x xs -> x ++ "\n" ++ xs) ""
        . map (showLn . map fst)
        . groupBy (\x y -> snd x == snd y)  . sortOn snd 
        . foldPaper 1000
        . lines


-- Main function
main :: IO ()
main = do
  puzzle <- readFile "puzzle.aoc"
  putStrLn $
    "How many dots are visible after completing just the first fold instruction\n"
     ++ "on your transparent paper?"
  print $ partOne puzzle
  putStrLn 
    "What code do you use to activate the infrared thermal imaging camera system?"
  putStrLn $ partTwo puzzle
