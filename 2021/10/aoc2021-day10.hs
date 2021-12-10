import Data.Map hiding (map, filter, foldr, foldl')
import Data.List


errorScore :: Map Char Int
errorScore = fromList [ ('N', 0) -- Neutral Marker, if no syntax error found
                      , (')', 3)
                      , (']', 57)
                      , ('}', 1197)
                      , ('>', 25137) ]

incompleteScore :: Map Char Int
incompleteScore = fromList [ (')', 1)
                           , (']', 2)
                           , ('}', 3)
                           , ('>', 4) ]


syntax :: [Char] -> [Char] -> Char
-- Open Chunks
syntax stack ('(':xs) = syntax (')':stack) xs
syntax stack ('{':xs) = syntax ('}':stack) xs
syntax stack ('[':xs) = syntax (']':stack) xs
syntax stack ('<':xs) = syntax ('>':stack) xs
-- Closing Chunks
syntax (s:ss) (x:xs) | s == x    = syntax ss xs
                     | otherwise = x
-- All Chunks were correctly closed 
syntax _ [] = 'N'


isIncomplete :: [Char] -> [Char] -> Bool
-- Open Chunks
isIncomplete stack ('(':xs) = isIncomplete (')':stack) xs
isIncomplete stack ('{':xs) = isIncomplete ('}':stack) xs
isIncomplete stack ('[':xs) = isIncomplete (']':stack) xs
isIncomplete stack ('<':xs) = isIncomplete ('>':stack) xs
-- Closing Chunks
isIncomplete (s:ss) (x:xs) | s == x    = isIncomplete ss xs
                           | otherwise = False
-- All Chunks were correctly closed 
isIncomplete _ [] = True


closingScore :: [Char] -> [Char] -> Int
-- Open Chunks
closingScore stack ('(':xs) = closingScore (')':stack) xs
closingScore stack ('{':xs) = closingScore ('}':stack) xs
closingScore stack ('[':xs) = closingScore (']':stack) xs
closingScore stack ('<':xs) = closingScore ('>':stack) xs
-- Closing Chunks
closingScore (s:ss) (x:xs) | s == x    = closingScore ss xs
                           | otherwise = error "Corrupted line found"
-- All Chunks were correctly closed or incomplete
closingScore stack [] = ((foldr (\x xs -> xs * 5 + x) 0) 
                      . reverse
                      . (map (incompleteScore !))) stack


partOne :: String -> Int
partOne = sum
        . map ((errorScore !) . (syntax []))
        . lines

partTwo :: String -> Int
partTwo = middle . sort             -- Take the middle score
        . map (closingScore [])     -- Closing scores of LOC
        . filter (isIncomplete [])  -- Exclude corrupted lines
        . lines
  where middle :: [a] -> a
        middle xs = xs !! (length xs `div` 2)


main :: IO ()
main = do
  puzzle <- readFile "puzzle.aoc"
  print "What is the total syntax error score for those errors?"
  print $partOne puzzle
  print "What is the middle score?"
  print $partTwo puzzle
