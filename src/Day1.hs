module Main where
import RIO hiding (some)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void Text

numbers :: Parser [Int]
numbers = some $ decimal <* space

parseInts :: Text -> [Int]
parseInts t = case parse numbers "" t of
                Left e -> error $ show e
                Right r -> r

pairs :: [a] -> [(a, a)]
pairs (x:xs@(y:_)) = (x, y) : pairs xs
pairs _ = []

triples :: [a] -> [(a, a, a)]
triples (x:xs@(y:z:_)) = (x, y, z) : triples xs
triples _ = []

sumTriple :: (Int, Int, Int) -> Int
sumTriple (a, b, c) = a + b + c

countIncreases :: [Int] -> Int
countIncreases = length . filter (\(x,y) -> y > x) . pairs

main :: IO ()
main = runSimpleApp $ do
  testInput <- readFileUtf8 "inputs/01_0.txt"
  fullInput <- readFileUtf8 "inputs/01_1.txt"
  logInfo "Part 0:"
  logInfo $ displayShow $ countIncreases $ parseInts testInput
  logInfo "Part 1:"
  logInfo $ displayShow $ countIncreases $ parseInts fullInput
  logInfo "Part 2:"
  logInfo $ displayShow $ countIncreases $ fmap sumTriple $ triples $ parseInts fullInput
