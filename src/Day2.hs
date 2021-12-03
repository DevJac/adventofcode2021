module Main where
import RIO hiding (many, Down)
import RIO.Text hiding (filter)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void Text

data Direction = Forward | Up | Down deriving Show

data Action = Action Direction Int deriving Show

mag :: Action -> Int
mag (Action _ m) = m

direction :: Parser Direction
direction = toDirection <$> (chunk "forward" <|> chunk "up" <|> chunk "down")
                 where
                   toDirection "forward" = Forward
                   toDirection "up" = Up
                   toDirection "down" = Down
                   toDirection s = error $ "parseDirection: Unknown Direction: " ++ unpack s

action :: Parser Action
action = do
  d <- direction
  _ <- space
  Action d <$> decimal

actions :: Parser [Action]
actions = many (action <* space)

parseActions :: Text -> [Action]
parseActions t = case parse actions "" t of
                 Left e -> error $ show e
                 Right r -> r

horizontal :: Action -> Bool
horizontal (Action Forward _) = True
horizontal (Action Up _) = False
horizontal (Action Down _) = False

negateUp :: Action -> Action
negateUp (Action Up m) = Action Up (-m)
negateUp a = a

sumHorizontal :: [Action] -> Int
sumHorizontal = sum . fmap mag . filter horizontal

sumVertical :: [Action] -> Int
sumVertical = sum . fmap (mag . negateUp) . filter (not . horizontal)

aim :: [Action] -> Int -> (Int, Int) -> (Int, Int)
aim (Action Up m:as) am (h, d) = aim as (am-m) (h, d)
aim (Action Down m:as) am (h, d) = aim as (am+m) (h, d)
aim (Action Forward m:as) am (h, d) = aim as am (h+m, d+(m*am))
aim [] _ (h, d) = (h, d)

main :: IO ()
main = runSimpleApp $ do
  testInput <- readFileUtf8 "inputs/02_0.txt"
  fullInput <- readFileUtf8 "inputs/02_1.txt"
  logInfo "Part 0:"
  logInfo $ displayShow $ sumHorizontal (parseActions testInput) * sumVertical (parseActions testInput)
  logInfo "Part 1:"
  logInfo $ displayShow $ sumHorizontal (parseActions fullInput) * sumVertical (parseActions fullInput)
  logInfo "Part 2:"
  let (h, d) = aim (parseActions fullInput) 0 (0, 0)
  logInfo $ displayShow $ h*d
