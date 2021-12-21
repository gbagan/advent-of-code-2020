module Day03 (solve) where
import           Data.Functor (($>))
import           Text.Megaparsec (sepEndBy1, some, (<|>))
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate, count)

parser :: Parser [[Bool]]
parser = some bit `sepEndBy1` P.eol where
        bit = P.char '.' $> False <|> P.char '#' $> True

descent :: Int -> Int -> [[Bool]] -> Int
descent x y = count id . zipWith go [0,x..] . downs where
    go i line = line !! (i `mod` 31)
    downs [] = []
    downs (x:xs) = x : downs (drop (y-1) xs)

part1 :: [[Bool]] -> Int
part1 = descent 3 1

part2 :: [[Bool]] -> Int
part2 l = product [descent 1 1 l, descent 3 1 l,  descent 5 1 l, descent 7 1 l, descent 1 2 l]

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . part1) (pure . part2)