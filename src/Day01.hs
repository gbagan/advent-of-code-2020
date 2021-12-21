module Day01 (solve) where
import           Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import           Text.Megaparsec (sepEndBy1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate)

parser :: Parser [Int]
parser = L.decimal `sepEndBy1` P.eol

part1 :: Int -> [Int] -> Maybe Int
part1 n l = listToMaybe [x * y | x <- l, let y = n - x, Set.member y s]
        where s = Set.fromList l

part2 :: Int -> [Int] -> Maybe Int
part2 n l = listToMaybe [x * y * z | x <- l, y <- l, let z = n - x - y, Set.member z s]
        where s = Set.fromList l

solve :: String -> IO ()
solve = aocTemplate parser pure (part1 2020) (part2 2020)