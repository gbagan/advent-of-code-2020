module Day02 (solve) where
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate, count)

data Line = Line Int Int Char String

parser :: Parser [Line]
parser = line `sepEndBy1` P.eol where
        line = Line <$> (L.decimal <* P.char '-') <*> (L.decimal <* P.char ' ') 
                <*> (P.lowerChar <* P.string ": ") <*> some P.lowerChar

part1 :: [Line] -> Int
part1 = count test where
    test (Line n m c s) = n <= x && x <= m where x = count (==c) s

part2 :: [Line] -> Int
part2 = count test where
    test (Line n m c s) = (s !! (n-1) == c) /= (s !! (m-1) == c)

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . part1) (pure . part2)