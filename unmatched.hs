{-import System.IO(getChar)-}

data Location = Location { line :: Int, column :: Int} deriving(Show)
data LocateChar = LocateChar { c :: Char, location :: Location} deriving(Show)

type Tuple2 t = (t, t)
type Tuple3 t = (t, t, t)
type Locate3CharPair = Tuple3 (Tuple2 [LocateChar])

l3cpEmpty :: Locate3CharPair
l3cpEmpty = (([], []), ([], []), ([], []))

main :: IO ()
main = do s <- getContents
          print $ findUnmatched s

findUnmatched :: Locate3CharPair -> Locate3CharPair
findUnmatched 

getPairs :: String -> (Location, Locate3CharPair)
getPairs s = foldl handleChar (Location 0 0, l3cpEmpty) s
    where notMatchee :: Char -> Bool
          notMatchee = flip notElem "{}[]()" 
          handleChar :: (Location, Locate3CharPair) -> Char -> (Location, Locate3CharPair)
          handleChar (loc@(Location {line = line, column = column}),
            pairs@(cc@(lCurly, rCurly), ss@(lSquare, rSquare), pp@(lParen, rParen))) c 
            | notMatchee c = (newLoc, pairs)
            | c == '{' = (newLoc, ((located:lCurly, rCurly), ss, pp))
            | c == '}' = (newLoc, ((lCurly, located:rCurly), ss, pp))
            | c == '[' = (newLoc, (cc, (located:lSquare, rSquare), pp))
            | c == ']' = (newLoc, (cc, (lSquare, located:rSquare), pp))
            | c == ']' = (newLoc, (cc, ss, (located:lParen, located:rParen)))
            where located = LocateChar c loc
                  newLoc = if c /= '\n' then Location line (column + 1) else Location (line + 1) 0
