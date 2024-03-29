import           Assets                         ( periodicTable
                                                , tableString
                                                )
import           Data.Char                      ( toLower )
import           System.Environment             ( getArgs )
import           System.Exit                    ( die )

getElement :: [Char] -> ([Char], [Char], Integer, Double, [Char], [Char])
getElement x | null $ fetch x = error "No entries found"
             | otherwise      = head $ fetch x
 where
  fetch input = filter
    (\(a, b, c, d, e, f) ->
      map toLower a == inp || map toLower b == inp || show c == inp
    )
    periodicTable
    where inp = map toLower input

renderElement :: [Char] -> IO ()
renderElement arg = putStr $ concatMap
  concat
  [ ["\x1b[35mSymbol         \x1b[32m->\x1b[0m  ", symbol, "\n"]
  , ["\x1b[35mName           \x1b[32m->\x1b[0m  ", name, "\n"]
  , ["\x1b[35mAtomic Number  \x1b[32m->\x1b[0m  ", show atNumber, "\n"]
  , ["\x1b[35mAtomic Mass    \x1b[32m->\x1b[0m  ", show atMass, "\n"]
  , ["\x1b[35mType           \x1b[32m->\x1b[0m  ", kind, "\n"]
  , ["\x1b[35mState          \x1b[32m->\x1b[0m  ", state, "\n"]
  ]
  where (symbol, name, atNumber, atMass, kind, state) = getElement arg

main :: IO ()
main = do
  args <- getArgs
  if null args
    then
      die
      $  tableString
      ++ "\nProvide search parameter; (Atomic Number | Name | Symbol)"
    else renderElement $ head args
