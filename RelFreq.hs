
import System.IO
import Data.Foldable

main = do
    let words = []
    inHand <- readFile "assets/unigram_freq.csv"

    let parsedData = filter ((\w -> length w == 5).fst) $ map parseLine $ lines inHand
    let total = fromIntegral $ foldl' (\a b-> a + snd b) 0 parsedData

    let parsedData' = map (\(a,b) -> (a, fromIntegral b/total)) parsedData

    writeFile "assets/word-relative-frequency" $ foldl' (\ a (b, c) -> a ++ b ++ " " ++ show c ++ "\n") "" parsedData'
    putStrLn "Done!"

parseLine :: String -> (String, Int)
parseLine str = (fst broke, read $ tail $ snd broke)
    where
        broke = break (==',') str
