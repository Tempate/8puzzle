module Main where
import Tests

main :: IO ()
main = do
    putStrLn $ "[*] Detects solvable boards correctly: " ++ if solvableBoardsTest then "yes" else error "no"
    putStrLn $ "[*] Detects unsolvable boards correctly: " ++ if unsolvableBoardsTest then "yes" else error "no"
    putStrLn $ "[*] Checks solutions correctly: " ++ if isSolutionTest then "yes" else error "no"
    putStrLn $ "[*] Finds solutions correctly: " ++ if solutionsTest then "yes" else error "no"