module Main where
import Test.QuickCheck
import Tests

main :: IO ()
main = do
    putStrLn $ "[*] Detects solvable boards correctly: " ++ show solvableBoardsTest
    putStrLn $ "[*] Detects unsolvable boards correctly: " ++ show unsolvableBoardsTest
    putStrLn $ "[*] Checks solutions correctly: " ++ show isSolutionTest
    putStrLn $ "[*] Finds solutions correctly: " ++ show solutionsTest