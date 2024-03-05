import Test.HUnit

testAddEnd :: Test
testAddEnd = TestCase $ do
    let result = addEnd [1..10] 23
    assertEqual "Adding element at the end" [1,2,3,4,5,6,7,8,9,10,23] result

tests :: Test
tests = TestList [testAddEnd]

main :: IO ()
main = do
    putStrLn "Running tests..."
    runTestTT tests