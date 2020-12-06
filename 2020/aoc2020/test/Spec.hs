import Test.QuickCheck
import Day01

prop_sumDigits :: NonNegative Int -> Bool
prop_sumDigits (NonNegative i)
  | i < 10 = sumDigits str == i
  | i >= 10 && i < 100 = sumDigits str == (i `div` 10 + i `mod` 10)
  | otherwise = True
  where str = show i

main :: IO ()
main = do
  verboseCheck prop_sumDigits
  putStrLn "Done"
