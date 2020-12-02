import Test.QuickCheck
import Day01

prop_sumDigits :: NonNegative Int -> Bool
prop_sumDigits (NonNegative i)
  | i < 10 = sumDigits str == i
  | i == 10 = sumDigits str == 1
  | i == 11 = sumDigits str == 2
  | otherwise = True
  where str = show i

main :: IO ()
main = do
  quickCheck prop_sumDigits
  putStrLn "Done"
