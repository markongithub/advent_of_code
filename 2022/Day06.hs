module Day06 where

import Data.Map (Map)
import qualified Data.Map as Map

type LastSeen = Map Char Int

findNUnique0 :: Int -> LastSeen -> Int -> Int -> [Char] -> Int
findNUnique0 threshold lastSeen nextPossibility count (x:xs) = let
  newLastSeen = Map.insert x count lastSeen
  newNP = case (Map.lookup x lastSeen) of
    Just n -> max nextPossibility (n + threshold)
    Nothing -> nextPossibility
  recurse = findNUnique0 threshold newLastSeen newNP (count + 1) xs
  shouldRecurse = count < newNP
  in if shouldRecurse then recurse else count

findNUnique :: Int -> [Char] -> Int
findNUnique threshold ls = findNUnique0 threshold Map.empty threshold 1 ls

testInput = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
day6input = "tzltzltthfthtdtstftbfbnffdjjvnvcccnznpndppsvvlnvlnvndntnllwffvwwcpwccssqbqnbbwzbwwfjjscspspfsfvvzjvvmjvjwjljbbbqtbbcqbccdqcdcpddnvnjvvqwwbrbggjllhbbzlblrrrcwwrfrmffgddfsddnqnqpnnmzzwlzwzqwqgqnngnfnvnhvvfnvvszzrbrqbqnbbrsbsvssgngwgqwqbqsbsvbbdvdfvdvbbqlqdlqdllnppwcclwlvwlwjlwwdjjjcschhhmrrnnrznzlnnwmnwnfngghngnpggmrrzrttdbdtddjhdhrrqddnhnjjntjtbtmtffwcfcnfcnfnjjfnjfjjhrhhgfgttgmttltfllfqfnqqcbbrlblrbrcrssrllhddmnddmcdddvttfdfqfmqqcncggczczdccvhcvcjvvgnnqttltcchzhjjwmmqvqtqnqbbhddcqdccnwccttgwwzssncncjjprjjpbprrndnrnrhnhrnhngnhhbjbccpmpmrppjvpjpqjjsvjjhttcclmmqzzlggtqqqgdqqbhhtmmqfmqmwmjwmjmmnsmsvsrrvpvhhtshhhmqmdqqqqlfffwgwhwjjrfjfmfrmffgzzjvzznqqwggcvcnvvcpvpjpjbpbrpbrrzhhcffqqlzzrmzrmzmzrrqdqrqnqrnrnqnrnsrrnjrrgllgqlljldjjmvvqbqhhsmsmddsfdddcmmphhtjhjzhjjjhcjjzppwhwvvdnvnvpnvnlnvnffrjjtzzdqzzngzgqghhmvvgwwqhwhlhvvfzzdpzddtbdtbtvtlvlqlllvvdvsdsslccbscsbbcggldlvvdccdscdsdzsddpggcfggnffpjpnpcncfcqqlvvszzbpbqbcqczcqzcqzqqlrlwwbgglrrbgbhhlbbsffrprtpppdwdhwhbbpssvbvnntbbfnfddmmfcfcjfftbfbdbccngcgjcjtjftjffrwwtwlwddczcnzczpzcpccrqcrczrzcrzzhnntgtqgtgztgzzcpphshwswfwqqnzngzzbpzzpqpzqznzdzmzqqjzzhnhvnnqjnnrdrcctdtppsffzvvwvbbpttsrsspsddfvvfllrtlljpjlplcctthbbgbqgbgmmpwpvvghvhfhwfwcwvwrvwrvvcgccdncddbmbmfmlffgqggrzgzhhzfzdfzfhhzdhzhjzjhhbffrtrjtthssbpspddpttrggdndsnnjwwzrwrvvmqvmvdvhvjvnnqmmhrhrzzzwttnftfddsdfsfjsjcccvhvgvgzvgzvzbvvhwhbbpccfjfllgmlggtbggdsgsmmmnznqnhqhrqhrrtthwhqqvzqvvvqgqffpvptvpvwvbvsscsnnhlnhhzrzjrjrllcffpqpbpsbbflblppqffbqqpttccdgcgdgfdfgdgjddjljpljppdspddflftllhchmcmwcwddrqqsdsswwmwwwjdjvdvwwcgcddmhhqtqvqmvvnffqppnvpphpnnshsmhmdhmdddlnlmnmsnnpttlfttnqnhqnqrnrddplplbpbdpdhppccbnbwwpwzwgwdgdnggcmmtnmnznmzmwmrmtrrjppjgjmggndgnnjzzrczzftztzgzvgvddgmdgmgpmmgjmmnrrmvmnvvhqqggsnnplnlnbbrcbbpphssnqsnspnncfchhvjjdjvjfjddbjbddhlllpdnrhtzhqpphzfbjclncdlrbtzhcwslpnstdvjslnzfrvfdlmpgpfhrqtjvqvjlqgdcjrbjtjrgvbfwjzsvrbmffnhvjqnshvdjbgwmpwlfjznngzpqbvlztnvgvjsnwvhpfwbhfsmjgwwjdrrwbtvpwtzvjfhwrmnrhdsvgpgdsfgndmffqfplsgjrsvztzlznqsrbldbmmhqmjtrzscrbwlpgztlrvllprnhzsvtnvwzmjwqhqpjqhntcrscwcdnwzpvbdczzcmzrmdwthdtszqzftcsfbwfqggpcntfrgwpmjpdzjnczwcjmdjnrqjfwqbznznmcdvzqlpqschnmcfqjjrjwfmqqftfdhdzffvshqbmrgrpvlgqcgsbsngttvcpjswdgrhbblrhjllfbzngqjzzbdwtnlnrbpftvwbmrhvcnntdrbvtrtpcsqdsrvpsgggfpwcbzhwhwmmmmmgjzgdtwnzjdwjfljghbjvjnsgshmdpztnbbrnwfvzhtzqpzttftdmcmqzlnrgncwwtpwqrgmpmwwchwhbbbblcndbsrrqtnztcmqhvdwfcswnswvhqdtqfdrhjgczqvrzqczmnpcgbwntjvlsfrzrrjtsvfzfmbwwsftwqvttpjvbggrlcspnfhwwmrhdbbhdcjvmrhppvcmtmfhszjlcjjsdqfvjttcmffwzfpmjmjzhcrqmhhwzhjlnwphvvhmrbllsvpjljthjndffrdbmdjncnmdtcwfwjdwnrdlvqsbzczlhwrtpnzfwzzwbrqpglgvrjsnsprvwszmlrjcdgzwchmcqrjdlzqfvqwwfszpptprhcfsdfcrnhvhgvcdwgnqzjtmgznltbjjqwzlljrqcmpdncshzvsmvjwlmvtwbtjcgmqfslvwcfqpljzdjmdvqjlztbsbshcwhlvzcmzljvrrhrbzwvthgtnszpcrrdwcmtdncdzlbdscfbhrlqttcfshqrsgvzhlcnvfhppdqvblsznmctftmnslwgbmbgshgwvmzpdnmqmjgqnvrwprmbzrdprrbcwnslczvzgnssjqqzdrlntrnsrgbjjcpqvnqwvnwgslchqzbphcqsbvgvwzlnsndfrhqjvtlnqpcsgswzfvhjmfgwgfvhjgzntdbztmjsbmtwlfmvvgvztvwwmqclcgctqbvljgfngcvfqlmmvqmbtrnnbhqjjndzqhvvdztjgvwgrtltfrlzrjcpgwvpqwmcwmqccjtjhhbrrqphlpljvhjzpdfcdsgzfpnzdzhfdqjsnrvmwstrmmwmlhbvrjbtnmvwcqnzqzzpwzjdnfhqwwlsvgnnjgffzcnrtbjfwllnrgppchqwnfpwpgnfbvwcbjrlscnwlswjmnrcrhtdhgpzvgtfcqzgqtwvlhrgbmjvvzhrlzfvmrdjjctvfwsgmjwbqslhmjlcvlwrdqfmbhcfrmrvqtslplwpsgrfmntmtvmvqttbspmftgqdzlcfplcvvfmmjttwqjpdtjzzsfjcprvbwdvfrpzddhwrlmsnpjzqgdlfdzvdjnjtgtfflzzvjlmnnvmglrptsnppwscznltcvzfjmwshnsqsvsjpqwsqlbwzslhgrdcbbvcjspqfntbcpwwrphgpmwbpqdcfvvtlsgpfshtcrdftsltwnbnmzfwcwlmrhlntmmnnpsdchvntcwbnmjdgwcmzzvbrhbdbmlgwppzwsqvcccdbfzfsfhtmbppnwbtjvrvjtmddhmrjdqgnmrnjjpqsgtbgcvtclzzstlpldtqbnnvqjfbjcfzblvcwhjphzcgwfljjhzzmwdcrzsssznztcwpjlbcffnlmsfjbmtvhhcljmtqdprdmdgwgpnnlmhgwpsgprfqnspmntrdjwjmrflsbfpqhzswbsrdbdhjmvtwmjjnmpllgfllzgwwmswjcmggbrvsbbhjmsdzzpbhbrlphwdsmjdzsqjfrmdmpljnwscjrhdvzqbhhvpmhwqfrrhzlncrrrzhmjdwqjcbsqjbhbdbjzpslrnnbzctnnlhqmqqbdzfbrpfgwsrdglnplpspnnqhtbhzhzgtchcbqcmmcmvlllczqbtmbstzmnlhhhbmmbtjwnbgwjbfhgvfhqlsgdnnrsgghjzjlqfwbbgztdqzbhhwhcwtjwsgstjpzcjjvqbpfpvlqfqshvfzbwmfcwfgqvgmbppfvzgzznzhsqbvzlztsnmnrbgqzbmbhlvqhfncdfcpttgzpvvzdbhvqdtqsblqvrsrnmsfbqhrpvlzffdzptzghvmbmdzjrsqzhqddqm"

solvePart1 = findNUnique 4 day6input
solvePart2 = findNUnique 14 day6input
