import qualified Data.Map as M
import Data.Map ((!))

toTwenty = M.fromList [
  (0, "zero"),
  (1, "un"),
  (2, "deux"),
  (3, "trois"),
  (4, "quatre"),
  (5, "cinq"),
  (6, "six"),
  (7, "sept"),
  (8, "huit"),
  (9, "neuf"),
  (10, "dix"),
  (11, "onze"),
  (12, "douze"),
  (13, "treize"),
  (14, "quatorze"),
  (15, "quinze"),
  (16, "seize"),
  (17, "dix-sept"),
  (18, "dix-huit"),
  (19, "dix-neuf") ]

tens = M.fromList [
  (2, "vingt"),
  (3, "trente"),
  (4, "quarante"),
  (5, "cinqante"),
  (6, "soixante"),
  (8, "quatre-vingt") ]

frenchNumber n | n < 20 = toTwenty ! (n `mod` 20)
frenchNumber n | n < 60 = 
    case singleDigit of
      0 -> tens ! tensDigit
      1 -> (tens ! tensDigit) ++ "-et-un"
      d -> (tens ! tensDigit) ++ "-" ++ (toTwenty ! singleDigit)
  where
    (tensDigit, singleDigit) = n `divMod` 10
frenchNumber n | n < 100 = 
    case singleDigit of
      0 -> tens ! (2 * twentiesDigit)
      d -> (tens ! (2 * twentiesDigit)) ++ "-" ++ (toTwenty ! singleDigit)
  where
    (twentiesDigit, singleDigit) = n `divMod` 20

main = mapM_ (\n -> putStrLn $ show n ++ "\t" ++ frenchNumber n) [1..99]

