import System.Environment
import System.Exit
import Data.List.Split
import Data.Maybe
import Numeric

data MarketState = MarketState {
  subsidy :: Double,
  prob0 :: Double,
  weightedP :: Double,
  weighted1mP :: Double,
  totalMarket :: Double
}

data Bet = Bet {
  probability :: Double,
  amount :: Double,
  maxLoss :: Double
}

data ProbRange = ProbRange {
  weightedProb :: Double,
  weighted1mProb :: Double
}

data MarketAndPayout = MarketAndPayout {
  market :: MarketState,
  pay0 :: Double,
  pay1 :: Double,
  updatedBet :: Bet,
  finalPay0 :: Double,
  finalPay1 :: Double
}

listFolder :: [a] -> (a -> b -> b) -> b -> [b]
listFolder [] f b = []
listFolder (h:body) f b =
  f h b : listFolder body f (f h b)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

maxSubsidyMultiplier = 10

scoringBrier :: Double -> Double
scoringBrier p = (1 - p) * (1 - p)

scoringBrierRev :: Double -> Double
scoringBrierRev s = 1 - sqrt s

initMarketState :: Bet -> MarketState
initMarketState bet = updateMarketState (MarketState (amount bet) (probability bet) 0 0 0) bet

initMarketAndPayout :: Bet -> MarketAndPayout
initMarketAndPayout bet = MarketAndPayout (initMarketState bet) 0 0 bet 0 0

updateMarketState :: MarketState -> Bet -> MarketState
updateMarketState ms bet = (MarketState
  (subsidy ms)
  (prob0 ms)
  ((weightedP ms) + (scoringBrier (probability bet)) * (amount bet))
  ((weighted1mP ms) + (scoringBrier (1 - probability bet)) * (amount bet))
  ((totalMarket ms) + (amount bet)))

getProbabilityRange :: MarketState -> ProbRange
getProbabilityRange ms = ProbRange (scoringBrierRev (weightedP ms / totalMarket ms)) (1 - scoringBrierRev (weighted1mP ms / totalMarket ms))

probRange :: MarketAndPayout -> ProbRange
probRange = getProbabilityRange . market

getUpdateRangeFromProbabilityRanges :: ProbRange-> ProbRange -> (Double, Double)
getUpdateRangeFromProbabilityRanges old new = (scoringBrier (weightedProb new) - scoringBrier (weightedProb old), scoringBrier (1 - weighted1mProb new) - scoringBrier (1 - weighted1mProb old))

getBetFromMaxLoss :: MarketState -> Bet -> Double
getBetFromMaxLoss ms bet =
  let pRange = getProbabilityRange ms
  in let probDiff = if' (probability bet < weightedProb pRange) (scoringBrier (1 - weighted1mProb pRange) - scoringBrier (1 - probability bet)) (scoringBrier (weightedProb pRange) - scoringBrier (probability bet))
    in if' ((subsidy ms) * probDiff > maxLoss bet)
      (min ((totalMarket ms) * maxLoss bet / ((subsidy ms) * probDiff - maxLoss bet)) (maxSubsidyMultiplier * subsidy ms))
      (error ((showFloat3Space (probability bet)) "Placing bet with above the allowed maxLoss"))

updateMarketAndPayout ::  Bet -> MarketAndPayout -> MarketAndPayout
updateMarketAndPayout bet marketAndPay =
  let ms = market marketAndPay
  in let newBet = if' (maxLoss bet > 0) (Bet (probability bet) (getBetFromMaxLoss ms bet) (maxLoss bet)) bet
  in let newMS = updateMarketState ms newBet
  in let updateRange = getUpdateRangeFromProbabilityRanges (getProbabilityRange ms) (getProbabilityRange newMS)
  in let newPay0 = (amount bet) + (subsidy ms) * (fst updateRange)
  in let newPay1 = (amount bet) + (subsidy ms) * (snd updateRange)
  in MarketAndPayout newMS newPay0 newPay1 newBet 0 0

resolveToInitialPayout :: [Bet] -> [MarketAndPayout]
resolveToInitialPayout [] = []
resolveToInitialPayout (initialBet:otherBets) = listFolder otherBets updateMarketAndPayout (initMarketAndPayout initialBet)

addFinalPay :: MarketState -> MarketAndPayout-> MarketAndPayout
addFinalPay finalState mkt =
  let ratio = (subsidy finalState) * (amount (updatedBet mkt)) / (totalMarket finalState)
  in MarketAndPayout (market mkt) (pay0 mkt) (pay1 mkt) (updatedBet mkt) (ratio * (1 - scoringBrier (prob0 finalState))) (ratio * (1 - scoringBrier (1 - prob0 finalState)))--(probRange mkt)

resolveAllBetsToFinal :: [Bet] -> [MarketAndPayout]
resolveAllBetsToFinal bets =
  let initialPayouts = resolveToInitialPayout bets
  in map (addFinalPay (market (last initialPayouts))) initialPayouts

parseLine :: Maybe String -> String -> Bet
parseLine params a =
  let splitList = splitOn "," a
  in Bet (read (splitList !! 0)) (read (splitList !! 1)) (if' (isJust params && fromJust params == "maxLoss") (read (splitList !! 1)) 0)

parseInputFile :: Maybe String -> String -> [Bet]
parseInputFile params input = map (parseLine params) (lines input)

showFloat3Space :: Double -> ShowS
showFloat3Space float = showFFloat (Just 3) float . showString ","

outputToString :: MarketAndPayout -> ShowS
outputToString mkt =
  showFloat3Space (weightedProb (probRange mkt)) .
  showFloat3Space (weighted1mProb (probRange mkt)) .
  showFloat3Space (amount (updatedBet mkt)) .
  showFloat3Space (pay0 mkt) .
  showFloat3Space (pay1 mkt) .
  showFloat3Space (finalPay0 mkt) .
  showFloat3Space (finalPay1 mkt) .
  showString "\n"

marketAndPayToString :: MarketAndPayout -> ShowS
marketAndPayToString = outputToString

outputListToString :: [MarketAndPayout] -> String
outputListToString l = "ProbLow, ProbHigh, EffectiveBetAmount, Payout If False, Payout If True, Final Additional Payout if False, Final Additional Payout If True\n" ++ (foldr (.) id (map marketAndPayToString l) "")

inputToOutput :: Maybe String -> String -> String
inputToOutput params inp =  outputListToString (resolveAllBetsToFinal (parseInputFile params inp))

readInputAndWriteOutput :: FilePath -> FilePath -> Maybe String -> IO ()
readInputAndWriteOutput inputFileName outputFileName param = readFile inputFileName >>= ((writeFile outputFileName) . (inputToOutput param))

parse :: [String] -> IO()
parse [] = error "Usage: prediction inputFile outputFile"
parse [x] = error "Usage: prediction inputFile outputFile"
parse [x,y] = readInputAndWriteOutput x y Nothing
parse [x,y,z] = readInputAndWriteOutput x y (Just z)
parse args = mapM_ putStrLn args

main = getArgs >>= parse

--TODO
--check the string for parameters
--Aggregate all numbers in a function
--use CSV for both
