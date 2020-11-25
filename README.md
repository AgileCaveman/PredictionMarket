Haskell Implemenation of a novel prediction market mechanism

Usage:

./Prediction input.csv output.csv

or 

./Prediction input.csv output.csv maxLoss

inputBets is a CSV with:
probability,amount 

The first bet is special in that it is a subsidy by the market creator

outputBets outputs a CSV with a header:
ProbLow, ProbHigh, EffectiveBetAmount, Payout If False, Payout If True, Final Additional Payout if False, Final Additional Payout If True

The information is starting from the 2nd bet and going down:
ProbLow, ProbHigh is probability range after the bet
EffectiveBetAmount is either the bet amount or, if maxLoss is present the 
Payout If False is the payout if the event is false
Payout If True is the payout if the event is true
Final Additional Payout if False and Final Additional Payout If True are payouts if the market creator wishes to spend more of the subsidy 

the last param is either nothing or string "maxLoss" 

If maxLoss is present, then the amount of bet is considered the "maxium" the better is allowed to lose and the bet is calculated from that. This will throw an error if the amount is above the allowed maxLoss amount






