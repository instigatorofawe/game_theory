source("R/regret_match.R")

payouts_p1 = array(c(0,2,7,6), c(2,2))
payouts_p2 = array(c(0,7,2,6), c(2,2))

regrets_p1 = c(0, 0)
regrets_p2 = c(0, 0)

for (i in seq_len(100)) {
    strategy_p1 = regret_match(regrets_p1)
    strategy_p2 = regret_match(regrets_p2)
    
    evs_p1 = sapply(seq_len(dim(payouts_p1)[1]), function(x) sum(payouts_p1[x,] * strategy_p2))
    evs_p2 = sapply(seq_len(dim(payouts_p2)[2]), function(x) sum(payouts_p2[,x] * strategy_p1))
    
    overall_ev_p1 = sum(strategy_p1 * evs_p1)
    overall_ev_p2 = sum(strategy_p2 * evs_p2)
    
    regrets_p1 = regrets_p1 + evs_p1 - overall_ev_p1
    regrets_p2 = regrets_p2 + evs_p2 - overall_ev_p2
}