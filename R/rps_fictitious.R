library(magrittr)

payouts = array(c(0.5, 2, 0, 0, 0.5, 1, 1, 0, 0.5), dim = c(3,3))
colnames(payouts) = c("Rock", "Paper", "Scissors")
rownames(payouts) = c("Rock", "Paper", "Scissors")

strategies = array(1, dim=c(2, 3))
colnames(strategies) = c("Rock", "Paper", "Scissors")
rownames(strategies) = c("Player 1", "Player 2")

for (c in seq_len(1e4)) {

    evs_p1 = sapply(seq_len(3), function(x) sum(payouts[x,] * strategies[2,]/sum(strategies[2,])))
    evs_p2 = sapply(seq_len(3), function(x) sum(payouts[x,] * strategies[1,]/sum(strategies[1,])))
    strategies[1,which.max(evs_p1)] = strategies[1,which.max(evs_p1)] + 1
    strategies[2,which.max(evs_p2)] = strategies[2,which.max(evs_p2)] + 1
    
}

print(strategies/rowSums(strategies))