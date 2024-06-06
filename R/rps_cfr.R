library(magrittr)

regrets = array(0, dim = c(2, 3)) # (player_1, player_2) x (rock, paper, scissors)
payouts = array(c(0.5, 2, 0, 0, 0.5, 1, 1, 0, 0.5), dim = c(3,3))
colnames(payouts) = c("Rock", "Paper", "Scissors")
rownames(payouts) = c("Rock", "Paper", "Scissors")

for (c in seq_len(1e3)) {
    strategies = apply(regrets, 1, function(x) {
        if (any(x > 0)) {
            return(pmax(x, 0)/sum(pmax(x, 0)))
        } else {
            return(rep(1, length(x))/length(x))
        }
    }) %>% t
    
    evs_p1 = sapply(seq_len(3), function(x) sum(payouts[x,] * strategies[2,]))
    ev_p1 = sum(strategies[1,] * evs_p1)
    regrets[1,] = regrets[1,] + evs_p1 - ev_p1
    evs_p2 = sapply(seq_len(3), function(x) sum(payouts[x,] * strategies[1,]))
    ev_p2 = sum(strategies[2,] * evs_p2)
    regrets[2,] = regrets[2,] + evs_p2 - ev_p2
}

colnames(strategies) = c("Rock", "Paper", "Scissors")
rownames(strategies) = c("Player 1", "Player 2")

print(strategies)