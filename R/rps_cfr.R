library(magrittr)

source("R/regret_match.R")

regrets = array(0, dim = c(2, 3)) # (player_1, player_2) x (rock, paper, scissors)
payouts = array(c(0.5, 1.1, 0, 0, 0.5, 1, 1, 0, 0.5), dim = c(3,3))
colnames(payouts) = c("Rock", "Paper", "Scissors")
rownames(payouts) = c("Rock", "Paper", "Scissors")

strategies = apply(regrets, 1, regret_match) %>% t
average_strategy = strategies

for (c in seq_len(1e4)) {
    evs_p1 = sapply(seq_len(3), function(x) sum(payouts[x,] * strategies[2,]))
    ev_p1 = sum(strategies[1,] * evs_p1)
    regrets[1,] = regrets[1,] + evs_p1 - ev_p1
    evs_p2 = sapply(seq_len(3), function(x) sum(payouts[x,] * strategies[1,]))
    ev_p2 = sum(strategies[2,] * evs_p2)
    regrets[2,] = regrets[2,] + evs_p2 - ev_p2
    
    strategies = apply(regrets, 1, regret_match) %>% t
    average_strategy = (average_strategy * c + strategies)/(c+1)
}

colnames(average_strategy) = c("Rock", "Paper", "Scissors")
rownames(average_strategy) = c("Player 1", "Player 2")

print(average_strategy)