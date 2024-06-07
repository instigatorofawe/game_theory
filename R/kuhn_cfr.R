rm(list=ls())

library(tidyverse)
library(magrittr)

source("R/regret_match.R")

states = c("KQ", "QK", "KJ", "JK", "QJ", "JQ")
infosets_p1 = lapply(c("J", "Q", "K"), function(x) which(str_sub(states, 1, 1) == x))
infosets_p2 = lapply(c("J", "Q", "K"), function(x) which(str_sub(states, 2, 2) == x))

regrets_root = array(0, c(2, 3)) 
regrets_b = array(0, c(2, 3)) 
regrets_x = array(0, c(2, 3)) # (J, Q, K) x  (b, x)
regrets_x_b = array(0, c(2, 3)) # (J, Q, K) x  (c, f)

payouts_b_c = c(2, -2, 2, -2, 2, -2)
payouts_b_f = c(1, 1, 1, 1, 1, 1)
payouts_x_x = c(1, -1, 1, -1, 1, -1)

expand_strategy = function(strategy, infoset) {
    result = array(dim=c(dim(strategy)[1], length(states)))
    for (j in seq_along(infoset)) {
        result[,infoset[[j]]] = strategy[,j]
    }
    return(result)
}

sum_regrets = function(regrets, infoset) {
    result = array(dim=c(dim(regrets)[1], length(infoset)))
    for (j in seq_along(infoset)) {
        result[,j] = rowSums(regrets[,infoset[[j]]])
    }
    return(result)
}

for (i in seq_len(1e2)) {
    # Compute strategy profiles
    strategy_root = apply(regrets_root %>% t, 1, regret_match) %>% expand_strategy(infosets_p1)
    strategy_b = apply(regrets_b %>% t, 1, regret_match) %>% expand_strategy(infosets_p2)
    strategy_x = apply(regrets_x %>% t, 1, regret_match) %>% expand_strategy(infosets_p2)
    strategy_x_b = apply(regrets_x_b %>% t, 1, regret_match) %>% expand_strategy(infosets_p1)
    
    # Compute probability of being in each node, given initial state
    p_b = strategy_root[1,]
    p_b_c = strategy_root[1,] * strategy_b[1,]
    p_b_f = strategy_root[1,] * strategy_b[2,]
    
    # Compute EV from terminal nodes
    ev_b = p_b_c * payouts_b_c + p_b_f * payouts_b_f
    regrets_b = regrets_b -(rbind(p_b * payouts_b_c - ev_b, p_b * payouts_b_f - ev_b) %>% sum_regrets(infosets_p2))
    
    p_x = strategy_root[2,]
    p_x_b = strategy_root[2,] * strategy_x[1,]
    p_x_b_c = strategy_root[2,] * strategy_x[1,] * strategy_x_b[1,]
    p_x_b_f = strategy_root[2,] * strategy_x[1,] * strategy_x_b[2,]
    
    ev_x_b = p_x_b_c * payouts_b_c + p_x_b_f * payouts_b_f
    regrets_x_b = regrets_x_b + (rbind(p_x_b * payouts_b_c - ev_x_b, p_x_b * payouts_b_f - ev_x_b) %>% sum_regrets(infosets_p1))
    
    p_x_x = strategy_root[2,] * strategy_x[2,]
    ev_x = p_x_b * ev_x_b +  p_x_x * payouts_x_x
    regrets_x = regrets_x -(rbind(p_x * ev_x_b - ev_x, p_x * payouts_x_x - ev_x) %>% sum_regrets(infosets_p2))
    
    ev_root = p_b * ev_b + p_x * ev_x
    regrets_root = regrets_root + (rbind(ev_b - ev_root, ev_x - ev_root) %>% sum_regrets(infosets_p1))
}