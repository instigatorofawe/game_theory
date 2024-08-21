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

expand_strategy = function(strategy, infoset, epsilon=0) {
    strategy[strategy==0] = epsilon
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

map_infosets = function(x, infoset, f) {
    sapply(seq_along(infoset), function(j) {
        f(x[infoset[[j]]])
    })
}


strategy_root = apply(regrets_root %>% t, 1, regret_match) 
strategy_b = apply(regrets_b %>% t, 1, regret_match) 
strategy_x = apply(regrets_x %>% t, 1, regret_match)
strategy_x_b = apply(regrets_x_b %>% t, 1, regret_match) 

average_strategy_root = strategy_root
average_strategy_b = strategy_b
average_strategy_x = strategy_x
average_strategy_x_b = strategy_x_b

colnames(average_strategy_root) = c("J", "Q", "K")
rownames(average_strategy_root) = c("Bet", "Check")

colnames(average_strategy_b) = c("J", "Q", "K")
rownames(average_strategy_b) = c("Call", "Fold")

colnames(average_strategy_x) = c("J", "Q", "K")
rownames(average_strategy_x) = c("Bet", "Check")

colnames(average_strategy_x_b) = c("J", "Q", "K")
rownames(average_strategy_x_b) = c("Call", "Fold")

total_p_root = rep(1/3, 3)
total_p_b = rep(1/6, 3)
total_p_x = rep(1/6, 3)
total_p_x_b = rep(1/12, 3)

epsilon = 0
epsilon_expand = 1e-6

for (i in seq_len(1e4)) {
    expanded_strategy_root = strategy_root %>% expand_strategy(infosets_p1, epsilon_expand)
    expanded_strategy_b = strategy_b %>% expand_strategy(infosets_p2, epsilon_expand)
    expanded_strategy_x = strategy_x %>% expand_strategy(infosets_p2, epsilon_expand)
    expanded_strategy_x_b = strategy_x_b %>% expand_strategy(infosets_p1, epsilon_expand)
    
    # Compute probability of each state, for each history
    p_root = rep(1/6, 6)
    p_b = p_root * expanded_strategy_root[1,]
    p_b_c = p_b * expanded_strategy_b[1,]
    p_b_f = p_b * expanded_strategy_b[2,]
    
    ## From terminal nodes: b
    # Compute EV of each state
    ev_b = expanded_strategy_b[1,] * payouts_b_c + expanded_strategy_b[2,] * payouts_b_f
    # ev_b %<>% replace_na(0)
    
    # Compute EV of each infoset
    p_b_info = map_infosets(p_b, infosets_p2, sum)
    ev_b_info = map_infosets(ev_b * p_b, infosets_p2, sum) / p_b_info
    ev_b_c_info = map_infosets(payouts_b_c * p_b, infosets_p2, sum) / p_b_info
    ev_b_f_info = map_infosets(payouts_b_f * p_b, infosets_p2, sum) / p_b_info
    
    ev_b_info %<>% replace_na(0)
    ev_b_c_info %<>% replace_na(0)
    ev_b_f_info %<>% replace_na(0)
   
    if (i %% 2 == 1) {
        regrets_b = regrets_b - t(cbind(ev_b_c_info - ev_b_info, ev_b_f_info - ev_b_info) * p_b_info) # Weighted instant regret
        regrets_b[regrets_b<0] = 0
        regrets_b = regrets_b * i / (i+1)
        
        # Update strategy
        strategy_b = apply(regrets_b %>% t, 1, function(x) regret_match(x, epsilon)) 
        average_strategy_b = t((t(average_strategy_b) * total_p_b + t(strategy_b) * p_b_info) / (total_p_b + p_b_info))
        total_p_b = total_p_b + p_b_info
        total_p_b = total_p_b * i / (i+1)
    } 
    
    ## xx
    p_x = p_root * expanded_strategy_root[2,]
    p_x_b = p_x * expanded_strategy_x[1,]
    p_x_b_c = p_x_b * expanded_strategy_x_b[1,]
    p_x_b_f = p_x_b * expanded_strategy_x_b[2,]
    
    # Compute EV of each state
    ev_x_b = (p_x_b_c * payouts_b_c + p_x_b_f * -payouts_b_f) / (p_x_b_c + p_x_b_f)
    ev_x_b %<>% replace_na(0)
    
    # Compute EV of each infoset
    p_x_b_info = map_infosets(p_x_b, infosets_p1, sum)
    ev_x_b_info = map_infosets(ev_x_b * p_x_b, infosets_p1, sum) / p_x_b_info
    ev_x_b_c_info = map_infosets(payouts_b_c * p_x_b, infosets_p1, sum) / p_x_b_info
    ev_x_b_f_info = map_infosets(-payouts_b_f * p_x_b, infosets_p1, sum) / p_x_b_info
    
    ev_x_b_info %<>% replace_na(0)
    ev_x_b_c_info %<>% replace_na(0)
    ev_x_b_f_info %<>% replace_na(0)
    
    if (i %% 2 == 0) {
        regrets_x_b = regrets_x_b + t(cbind(ev_x_b_c_info - ev_x_b_info, ev_x_b_f_info - ev_x_b_info) * p_x_b_info)
        regrets_x_b[regrets_x_b<0] = 0
        regrets_x_b = regrets_x_b * i / (i+1)
        
        strategy_x_b = apply(regrets_x_b %>% t, 1, function(x) regret_match(x, epsilon))
        average_strategy_x_b = t((t(average_strategy_x_b) * total_p_x_b + t(strategy_x_b) * p_x_b_info) / (total_p_x_b + p_x_b_info))
        total_p_x_b = total_p_x_b + p_x_b_info
        total_p_x_b = total_p_x_b * i / (i+1)
    }
    
    ## x
    p_x_x = p_x * expanded_strategy_x[2,]
    ev_x = (p_x_x * payouts_x_x + p_x_b * ev_x_b) / (p_x_x + p_x_b)
    ev_x %<>% replace_na(0)
    
    p_x_info = map_infosets(p_x, infosets_p2, sum)
    ev_x_info = map_infosets(ev_x * p_x, infosets_p2, sum) / p_x_info
    ev_x_b_info = map_infosets(ev_x_b * p_x, infosets_p2, sum) / p_x_info
    ev_x_x_info = map_infosets(payouts_x_x * p_x, infosets_p2, sum) / p_x_info
    
    ev_x_info %<>% replace_na(0)
    ev_x_b_info %<>% replace_na(0)
    ev_x_x_info %<>% replace_na(0)
    
    if (i %% 2 == 1) {
        regrets_x = regrets_x - t(cbind(ev_x_b_info - ev_x_info, ev_x_x_info - ev_x_info) * p_x_info)
        regrets_x[regrets_x<0] = 0
        regrets_x = regrets_x * i / (i+1)
        
        strategy_x = apply(regrets_x %>% t, 1, function(x) regret_match(x, epsilon))
        average_strategy_x = t((t(average_strategy_x) * total_p_x + t(strategy_x) * p_x_info) / (total_p_x + p_x_info))
        total_p_x = total_p_x + p_x_info
        total_p_x = total_p_x * i / (i+1)
    }
    
    ## root
    ev_root = (p_b * ev_b + p_x * ev_x) / (p_b + p_x)
    ev_root %<>% replace_na(0)
    
    p_root_info = map_infosets(p_root, infosets_p1, sum)
    ev_root_info = map_infosets(ev_root * p_root, infosets_p1, sum) / p_root_info
    ev_b_info = map_infosets(ev_b * p_root, infosets_p1, sum) / p_root_info
    ev_x_info = map_infosets(ev_x * p_root, infosets_p1, sum) / p_root_info
    
    ev_root_info %<>% replace_na(0)
    ev_b_info %<>% replace_na(0)
    ev_x_info %<>% replace_na(0)
    
    if (i %% 2 == 0) {
        regrets_root = regrets_root + t(cbind(ev_b_info - ev_root_info, ev_x_info - ev_root_info) * p_root_info)
        regrets_root[regrets_root<0] = 0
        regrets_root = regrets_root * i / (i+1)
        
        strategy_root = apply(regrets_root %>% t, 1, function(x) regret_match(x, epsilon))
        average_strategy_root = t((t(average_strategy_root) * total_p_root + t(strategy_root) * p_root_info) / (total_p_root + p_root_info))
        total_p_root = total_p_root + p_root_info
        total_p_root = total_p_root * i / (i+1)
    }
}

print(average_strategy_root)
print(average_strategy_b)
print(average_strategy_x)
print(average_strategy_x_b)