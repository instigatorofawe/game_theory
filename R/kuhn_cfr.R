library(magrittr)

source("R/regret_match.R")

regrets_root = array(0, c(3, 2)) # (J, Q, K) x  (b, x)
regrets_b = array(0, c(3, 2)) # (J, Q, K) x  (c, f)
regrets_x = array(0, c(3, 2)) # (J, Q, K) x  (b, x)
regrets_x_b = array(0, c(3, 2)) # (J, Q, K) x  (c, f)

payout_b_c = array(c(0, 2, 2, -2, 0, 2, -2, -2, 0), c(3,3))
payout_b_f = array(-1, c(3,3))
payout_x_x = array(c(0, 1, 1, -1, 0, 1, -1, -1, 0), c(3,3))

for (i in seq_len(1e3)) {
    # Compute strategy profiles
    strategy_root = apply(regrets_root, 1, regret_match) %>% t
    strategy_b = apply(regrets_b, 1, regret_match) %>% t
    strategy_x = apply(regrets_x, 1, regret_match) %>% t
    strategy_x_b = apply(regrets_x_b, 1, regret_match) %>% t
    
    # Compute beliefs for each player for each information set at each node
    beliefs_root_p1 = array(c(0,0.5,0.5,0.5,0,0.5,0.5,0.5,0), c(3,3))
    beliefs_root_p2 = beliefs_root_p1
    
    beliefs_b_p1 = apply(beliefs_root_p1, 1, function(x) (x*strategy_root[,1])/sum(x*strategy_root[,1])) %>% t
    beliefs_b_p1[is.na(beliefs_b_p1)] = 0
    beliefs_b_p2 = beliefs_root_p2
    
    beliefs_b_c_p1 = beliefs_b_p1
    beliefs_b_c_p2 = apply(beliefs_b_p2, 1, function(x) (x*strategy_b[,1])/sum(x*strategy_b[,1])) %>% t
    beliefs_b_c_p2[is.na(beliefs_b_c_p2)] = 0
    
    beliefs_b_f_p1 = beliefs_b_p1
    beliefs_b_f_p2 = apply(beliefs_b_p2, 1, function(x) (x*strategy_b[,2])/sum(x*strategy_b[,2])) %>% t
    beliefs_b_f_p2[is.na(beliefs_b_f_p2)] = 0
    
    beliefs_x_p1 = apply(beliefs_root_p1, 1, function(x) (x*strategy_root[,2])/sum(x*strategy_root[,2])) %>% t
    beliefs_x_p1[is.na(beliefs_x_p1)] = 0
    beliefs_x_p2 = beliefs_root_p2
    
    beliefs_x_x_p1 = beliefs_x_p1
    beliefs_x_x_p2 = apply(beliefs_x_p2, 1, function(x) (x*strategy_x[,2])/sum(x*strategy_x[,2])) %>% t
    beliefs_x_x_p2[is.na(beliefs_x_x_p2)] = 0
    
    beliefs_x_b_p1 = beliefs_x_p1
    beliefs_x_b_p2 = apply(beliefs_x_p2, 1, function(x) (x*strategy_x[,1])/sum(x*strategy_x[,1])) %>% t
    beliefs_x_b_p2[is.na(beliefs_x_b_p2)] = 0
    
    beliefs_x_b_c_p1 = apply(beliefs_x_b_p1, 1, function(x) (x * strategy_x_b[,1]) / sum(x*strategy_x_b[,1])) %>% t
    beliefs_x_b_c_p1[is.na(beliefs_x_b_c_p1)] = 0
    beliefs_x_b_c_p2 = beliefs_x_b_p2
    
    beliefs_x_b_f_p1 = apply(beliefs_x_b_p1, 1, function(x) (x * strategy_x_b[,2]) / sum(x*strategy_x_b[,2])) %>% t
    beliefs_x_b_f_p1[is.na(beliefs_x_b_f_p1)] = 0
    beliefs_x_b_f_p2 = beliefs_x_b_p2
    
    # Compute EV for each player for each information set at each node, beginning with terminal nodes
    ev_x_b_c_p1 = rowSums(payout_b_c * beliefs_x_b_c_p2)
    ev_x_b_c_p2 = rowSums(payout_b_c * beliefs_x_b_c_p1)
    
    ev_x_b_f_p1 = rowSums(payout_b_f * beliefs_x_b_p2)
    ev_x_b_f_p2 = rowSums(-payout_b_f * beliefs_x_b_p1)
    
    ev_x_b_p1 = rowSums(cbind(ev_x_b_c_p1, ev_x_b_f_p1) * strategy_x_b)
    ev_x_b_p2 = rowSums(cbind(ev_x_b_c_p2, ev_x_b_f_p2) * strategy_x_b)
    
    regrets_x_b = regrets_x_b - ev_x_b_p1 + cbind(ev_x_b_c_p1, ev_x_b_f_p1)
    regrets_x_b[regrets_x_b<0] = 0 
    
    ev_x_x_p1 = rowSums(payout_x_x * beliefs_x_x_p2)
    ev_x_x_p2 = rowSums(payout_x_x * beliefs_x_x_p1)
    
    ev_x_p1 = rowSums(cbind(ev_x_b_p1, ev_x_x_p1) * strategy_x)
    ev_x_p2 = rowSums(cbind(ev_x_b_p2, ev_x_x_p2) * strategy_x)
    
    regrets_x = regrets_x - ev_x_p2 + cbind(ev_x_b_p2, ev_x_x_p2)
    regrets_x[regrets_x<0] = 0
    
    ev_b_c_p1 = rowSums(payout_b_c * beliefs_b_c_p2)
    ev_b_c_p2 = rowSums(payout_b_c * beliefs_b_c_p1)
    
    ev_b_f_p1 = rowSums(-payout_b_f * beliefs_b_p2)
    ev_b_f_p2 = rowSums(payout_b_f * beliefs_b_p1)
    
    ev_b_p1 = rowSums(cbind(ev_b_c_p1, ev_b_f_p1) * strategy_b)
    ev_b_p2 = rowSums(cbind(ev_b_c_p2, ev_b_f_p2) * strategy_b)
    
    regrets_b = regrets_b - ev_b_p2 + cbind(ev_b_c_p2, ev_b_f_p2)
    regrets_b[regrets_b<0] = 0
    
    ev_root_p1 = rowSums(cbind(ev_b_p1, ev_x_p1) * strategy_root)
    ev_root_p2 = rowSums(cbind(ev_b_p2, ev_x_p2) * strategy_root)
    
    regrets_root = regrets_root - ev_root_p1 + cbind(ev_b_p1, ev_x_p1)
    regrets_root[regrets_root<0] = 0
}