source("R/regret_match.R")
# Infosets are K, Q, J
root_regrets = array(0, dim=c(2, 3))
root_strategies = array(1/2, dim=c(2, 3))
root_avg_strategies = array(1/2, dim=c(2, 3))
root_iter_count = rep(1, 3)

b_regrets = array(0, dim=c(2, 3))
b_strategies = array(1/2, dim=c(2, 3))
b_avg_strategies = array(1/2, dim=c(2, 3))
b_iter_count = rep(1, 3)

x_regrets = array(0, dim=c(2, 3))
x_strategies = array(1/2, dim=c(2, 3))
x_avg_strategies = array(1/2, dim=c(2, 3))
x_iter_count = rep(1, 3)

x_b_regrets = array(0, dim=c(2, 3))
x_b_strategies = array(1/2, dim=c(2, 3))
x_b_avg_strategies = array(1/2, dim=c(2, 3))
x_b_iter_count = rep(1, 3)

set.seed(123456789)

b_f_payout = 1
x_b_f_payout = -1

for (iteration in seq_len(100000)) {
    # Generate external sample for OOP
    cards = sample(seq_len(3), 2, replace=F)
    
    # Compute value of terminal nodes
    x_x_payout = (cards[1] < cards[2]) * 2 - 1
    b_c_payout = (cards[1] < cards[2]) * 4 - 2
    x_b_c_payout = (cards[1] < cards[2]) * 4 - 2
    
    # Compute IP actions
    x_action = 2 - (runif(1) < x_strategies[1,cards[2]]) * 1
    b_action = 2 - (runif(1) < b_strategies[1,cards[2]]) * 1
    
    # Compute EV of action nodes leading to terminal nodes
    # Update OOP strategy
    x_b_ev = x_b_strategies[1,cards[1]] * x_b_c_payout + x_b_strategies[2,cards[1]] * x_b_f_payout
    
    b_ev = c(b_c_payout, b_f_payout)[b_action]
    x_ev = c(x_b_ev, x_x_payout)[x_action]
    
    root_ev = root_strategies[1,cards[1]] * b_ev + root_strategies[2,cards[1]] * x_ev
    
    root_regrets[,cards[1]] = root_regrets[,cards[1]] + c(b_ev - root_ev, x_ev - root_ev)
    root_strategies[,cards[1]] = regret_match(root_regrets[,cards[1]])
    root_avg_strategies[,cards[1]] = (root_avg_strategies[,cards[1]] * root_iter_count[cards[1]] + root_strategies[,cards[1]]) / (root_iter_count[cards[1]] + 1)
    root_iter_count[cards[1]] = root_iter_count[cards[1]] + 1
    
    if (x_action == 1) {
        # If reachable, update node strategy
        x_b_regrets[,cards[1]] = x_b_regrets[,cards[1]] + c(x_b_c_payout - x_b_ev, x_b_f_payout - x_b_ev)
        x_b_strategies[,cards[1]] = regret_match(x_b_regrets[,cards[1]])
        x_b_avg_strategies[,cards[1]] = (x_b_avg_strategies[,cards[1]] * x_b_iter_count[cards[1]] + x_b_strategies[,cards[1]]) / (x_b_iter_count[cards[1]] + 1)
        x_b_iter_count[cards[1]] = x_b_iter_count[cards[1]] + 1
    }
    
    
    # Generate external sample for IP
    cards = sample(seq_len(3), 2, replace=F)
    
    x_x_payout = (cards[1] < cards[2]) * 2 - 1
    b_c_payout = (cards[1] < cards[2]) * 4 - 2
    x_b_c_payout = (cards[1] < cards[2]) * 4 - 2
    
    # Compute OOP actions
    root_action = 2 - (runif(1) < root_strategies[1,cards[1]]) * 1
    if (root_action == 2) {
        # If reachable, update node strategy
        x_b_action = 2 - (runif(1) < x_b_strategies[1,cards[1]]) * 1
        x_b_ev = c(x_b_c_payout, x_b_f_payout)[x_b_action]
        
        x_ev = x_strategies[1,cards[2]] * x_b_ev + x_strategies[2,cards[2]] * x_x_payout
        x_regrets[,cards[2]] = x_regrets[,cards[2]] - c(x_b_ev - x_ev, x_x_payout - x_ev)
        x_strategies[,cards[2]] = regret_match(x_regrets[,cards[2]])
        x_avg_strategies[,cards[2]] = (x_avg_strategies[,cards[2]] * x_iter_count[cards[2]] + x_strategies[,cards[2]]) / (x_iter_count[cards[2]] + 1)
        x_iter_count[cards[2]] = x_iter_count[cards[2]] + 1
    } else {
        b_ev = b_strategies[1,cards[2]] * b_c_payout + b_strategies[2,cards[2]] * b_f_payout
        b_regrets[,cards[2]] = b_regrets[,cards[2]] - c(b_c_payout - b_ev, b_f_payout - b_ev)
        b_strategies[,cards[2]] = regret_match(b_regrets[,cards[2]])
        b_avg_strategies[,cards[2]] = (b_avg_strategies[,cards[2]] * b_iter_count[cards[2]] + b_strategies[,cards[2]]) / (b_iter_count[cards[2]] + 1)
        b_iter_count[cards[2]] = b_iter_count[cards[2]] + 1
    }
    
    # Update IP strategy
    
}