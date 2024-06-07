regret_match = function(x) {
    if (any(x > 0)) {
        return(pmax(x, 0)/sum(pmax(x, 0)))
    } else {
        return(rep(1, length(x))/length(x))
    }
}