regret_match = function(x, epsilon=0) {
    if (any(x > 0)) {
        return(pmax(x, epsilon)/sum(pmax(x, epsilon)))
    } else {
        return(rep(1, length(x))/length(x))
    }
}