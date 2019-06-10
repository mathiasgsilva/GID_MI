qmeans.cummeans <- function(x,sizes){
#x is a vector of decile (non-cumulative) means
#sizes is a vector of the sample size of each decile    
out <- cumsum(x*sizes)/sum(sizes)
return(out)    
    }
