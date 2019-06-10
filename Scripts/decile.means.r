decile.means <- function(x,deciles,sizes=FALSE){
out <- c(mean(x[x<deciles[1]]),
                mean(x[x>=deciles[1] & x<deciles[2]]),
                mean(x[x>=deciles[2] & x<deciles[3]]),
                mean(x[x>=deciles[3] & x<deciles[4]]),
                mean(x[x>=deciles[4] & x<deciles[5]]),
                mean(x[x>=deciles[5] & x<deciles[6]]),
                mean(x[x>=deciles[6] & x<deciles[7]]),
                mean(x[x>=deciles[7] & x<deciles[8]]),
                mean(x[x>=deciles[8] & x<deciles[9]]),
         mean(x[x>=deciles[9]]))
out_sizes <- c(sum(x<deciles[1]),
               sum(x>=deciles[1] & x<deciles[2]),
               sum(x>=deciles[2] & x<deciles[3]),
               sum(x>=deciles[3] & x<deciles[4]),
               sum(x>=deciles[4] & x<deciles[5]),
               sum(x>=deciles[5] & x<deciles[6]),
               sum(x>=deciles[6] & x<deciles[7]),
               sum(x>=deciles[7] & x<deciles[8]),
               sum(x>=deciles[8] & x<deciles[9]),
               sum(x>=deciles[9]))
if(sizes==FALSE){
    return(out)
}
if(sizes==TRUE){
return(list("dmeans"=out,"sizes"=out_sizes))
    }
}


##########

