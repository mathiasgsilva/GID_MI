iqi <- function(contcod,bin_year){
    out <- list()
for(i in 1:10){    
    out[[i]] <- rep(data$RRinc[data$contcod==contcod & data$bin_year==bin_year][i],data$pop[data$contcod==contcod & data$bin_year==bin_year][i]*10000)
    }
return(do.call(c,out))
    }
