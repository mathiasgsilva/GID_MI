ptile.means <- function(x,ptiles){
#x is vector of incomes
#ptiles is a vector of probabilities (from first non-zero to last inferior than 1)
    out <- list()
    qtiles <- quantile(x,probs=ptiles)
out[[1]] <- mean(x[x<qtiles[1]],na.rm=TRUE)
for(p in 2:length(qtiles)){
    out[[p]] <- mean(x[x>=qtiles[p-1] & x<qtiles[p]],na.rm=TRUE)
}
out[[length(qtiles)+1]] <- mean(x[x>=qtiles[length(qtiles)]],na.rm=TRUE)
out <- unlist(out)
return(out)
}


#x=rgb2(n=10000,shape1=1.5,scale=100,shape2=1,shape3=1.5)
#ptiles=seq(0.1,0.9,0.1)
#ptile.means(x=x,ptiles=ptiles)    
    

###########    
