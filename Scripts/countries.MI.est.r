countries.MI.est <- function(cnts,year,t,M,N=rep(10000,length(cnts)),intervals=TRUE,interval.bounds=c(0.05,0.95),trim.means=TRUE,fullout=FALSE){
#t must be of one of three types:
#i) a unique value to apply for all country-imputations
#ii) a vector specifying a unique value to apply for each country for all imputations
#iii) a list specifying M values to apply for each country for each imputation    
out <- list()
year <- year
for(k in 1:length(cnts)){  
print(paste(round(k/length(cnts)*100,digits=3),"% of countries",sep=""))
cont <- cnts[k]
if(length(t)==1){
    out[[k]] <- MI.ests(M=M,N=N[k],contcod=cnts[k],bin_year=year,t=t,intervals=intervals,interval.bounds=interval.bounds,trim.means=trim.means,samples=FALSE)
}
if(length(t)==length(cnts)){
    out[[k]] <- MI.ests(M=M,N=N[k],contcod=cnts[k],bin_year=year,t=t[[k]],intervals=intervals,interval.bounds=interval.bounds,trim.means=trim.means,samples=FALSE)
}
if(length(t)!=1 & length(t)!=length(cnts)){
return("Incorrect dimension for t")
}    
}
if(fullout==TRUE){
return(out)
}
else{    
outs <- cbind.data.frame(cnts,t(sapply(1:length(cnts),function(x){do.call(c,t(out[[x]][2:9]))})))
rm(out)
if(intervals==TRUE){
colnames(outs) <- c("contcod","Gini-MI",paste("Gini-MI-",interval.bounds[1],sep=""),paste("Gini-MI-",.5,sep=""),paste("Gini-MI-",interval.bounds[2],sep=""),"MLD-MI",paste("MLD-MI-",interval.bounds[1],sep=""),paste("MLD-MI-",.5,sep=""),paste("MLD-MI-",interval.bounds[2],sep=""),"TheilT-MI",paste("TheilT-MI-",interval.bounds[1],sep=""),paste("TheilT-MI-",.5,sep=""),paste("TheilT-MI-",interval.bounds[2],sep=""),"Mean-MI",paste("Mean-MI-",interval.bounds[1],sep=""),paste("Mean-MI-",.5,sep=""),paste("Mean-MI-",interval.bounds[2],sep=""),"Bottom 50%-MI",paste("Bottom 50%-MI-",interval.bounds[1],sep=""),paste("Bottom 50%-MI-",.5,sep=""),paste("Bottom 50%-MI-",interval.bounds[2],sep=""),"Middle 40%-MI",paste("Middle 40%-MI-",interval.bounds[1],sep=""),paste("Middle 40%-MI-",.5,sep=""),paste("Middle 40%-MI-",interval.bounds[2],sep=""),"Top 10%-MI",paste("Top 10%-MI-",interval.bounds[1],sep=""),paste("Top 10%-MI-",.5,sep=""),paste("Top 10%-MI-",interval.bounds[2],sep=""),"Top 1%-MI",paste("Top 1%-MI-",interval.bounds[1],sep=""),paste("Top 1%-MI-",.5,sep=""),paste("Top 1%-MI-",interval.bounds[2],sep=""))
}    
if(intervals==FALSE){
colnames(outs) <- c("contcod","Gini-MI","MLD-MI","TheilT-MI","Mean-MI","Bottom 50%-MI","Middle 40%-MI","Top 10%-MI","Top 1%-MI")        
}
return(outs)
}
}
######
#

#cnts <- c("USA","URY","CHN","ETH")
#year=1993
#M=50
#t <- list()
#t[[1]] <- rbeta(M,shape1=10,shape2=10)*(1-0.95)+0.95
#t[[2]] <- rbeta(M,shape1=10,shape2=10)*(1-0.95)+0.95
#t[[3]] <- rbeta(M,shape1=10,shape2=10)*(1-0.95)+0.95

#lolo <- countries.MI.est(cnts=cnts,year=1993,t=0.98,M=50,N=rep(10000,length(cnts)),intervals=TRUE,trim.means=TRUE)
#lolo1=MI.ests(M=500,N=10000,contcod="URY",bin_year=1993,t=0.98,samples=FALSE,intervals=TRUE)
#lolo2 <- countries.MI.est(cnts=cnts,year=2008,t=1,M=100,N=rep(10000,length(cnts)),intervals=TRUE)
#all93t1 <- countries.MI.est(cnts=unique(data$contcod[data$mysample2==1 & data$bin_year==1993]),year=1993,t=1,M=500,intervals=TRUE)
