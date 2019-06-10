GID.MI.stat <- function(file,M,FUN,MIsd=FALSE,R=100,bootfunc=NULL){
require(data.table)
require(boot)
stat <- rep(0,M)
if(MIsd==TRUE){
sd <- rep(0,M)
    }
for(m in 1:M){
print(paste((m/M)*100,"% of samples",sep=""))    
sample <- fread(file,select=m,data.table=FALSE)
stat[m] <- as.numeric(sapply(sample,FUN=FUN))
if(MIsd==TRUE){
    sd[m] <- sd(boot(sample[,1],bootfunc,R)$t)
}
}
if(MIsd==TRUE){
return(list(cbind(stat,sd),mean(stat,na.rm=TRUE),sqrt(mean(sd**2,na.rm=TRUE)+(1+(1/M))*var(stat,na.rm=TRUE))))
}
else{
return(list(stat,mean(stat)))
}    
}

#####

#stat1 <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=Gini,MIsd=TRUE,bootfunc=function(x,i){Gini(x[i])})
#stat2 <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){entropy(x,parameter=0)},MIsd=TRUE,bootfunc=function(x,i){entropy(x[i],parameter=0)})
#stat3 <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){entropy(x,parameter=1)},MIsd=TRUE,bootfunc=function(x,i){entropy(x[i],parameter=1)})
#stat4 <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=mean,MIsd=TRUE,bootfunc=function(x,i){mean(x[i])})
#stat5 <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){sum(x[x>=quantile(x,probs=0.9)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.9)])/sum(x[i])})
#stat6 <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){sum(x[x>=quantile(x,probs=0.99)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.99)])/sum(x[i])})
#stat7 <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){sum(x[x>=quantile(x,probs=0.4) & x<=quantile(x,probs=0.8)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.4) & x[i]<=quantile(x[i],probs=0.8)])/sum(x[i])})
                                        #stat8 <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){sum(x[x<=quantile(x,probs=0.5)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]<=quantile(x[i],probs=0.5)])/sum(x[i])})
#ptiles <- seq(0.1,0.9,0.1)
#qtiles <- list()
#for(s in 1:length(ptiles)){
#qtile[[s]] <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){quantile(x,probs=ptiles[s])},MIsd=TRUE,bootfunc=function(x,i){quantile(x[i],probs=ptiles[s])})
#    }
#gid.ptile.means <- list()
#gid.ptile.means[[1]] <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){mean(x[x<=qtiles[[1]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]<=qtiles[[1]][[2]]],na.rm=TRUE)})
#for(p in 2:length(qtiles)){
#gid.ptile.means[[p]] <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){mean(x[x>=qtiles[[p-1]][[2]] & x<qtiles[[p]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]>=qtiles[[p-1]][[2]] & x[i]<qtiles[[p]][[2]]],na.rm=TRUE)})
#}
#gid.ptile.means[[length(qtiles)+1]] <- GID.stat(file=paste(output_data_dir,"GID08.t98.txt",sep=""),M=10,FUN=function(x){mean(x[x>=qtiles[[length(qtiles)]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]>=qtiles[[length(qtiles)]][[2]]],na.rm=TRUE)})

