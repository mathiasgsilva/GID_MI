GID.MI.est <- function(cnts,year,t,M,N=rep(10000,length(cnts)),seed=12345,ptiles=seq(0.1,0.9,0.1),gid_intervals=FALSE,gid_samples=FALSE,gid_samplesonly=FALSE){
#t must be of one of three types:
#i) a unique value to apply for all country-imputations
#ii) a vector specifying a unique value to apply for each country for all imputations
#iii) a list specifying M values to apply for each country for each imputation
if(gid_samplesonly==TRUE){
gid_samples=TRUE
}        
set.seed(seed)
out_cty <- list()
out_gid <- list()
#year <- year
for(k in 1:length(cnts)){  
print(paste(round(k/length(cnts)*100,digits=3),"% of countries",sep=""))
#cont <- cnts[k]
if(length(t)==1){
    cty_sample <- do.call(cbind,MI.ests(M=M,N=N[k],contcod=cnts[k],bin_year=year,t=t,intervals=FALSE,samplesonly=TRUE))
cty_sample$contcod=cnts[k]
if(TRUE%in%is.na(cty_sample)){
out_cty[[k]] <- rep(NA,N[k])
}
else{
out_cty[[k]] <- do.call(cbind,cty_sample)
}    
}
if(length(t)==length(cnts)){
cty_sample <- do.call(cbind,MI.ests(M=M,N=N[k],contcod=cnts[k],bin_year=year,t=t[[k]],intervals=FALSE,samplesonly=TRUE))
if(TRUE%in%is.na(cty_sample)){
cty_sample <- as.data.frame(matrix(NA,nrow=N[k],ncol=M))
cty_sample$contcod=cnts[k]
out_cty[[k]] <- do.call(cbind,cty_sample)
}
else{
cty_sample$contcod=cnts[k]    
out_cty[[k]] <- do.call(cbind,cty_sample)
}    
}
if(length(t)!=1 & length(t)!=length(cnts)){
  return("Incorrect dimension for t")
}
}
out_gid[[1]] <- do.call(rbind,out_cty)
if(gid_samplesonly==TRUE){
return(out_gid[[1]])
}
if(gid_samplesonly==FALSE){
outs <- list("MI.Gini"=rep(0,M),"MI-MLD"=rep(0,M),"MI.Mean"=rep(0,M),"MI.b50.s"=rep(0,M),"MI.m40.s"=rep(0,M),"MI.t10.s"=rep(0,M),"MI.t1.s"=rep(0,M),"MI.pmeans"=list())
for(z in 1:M){
print(paste(round((z/M)*100,digits=3),"% of MI estimates (GID)",sep=""))
outs[[1]][z] <- Gini(as.numeric(out_gid[[1]][,z]))
outs[[2]][z] <- entropy(as.numeric(out_gid[[1]][,z]),parameter=0)    
outs[[3]][z] <- mean(as.numeric(out_gid[[1]][,z]),na.rm=TRUE)
outs[[4]][z] <- sum(as.numeric(out_gid[[1]][,z])[as.numeric(out_gid[[1]][,z])<quantile(as.numeric(out_gid[[1]][,z]),0.5,na.rm=TRUE)],na.rm=TRUE)/sum(as.numeric(out_gid[[1]][,z]),na.rm=TRUE)
outs[[5]][z] <- sum(as.numeric(out_gid[[1]][,z])[as.numeric(out_gid[[1]][,z])<quantile(as.numeric(out_gid[[1]][,z]),0.7,na.rm=TRUE) & as.numeric(out_gid[[1]][,z])>=quantile(as.numeric(out_gid[[1]][,z]),0.3,na.rm=TRUE)],na.rm=TRUE)/sum(as.numeric(out_gid[[1]][,z]),na.rm=TRUE)
outs[[6]][z] <- sum(as.numeric(out_gid[[1]][,z])[as.numeric(out_gid[[1]][,z])>=quantile(as.numeric(out_gid[[1]][,z]),0.9,na.rm=TRUE)],na.rm=TRUE)/sum(as.numeric(out_gid[[1]][,z]),na.rm=TRUE)
outs[[7]][z] <- sum(as.numeric(out_gid[[1]][,z])[as.numeric(out_gid[[1]][,z])>=quantile(as.numeric(out_gid[[1]][,z]),0.99,na.rm=TRUE)],na.rm=TRUE)/sum(as.numeric(out_gid[[1]][,z]),na.rm=TRUE)
outs[[8]][[z]] <- ptile.means(as.numeric(out_gid[[1]][,z]),ptiles=ptiles)
}
outs.pmeans <- apply(do.call(rbind,outs[[8]]),2,mean,na.rm=TRUE)
names(outs.pmeans) <- c(paste("MI.dmeans.<",min(ptiles)*100,"%",sep=""),paste("MI.dmeans.",ptiles[2:length(ptiles)]*100,"%",sep=""),paste("MI.dmeans.>",max(ptiles)*100,"%",sep=""))
if(gid_intervals==TRUE){
int.outs.pmeans <- apply(do.call(rbind,outs[[8]]),2,quantile,probs=c(.05,.95),na.rm=TRUE)
outs <- list(unlist(apply(do.call(cbind,outs[1:7]),2,mean,na.rm=TRUE)),unlist(apply(do.call(cbind,outs[1:7]),2,quantile,probs=c(.05,.95),na.rm=TRUE)),outs.pmeans,int.outs.pmeans)
colnames(outs[[4]]) <- names(outs.pmeans)
}
if(gid_intervals==FALSE){
outs <- list(unlist(apply(do.call(cbind,outs[1:7]),2,mean,na.rm=TRUE)),outs.pmeans)
}
if(gid_samples==TRUE){
return(list("samples"=out_gid[[1]],"MI estimates"=outs))
}
if(gid_samples==FALSE){
return(list("MI estimates"=outs))
}
}
}


##############

#test1 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.98,M=100,N=N.CHN08,gid_samplesonly=TRUE)
#test2 <- GID.MI.est(cnts=c("CHN","URY","USA"),year=2008,t=1,M=500,gid_samples=TRUE)
#test3 <- GID.MI.est(cnts=c("CHN","URY","USA"),year=2008,t=1,M=500,gid_samplesonly=TRUE)
#test4 <- GID.MI.est(cnts=c("CHN","URY","USA"),year=2008,t=1,M=500,gid_samples=TRUE,gid_intervals=TRUE)

##################
##OLD CODE########
##################

#GID.MI.est <- function(cnts,year,t,M,N=rep(10000,length(cnts)),seed=12345,ptiles=seq(0.1,0.9,0.1)){
#  set.seed(seed)
#  out_cty <- list()
#  out_gid <- list()
#  year <<- year
#  for(k in 1:length(cnts)){  
#    print(paste(round(k/length(cnts)*100,digits=3),"% of countries",sep=""))
#    cont <<- cnts[k]
#    out_cty[[k]] <- MI.ests(M=M,N=N[k],contcod=cnts[k],bin_year=year,t=t,samples=TRUE)$samples
#  }
#  out_gid[[1]] <- do.call(rbind,out_cty)
#  rm(out_cty)
#  gc()
#  outs <- list("MI.Gini"=rep(0,M),"MI-MLD"=rep(0,M),"MI.Mean"=rep(0,M),"MI.b50.s"=rep(0,M),"MI.m40.s"=rep(0,M),"MI.t10.s"=rep(0,M),"MI.t1.s"=rep(0,M),"MI.pmeans"=list())
#  #"MI.10.dmean"=rep(0,M),"MI.20.dmean"=rep(0,M),"MI.30.dmean"=rep(0,M),"MI.40.dmean"=rep(0,M),"MI.50.dmean"=rep(0,M),"MI.60.dmean"=rep(0,M),"MI.70.dmean"=rep(0,M),"MI.80.dmean"=rep(0,M),"MI.90.dmean"=rep(0,M),"MI.100.dmean"=rep(0,M))
#  for(z in 1:M){
#    print(paste(round((z/M)*100,digits=3),"% of imputations (GID)",sep=""))
#    outs[[1]][z] <- Gini(out_gid[[1]][,z])
#    outs[[2]][z] <- entropy(out_gid[[1]][,z],parameter=0)    
#    outs[[3]][z] <- mean(out_gid[[1]][,z][out_gid[[1]][,z]<Inf],na.rm=TRUE)
#    outs[[4]][z] <- sum(out_gid[[1]][,z][out_gid[[1]][,z]<quantile(out_gid[[1]][,z],0.5,na.rm=TRUE)],na.rm=TRUE)/sum(out_gid[[1]][,z],na.rm=TRUE)
#    outs[[5]][z] <- sum(out_gid[[1]][,z][out_gid[[1]][,z]<quantile(out_gid[[1]][,z],0.7,na.rm=TRUE) & out_gid[[1]][,z]>=quantile(out_gid[[1]][,z],0.3,na.rm=TRUE)],na.rm=TRUE)/sum(out_gid[[1]][,z],na.rm=TRUE)
#    outs[[6]][z] <- sum(out_gid[[1]][,z][out_gid[[1]][,z]>=quantile(out_gid[[1]][,z],0.9,na.rm=TRUE)],na.rm=TRUE)/sum(out_gid[[1]][,z],na.rm=TRUE)
#    outs[[7]][z] <- sum(out_gid[[1]][,z][out_gid[[1]][,z]>=quantile(out_gid[[1]][,z],0.99,na.rm=TRUE)],na.rm=TRUE)/sum(out_gid[[1]][,z],na.rm=TRUE)
#    outs[[8]][[z]] <- ptile.means(out_gid[[1]][,z],ptiles=ptiles)
#    
#    #    dmeans <- decile.means(out_gid[[1]][,z],deciles=quantile(out_gid[[1]][,z],probs=seq(0.1,0.9,0.1),na.rm=TRUE)) 
#    #    dmeans <- decile.means(out_gid[[1]][,z],deciles=quantile(out_gid[[1]][,z],probs=seq(0.1,0.9,0.1),na.rm=TRUE)) 
#    #    outs[[8]][z] <- dmeans[1]
#    #    outs[[9]][z] <- dmeans[2]
#    #    outs[[10]][z] <- dmeans[3]
#    #    outs[[11]][z] <- dmeans[4]
#    #    outs[[12]][z] <- dmeans[5]
#    #    outs[[13]][z] <- dmeans[6]
#    #   outs[[14]][z] <- dmeans[7]
#    #    outs[[15]][z] <- dmeans[8]
#    #    outs[[16]][z] <- dmeans[9]
#    #    outs[[17]][z] <- dmeans[10]
#  }
#  rm(out_gid)
#  gc()
#  outs.pmeans <- apply(do.call(rbind,outs[[8]]),2,mean,na.rm=TRUE)
#  names(outs.pmeans) <- c(paste("MI.dmeans.<",min(ptiles)*100,"%",sep=""),paste("MI.dmeans.",ptiles[2:length(ptiles)]*100,"%",sep=""),paste("MI.dmeans.>",max(ptiles)*100,"%",sep=""))
#  outs <- list(unlist(apply(do.call(cbind,outs[1:7]),2,mean,na.rm=TRUE)),outs.pmeans)
#  #colnames(outs) <- c("Gini-MI","Mean-MI","Bottom 50%-MI","Middle 40%-MI","Top 10%-MI","Top 1%-MI")
#  return(outs)
#}
