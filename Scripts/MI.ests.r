######
MI.ests <- function(M,N,contcod,bin_year,t,samples=FALSE,samplesonly=FALSE,intervals=FALSE,interval.bounds=c(0.05,0.95),trim.means=TRUE){
if(samplesonly==TRUE){
samples=samplesonly  
}
  if(length(t)==1){    
    pars <- country.est(contcod=contcod,bin_year=bin_year,t=t)$Parameters
}
if(length(t)==M){
    pars <- list()
    pars <- cbind(t,t(sapply(t,function(x){country.est(contcod=contcod,bin_year=bin_year,t=x)$Parameters})))
}
if(length(t)!=M & length(t)!=1){
return("Incorrect dimension for t")
    }
if(samples==TRUE){
    samples_out <- data.frame(rep(0,N))
}
if(samplesonly==FALSE){  
MI.Gini <- rep(0,M)
MI.MLD <- rep(0,M)
MI.TheilT <- rep(0,M)
MI.Mean <- rep(0,M)
MI.t10.s <- rep(0,M)
MI.t1.s <- rep(0,M)
MI.b50.s <-rep(0,M)
MI.m40.s <- rep(0,M)
}
set.seed(12345)
for(i in 1:M){
print(paste((i/M)*100,"% of imputations",sep=""))
sample <- rep(0,N)
if(length(t)==1){    
    sample <- rgb2(n=N,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4])    
}
if(length(t)==M){
sample <- rgb2(n=N,shape1=pars[i,2],scale=pars[i,3],shape2=pars[i,4],shape3=pars[i,5])
}
if(Inf%in%sample){
it.inf <- 1
while(Inf%in%sample){
it.inf <- it.inf+1
if(length(t)==1){    
sample[which(sample==Inf)] <- rgb2(n=length(which(sample==Inf)),shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4])    
}
if(length(t)==M){
sample[which(sample==Inf)] <- rgb2(n=length(which(sample==Inf)),shape1=pars[i,2],scale=pars[i,3],shape2=pars[i,4],shape3=pars[i,5])
}
if(it.inf>=5000){
print("Numerically infinite income problem after 5000 attempts")
if(intervals==FALSE){
return(as.list(rep(NA,9)))    
}
if(intervals==TRUE){
return(split(matrix(NA,nrow=9,ncol=3), 1:9))    
    }
}
}
}
if(samples==TRUE){
samples_out[,i] <- sample
}
if(samplesonly==FALSE){
MI.Gini[i] <- Gini(sample)
MI.MLD[i] <- entropy(sample,parameter=0)
MI.TheilT[i] <- entropy(sample,parameter=1)    
MI.Mean[i] <- mean(sample)
MI.t10.s[i] <- sum(sample[sample>=quantile(sample,probs=0.9)])/sum(sample)
MI.t1.s[i] <- sum(sample[sample>=quantile(sample,probs=0.99)])/sum(sample)
MI.b50.s[i] <- sum(sample[sample<quantile(sample,probs=0.5)])/sum(sample)
MI.m40.s[i] <- sum(sample[sample>=quantile(sample,probs=0.3) & sample<=quantile(sample,probs=0.7)])/sum(sample)    
}
}
if(samplesonly==TRUE){
return(list("samples"=samples_out))  
}
if(samplesonly==FALSE){
if(samples==TRUE){
if(intervals==FALSE){    
    return(list("samples"=samples_out,pars,mean(MI.Gini,na.rm=T),mean(MI.MLD,na.rm=T),mean(MI.TheilT,na.rm=T),mean(MI.Mean,na.rm=T),mean(MI.b50.s,na.rm=T),mean(MI.m40.s,na.rm=T),mean(MI.t10.s,na.rm=T),mean(MI.t1.s,na.rm=T)))
}
if(intervals==TRUE){
if(trim.means==TRUE){    
    return(list("samples"=samples_out,pars,c(mean(MI.Gini[MI.Gini>=quantile(MI.Gini,interval.bounds[1]) & MI.Gini<=quantile(MI.Gini,interval.bounds[2])],na.rm=T),quantile(MI.Gini,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.MLD[MI.MLD>=quantile(MI.MLD,interval.bounds[1]) & MI.MLD<=quantile(MI.MLD,interval.bounds[2])],na.rm=T),quantile(MI.MLD,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.TheilT[MI.TheilT>=quantile(MI.TheilT,interval.bounds[1]) & MI.TheilT<=quantile(MI.TheilT,interval.bounds[2])],na.rm=T),quantile(MI.TheilT,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.Mean[MI.Mean>=quantile(MI.Mean,interval.bounds[1]) & MI.Mean<=quantile(MI.Mean,interval.bounds[2])],na.rm=T),quantile(MI.Mean,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.b50.s[MI.b50.s>=quantile(MI.b50.s,interval.bounds[1]) & MI.b50.s<=quantile(MI.b50.s,interval.bounds[2])],na.rm=T),quantile(MI.b50.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.m40.s[MI.m40.s>=quantile(MI.m40.s,interval.bounds[1]) & MI.m40.s<=quantile(MI.m40.s,interval.bounds[2])],na.rm=T),quantile(MI.m40.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.t10.s[MI.t10.s>=quantile(MI.t10.s,interval.bounds[1]) & MI.t10.s<=quantile(MI.t10.s,interval.bounds[2])],na.rm=T),quantile(MI.t10.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.t1.s[MI.t1.s>=quantile(MI.t1.s,interval.bounds[1]) & MI.t1.s<=quantile(MI.t1.s,interval.bounds[2])],na.rm=T),quantile(MI.t1.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE))))
}
if(trim.means==FALSE){    
    return(list("samples"=samples_out,pars,c(mean(MI.Gini,na.rm=T),quantile(MI.Gini,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.MLD,na.rm=T),quantile(MI.MLD,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.TheilT,na.rm=T),quantile(MI.TheilT,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.Mean,na.rm=T),quantile(MI.Mean,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.b50.s,na.rm=T),quantile(MI.b50.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.m40.s,na.rm=T),quantile(MI.m40.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.t10.s,na.rm=T),quantile(MI.t10.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.t1.s,na.rm=T),quantile(MI.t1.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE))))
}
}
}
else{
if(intervals==FALSE){    
    return(list(pars,mean(MI.Gini,na.rm=T),mean(MI.MLD,na.rm=T),mean(MI.TheilT,na.rm=T),mean(MI.Mean,na.rm=T),mean(MI.b50.s,na.rm=T),mean(MI.m40.s,na.rm=T),mean(MI.t10.s,na.rm=T),mean(MI.t1.s,na.rm=T)))
}
if(intervals==TRUE){
if(trim.means==TRUE){    
    return(list(pars,c(mean(MI.Gini[MI.Gini>=quantile(MI.Gini,interval.bounds[1]) & MI.Gini<=quantile(MI.Gini,interval.bounds[2])],na.rm=T),quantile(MI.Gini,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.MLD[MI.MLD>=quantile(MI.MLD,interval.bounds[1]) & MI.MLD<=quantile(MI.MLD,interval.bounds[2])],na.rm=T),quantile(MI.MLD,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.TheilT[MI.TheilT>=quantile(MI.TheilT,interval.bounds[1]) & MI.TheilT<=quantile(MI.TheilT,interval.bounds[2])],na.rm=T),quantile(MI.TheilT,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.Mean[MI.Mean>=quantile(MI.Mean,interval.bounds[1]) & MI.Mean<=quantile(MI.Mean,interval.bounds[2])],na.rm=T),quantile(MI.Mean,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.b50.s[MI.b50.s>=quantile(MI.b50.s,interval.bounds[1]) & MI.b50.s<=quantile(MI.b50.s,interval.bounds[2])],na.rm=T),quantile(MI.b50.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.m40.s[MI.m40.s>=quantile(MI.m40.s,interval.bounds[1]) & MI.m40.s<=quantile(MI.m40.s,interval.bounds[2])],na.rm=T),quantile(MI.m40.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.t10.s[MI.t10.s>=quantile(MI.t10.s,interval.bounds[1]) & MI.t10.s<=quantile(MI.t10.s,interval.bounds[2])],na.rm=T),quantile(MI.t10.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.t1.s[MI.t1.s>=quantile(MI.t1.s,interval.bounds[1]) & MI.t1.s<=quantile(MI.t1.s,interval.bounds[2])],na.rm=T),quantile(MI.t1.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE))))
}    
if(trim.means==FALSE){    
    return(list(pars,c(mean(MI.Gini,na.rm=T),quantile(MI.Gini,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.MLD,na.rm=T),quantile(MI.MLD,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.TheilT,na.rm=T),quantile(MI.TheilT,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.Mean,na.rm=T),quantile(MI.Mean,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.b50.s,na.rm=T),quantile(MI.b50.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.m40.s,na.rm=T),quantile(MI.m40.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.t10.s,na.rm=T),quantile(MI.t10.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE)),c(mean(MI.t1.s,na.rm=T),quantile(MI.t1.s,probs=c(interval.bounds[1],.5,interval.bounds[2]),na.rm=TRUE))))
}
}
}
}
}
################

#test1 = MI.ests(M=100,N=1000,contcod="URY",bin_year=2008,t=1,samples=TRUE)
#test2 = MI.ests(M=1000,N=1000,contcod="URY",bin_year=2008,t=runif(1000,0.98,0.999),samples=TRUE)
#test3 = MI.ests(M=10,N=1000,contcod="URY",bin_year=2008,t=1,samples=FALSE)
#set.seed(1234)
#test4 = MI.ests(M=100,N=1000,contcod="USA",bin_year=2008,t=(rbeta(100,shape1=1,shape2=20)*(0.999-0.95)+0.95),samples=FALSE,samplesonly=TRUE)
#set.seed(1234)
#test5 = MI.ests(M=100,N=1000,contcod="USA",bin_year=2008,t=(rbeta(100,shape1=1,shape2=20)*(0.999-0.95)+0.95),samples=TRUE,samplesonly=FALSE)
#plot(test4[[1]][,1],sapply(1:nrow(test4[[1]]),function(x){gini.gb2(shape1=test4[[1]][x,2],shape2=test4[[1]][x,4],shape3=test4[[1]][x,5])}))
##################
#OLD CODE#
##################
#MI.ests <- function(M,N,contcod,bin_year,t,samples=FALSE){
#pars <- country.est(contcod=contcod,bin_year=bin_year,t=t)$Parameters
#if(samples==TRUE){
#    samples_out <- data.frame(rep(0,N))
#}
#MI.Gini <- rep(0,M)
#MI.MLD <- rep(0,M)
#MI.TheilT <- rep(0,M)
#MI.Mean <- rep(0,M)
#MI.t10.s <- rep(0,M)
#MI.t1.s <- rep(0,M)
#MI.b50.s <-rep(0,M)
#MI.m40.s <- rep(0,M)
#set.seed(12345)
#for(i in 1:M){
#parsdraw <- c(rnorm(1,mean=test1[1],sd=0.3),rnorm(1,mean=test1[2],sd=50),rnorm(1,mean=test1[3],sd=0.3),rnorm(1,mean=test1[4],sd=0.3))
#sample <- rgb2(n=N,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4])
#if(samples==TRUE){
#samples_out[,i] <- sample
#        }
#MI.Gini[i] <- Gini(sample)
#MI.MLD[i] <- entropy(sample,parameter=0)
#MI.TheilT[i] <- entropy(sample,parameter=1)    
#MI.Mean[i] <- mean(sample)
#MI.t10.s[i] <- sum(sample[sample>=quantile(sample,probs=0.9)])/sum(sample)
#MI.t1.s[i] <- sum(sample[sample>=quantile(sample,probs=0.99)])/sum(sample)
#MI.b50.s[i] <- sum(sample[sample<quantile(sample,probs=0.5)])/sum(sample)
#MI.m40.s[i] <- sum(sample[sample>=quantile(sample,probs=0.4) & sample<=quantile(sample,probs=0.8)])/sum(sample)    
#}
#if (samples==TRUE){
#return(list("samples"=samples_out,c(pars[1],pars[2],pars[3],pars[4],gini.gb2(shape1=pars[1],shape2=pars[3],shape3=pars[4]),moment.gb2(1,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4]),mean(MI.Gini,na.rm=T),mean(MI.MLD,na.rm=T),mean(MI.TheilT,na.rm=T),median(MI.Mean,na.rm=T),mean(MI.b50.s,na.rm=T),mean(MI.m40.s,na.rm=T),mean(MI.t10.s,na.rm=T),mean(MI.t1.s,na.rm=T))))
#}
#else{
#    return(c(pars[1],pars[2],pars[3],pars[4],gini.gb2(shape1=pars[1],shape2=pars[3],shape3=pars[4]),moment.gb2(1,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4]),mean(MI.Gini,na.rm=T),mean(MI.MLD,na.rm=T),mean(MI.TheilT,na.rm=T),median(MI.Mean,na.rm=T),mean(MI.b50.s,na.rm=T),mean(MI.m40.s,na.rm=T),mean(MI.t10.s,na.rm=T),mean(MI.t1.s,na.rm=T)))
#    }
#}



#test1 = MI.ests(M=10,N=1000,contcod="URY",bin_year=2008,t=1,samples=TRUE)
