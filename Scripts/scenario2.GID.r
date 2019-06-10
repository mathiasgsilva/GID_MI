N.CHN93 <- unique((data$totpop[which(data$contcod%in%cnts9308 & data$bin_year==1993)]/data$totpop[which(data$contcod%in%cnts9308 & data$contcod=="CHN" & data$bin_year==1993)][1]))*88000
N.CHN08 <- unique((data$totpop[which(data$contcod%in%cnts9308 & data$bin_year==2008)]/data$totpop[which(data$contcod%in%cnts9308 & data$contcod=="CHN" & data$bin_year==2008)][1]))*80000

M=100
a=.95
b=.999    
alpha.0=2
beta.0=2
t.sym <- list()
set.seed(12345)
for(x in 1:length(cnts9308)){
t.sym[[x]] <- rbeta(n=M,shape1=alpha.0,shape2=beta.0)*(b-a)+a
}

GID93.t.sym <- GID.MI.est(cnts=cnts9308,year=1993,t=t.sym,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t.sym,file=paste(output_data_dir,"GID93.t.sym.txt",sep=""),row.names=FALSE)
rm(GID93.t.sym)
gc()
GID08.t.sym <- GID.MI.est(cnts=cnts9308,year=2008,t=t.sym,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t.sym,file=paste(output_data_dir,"GID08.t.sym.txt",sep=""),row.names=FALSE)
rm(GID08.t.sym)
gc()

alpha.1=2
beta.1=4
t.pskew <- list()
set.seed(12345)
for(x in 1:length(cnts9308)){
t.pskew[[x]] <- rbeta(n=M,shape1=alpha.1,shape2=beta.1)*(b-a)+a
}

GID93.t.pskew <- GID.MI.est(cnts=cnts9308,year=1993,t=t.pskew,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t.pskew,file=paste(output_data_dir,"GID93.t.pskew.txt",sep=""),row.names=FALSE)
rm(GID93.t.pskew)
gc()
GID08.t.pskew <- GID.MI.est(cnts=cnts9308,year=2008,t=t.pskew,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t.pskew,file=paste(output_data_dir,"GID08.t.pskew.txt",sep=""),row.names=FALSE)
rm(GID08.t.pskew)
gc()

t.nskew <- list()
set.seed(12345)
for(x in 1:length(cnts9308)){
t.nskew[[x]] <- (1-rbeta(n=M,shape1=alpha.1,shape2=beta.1))*(b-a)+a
}

GID93.t.nskew <- GID.MI.est(cnts=cnts9308,year=1993,t=t.nskew,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t.nskew,file=paste(output_data_dir,"GID93.t.nskew.txt",sep=""),row.names=FALSE)
rm(GID93.t.nskew)
gc()
GID08.t.nskew <- GID.MI.est(cnts=cnts9308,year=2008,t=t.nskew,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t.nskew,file=paste(output_data_dir,"GID08.t.nskew.txt",sep=""),row.names=FALSE)
rm(GID08.t.nskew)
gc()

GID_files <- c("GID93.t.sym.txt","GID08.t.sym.txt","GID93.t.pskew.txt","GID08.t.pskew.txt","GID93.t.nskew.txt","GID08.t.nskew.txt")

for(l in 1:length(GID_files)){
print(paste(l/length(GID_files)*100,"% of files",sep=""))
stat1 <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=Gini,MIsd=TRUE,bootfunc=function(x,i){Gini(x[i])})
print(paste(1/8,"stats",sep=""))
stat2 <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){entropy(x,parameter=0)},MIsd=TRUE,bootfunc=function(x,i){entropy(x[i],parameter=0)})
print(paste(2/8,"stats",sep=""))
stat3 <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){entropy(x,parameter=1)},MIsd=TRUE,bootfunc=function(x,i){entropy(x[i],parameter=1)})
print(paste(3/8,"stats",sep=""))
stat4 <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=mean,MIsd=TRUE,bootfunc=function(x,i){mean(x[i])})
print(paste(4/8,"stats",sep=""))
stat5 <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){sum(x[x>=quantile(x,probs=0.9)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.9)])/sum(x[i])})
print(paste(5/8,"stats",sep=""))
stat6 <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){sum(x[x>=quantile(x,probs=0.99)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.99)])/sum(x[i])})
print(paste(6/8,"stats",sep=""))
stat7 <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){sum(x[x>=quantile(x,probs=0.4) & x<=quantile(x,probs=0.8)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.4) & x[i]<=quantile(x[i],probs=0.8)])/sum(x[i])})
print(paste(7/8,"stats",sep=""))
stat8 <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){sum(x[x<=quantile(x,probs=0.5)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]<=quantile(x[i],probs=0.5)])/sum(x[i])})
ptiles <- c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01))
qtiles <- list()
for(s in 1:length(ptiles)){
print(paste(s/length(ptiles),"quantiles",sep=""))    
qtiles[[s]] <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){quantile(x,probs=ptiles[s])},MIsd=TRUE,bootfunc=function(x,i){quantile(x[i],probs=ptiles[s])})
    }
gid.ptile.means <- list()
gid.ptile.means[[1]] <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){mean(x[x<=qtiles[[1]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]<=qtiles[[1]][[2]]],na.rm=TRUE)})
for(p in 2:length(qtiles)){
print(paste(p/length(qtiles),"quantile means",sep=""))    
gid.ptile.means[[p]] <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){mean(x[x>=qtiles[[p-1]][[2]] & x<qtiles[[p]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]>=qtiles[[p-1]][[2]] & x[i]<qtiles[[p]][[2]]],na.rm=TRUE)})
}
gid.ptile.means[[length(qtiles)+1]] <- GID.MI.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){mean(x[x>=qtiles[[length(qtiles)]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]>=qtiles[[length(qtiles)]][[2]]],na.rm=TRUE)})
out <- list("Gini"=stat1,"MLD"=stat2,"TheilT"=stat3,"Mean"=stat4,"top10.share"=stat5,"top1.share"=stat6,"middle40.share"=stat7,"bottom50.share"=stat8,"ptiles"=ptiles,"Quantiles"=qtiles,"Quantile Means"=gid.ptile.means)
save(out,file=paste(output_data_dir,GID_files[l],"stats.RData",sep=""))
}
