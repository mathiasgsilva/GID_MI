N.CHN93 <- unique((data$totpop[which(data$contcod%in%cnts9308 & data$bin_year==1993)]/data$totpop[which(data$contcod%in%cnts9308 & data$contcod=="CHN" & data$bin_year==1993)][1]))*88000
N.CHN08 <- unique((data$totpop[which(data$contcod%in%cnts9308 & data$bin_year==2008)]/data$totpop[which(data$contcod%in%cnts9308 & data$contcod=="CHN" & data$bin_year==2008)][1]))*80000

M=50

GID93.t1 <- GID.MI.est(cnts=cnts9308,year=1993,t=1,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t1,file=paste(output_data_dir,"GID93.t1.txt",sep=""),row.names=FALSE)
rm(GID93.t1)
gc()
GID93.t90 <- GID.MI.est(cnts=cnts9308,year=1993,t=0.90,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t90,file=paste(output_data_dir,"GID93.t90.txt",sep=""),row.names=FALSE)
rm(GID93.t90)
gc()
GID93.t95 <- GID.MI.est(cnts=cnts9308,year=1993,t=0.95,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t95,file=paste(output_data_dir,"GID93.t95.txt",sep=""),row.names=FALSE)
rm(GID93.t95)
gc()
GID93.t98 <- GID.MI.est(cnts=cnts9308,year=1993,t=0.98,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t98,file=paste(output_data_dir,"GID93.t98.txt",sep=""),row.names=FALSE)
rm(GID93.t98)
gc()
GID93.t985 <- GID.MI.est(cnts=cnts9308,year=1993,t=0.985,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t985,file=paste(output_data_dir,"GID93.t985.txt",sep=""),row.names=FALSE)
rm(GID93.t985)
gc()
GID93.t99 <- GID.MI.est(cnts=cnts9308,year=1993,t=0.99,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t99,file=paste(output_data_dir,"GID93.t99.txt",sep=""),row.names=FALSE)
rm(GID93.t99)
gc()
GID93.t995 <- GID.MI.est(cnts=cnts9308,year=1993,t=0.995,M=M,N=round(N.CHN93),gid_samplesonly=TRUE)
write.table(GID93.t995,file=paste(output_data_dir,"GID93.t995.txt",sep=""),row.names=FALSE)
rm(GID93.t995)
gc()
GID08.t1 <- GID.MI.est(cnts=cnts9308,year=2008,t=1,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t1,file=paste(output_data_dir,"GID08.t1.txt",sep=""),row.names=FALSE)
rm(GID08.t1)
gc()
GID08.t90 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.90,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t90,file=paste(output_data_dir,"GID08.t90.txt",sep=""),row.names=FALSE)
rm(GID08.t90)
gc()
GID08.t95 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.95,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t95,file=paste(output_data_dir,"GID08.t95.txt",sep=""),row.names=FALSE)
rm(GID08.t95)
gc()
GID08.t98 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.98,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t98,file=paste(output_data_dir,"GID08.t98.txt",sep=""),row.names=FALSE)
rm(GID08.t98)
gc()
GID08.t985 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.985,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t985,file=paste(output_data_dir,"GID08.t985.txt",sep=""),row.names=FALSE)
rm(GID08.t985)
gc()
GID08.t99 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.99,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t99,file=paste(output_data_dir,"GID08.t99.txt",sep=""),row.names=FALSE)
rm(GID08.t99)
gc()
GID08.t995 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.995,M=M,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t995,file=paste(output_data_dir,"GID08.t995.txt",sep=""),row.names=FALSE)
rm(GID08.t995)
gc()





GID_files <- c("GID93.t1.txt","GID93.t90.txt","GID93.t95.txt","GID93.t98.txt","GID93.t985.txt","GID93.t99.txt","GID93.t995.txt","GID08.t1.txt","GID08.t90.txt","GID08.t95.txt","GID08.t98.txt","GID08.t985.txt","GID08.t99.txt","GID08.t995.txt")

for(l in 1:length(GID_files)){
stat1 <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=Gini,MIsd=TRUE,bootfunc=function(x,i){Gini(x[i])})
stat2 <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){entropy(x,parameter=0)},MIsd=TRUE,bootfunc=function(x,i){entropy(x[i],parameter=0)})
stat3 <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){entropy(x,parameter=1)},MIsd=TRUE,bootfunc=function(x,i){entropy(x[i],parameter=1)})
stat4 <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=mean,MIsd=TRUE,bootfunc=function(x,i){mean(x[i])})
stat5 <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){sum(x[x>=quantile(x,probs=0.9)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.9)])/sum(x[i])})
stat6 <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){sum(x[x>=quantile(x,probs=0.99)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.99)])/sum(x[i])})
stat7 <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){sum(x[x>=quantile(x,probs=0.4) & x<=quantile(x,probs=0.8)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]>=quantile(x[i],probs=0.4) & x[i]<=quantile(x[i],probs=0.8)])/sum(x[i])})
stat8 <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){sum(x[x<=quantile(x,probs=0.5)])/sum(x)},MIsd=TRUE,bootfunc=function(x,i){sum(x[i][x[i]<=quantile(x[i],probs=0.5)])/sum(x[i])})
ptiles <- c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01))
qtiles <- list()
for(s in 1:length(ptiles)){
qtiles[[s]] <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){quantile(x,probs=ptiles[s])},MIsd=TRUE,bootfunc=function(x,i){quantile(x[i],probs=ptiles[s])})
    }
gid.ptile.means <- list()
gid.ptile.means[[1]] <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){mean(x[x<=qtiles[[1]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]<=qtiles[[1]][[2]]],na.rm=TRUE)})
for(p in 2:length(qtiles)){
gid.ptile.means[[p]] <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){mean(x[x>=qtiles[[p-1]][[2]] & x<qtiles[[p]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]>=qtiles[[p-1]][[2]] & x[i]<qtiles[[p]][[2]]],na.rm=TRUE)})
}
gid.ptile.means[[length(qtiles)+1]] <- GID.stat(file=paste(output_data_dir,GID_files[l],sep=""),M=M,FUN=function(x){mean(x[x>=qtiles[[length(qtiles)]][[2]]],na.rm=TRUE)},MIsd=TRUE,bootfunc=function(x,i){mean(x[i][x[i]>=qtiles[[length(qtiles)]][[2]]],na.rm=TRUE)})
out <- list("Gini"=stat1,"MLD"=stat2,"TheilT"=stat3,"Mean"=stat4,"top10.share"=stat5,"top1.share"=stat6,"middle40.share"=stat7,"bottom50.share"=stat8,"ptiles"=ptiles,"Quantiles"=qtiles,"Quantile Means"=gid.ptile.means)
save(out,file=paste(output_data_dir,GID_files[l],"stats.RData",sep=""))
}
