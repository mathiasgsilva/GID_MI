scenario1.MI <- function(output_data_dir,GID_files){
#output_data_dir is a character path to the directory containing write.table() output from the GID.MI.est(...,gid_samplesonly=TRUE) function
#GID_files is a character vector containing names of the files for which to run the estimates (i.e., the .txt tables of synthetic samples from the GID)   
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
}
