MI.stats.tables <- function(file,trims.pmeans=c(0.05,0.95),trims.stats=c(0,0.9),digits=4){
#file must be the character path to an .RData file containing output from the scenario1.MI function
#trims.stats is a vector with the proportion of least extremes estimates to use for trimming means and sd:
#i) For the measures which atypical values generate a long tail to the left (i.e., upper proportion of values to use)
#ii) For the measures which atypical values generate a long tail to the right (i.e., lower proportion of values to use)    
load(file)
c_ests <- list()
for(i in 1:length(out[[10]])){
c_ests$pmeans.bounds[[i]] <- quantile(out[[10]][[i]][[1]][,1],probs=c(trims.pmeans[1],0.5,trims.pmeans[2]))
c_ests$pmeans.tmeans[[i]] <- mean(out[[10]][[i]][[1]][which(out[[10]][[i]][[1]][,1]>=c_ests$pmeans.bounds[[i]][1] & out[[10]][[i]][[1]][,1]<=c_ests$pmeans.bounds[[i]][3]),1])
c_ests$pmeans.tsd[[i]] <- sqrt(mean((out[[10]][[i]][[1]][which(out[[10]][[i]][[1]][,1]>=c_ests$pmeans.bounds[[i]][1] & out[[10]][[i]][[1]][,1]<=c_ests$pmeans.bounds[[i]][3]),2]**2))+(1+(1/sum(which(out[[10]][[i]][[1]][,1]>=c_ests$pmeans.bounds[[i]][1] & out[[10]][[i]][[1]][,1]<=c_ests$pmeans.bounds[[i]][3]))))*var(out[[10]][[i]][[1]][which(out[[10]][[i]][[1]][,1]>=c_ests$pmeans.bounds[[i]][1] & out[[10]][[i]][[1]][,1]<=c_ests$pmeans.bounds[[i]][3]),1]))
}
for(i in 1:6){
c_ests$stats.bounds[[i]] <- quantile(out[[i]][[1]][,1],probs=c(trims.stats[1],0.5,trims.stats[2]))
c_ests$stats.tmeans[[i]] <- mean(out[[i]][[1]][which(out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),1])
c_ests$stats.tsd[[i]] <- sqrt(mean((out[[i]][[1]][which(out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),2]**2))+(1+(1/sum(which(out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]))))*var(out[[i]][[1]][which(out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),1]))
}
for(i in 7:8){
c_ests$stats.bounds[[i]] <- quantile(out[[i]][[1]][,1],probs=c(trims.stats[1],0.5,trims.stats[2]))
c_ests$stats.tmeans[[i]] <- mean(out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1]),1])
c_ests$stats.tsd[[i]] <- sqrt(mean((out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1]),2]**2))+(1+(1/sum(which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1]))))*var(out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1]),1]))
}    
return(list("stats"=cbind.data.frame("Statistic"=c("Gini","MLD","Theil-T","Mean","Top 10%","Top 1%","Middle 40%","Bottom 50%"),"Trimmed MI mean"=round(c_ests$stats.tmeans,digits=digits),"Trimmed MI sd"=round(c_ests$stats.tsd,digits=digits),round(do.call(rbind,c_ests$stats.bounds),digits=digits)),"pmeans"=cbind("Percentile"=out[[9]],"Trimmed MI mean"=round(c_ests$pmeans.tmeans,digits=digits),"Trimmed MI sd"=round(c_ests$pmeans.tsd,digits=digits),round(do.call(rbind,c_ests$pmeans.bounds),digits=digits))))
}




#########################
#OLD CODE
#########################


#MI.stats.tables <- function(file,trims.pmeans=c(0.05,0.95),trims.stats=c(0.05,0.95),digits=4){
#file must be the character path to an .RData file containing output from the scenario1.MI function
#load(file)
#c_ests <- list()
#for(i in 1:length(out[[10]])){
#c_ests$pmeans.bounds[[i]] <- quantile(out[[10]][[i]][[1]][,1],probs=c(trims.pmeans[1],0.5,trims.pmeans[2]))
#c_ests$pmeans.tmeans[[i]] <- mean(out[[10]][[i]][[1]][which(out[[10]][[i]][[1]][,1]>=c_ests$pmeans.bounds[[i]][1] & out[[10]][[i]][[1]][,1]<=c_ests$pmeans.bounds[[i]][3]),1])
#c_ests$pmeans.tsd[[i]] <- sqrt(mean((out[[10]][[i]][[1]][which(out[[10]][[i]][[1]][,1]>=c_ests$pmeans.bounds[[i]][1] & out[[10]][[i]][[1]][,1]<=c_ests$pmeans.bounds[[i]][3]),2]**2))+(1+(1/sum(which(out[[10]][[i]][[1]][,1]>=c_ests$pmeans.bounds[[i]][1] & out[[10]][[i]][[1]][,1]<=c_ests$pmeans.bounds[[i]][3]))))*var(out[[10]][[i]][[1]][which(out[[10]][[i]][[1]][,1]>=c_ests$pmeans.bounds[[i]][1] & out[[10]][[i]][[1]][,1]<=c_ests$pmeans.bounds[[i]][3]),1]))
#}
#for(i in 1:6){
#c_ests$stats.bounds[[i]] <- quantile(out[[i]][[1]][,1],probs=c(trims.stats[1],0.5,trims.stats[2]))
#c_ests$stats.tmeans[[i]] <- mean(out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1] & out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),1])
#c_ests$stats.tsd[[i]] <- sqrt(mean((out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1] & out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),2]**2))+(1+(1/sum(which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1] & out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]))))*var(out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1] & out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),1]))
#}
#for(i in 7:8){
#c_ests$stats.bounds[[i]] <- quantile(out[[i]][[1]][,1],probs=c(trims.stats[1],0.5,trims.stats[2]))
#c_ests$stats.tmeans[[i]] <- mean(out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1] & out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),1])
#c_ests$stats.tsd[[i]] <- sqrt(mean((out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1] & out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),2]**2))+(1+(1/sum(which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1] & out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]))))*var(out[[i]][[1]][which(out[[i]][[1]][,1]>=c_ests$stats.bounds[[i]][1] & out[[i]][[1]][,1]<=c_ests$stats.bounds[[i]][3]),1]))
#}    
#return(list("stats"=cbind.data.frame("Statistic"=c("Gini","MLD","Theil-T","Mean","Top 10%","Top 1%","Middle 40%","Bottom 50%"),"Trimmed MI mean"=round(c_ests$stats.tmeans,digits=digits),"Trimmed MI sd"=round(c_ests$stats.tsd,digits=digits),round(do.call(rbind,c_ests$stats.bounds),digits=digits)),"pmeans"=cbind("Percentile"=out[[9]],"Trimmed MI mean"=round(c_ests$pmeans.tmeans,digits=digits),"Trimmed MI sd"=round(c_ests$pmeans.tsd,digits=digits),round(do.call(rbind,c_ests$pmeans.bounds),digits=digits))))
#}

#######

#GID.93.t1 <- MI.stats.tables(file="GID93.t1.txtstats.RData",trims.stats=c(0.2,0.8),trims.pmeans=c(0,1))
