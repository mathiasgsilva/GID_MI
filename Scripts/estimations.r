rm(list=ls())
library(foreign)
library(GB2)
library(ineq)
#scripts_dir <- "/home/mathias/Documents/AMSE/Tesis/mscthesis/Ideas and scraps/scrap notes/scrap notes 3"
#LM_data_dir <- "/home/mathias/Documents/AMSE/Tesis/mscthesis/Ideas and scraps/scrap notes/scrap notes 4"
#trunc_data_dir <- "/home/mathias/Documents/AMSE/Tesis/mscthesis/Ideas and scraps/scrap notes/scrap notes 4/truncations/Data"
scripts_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/mscthesis/Scripts"
LM_data_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/mscthesis/Data"
output_data_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/"
setwd(LM_data_dir)
source(file=paste(scripts_dir,"decile.means.r",sep="/"))
source(file=paste(scripts_dir,"ptile.means.r",sep="/"))
source(file=paste(scripts_dir,"gb2.glc.r",sep="/"))
source(file=paste(scripts_dir,"nlst.gb2.r",sep="/"))
source(file=paste(scripts_dir,"country.est.r",sep="/"))
source(file=paste(scripts_dir,"MI.ests.r",sep="/"))
source(file=paste(scripts_dir,"countries.MI.est.r",sep="/"))
source(file=paste(scripts_dir,"GID.MI.est.r",sep="/"))
source(file=paste(scripts_dir,"GID.MI.stat.r",sep="/"))
data <- read.dta("LM_WPID_web_2.dta")
data_labels=attr(data,"var.labels")
data <- data[order(data$contcod,data$year,data$group),]

#Correct missing 1st decile mean (using given population mean income)
data[is.na(data$RRinc),"RRinc"] <- (data[data$contcod=="CHE" & data$bin_year==2008,"RRmean"][1]*sum(data[data$contcod=="CHE" & data$bin_year==2008,"pop"])-sum(data[data$contcod=="CHE" & data$bin_year==2008,"pop"]*data[data$contcod=="CHE" & data$bin_year==2008,"RRinc"],na.rm=T))/sum(data[data$contcod=="CHE" & data$bin_year==2008,"pop"])

#Country identifier (no disaggregation for rural/urban china, india, and indonesia)
data$mysample2 <- rep(0,nrow(data))
data$mysample2[which(data$contcod%in%c("CHN-U","CHN-R","IDN-U","IDN-R","IND-U","IND-R")==FALSE)] <- data$mysample[which(data$contcod%in%c("CHN-U","CHN-R","IDN-U","IDN-R","IND-U","IND-R")==FALSE)]
data$mysample2[data$mysample==0] <- rep(1,length(data$mysample2[data$mysample==0]))

#Country identifier (with disaggregation for rural/urban china,india, and indonesia)
data$mysample3 <- rep(0,nrow(data))
data$mysample3[which(data$contcod%in%c("CHN","IDN","IND")==FALSE)] <- data$mysample[which(data$contcod%in%c("CHN","IDN","IND")==FALSE)]

#Common countries in 1993-2008
cnts93 <- unique(data$contcod[data$bin_year==1993 & data$mysample2==1])
cnts08 <- unique(data$contcod[data$bin_year==2008 & data$mysample2==1])
cnts9308 <- cnts08[cnts08%in%cnts93]

##################
#GID estimates on common 93-08 sample with proportional-to-China sampling, such that smallest country has at least population of 30
##################
#Scenario 1 estimates
#source(file=paste(scripts_dir,"scenario1.GID.r",sep="/"))
#Scenario 2 estimates
#source(file=paste(scripts_dir,"scenario2.GID.r",sep="/"))



##################
#Individualized country estimates
##################
#WDI Ginis and top 10% share for full data comparison on country level estimates
wdi <- read.csv("WDI gini.csv",na.strings="..")
wdi_ginis <- wdi[which(wdi$Series.Code=="SI.POV.GINI"),3:ncol(wdi)]
wdi_top10 <- wdi[which(wdi$Series.Code=="SI.DST.10TH.10"),3:ncol(wdi)]
colnames(wdi_ginis) <- c("country","contcod",seq(1990,1996,1),seq(2000,2018,1))
colnames(wdi_top10) <- c("country","contcod",seq(1990,1996,1),seq(2000,2018,1))
wdi_ginis$ref93 <- apply(wdi_ginis[,c("1991","1992","1993","1994","1995")],1,mean,na.rm=TRUE)
wdi_ginis$ref08 <- apply(wdi_ginis[,c("2006","2007","2008","2009","2010")],1,mean,na.rm=TRUE)
wdi_top10$ref93 <- apply(wdi_top10[,c("1991","1992","1993","1994","1995")],1,mean,na.rm=TRUE)
wdi_top10$ref08 <- apply(wdi_top10[,c("2006","2007","2008","2009","2010")],1,mean,na.rm=TRUE)

all93t1 <- countries.MI.est(cnts=cnts9308,year=1993,t=1,M=50,intervals=TRUE,interval.bounds=c(.2,.8),trim.means=TRUE)
all93t1$gini_ref93 <- wdi_ginis[match(all93t1$contcod,wdi_ginis$contcod),"ref93"]/100
all93t1$t10_ref93 <- wdi_top10[match(all93t1$contcod,wdi_top10$contcod),"ref93"]/100
all93t985 <- countries.MI.est(cnts=cnts9308,year=1993,t=0.985,M=500,intervals=TRUE,interval.bounds=c(.2,.8),trim.means=TRUE)
all93t985$gini_ref93 <- wdi_ginis[match(all93t985$contcod,wdi_ginis$contcod),"ref93"]/100
all93t985$t10_ref93 <- wdi_top10[match(all93t985$contcod,wdi_top10$contcod),"ref93"]/100
all93t90 <- countries.MI.est(cnts=cnts9308,year=1993,t=0.90,M=50,intervals=TRUE,interval.bounds=c(.2,.8),trim.means=TRUE)
all93t90$gini_ref93 <- wdi_ginis[match(all93t90$contcod,wdi_ginis$contcod),"ref93"]/100
all93t90$t10_ref93 <- wdi_top10[match(all93t90$contcod,wdi_top10$contcod),"ref93"]/100
all08t95 <- countries.MI.est(cnts=cnts9308,year=2008,t=.95,M=50,intervals=TRUE,interval.bounds=c(.2,.8),trim.means=TRUE)
all08t95$gini_ref08 <- wdi_ginis[match(all08t95$contcod,wdi_ginis$contcod),"ref08"]/100
all08t95$t10_ref08 <- wdi_top10[match(all08t95$contcod,wdi_top10$contcod),"ref08"]/100
all08t1 <- countries.MI.est(cnts=cnts9308,year=2008,t=1,M=500,intervals=TRUE,interval.bounds=c(.2,.8),trim.means=TRUE)
all08t1$gini_ref08 <- wdi_ginis[match(all08t1$contcod,wdi_ginis$contcod),"ref08"]/100
all08t1$t10_ref08 <- wdi_top10[match(all08t1$contcod,wdi_top10$contcod),"ref08"]/100
all08t99 <- countries.MI.est(cnts=cnts9308,year=2008,t=.99,M=50,intervals=TRUE,interval.bounds=c(.2,.8),trim.means=TRUE)
all08t99$gini_ref08 <- wdi_ginis[match(all08t99$contcod,wdi_ginis$contcod),"ref08"]/100
all08t99$t10_ref08 <- wdi_top10[match(all08t99$contcod,wdi_top10$contcod),"ref08"]/100
all93t98 <- countries.MI.est(cnts=cnts9308,year=1993,t=0.98,M=50,intervals=TRUE)
all93t98$gini_ref93 <- wdi_ginis[match(all93t98$contcod,wdi_ginis$contcod),"ref93"]/100
all93t98$t10_ref93 <- wdi_top10[match(all93t98$contcod,wdi_top10$contcod),"ref93"]/100
all93t985 <- countries.MI.est(cnts=cnts9308,year=1993,t=0.985,M=500,intervals=TRUE)
all93t985$gini_ref93 <- wdi_ginis[match(all93t985$contcod,wdi_ginis$contcod),"ref93"]/100
all93t985$t10_ref93 <- wdi_top10[match(all93t985$contcod,wdi_top10$contcod),"ref93"]/100
all93t99 <- countries.MI.est(cnts=cnts9308,year=1993,t=0.99,M=500,intervals=TRUE)
all93t99$gini_ref93 <- wdi_ginis[match(all93t99$contcod,wdi_ginis$contcod),"ref93"]/100
all93t99$t10_ref93 <- wdi_top10[match(all93t99$contcod,wdi_top10$contcod),"ref93"]/100
all93t995 <- countries.MI.est(cnts=cnts9308,year=1993,t=0.995,M=500,intervals=TRUE)
all93t995$gini_ref93 <- wdi_ginis[match(all93t995$contcod,wdi_ginis$contcod),"ref93"]/100
all93t995$t10_ref93 <- wdi_top10[match(all93t995$contcod,wdi_top10$contcod),"ref93"]/100
all93t1 <- countries.MI.est(cnts=cnts9308,year=1993,t=1,M=500,intervals=TRUE)
all93t1$gini_ref93 <- wdi_ginis[match(all93t1$contcod,wdi_ginis$contcod),"ref93"]/100
all93t1$t10_ref93 <- wdi_top10[match(all93t1$contcod,wdi_top10$contcod),"ref93"]/100
all08t98 <- countries.MI.est(cnts=cnts9308,year=2008,t=0.98,M=500,intervals=TRUE)
all08t98$gini_ref08 <- wdi_ginis[match(all08t98$contcod,wdi_ginis$contcod),"ref08"]/100
all08t98$t10_ref08 <- wdi_top10[match(all08t98$contcod,wdi_top10$contcod),"ref08"]/100
all08t985 <- countries.MI.est(cnts=cnts9308,year=2008,t=0.985,M=500,intervals=TRUE)
all08t985$gini_ref08 <- wdi_ginis[match(all08t985$contcod,wdi_ginis$contcod),"ref08"]/100
all08t985$t10_ref08 <- wdi_top10[match(all08t985$contcod,wdi_top10$contcod),"ref08"]/100
all08t99 <- countries.MI.est(cnts=cnts9308,year=2008,t=0.99,M=500,intervals=TRUE)
all08t99$gini_ref08 <- wdi_ginis[match(all08t99$contcod,wdi_ginis$contcod),"ref08"]/100
all08t99$t10_ref08 <- wdi_top10[match(all08t99$contcod,wdi_top10$contcod),"ref08"]/100
all08t995 <- countries.MI.est(cnts=cnts9308,year=2008,t=0.995,M=500,intervals=TRUE)
all08t995$gini_ref08 <- wdi_ginis[match(all08t995$contcod,wdi_ginis$contcod),"ref08"]/100
all08t995$t10_ref08 <- wdi_top10[match(all08t995$contcod,wdi_top10$contcod),"ref08"]/100
all08t1 <- countries.MI.est(cnts=cnts9308,year=2008,t=1,M=500,intervals=TRUE)
all08t1$gini_ref08 <- wdi_ginis[match(all08t1$contcod,wdi_ginis$contcod),"ref08"]/100
all08t1$t10_ref08 <- wdi_top10[match(all08t1$contcod,wdi_top10$contcod),"ref08"]/100
save.image(file="sixths.RData")
rm(list=c(paste(paste("all",c(88,93,"08"),"t",sep="")[1],c(1,98,985,99,995),sep=""),paste(paste("all",c(88,93,"08"),"t",sep="")[2],c(1,98,985,99,995),sep=""),paste(paste("all",c(88,93,"08"),"t",sep="")[3],c(1,98,985,99,995),sep="")))
##################
#GID estimates with proportional-to-China sampling, such that smallest country has at least population of 30
##################
N.CHN88 <- unique((data$totpop[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988)]/data$totpop[which(data$contcod%in%trunc_points$contcod & data$contcod=="CHN" & data$bin_year==1988)][1]))*22500
#Smallest country: unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988)])[which(N.CHN88==sort(N.CHN88,decreasing=TRUE)[length(N.CHN88)])]
#data[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988),][which(N.CHN88==min(N.CHN88)),

#Estimates for 1988 with different truncations, countries for which the index is also available only:
GID88.t1 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988)]),year=1988,t=1,M=500,N=N.CHN88,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID88.t1,file="GID88.t1.RData")
rm(GID88.t1)
GID88.t98 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988)]),year=1988,t=0.98,M=500,N=N.CHN88,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID88.t98,file="GID88.t98.RData")
rm(GID88.t98)
GID88.t985 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988)]),year=1988,t=0.985,M=500,N=N.CHN88,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID88.t985,file="GID88.t985.RData")
rm(GID88.t985)
GID88.t99 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988)]),year=1988,t=0.99,M=500,N=N.CHN88,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID88.t99,file="GID88.t99.RData")
rm(GID88.t99)
GID88.t995 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988)]),year=1988,t=0.995,M=500,N=N.CHN88,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID88.t995,file="GID88.t995.RData")
rm(GID88.t995)


N.CHN93 <- unique((data$totpop[which(data$contcod%in%cnts9308 & data$bin_year==1993)]/data$totpop[which(data$contcod%in%cnts9308 & data$contcod=="CHN" & data$bin_year==1993)][1]))*88000
#Smallest country: unique(data$contcod[which(data$contcod%in%cnts9308 & data$bin_year==1993)])[which(N.CHN93==sort(N.CHN93,decreasing=TRUE)[length(N.CHN93)])]
#data[which(data$contcod==unique(data$contcod[which(data$contcod%in%cnts9308 & data$bin_year==1993)])[which(N.CHN93==sort(N.CHN93,decreasing=TRUE)[length(N.CHN93)])] & data$bin_year==1993),]

#Estimates for 1993 with different truncations, countries for which the index is also available only:
GID93.t1 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1993)]),year=1993,t=1,M=500,N=N.CHN93,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID93.t1,file="GID93.t1.RData")
rm(GID93.t1)
GID93.t98 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1993)]),year=1993,t=0.98,M=500,N=N.CHN93,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID93.t98,file="GID93.t98.RData")
rm(GID93.t98)
GID93.t985 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1993)]),year=1993,t=0.985,M=500,N=N.CHN93,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID93.t985,file="GID93.t985.RData")
rm(GID93.t985)
GID93.t99 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1993)]),year=1993,t=0.99,M=500,N=N.CHN93,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID93.t99,file="GID93.t99.RData")
rm(GID93.t99)
GID93.t995 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1993)]),year=1993,t=0.995,M=500,N=N.CHN93,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID93.t995,file="GID93.t995.RData")
rm(GID93.t995)

N.CHN08 <- unique((data$totpop[which(data$contcod%in%cnts9308 & data$bin_year==2008)]/data$totpop[which(data$contcod%in%cnts9308 & data$contcod=="CHN" & data$bin_year==2008)][1]))*80000
#Smallest country: data[which(data$contcod%in%trunc_points$contcod & data$bin_year==2008),][which(N.CHN08==min(N.CHN08)),]

#Estimates for 2008 with different truncations, countries for which the index is also available only:
GID08.t1 <- GID.MI.est(cnts=cnts9308,year=2008,t=1,M=100,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t1,file=paste(output_data_dir,"GID08.t1.txt",sep=""),row.names=FALSE)
rm(GID08.t1)
gc()
GID08.t98 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.98,M=100,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t98,file=paste(output_data_dir,"GID08.t98.txt",sep=""),row.names=FALSE)
rm(GID08.t98)
gc()
GID08.t985 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.985,M=100,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t985,file=paste(output_data_dir,"GID08.t985.txt",sep=""),row.names=FALSE)
rm(GID08.t985)
gc()
GID08.t99 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.99,M=100,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t99,file=paste(output_data_dir,"GID08.t99.txt",sep=""),row.names=FALSE)
rm(GID08.t99)
gc()
GID08.t995 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.995,M=100,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t995,file=paste(output_data_dir,"GID08.t995.txt",sep=""),row.names=FALSE)
rm(GID08.t995)
gc()
GID08.t95 <- GID.MI.est(cnts=cnts9308,year=2008,t=0.95,M=100,N=round(N.CHN08),gid_samplesonly=TRUE)
write.table(GID08.t95,file=paste(output_data_dir,"GID08.t95.txt",sep=""),row.names=FALSE)
rm(GID08.t95)
gc()

GID08.t98 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==2008)]),year=2008,t=0.98,M=500,N=N.CHN,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID08.t98,file="GID08.t98.RData")
rm(GID08.t98)
GID08.t985 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==2008)]),year=2008,t=0.985,M=500,N=N.CHN08,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID08.t985,file="GID08.t985.RData")
rm(GID08.t985)
GID08.t99 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==2008)]),year=2008,t=0.99,M=500,N=N.CHN08,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID08.t99,file="GID08.t99.RData")
rm(GID08.t99)
GID08.t995 <- GID.MI.est(cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==2008)]),year=2008,t=0.995,M=500,N=N.CHN08,ptiles=c(seq(0.05,0.95,0.05),seq(0.96,0.99,0.01)))
save(GID08.t995,file="GID08.t995.RData")
rm(GID08.t995)

##############################
#GIC:
##############################

#ptiles=c(seq(0.05,0.95,0.05),seq(0.96,1,0.01))
#cnts=unique(data$contcod[which(data$contcod%in%trunc_points$contcod & data$bin_year==1988)])
#t=1
#N.init
#N.end
#GID.MI.est for each year (using same set of countries now)
#load("GID08.t1.RData")
#load("GID93.t1.RData")
#load("GID88.t1.RData")
#plot(ptiles,(GID08.t1[[2]]/GID88.t1[[2]])-1)
#abline(h=(GID08.t1[[1]][3]/GID88.t1[[1]][3])-1,lty=2)
###############################
###SCRAP CODE##################
###############################

#Example using Russia 2008
#test1 <- country.est("BOL",2008,t=trunc_points[trunc_points$contcod=="BOL","t08"])$Parameters
#test2 <- country.est("BOL",2008,t=0.985)$Parameters
#gini.gb2(shape1=test1[1],shape2=test1[3],shape3=test1[4])
#gini.gb2(shape1=test2[1],shape2=test2[3],shape3=test2[4])

#countries.MI.est(cnts="CAF",year=2008,t=0.98,M=500)
#all93t1$gini_ref93 <- wdi_ginis[match(all93t1$contcod,wdi_ginis$contcod),"ref93"]/100
#all93t995$gini_ref93 <- wdi_ginis[match(all93t995$contcod,wdi_ginis$contcod),"ref93"]/100
#all08t1$gini_ref08 <- wdi_ginis[match(all08t1$contcod,wdi_ginis$contcod),"ref08"]/100
#all93t1$t10_ref93 <- wdi_top10[match(all93t1$contcod,wdi_top10$contcod),"ref93"]/100
#all93t995$t10_ref93 <- wdi_top10[match(all93t995$contcod,wdi_top10$contcod),"ref93"]/100
#all08t1$t10_ref08 <- wdi_top10[match(all08t1$contcod,wdi_top10$contcod),"ref08"]/100
##########

#Choosing the optimal t parameter as in the WID paper, for USA:

#opt.t <- function(contcod,bin_year,refs,N,M){
#aux.f <- function(t){
#pars <- country.est(contcod=contcod,bin_year=bin_year,t=t)$Parameters
#MI.t10.s <- rep(0,M)
#MI.t5.s <- rep(0,M)
#MI.t1.s <- rep(0,M)
#MI.t01.s <- rep(0,M)
#set.seed(12345)
#for(i in 1:M){
#parsdraw <- c(rnorm(1,mean=test1[1],sd=0.3),rnorm(1,mean=test1[2],sd=50),rnorm(1,mean=test1[3],sd=0.3),rnorm(1,mean=test1[4],sd=0.3))
#sample <- rgb2(n=N,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4])
#MI.t10.s[i] <- sum(sample[sample>=quantile(sample,probs=0.9)])/sum(sample)
#MI.t5.s[i] <- sum(sample[sample>=quantile(sample,probs=0.95)])/sum(sample)
#MI.t1.s[i] <- sum(sample[sample>=quantile(sample,probs=0.99)])/sum(sample)
#MI.t01.s[i] <- sum(sample[sample>=quantile(sample,probs=0.999)])/sum(sample)
#}
#sqres <- c((median(MI.t10.s,na.rm=T)-refs[1])**2,(median(MI.t5.s,na.rm=T)-refs[2])**2,(median(MI.t1.s,na.rm=T)-refs[3])**2,(median(MI.t01.s,na.rm=T)-refs[3])**2)
#return(sum(sqres))
#}
#topt <- optimize(f=aux.f,interval=c(0.9,0.999))
#return(topt$minimum)
#}

#topt <- opt.t(contcod="USA",bin_year=1993,refs=c(0.396,0.29,0.146,0.056),N=10000,M=500)
#topt93 <- topt$minimum
#topt03 <- topt03$minimum
                                        #topt08 <- opt.t(contcod="USA",bin_year=2008,refs=c(0.453,0.347,0.195,0.089),N=10000,M=500)
#topt08 <- topt08$minimum

#usa93 <- country.est(contcod="USA",bin_year=1993,t=topt93)$Parameters
#usa03 <- country.est(contcod="USA",bin_year=2003,t=topt03)$Parameters
#usa08 <- country.est(contcod="USA",bin_year=2008,t=topt08)$Parameters
#usa93 <- country.est(contcod="USA",bin_year=1993,t=0.976)$Parameters
#usa03 <- country.est(contcod="USA",bin_year=2003,t=0.976)$Parameters
#usa08 <- country.est(contcod="USA",bin_year=2008,t=0.976)$Parameters
#gini.gb2(shape1=usa93[1],shape2=usa93[3],shape3=usa93[4])
#gini.gb2(shape1=usa03[1],shape2=usa03[3],shape3=usa03[4])
#gini.gb2(shape1=usa08[1],shape2=usa08[3],shape3=usa08[4])
####
#chn08 <- data[data$contcod=="BRA" & data$bin_year==1993,c("RRinc","pop","RRmean")]
#chn08_t98 <- as.numeric(countries.MI.est(cnts="MEX",year=2008,t=0.98,M=500)[2:5])
#chn08_t1 <- country.est(contcod="BRA",bin_year=1993,t=1,theta.in.shape=c(1.5,1,1.5))$Parameters
#chn08_t995 <- country.est(contcod="BRA",bin_year=1993,t=0.999,theta.in.shape=c(1.5,1,1.5))$Parameters
#chn08_t985 <- country.est(contcod="MEX",bin_year=1993,t=0.985,theta.in.shape=c(1.5,1,1.5))$Parameters
#chn08_tind <- country.est(contcod="MEX",bin_year=1993,t=trunc_points[trunc_points$contcod=="MEX","t93"],theta.in.shape=c(1.5,1,1.5))$Parameters
#chn08_lc <- Lc(x=chn08$RRinc,n=chn08$pop,plot=TRUE)
#curve(gb2.glc(x,shape1=chn08_t1[1],scale=chn08_t1[2],shape2=chn08_t1[3],shape3=chn08_t1[4],GLC=FALSE),from=0,to=1,add=TRUE)
#curve(gb2.glc(x,shape1=chn08_tind[1],scale=chn08_tind[2],shape2=chn08_tind[3],shape3=chn08_tind[4],GLC=FALSE),from=0,to=1,add=TRUE,col=2)
#curve(gb2.glc(x,shape1=chn08_t995[1],scale=chn08_t995[2],shape2=chn08_t995[3],shape3=chn08_t995[4],GLC=FALSE),from=0,to=1,add=TRUE,col=3)

#hist(x=chn08$RRinc,breaks=chn08$RRinc,probability=TRUE)
#curve(dgb2(x,shape1=chn08_t1[1],scale=chn08_t1[2],shape2=chn08_t1[3],shape3=chn08_t1[4]),from=0,to=100000,add=TRUE)

#chn08_lc <- Lc(x=chn08$RRinc,n=chn08$pop,plot=TRUE)
#curve(gb2.glc(x,shape1=fit1$nls.estimation[1,1],scale=fit1$nls.estimation[1,2]*1000,shape2=1,shape3=fit1$nls.estimation[1,3],GLC=FALSE),from=0,to=1,add=TRUE,col=2)
#curve(gb2.glc(x,shape1=chn08_t1[1],scale=chn08_t1[2],shape2=chn08_t1[3],shape3=chn08_t1[4],GLC=FALSE),from=0,to=1,add=TRUE)



#curve(dgb2(x,shape1=chn08_t98[1],scale=chn08_t98[2],shape2=chn08_t98[3],shape3=chn08_t98[4]),from=0,to=100000,add=TRUE,col=2)
#curve(dgb2(x,shape1=chn08_t985[1],scale=chn08_t985[2],shape2=chn08_t985[3],shape3=chn08_t985[4]),from=0,to=100000,add=TRUE,col=3)


#curve(dgb2(x,shape1=chn08_t98[1],scale=chn08_t98[2],shape2=chn08_t98[3],shape3=chn08_t98[4]),from=0,to=100000,add=TRUE)
#curve(dgb2(x,shape1=chn08_t90[1],scale=chn08_t90[2],shape2=chn08_t90[3],shape3=chn08_t90[4]),from=0,to=100000,col=2,add=TRUE)
#curve(dgb2(x,shape1=chn08_t80[1],scale=chn08_t80[2],shape2=chn08_t80[3],shape3=chn08_t80[4]),from=0,to=100000,col=3,add=TRUE)

#####
#View(data[data$contcod=="CHE" & data$bin_year==2008,c("RRinc","pop","RRmean")])
#colnames(che08) <- c("dmeans","sizes","mean")

#recover.dmeans <- function(x){
#(x$mean[1]*sum(x$sizes)-sum(x$sizes*x$dmeans,na.rm=T))/sum(x$sizes)
#      }

######

#all08t985$gini_diff <- all08t985[,"Gini-MI"]-all08t985$gini_ref08


#To change:
#After obtaining all 4 use them as initial values in a 4 dimensional optimization of the gb2 to get a hessian


