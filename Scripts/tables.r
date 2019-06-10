rm(list=ls())
library(foreign)
library(GB2)
library(ineq)
library(xtable)
library(data.table)
#scripts_dir <- "/home/mathias/Documents/AMSE/Tesis/mscthesis/Ideas and scraps/scrap notes/scrap notes 3"
#LM_data_dir <- "/home/mathias/Documents/AMSE/Tesis/mscthesis/Ideas and scraps/scrap notes/scrap notes 4"
#trunc_data_dir <- "/home/mathias/Documents/AMSE/Tesis/mscthesis/Ideas and scraps/scrap notes/scrap notes 4/truncations/Data"
scripts_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/mscthesis/Scripts"
LM_data_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/mscthesis/Data"
output_scenario1_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/mscthesis/Data/Scenario 1/"
output_scenario2_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/mscthesis/Data/Scenario 2/"
tables_document_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/mscthesis/Document/Tables/"
images_document_dir <- "/home/mathias/Documents/Maestria/AMSE M2/tesis/mscthesis/Document/Images/"
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
source(file=paste(scripts_dir,"iqi.r",sep="/"))
source(file=paste(scripts_dir,"MI.stats.tables.r",sep="/"))
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

#######Table 1: Country-year coverage table
aux1 <- unique(data[data$mysample2==1,c("contcod","region","bin_year")])
r1 <- c(length(aux1[aux1$bin_year==1988,"contcod"]),sum(aux1[aux1$bin_year==1988,"contcod"]%in%aux1[aux1$bin_year==1993,"contcod"]),sum(aux1[aux1$bin_year==1988,"contcod"]%in%aux1[aux1$bin_year==1998,"contcod"]),sum(aux1[aux1$bin_year==1988,"contcod"]%in%aux1[aux1$bin_year==2003,"contcod"]),sum(aux1[aux1$bin_year==1988,"contcod"]%in%aux1[aux1$bin_year==2008,"contcod"]))
r2 <- c(sum(aux1[aux1$bin_year==1993,"contcod"]%in%aux1[aux1$bin_year==1988,"contcod"]),length(aux1[aux1$bin_year==1993,"contcod"]),sum(aux1[aux1$bin_year==1993,"contcod"]%in%aux1[aux1$bin_year==1998,"contcod"]),sum(aux1[aux1$bin_year==1993,"contcod"]%in%aux1[aux1$bin_year==2003,"contcod"]),sum(aux1[aux1$bin_year==1993,"contcod"]%in%aux1[aux1$bin_year==2008,"contcod"]))
r3 <- c(sum(aux1[aux1$bin_year==1998,"contcod"]%in%aux1[aux1$bin_year==1988,"contcod"]),sum(aux1[aux1$bin_year==1998,"contcod"]%in%aux1[aux1$bin_year==1993,"contcod"]),length(aux1[aux1$bin_year==1998,"contcod"]),sum(aux1[aux1$bin_year==1998,"contcod"]%in%aux1[aux1$bin_year==2003,"contcod"]),sum(aux1[aux1$bin_year==1998,"contcod"]%in%aux1[aux1$bin_year==2008,"contcod"]))
r4 <- c(sum(aux1[aux1$bin_year==2003,"contcod"]%in%aux1[aux1$bin_year==1988,"contcod"]),sum(aux1[aux1$bin_year==2003,"contcod"]%in%aux1[aux1$bin_year==1993,"contcod"]),sum(aux1[aux1$bin_year==2003,"contcod"]%in%aux1[aux1$bin_year==1998,"contcod"]),length(aux1[aux1$bin_year==2003,"contcod"]),sum(aux1[aux1$bin_year==2003,"contcod"]%in%aux1[aux1$bin_year==2008,"contcod"]))
r5 <- c(sum(aux1[aux1$bin_year==2008,"contcod"]%in%aux1[aux1$bin_year==1988,"contcod"]),sum(aux1[aux1$bin_year==2008,"contcod"]%in%aux1[aux1$bin_year==1993,"contcod"]),sum(aux1[aux1$bin_year==2008,"contcod"]%in%aux1[aux1$bin_year==1998,"contcod"]),sum(aux1[aux1$bin_year==2008,"contcod"]%in%aux1[aux1$bin_year==2003,"contcod"]),length(aux1[aux1$bin_year==2008,"contcod"]))
tab <- rbind(r1,r2,r3,r4,r5)
colnames(tab) <- c(1988,1993,1998,2003,2008)
rownames(tab) <- colnames(tab)
rm(list=c("aux1","r1","r2","r3","r4","r5"))
print(xtable(tab,caption="Country coverage by years in LM-WPID",label="ctycvge"),file=paste(document_dir,"/ctycvge.tex",sep=""))

#######Table 2: Region-country composition of 1993-2008 common sample
aux1 <- unique(data[data$contcod%in%cnts9308,c("contcod","country","region","bin_year","totpop")])
#Population coverage in 1993
popcov93 <- c((sum(aux1[aux1$bin_year==1993 & aux1$region=="Mature economies","totpop"])*1000000)/(5.545*1000000000),(sum(aux1[aux1$bin_year==1993 & aux1$region=="M. East & N. Africa","totpop"])*1000000)/(5.545*1000000000),(sum(aux1[aux1$bin_year==1993 & aux1$region=="L. America & Carib.","totpop"])*1000000)/(5.545*1000000000),(sum(aux1[aux1$bin_year==1993 & aux1$region=="Russia, C. Asia, SE Europe","totpop"])*1000000)/(5.545*1000000000),(sum(aux1[aux1$bin_year==1993 & aux1$region=="Sub-Saharan Africa","totpop"])*1000000)/(5.545*1000000000),(sum(aux1[aux1$bin_year==1993 & aux1$region=="Other Asia","totpop"])*1000000)/(5.545*1000000000),(sum(aux1[aux1$bin_year==1993 & aux1$region=="China","totpop"])*1000000)/(5.545*1000000000),(sum(aux1[aux1$bin_year==1993 & aux1$region=="India","totpop"])*1000000)/(5.545*1000000000))
#Population coverage in 2008
popcov08 <- c((sum(aux1[aux1$bin_year==2008 & aux1$region=="Mature economies","totpop"])*1000000)/(6.766*1000000000),(sum(aux1[aux1$bin_year==2008 & aux1$region=="M. East & N. Africa","totpop"])*1000000)/(6.766*1000000000),(sum(aux1[aux1$bin_year==2008 & aux1$region=="L. America & Carib.","totpop"])*1000000)/(6.766*1000000000),(sum(aux1[aux1$bin_year==2008 & aux1$region=="Russia, C. Asia, SE Europe","totpop"])*1000000)/(6.766*1000000000),(sum(aux1[aux1$bin_year==2008 & aux1$region=="Sub-Saharan Africa","totpop"])*1000000)/(6.766*1000000000),(sum(aux1[aux1$bin_year==2008 & aux1$region=="Other Asia","totpop"])*1000000)/(6.766*1000000000),(sum(aux1[aux1$bin_year==2008 & aux1$region=="China","totpop"])*1000000)/(6.766*1000000000),(sum(aux1[aux1$bin_year==2008 & aux1$region=="India","totpop"])*1000000)/(6.766*1000000000))

#######Plot 1: Fitted Lorenz curves for China, Mali, France, Uruguay, United States, and Morocco
chn.iqi.93 <- iqi("CHN",1993)
chn.iqi.08 <- iqi("CHN",2008)
mli.iqi.93 <- iqi("MLI",1993)
mli.iqi.08 <- iqi("MLI",2008)
fra.iqi.93 <- iqi("FRA",1993)
fra.iqi.08 <- iqi("FRA",2008)
#ury.iqi.93 <- iqi("URY",1993)
#ury.iqi.08 <- iqi("URY",2008)
usa.iqi.93 <- iqi("USA",1993)
usa.iqi.08 <- iqi("USA",2008)
mar.iqi.93 <- iqi("MAR",1993)
mar.iqi.08 <- iqi("MAR",2008)
set.seed(4444)
chn.sample.93 <- as.numeric(MI.ests(contcod="CHN",bin_year=1993,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#chn.sample.93.c <- as.numeric(MI.ests(contcod="CHN",bin_year=1993,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
chn.sample.08 <- as.numeric(MI.ests(contcod="CHN",bin_year=2008,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#chn.sample.08.c <- as.numeric(MI.ests(contcod="CHN",bin_year=2008,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
mli.sample.93 <- as.numeric(MI.ests(contcod="MLI",bin_year=1993,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#mli.sample.93.c <- as.numeric(MI.ests(contcod="MLI",bin_year=1993,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
mli.sample.08 <- as.numeric(MI.ests(contcod="MLI",bin_year=2008,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#mli.sample.08.c <- as.numeric(MI.ests(contcod="MLI",bin_year=2008,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
fra.sample.93 <- as.numeric(MI.ests(contcod="FRA",bin_year=1993,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#fra.sample.93.c <- as.numeric(MI.ests(contcod="FRA",bin_year=1993,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
fra.sample.08 <- as.numeric(MI.ests(contcod="FRA",bin_year=2008,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#fra.sample.08.c <- as.numeric(MI.ests(contcod="FRA",bin_year=2008,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
#ury.sample.93 <- as.numeric(MI.ests(contcod="URY",bin_year=1993,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
#ury.sample.93.c <- as.numeric(MI.ests(contcod="URY",bin_year=1993,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
#ury.sample.08 <- as.numeric(MI.ests(contcod="URY",bin_year=2008,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
#ury.sample.08.c <- as.numeric(MI.ests(contcod="URY",bin_year=2008,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
usa.sample.93 <- as.numeric(MI.ests(contcod="USA",bin_year=1993,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#usa.sample.93.c <- as.numeric(MI.ests(contcod="USA",bin_year=1993,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
usa.sample.08 <- as.numeric(MI.ests(contcod="USA",bin_year=2008,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#usa.sample.08.c <- as.numeric(MI.ests(contcod="USA",bin_year=2008,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
mar.sample.93 <- as.numeric(MI.ests(contcod="MAR",bin_year=1993,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#mar.sample.93.c <- as.numeric(MI.ests(contcod="MAR",bin_year=1993,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])
mar.sample.08 <- as.numeric(MI.ests(contcod="MAR",bin_year=2008,t=1,M=10,samplesonly=TRUE,N=10000)$samples[[1]])
#mar.sample.08.c <- as.numeric(MI.ests(contcod="MAR",bin_year=2008,t=.975,M=10,samplesonly=TRUE,N=10000)$samples[[sample(1:10,1)]])

cairo_ps(paste(images_document_dir,"plotLcsc0chn.eps",sep=""),width=7,height=7,pointsize=12)
plot(Lc(chn.iqi.93),main="China",lwd=3,xlab="u",ylab="L(u)",col="firebrick1")
for(i in 1:10){
    abline(h=i/10,col="lightgray",lty="dotted")
    abline(v=i/10,col="lightgray",lty="dotted")  
    }
lines(Lc(chn.sample.93)$p,Lc(chn.sample.93)$L,col="dodgerblue2",lwd=3)
lines(Lc(chn.iqi.08)$p,Lc(chn.iqi.08)$L,col="firebrick1",lwd=3,lty=3)
lines(Lc(chn.sample.08)$p,Lc(chn.sample.08)$L,col="dodgerblue2",lty=3,lwd=3)
dev.off()
cairo_ps(paste(images_document_dir,"plotLcsc0fra.eps",sep=""),width=7,height=7,pointsize=12)
plot(Lc(fra.iqi.93),main="France",lwd=3,xlab="u",ylab="L(u)",col="firebrick1")
for(i in 1:10){
    abline(h=i/10,col="lightgray",lty="dotted")
    abline(v=i/10,col="lightgray",lty="dotted")  
    }
lines(Lc(fra.sample.93)$p,Lc(fra.sample.93)$L,col="dodgerblue2",lwd=3)
lines(Lc(fra.iqi.08)$p,Lc(fra.iqi.08)$L,col="firebrick1",lwd=3,lty=3)
lines(Lc(fra.sample.08)$p,Lc(fra.sample.08)$L,col="dodgerblue2",lty=3,lwd=3)
dev.off()
cairo_ps(paste(images_document_dir,"plotLcsc0mli.eps",sep=""),width=7,height=7,pointsize=12)
plot(Lc(mli.iqi.93),main="Mali",lwd=3,xlab="u",ylab="L(u)",col="firebrick1")
for(i in 1:10){
    abline(h=i/10,col="lightgray",lty="dotted")
    abline(v=i/10,col="lightgray",lty="dotted")  
    }
lines(Lc(mli.sample.93)$p,Lc(mli.sample.93)$L,col="dodgerblue2",lwd=3)
lines(Lc(mli.iqi.08)$p,Lc(mli.iqi.08)$L,col="firebrick1",lwd=3,lty=3)
lines(Lc(mli.sample.08)$p,Lc(mli.sample.08)$L,col="dodgerblue2",lty=3,lwd=3)
dev.off()
cairo_ps(paste(images_document_dir,"plotLcsc0usa.eps",sep=""),width=7,height=7,pointsize=12)
plot(Lc(usa.iqi.93),main="USA",lwd=3,xlab="u",ylab="L(u)",col="firebrick1")
for(i in 1:10){
    abline(h=i/10,col="lightgray",lty="dotted")
    abline(v=i/10,col="lightgray",lty="dotted")  
    }
lines(Lc(usa.sample.93)$p,Lc(usa.sample.93)$L,col="dodgerblue2",lwd=3)
lines(Lc(usa.iqi.08)$p,Lc(usa.iqi.08)$L,col="firebrick1",lwd=3,lty=3)
lines(Lc(usa.sample.08)$p,Lc(usa.sample.08)$L,col="dodgerblue2",lty=3,lwd=3)
dev.off()
cairo_ps(paste(images_document_dir,"plotLcsc0mar.eps",sep=""),width=7,height=7,pointsize=12)
plot(Lc(mar.iqi.93),main="Morocco",lwd=3,xlab="u",ylab="L(u)",col="firebrick1")
for(i in 1:10){
    abline(h=i/10,col="lightgray",lty="dotted")
    abline(v=i/10,col="lightgray",lty="dotted")  
    }
lines(Lc(mar.sample.93)$p,Lc(mar.sample.93)$L,col="dodgerblue2",lwd=3)
lines(Lc(mar.iqi.08)$p,Lc(mar.iqi.08)$L,col="firebrick1",lwd=3,lty=3)
lines(Lc(mar.sample.08)$p,Lc(mar.sample.08)$L,col="dodgerblue2",lty=3,lwd=3)
dev.off()
cairo_ps(paste(images_document_dir,"plotLcsc0legend.eps",sep=""),width=7,height=7,pointsize=12)
plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(0,100),ylim=c(0,100))
text(x=50,y=95,labels="Legend",cex=3)
legend(x=15,y=85,legend=c("1993-IQI","1993-MI","2008-IQI","2008-MI"),col=c("firebrick1","dodgerblue2","firebrick1","dodgerblue2"),lty=c(1,1,3,3),lwd=3,cex=3)
dev.off()


######Plot 2: Error distribution for baseline estimates against WB Gini and Top 10% shares
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
set.seed(12345)
all93t1 <- countries.MI.est(cnts=cnts9308,year=1993,t=1,M=50,intervals=TRUE,interval.bounds=c(0,1),trim.means=TRUE)
all93t1$gini_ref93 <- wdi_ginis[match(all93t1$contcod,wdi_ginis$contcod),"ref93"]/100
all93t1$t10_ref93 <- wdi_top10[match(all93t1$contcod,wdi_top10$contcod),"ref93"]/100

all08t1 <- countries.MI.est(cnts=cnts9308,year=2008,t=1,M=50,intervals=TRUE,interval.bounds=c(0,1),trim.means=TRUE)
all08t1$gini_ref08 <- wdi_ginis[match(all08t1$contcod,wdi_ginis$contcod),"ref08"]/100
all08t1$t10_ref08 <- wdi_top10[match(all08t1$contcod,wdi_top10$contcod),"ref08"]/100
all08t1$gini_error <- all08t1$'Gini-MI'-all08t1$gini_ref08
all08t1$t10_error <- all08t1$'Top 10%-MI'-all08t1$t10_ref08
all93t1$gini_error <- all93t1$'Gini-MI'-all93t1$gini_ref93
all93t1$t10_error <- all93t1$'Top 10%-MI'-all93t1$t10_ref93

all93t1[,'contcod'][all93t1$gini_error<=-.05 | all93t1$gini_error>=0.05]
all08t1[,'contcod'][all08t1$gini_error<=-.05 | all08t1$gini_error>=0.05]
all93t1[,'contcod'][all93t1$t10_error<=-.05 | all93t1$t10_error>=0.05]
all08t1[,'contcod'][all08t1$t10_error<=-.05 | all08t1$t10_error>=0.05]

cairo_ps(paste(images_document_dir,"plotWBdiscs93.eps",sep=""),width=7,height=7,pointsize=12)
plot(density(all93t1$gini_error,na.rm=TRUE),xlab="Discrepancy (p.p.)",main="(1993)",lwd=2,ylim=c(0,50),col="firebrick1")
lines(density(all93t1$t10_error,na.rm=TRUE),col="dodgerblue2",lwd=2)
legend(x=-.08,y=47,legend=c(paste("Gini (N=",sum(is.na(all93t1$gini_error)==FALSE),")",sep=""),paste("Top 10% share (N=",sum(is.na(all93t1$t10_error)==FALSE),")",sep="")),col=c("firebrick1","dodgerblue2"),lwd=c(2,2))
dev.off()
cairo_ps(paste(images_document_dir,"plotWBdiscs08.eps",sep=""),width=7,height=7,pointsize=12)
plot(density(all08t1$gini_error,na.rm=TRUE),xlab="Discrepancy (p.p.)",main="(2008)",lwd=2,ylim=c(0,37),col="firebrick1")
lines(density(all08t1$t10_error,na.rm=TRUE),col="dodgerblue2",lwd=2)
legend(x=-.088,y=35,legend=c(paste("Gini (N=",sum(is.na(all08t1$gini_error)==FALSE),")",sep=""),paste("Top 10% share (N=",sum(is.na(all08t1$t10_error)==FALSE),")",sep="")),col=c("firebrick1","dodgerblue2"),lwd=c(2,2))
dev.off()

######Plot 3: Baseline GID KDE (one sample for each year)
GIDsample93t1 <- density(log(as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=1,data.table=FALSE)))))
GIDsample08t1 <- density(log(as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=1,data.table=FALSE)))))
cairo_ps(paste(images_document_dir,"plotbaseGID.eps",sep=""),width=7,height=7,pointsize=12)
plot(GIDsample93t1,lwd=2,main="GID density estimates (1993-2008)",xlab="Log of incomes",col="dodgerblue2")
lines(GIDsample08t1,col="firebrick1",lwd=2)
legend(x=6.475,y=0.46,legend=c(paste("1993 - (N=",GIDsample93t1$n,", bw=",round(GIDsample93t1$bw,digits=3),")",sep=""),paste("2008 - (N=",GIDsample08t1$n,", bw=",round(GIDsample08t1$bw,digits=3),")",sep="")),col=c("dodgerblue2","firebrick1"),lwd=c(2,2))
dev.off()

######Table 3: Income inequality measures under scenario I
MI.stats.tables(paste(output_scenario1_dir,"GID93.t1.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID93.t995.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID93.t99.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID93.t985.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID93.t98.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID93.t95.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID93.t90.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]

MI.stats.tables(paste(output_scenario1_dir,"GID08.t1.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID08.t995.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID08.t99.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID08.t985.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID08.t98.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID08.t95.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario1_dir,"GID08.t90.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]

#######Plot: Lorenz curves for corrected vs non corrected GIDs
GIDsample93t1 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=1,data.table=FALSE)))
GIDsample93t12 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=2,data.table=FALSE)))
GIDsample93t13 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=3,data.table=FALSE)))
GIDsample93t14 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=4,data.table=FALSE)))
GIDsample93t15 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=5,data.table=FALSE)))
GIDsample93t16 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=6,data.table=FALSE)))
GIDsample93t17 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=7,data.table=FALSE)))
GIDsample93t18 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=8,data.table=FALSE)))
GIDsample93t19 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=9,data.table=FALSE)))
GIDsample93t110 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=10,data.table=FALSE)))
GIDsample93t95 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=1,data.table=FALSE)))
GIDsample93t952 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=2,data.table=FALSE)))
GIDsample93t953 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=3,data.table=FALSE)))
GIDsample93t954 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=4,data.table=FALSE)))
GIDsample93t955 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=5,data.table=FALSE)))
GIDsample93t956 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=6,data.table=FALSE)))
GIDsample93t957 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=7,data.table=FALSE)))
GIDsample93t958 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=8,data.table=FALSE)))
GIDsample93t959 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=9,data.table=FALSE)))
GIDsample93t9510 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=10,data.table=FALSE)))

GIDsample08t1 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=1,data.table=FALSE)))
GIDsample08t12 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=2,data.table=FALSE)))
GIDsample08t13 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=3,data.table=FALSE)))
GIDsample08t14 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=4,data.table=FALSE)))
GIDsample08t15 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=5,data.table=FALSE)))
GIDsample08t16 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=6,data.table=FALSE)))
GIDsample08t17 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=7,data.table=FALSE)))
GIDsample08t18 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=8,data.table=FALSE)))
GIDsample08t19 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=9,data.table=FALSE)))
GIDsample08t1 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=10,data.table=FALSE)))
GIDsample08t95 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=1,data.table=FALSE)))
GIDsample08t952 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=2,data.table=FALSE)))
GIDsample08t953 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=3,data.table=FALSE)))
GIDsample08t954 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=4,data.table=FALSE)))
GIDsample08t955 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=5,data.table=FALSE)))
GIDsample08t956 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=6,data.table=FALSE)))
GIDsample08t957 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=7,data.table=FALSE)))
GIDsample08t958 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=8,data.table=FALSE)))
GIDsample08t959 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=9,data.table=FALSE)))
GIDsample08t9510 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=10,data.table=FALSE)))

cairo_ps(paste(images_document_dir,"Lc08scenario1.eps",sep=""),width=7,height=7,pointsize=12)
plot(Lc(GIDsample08t1),lwd=3,xlab="u",ylab="L(u)",main="(2008)",col="firebrick1")
for(i in 1:10){
    abline(h=i/10,col="lightgray",lty="dotted")
    abline(v=i/10,col="lightgray",lty="dotted")  
}
lines(Lc(GIDsample08t12),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t13),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t14),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t15),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t16),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t17),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t18),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t19),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t110),col="firebrick1",lwd=1)
lines(Lc(GIDsample08t95),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t952),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t953),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t954),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t955),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t956),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t957),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t958),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t959),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample08t9510),col="dodgerblue2",lwd=1)
legend(x=0,y=0.8,legend=c("t=1","t=.95"),col=c("firebrick1","dodgerblue2"),lty=c(1,1))
dev.off()
cairo_ps(paste(images_document_dir,"Lc93scenario1.eps",sep=""),width=7,height=7,pointsize=12)
plot(Lc(GIDsample93t1),lwd=3,xlab="u",ylab="L(u)",main="(1993)",col="firebrick1")
for(i in 1:10){
    abline(h=i/10,col="lightgray",lty="dotted")
    abline(v=i/10,col="lightgray",lty="dotted")  
    }
lines(Lc(GIDsample93t12),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t13),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t14),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t15),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t16),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t17),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t18),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t19),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t110),col="firebrick1",lwd=1)
lines(Lc(GIDsample93t95),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t952),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t953),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t954),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t955),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t956),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t957),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t958),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t959),col="dodgerblue2",lwd=1)
lines(Lc(GIDsample93t9510),col="dodgerblue2",lwd=1)
legend(x=0,y=0.8,legend=c("t=1","t=.95"),col=c("firebrick1","dodgerblue2"),lty=c(1,1))
dev.off()


######Plot 4: GIC under Scenario I
GID.93.t1 <- MI.stats.tables(file=paste(output_scenario1_dir,"GID93.t1.txtstats.RData",sep=""),trims.stats=c(0.2,0.8),trims.pmeans=c(0,1))
GID.08.t1 <- MI.stats.tables(file=paste(output_scenario1_dir,"GID08.t1.txtstats.RData",sep=""),trims.stats=c(0.2,0.8),trims.pmeans=c(0,1))
GID.93.t985 <- MI.stats.tables(file=paste(output_scenario1_dir,"GID93.t985.txtstats.RData",sep=""),trims.stats=c(0.2,0.8),trims.pmeans=c(0,1))
GID.08.t985 <- MI.stats.tables(file=paste(output_scenario1_dir,"GID08.t985.txtstats.RData",sep=""),trims.stats=c(0.2,0.8),trims.pmeans=c(0,1))
GID.93.t95 <- MI.stats.tables(file=paste(output_scenario1_dir,"GID93.t95.txtstats.RData",sep=""),trims.stats=c(0.2,0.8),trims.pmeans=c(0,1))
GID.08.t95 <- MI.stats.tables(file=paste(output_scenario1_dir,"GID08.t95.txtstats.RData",sep=""),trims.stats=c(0.2,0.8),trims.pmeans=c(0,1))

GIC.9308.t1 <- cbind(GID.93.t1$pmeans[,1],(GID.08.t1$pmeans[,2]/GID.93.t1$pmeans[,2])-1)
GIC.9308.t1.lb <- (GID.08.t1$pmeans[,4]/GID.93.t1$pmeans[,5])-1
GIC.9308.t1.ub <- (GID.08.t1$pmeans[,5]/GID.93.t1$pmeans[,4])-1

GIC.9308.t985 <- cbind(GID.93.t985$pmeans[,1],(GID.08.t985$pmeans[,2]/GID.93.t985$pmeans[,2])-1)
GIC.9308.t985.lb <- (GID.08.t985$pmeans[,4]/GID.93.t985$pmeans[,5])-1
GIC.9308.t985.ub <- (GID.08.t985$pmeans[,5]/GID.93.t985$pmeans[,4])-1

GIC.9308.t95 <- cbind(GID.93.t95$pmeans[,1],(GID.08.t95$pmeans[,2]/GID.93.t95$pmeans[,2])-1)
GIC.9308.t95.lb <- (GID.08.t95$pmeans[,4]/GID.93.t95$pmeans[,5])-1
GIC.9308.t95.ub <- (GID.08.t95$pmeans[,5]/GID.93.t95$pmeans[,4])-1

cairo_ps(paste(images_document_dir,"GIC1.eps",sep=""),width=12,height=8,pointsize=12)
plot(GIC.9308.t1[,1],GIC.9308.t1[,2],type="o",xlab="GID Percentile",ylab="Growth rate",main="GIC 1993-2008",lwd=2,col="firebrick1")
lines(GIC.9308.t1[,1],GIC.9308.t1.lb,lty=2,col="firebrick1",lwd=2)
lines(GIC.9308.t1[,1],GIC.9308.t1.ub,lty=2,col="firebrick1",lwd=2)
lines(GIC.9308.t985[,1],GIC.9308.t985[,2],type="o",col="blue3",lwd=2)
lines(GIC.9308.t985[,1],GIC.9308.t985.lb,lty=2,col="blue3",lwd=2)
lines(GIC.9308.t985[,1],GIC.9308.t985.ub,lty=2,col="blue3",lwd=2)
lines(GIC.9308.t95[,1],GIC.9308.t95[,2],type="o",col="dodgerblue2",lwd=2)
lines(GIC.9308.t95[,1],GIC.9308.t95.lb,lty=2,col="dodgerblue2",lwd=2)
lines(GIC.9308.t95[,1],GIC.9308.t95.ub,lty=2,col="dodgerblue2",lwd=2)
for(i in 1:7){
abline(h=seq(.2,.8,.1)[i],col="lightgray",lty="dotted")
}
for(i in 1:9){
abline(v=seq(.1,1,.1)[i],col="lightgray",lty="dotted")
}           
legend(x=0.05,y=0.85,legend=c("t=1","t=0.985","t=0.95"),col=c("firebrick1","blue3","dodgerblue2"),lty=c(1,1,1),lwd=c(2,2,2))
dev.off()

######Plot 5: Scenario II priors
cairo_ps(paste(images_document_dir,"betas.eps",sep=""),width=7,height=7,pointsize=12)
curve(dbeta(x,shape1=2,shape2=2),from=0,to=1,xaxt='n',col="firebrick1",ylim=c(0,2.8),lwd=2,main="Beta prior assumptions on t",ylab="Density",xlab="t")
curve(dbeta(x,shape1=2,shape2=4),from=0,to=1,xaxt='n',add=TRUE,col="dodgerblue2",lwd=2)
curve(dbeta(1-x,shape1=2,shape2=4),from=0,to=1,xaxt='n',add=TRUE,col="darkolivegreen3",lwd=2)
legend(x=-0.035,y=2.9,legend=c(expression(paste(tilde(t[j])%~%"Beta(2,2)",sep="")),expression(paste(tilde(t[j])%~%"Beta(2,4)",sep="")),expression(paste(1-tilde(t[j])%~%"Beta(2,4)",sep=""))),cex=0.9,col=c("firebrick1","dodgerblue2","darkolivegreen3"),lwd=c(2,2,2))
axis(1,at=c(0,0.5,1),labels=c("0.95","0.9745","0.999"))
dev.off()

######Table 4: Income inequality measures under scenario II
MI.stats.tables(paste(output_scenario1_dir,"GID93.t1.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario2_dir,"GID93.t.sym.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario2_dir,"GID93.t.pskew.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario2_dir,"GID93.t.nskew.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.8,.8),digits=4)$stats[,c(2,3)]

MI.stats.tables(paste(output_scenario1_dir,"GID08.t1.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.2,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario2_dir,"GID08.t.sym.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.2,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario2_dir,"GID08.t.pskew.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.2,.8),digits=4)$stats[,c(2,3)]
MI.stats.tables(paste(output_scenario2_dir,"GID08.t.nskew.txtstats.RData",sep=""),trims.pmeans=c(.05,.95),trims.stats=c(.2,.8),digits=4)$stats[,c(2,3)]

######Plot 6: GIC under Scenario II
GID.93.t1 <- MI.stats.tables(file=paste(output_scenario1_dir,"GID93.t1.txtstats.RData",sep=""),trims.stats=c(0.8,0.8),trims.pmeans=c(0,1))
GID.08.t1 <- MI.stats.tables(file=paste(output_scenario1_dir,"GID08.t1.txtstats.RData",sep=""),trims.stats=c(0.8,0.8),trims.pmeans=c(0,1))
GID.93.t.sym <- MI.stats.tables(file=paste(output_scenario2_dir,"GID93.t.sym.txtstats.RData",sep=""),trims.stats=c(0.8,0.8),trims.pmeans=c(0,1))
GID.08.t.sym <- MI.stats.tables(file=paste(output_scenario2_dir,"GID08.t.sym.txtstats.RData",sep=""),trims.stats=c(0.8,0.8),trims.pmeans=c(0,1))
GID.93.t.pskew <- MI.stats.tables(file=paste(output_scenario2_dir,"GID93.t.pskew.txtstats.RData",sep=""),trims.stats=c(0.8,0.8),trims.pmeans=c(0,1))
GID.08.t.pskew <- MI.stats.tables(file=paste(output_scenario2_dir,"GID08.t.pskew.txtstats.RData",sep=""),trims.stats=c(0.8,0.8),trims.pmeans=c(0,1))
GID.93.t.nskew <- MI.stats.tables(file=paste(output_scenario2_dir,"GID93.t.nskew.txtstats.RData",sep=""),trims.stats=c(0.8,0.8),trims.pmeans=c(0,1))
GID.08.t.nskew <- MI.stats.tables(file=paste(output_scenario2_dir,"GID08.t.nskew.txtstats.RData",sep=""),trims.stats=c(0.8,0.8),trims.pmeans=c(0,1))

GIC.9308.t1 <- cbind(GID.93.t1$pmeans[,1],(GID.08.t1$pmeans[,2]/GID.93.t1$pmeans[,2])-1)
GIC.9308.t1.lb <- (GID.08.t1$pmeans[,4]/GID.93.t1$pmeans[,5])-1
GIC.9308.t1.ub <- (GID.08.t1$pmeans[,5]/GID.93.t1$pmeans[,4])-1

GIC.9308.t.sym <- cbind(GID.93.t.sym$pmeans[,1],(GID.08.t.sym$pmeans[,2]/GID.93.t.sym$pmeans[,2])-1)
GIC.9308.t.sym.lb <- (GID.08.t.sym$pmeans[,4]/GID.93.t.sym$pmeans[,5])-1
GIC.9308.t.sym.ub <- (GID.08.t.sym$pmeans[,5]/GID.93.t.sym$pmeans[,4])-1

GIC.9308.t.pskew <- cbind(GID.93.t.pskew$pmeans[,1],(GID.08.t.pskew$pmeans[,2]/GID.93.t.pskew$pmeans[,2])-1)
GIC.9308.t.pskew.lb <- (GID.08.t.pskew$pmeans[,4]/GID.93.t.pskew$pmeans[,5])-1
GIC.9308.t.pskew.ub <- (GID.08.t.pskew$pmeans[,5]/GID.93.t.pskew$pmeans[,4])-1

GIC.9308.t.nskew <- cbind(GID.93.t.nskew$pmeans[,1],(GID.08.t.nskew$pmeans[,2]/GID.93.t.nskew$pmeans[,2])-1)
GIC.9308.t.nskew.lb <- (GID.08.t.nskew$pmeans[,4]/GID.93.t.nskew$pmeans[,5])-1
GIC.9308.t.nskew.ub <- (GID.08.t.nskew$pmeans[,5]/GID.93.t.nskew$pmeans[,4])-1

cairo_ps(paste(images_document_dir,"GIC2.eps",sep=""),width=12,height=8,pointsize=12)
plot(GIC.9308.t1[,1],GIC.9308.t1[,2],type="o",xlab="GID Percentile",ylab="Growth rate",main="GIC 1993-2008",lwd=2,col="gray23",ylim=c(0,0.95))
lines(GIC.9308.t1[,1],GIC.9308.t1.lb,lty=2,col="gray23",lwd=2)
lines(GIC.9308.t1[,1],GIC.9308.t1.ub,lty=2,col="gray23",lwd=2)
lines(GIC.9308.t.sym[,1],GIC.9308.t.sym[,2],type="o",col="firebrick1",lwd=2)
lines(GIC.9308.t.sym[,1],GIC.9308.t.sym.lb,lty=2,col="firebrick1",lwd=2)
lines(GIC.9308.t.sym[,1],GIC.9308.t.sym.ub,lty=2,col="firebrick1",lwd=2)
lines(GIC.9308.t.pskew[,1],GIC.9308.t.pskew[,2],type="o",col="dodgerblue2",lwd=2)
lines(GIC.9308.t.pskew[,1],GIC.9308.t.pskew.lb,lty=2,col="dodgerblue2",lwd=2)
lines(GIC.9308.t.pskew[,1],GIC.9308.t.pskew.ub,lty=2,col="dodgerblue2",lwd=2)
lines(GIC.9308.t.nskew[,1],GIC.9308.t.nskew[,2],type="o",col="darkolivegreen3",lwd=2)
lines(GIC.9308.t.nskew[,1],GIC.9308.t.nskew.lb,lty=2,col="darkolivegreen3",lwd=2)
lines(GIC.9308.t.nskew[,1],GIC.9308.t.nskew.ub,lty=2,col="darkolivegreen3",lwd=2)
for(i in 1:9){
abline(h=seq(.1,.9,.1)[i],col="lightgray",lty="dotted")
}
for(i in 1:9){
abline(v=seq(.1,1,.1)[i],col="lightgray",lty="dotted")
}           
legend(x=0.05,y=0.85,legend=c("t=1",expression(paste(tilde(t[j])%~%"Beta(2,2)",sep="")),expression(paste(tilde(t[j])%~%"Beta(2,4)",sep="")),expression(paste(1-tilde(t[j])%~%"Beta(2,4)",sep=""))),col=c("gray23","firebrick1","dodgerblue2","darkolivegreen3"),lty=c(1,1,1,1),lwd=c(2,2,2,2))
dev.off()


######PLOT: Density examples for GIC1
GIDsample93t1 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t1.txt",select=1,data.table=FALSE)))
GIDsample93t985 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t985.txt",select=1,data.table=FALSE)))
GIDsample93t95 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID93.t95.txt",select=1,data.table=FALSE)))
GIDsample08t1 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t1.txt",select=1,data.table=FALSE)))
GIDsample08t985 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t985.txt",select=1,data.table=FALSE)))
GIDsample08t95 <- as.numeric(unlist(fread("/home/mathias/Documents/Maestria/AMSE M2/tesis/GID08.t95.txt",select=1,data.table=FALSE)))

refkde <- density(log(GIDsample93t1))

cairo_ps(paste(images_document_dir,"GIC1kd93.eps",sep=""),width=14,height=8,pointsize=12)
plot(refkde,col="firebrick1",lty=1,main="(1993)",lwd=2,xlim=c(2.5,12),xlab="Log of incomes")
lines(density(log(GIDsample93t985),bw=refkde$bw),col="blue3",lty=2,lwd=2)
lines(density(log(GIDsample93t95),bw=refkde$bw),col="dodgerblue2",lty=2,lwd=2)
abline(v=log(quantile(GIDsample93t1,.05)),col="firebrick1",lty=4,lwd=2)
abline(v=log(quantile(GIDsample93t985,.05)),col="blue3",lwd=2,lty=4)
abline(v=log(quantile(GIDsample93t95,.05)),col="dodgerblue2",lty=4,lwd=2)
abline(v=log(quantile(GIDsample93t1,.6)),col="firebrick1",lty=4,lwd=2)
abline(v=log(quantile(GIDsample93t985,.6)),col="blue3",lwd=2,lty=4)
abline(v=log(quantile(GIDsample93t95,.6)),col="dodgerblue2",lty=4,lwd=2)
abline(v=log(quantile(GIDsample93t1,.99)),col="firebrick1",lty=4,lwd=2)
abline(v=log(quantile(GIDsample93t985,.99)),col="blue3",lwd=2,lty=4)
abline(v=log(quantile(GIDsample93t95,.99)),col="dodgerblue2",lty=4,lwd=2)
text(x=log(quantile(GIDsample93t1,.05))-0.1,y=0.4,labels=".05",srt=90)
text(x=log(quantile(GIDsample93t1,.6))-0.1,y=0.4,labels=".60",srt=90)
text(x=log(quantile(GIDsample93t1,.99))-0.1,y=0.4,labels=".99",srt=90)
legend(x=2.2,y=0.45,legend=c("t=1","t=.985","t=.95"),col=c("firebrick1","blue3","dodgerblue2"),lty=c(1,2,2),lwd=c(2,2,2))
dev.off()
cairo_ps(paste(images_document_dir,"GIC1kd08.eps",sep=""),width=14,height=8,pointsize=12)
plot(density(log(GIDsample08t1),bw=refkde$bw),col="firebrick1",lty=1,main="(2008)",lwd=2,xlim=c(2.5,12),xlab=paste("Bandwidth=",round(refkde$bw,digits=4),sep=""))
lines(density(log(GIDsample08t985),bw=refkde$bw),col="blue3",lty=2,lwd=2)
lines(density(log(GIDsample08t95),bw=refkde$bw),col="dodgerblue2",lty=2,lwd=2)
abline(v=log(quantile(GIDsample08t1,.05)),col="firebrick1",lty=4,lwd=2)
abline(v=log(quantile(GIDsample08t985,.05)),col="blue3",lty=4,lwd=2)
abline(v=log(quantile(GIDsample08t95,.05)),col="dodgerblue2",lty=4,lwd=2)

abline(v=log(quantile(GIDsample08t1,.6)),col="firebrick1",lty=4,lwd=2)
abline(v=log(quantile(GIDsample08t985,.6)),col="blue3",lwd=2,lty=4)
abline(v=log(quantile(GIDsample08t95,.6)),col="dodgerblue2",lty=4,lwd=2)
abline(v=log(quantile(GIDsample08t1,.99)),col="firebrick1",lty=4,lwd=2)
abline(v=log(quantile(GIDsample08t985,.99)),col="blue3",lwd=2,lty=4)
abline(v=log(quantile(GIDsample08t95,.99)),col="dodgerblue2",lty=4,lwd=2)
text(x=log(quantile(GIDsample08t1,.05))-0.1,y=0.32,labels=".05",srt=90)
text(x=log(quantile(GIDsample08t1,.6))-0.1,y=0.32,labels=".60",srt=90)
text(x=log(quantile(GIDsample08t1,.99))-0.1,y=0.32,labels=".99",srt=90)
legend(x=2.2,y=0.35,legend=c("t=1","t=.985","t=.95"),col=c("firebrick1","blue3","dodgerblue2"),lty=c(1,2,2),lwd=c(2,2,2))
dev.off()
