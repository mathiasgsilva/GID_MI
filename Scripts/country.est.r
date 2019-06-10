country.est <- function(contcod,bin_year,t,theta.in.shape=c(1.5,1,1.5)){
    y <- list("dmeans"=data[data$contcod==contcod & data$bin_year==bin_year,"RRinc"],"sizes"=data[data$contcod==contcod & data$bin_year==bin_year,"pop"],"mean"=data[data$contcod==contcod & data$bin_year==bin_year,"RRmean"][1])
return(nlst.gb2(x=y,theta.in=c(theta.in.shape[1],y$mean,theta.in.shape[2],theta.in.shape[3]),t=t))
}
