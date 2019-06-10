nlst.gb2 <- function(x,theta.in,t){
#x is a list containing non-cumulative decile mean incomes (dmeans), decile population (sizes), and mean income
h.theta <- function(theta){
#Restriction on 'a' parameter to be larger than 1 (otherwise no 'lump')        
theta[1] <- 1+(theta[1]**2)
#Mean existence restriction
if(theta[1]*theta[3]<=1 | (-theta[1]*theta[2])>=1){
return(Inf)
}
#Restriction on 'q' parameter to be 1
#theta[3] <- 1
#Restriction on 'p' parameter to be 1
#theta[2] <- 1
#Restriction on 'a', 'p', and 'q' to be larger than 1
#theta <- 1+(theta**2)    
#Fit including endpoint 1
sum((cumsum(x$dmeans*x$sizes)/sum(x$sizes*x$dmeans)-gb2.glc(r=(cumsum(x$sizes)/sum(x$sizes))*t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE)/gb2.glc(r=t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE))**2)
#Without forcing endpoint 1
#sum((cumsum(x$dmeans*x$sizes)[1:9]/sum(x$sizes*x$dmeans)-gb2.glc(r=(cumsum(x$sizes[1:9])/sum(x$sizes))*t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE)/gb2.glc(r=t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE))**2)
#sum((cumsum(x$dmeans*x$sizes)[1:9]/sum(x$sizes*x$dmeans)-gb2.glc(r=cumsum(x$dmeans*x$sizes)[1:9]/sum(x$sizes*x$dmeans)*t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE)/gb2.glc(r=t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE))**2)    
}
ests1 <- optim(par=theta.in[c(1,3,4)],fn=h.theta)$par
#No restriction:
#out <- c(ests1[1],x[[3]]*beta(ests1[2],ests1[3])/beta(ests1[2]+1/(ests1[1]),ests1[3]-1/(ests1[1])),ests1[2],ests1[3])
#Restriction on 'a' parameter to be larger than 1 (otherwise no 'lump'):        
out <- c(1+(ests1[1]**2),(x[[3]]*t*beta(ests1[2],ests1[3]))/(beta(ests1[2]+1/(1+(ests1[1]**2)),ests1[3]-1/(1+(ests1[1]**2)))*gb2.glc(r=t,shape1=(1+(ests1[1]**2)),scale=100,shape2=ests1[2],shape3=ests1[3],GLC=FALSE)),ests1[2],ests1[3])
#Restriction on all shape parameters to be larger than 1:
#out <- c(1+(ests1[1]**2),x[[3]]*beta((1+(ests1[2]**2)),(1+(ests1[3]**2)))/beta((1+(ests1[2]**2))+1/(1+(ests1[1]**2)),(1+(ests1[3]**2))-1/(1+(ests1[1]**2))),(1+(ests1[2]**2)),(1+(ests1[3]**2))) 
return(list("Parameters"=out,"Gini"=gini.gb2(shape1=out[1],shape2=out[3],shape3=out[4]),"Mean"=moment.gb2(k=1,shape1=out[1],scale=out[2],shape2=out[3],shape3=out[4])))
}        


#################
#OLD CODE########
#################
#nlst.gb2 <- function(x,theta.in,t){
#x is a list containing non-cumulative decile mean incomes (dmeans), decile population (sizes), and mean income
#h.theta <- function(theta){
#Restriction on 'a' parameter to be larger than 1 (otherwise no 'lump')        
#theta[1] <- 1+(theta[1]**2)
#Restriction on 'q' parameter to be 1
#theta[3] <- 1
#Restriction on 'p' parameter to be 1
#theta[2] <- 1
#Restriction on 'a', 'p', and 'q' to be larger than 1
#theta <- 1+(theta**2)    
#Fit including endpoint 1
#sum((cumsum(x$dmeans*x$sizes)/sum(x$sizes*x$dmeans)-gb2.glc(r=(cumsum(x$sizes)/sum(x$sizes))*t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE)/gb2.glc(r=t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE))**2)
#Without forcing endpoint 1
#sum((cumsum(x$dmeans*x$sizes)[1:9]/sum(x$sizes*x$dmeans)-gb2.glc(r=(cumsum(x$sizes[1:9])/sum(x$sizes))*t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE)/gb2.glc(r=t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE))**2)
#sum((cumsum(x$dmeans*x$sizes)[1:9]/sum(x$sizes*x$dmeans)-gb2.glc(r=cumsum(x$dmeans*x$sizes)[1:9]/sum(x$sizes*x$dmeans)*t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE)/gb2.glc(r=t,shape1=theta[1],scale=theta.in[2],shape2=theta[2],shape3=theta[3],GLC=FALSE))**2)    
#}
#ests1 <- optim(par=theta.in[c(1,3,4)],fn=h.theta)$par
#No restriction:
#out <- c(ests1[1],x[[3]]*beta(ests1[2],ests1[3])/beta(ests1[2]+1/(ests1[1]),ests1[3]-1/(ests1[1])),ests1[2],ests1[3])
#Restriction on 'a' parameter to be larger than 1 (otherwise no 'lump'):        
#out <- c(1+(ests1[1]**2),x[[3]]*beta(ests1[2],ests1[3])/beta(ests1[2]+1/(1+(ests1[1]**2)),ests1[3]-1/(1+(ests1[1]**2))),ests1[2],ests1[3])
#Restriction on all parameters to be larger than 1:
#out <- c(1+(ests1[1]**2),x[[3]]*beta((1+(ests1[2]**2)),(1+(ests1[3]**2)))/beta((1+(ests1[2]**2))+1/(1+(ests1[1]**2)),(1+(ests1[3]**2))-1/(1+(ests1[1]**2))),(1+(ests1[2]**2)),(1+(ests1[3]**2))) 
#return(list("Parameters"=out,"Gini"=gini.gb2(shape1=out[1],shape2=out[3],shape3=out[4]),"Mean"=moment.gb2(k=1,shape1=out[1],scale=out[2],shape2=out[3],shape3=out[4])))
#}        
