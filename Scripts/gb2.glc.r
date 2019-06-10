gb2.glc <- function(r,shape1,scale,shape2,shape3,GLC=TRUE){
pars <- c(shape1,scale,shape2,shape3)
moment.gb2(GLC,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4])*incompl.gb2(qgb2(prob=r,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4]),k=1,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4])
    }

#T <- 10000
#pop_params <- c(15,100,0.8,1.5)
#data_full <- rgb2(T,shape1=pop_params[1],scale=pop_params[2],shape2=pop_params[3],shape3=pop_params[4])
#data_full = data_full[data_full<Inf]
#gb2.glc(0.7651,shape1=pop_params[1],scale=pop_params[2],shape2=pop_params[3],shape3=pop_params[4],GLC=FALSE)
#r=0.7651
#(qbeta(p=r,shape1=pop_params[3],shape2=pop_params[4])**pop_params[1])/(1+(qbeta(p=r,shape1=pop_params[3]+(1/pop_params[1]),shape2=pop_params[4]-(1/pop_params[1]))**pop_params[1]))

#sum(sort(data_full)[1:round(0.7651*length(data_full))])/sum(data_full)
#Lc(data_full)[[2]][round(0.7651*length(data_full))]

###############OLD CODE###################
#gb2.glc <- function(r,shape1,scale,shape2,shape3,GLC=TRUE){
#pars <- c(shape1,scale,shape2,shape3)
#moment.gb2(GLC,shape1=pars[1],scale=pars[2],shape2=pars[3],shape3=pars[4])*pbeta(qbeta(r,shape1=pars[3],shape2=pars[4]),shape1=pars[3]+1/pars[1],shape2=pars[4]-1/pars[1])
#    }

#Example:
#T <- 10000
#pop_params <- c(1.5,100,1,1.5)
#data_full <- rgb2(T,shape1=pop_params[1],scale=pop_params[2],shape2=pop_params[3],shape3=pop_params[4])
#Relative lorenz curve:
#gb2.glc(0.7651,shape1=pop_params[1],scale=pop_params[2],shape2=pop_params[3],shape3=pop_params[4],GLC=FALSE)
#sum(sort(data_full)[1:round(0.7651*T)])/sum(data_full)
#Lc(data_full)[[2]][7651]
#Generalized lorenz curve:
#gb2.glc(cumsum(x$sizes)/sum(x$sizes),shape1=pop_params[1],scale=pop_params[2],shape2=pop_params[3],shape3=pop_params[4],GLC=TRUE)
#cumsum(x$dmeans*x$sizes)/sum(x$sizes)

