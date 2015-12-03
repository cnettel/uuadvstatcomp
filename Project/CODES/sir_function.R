##################################################################################################
### Advanced Statistical computing course
### Project code
### Author: Anne-Gaelle Dosne
### Date: November 2015
##################################################################################################

### Comments #####################################################################################
### WHAT THIS CODE DOES: define SIR function
### INPUT: see arguments (true, proposal, SIR settimgs)
### OUTPUT: list of 2 : 
### - all_sim: dataframe with all sampled and resampleds parameters and their IR per iteration  
### - stats_distrib: dataframe with posterior mean, variance, CI95% and covmat + same for proposal for all iterations  
##################################################################################################


sir <- function (name="SIR", dir=getwd(), sampling="MC", nparams, true_center, true_cov, prop_center, prop_cov, nit, m, n, par=0, nodes=1, seed=123, write=TRUE) {
#   name: name of problem (character, length 1) 
#   dir: working directory (character, length 1) 
#   sampling: type of sampling, "MC" or "LHS" (character, length 1) 
#   nparams: dimensionality (integer, length 1) 
#   true_center: center of true distribution (numeric vector, length nparams) 
#   true_cov: covariance matrix of true distribution (positive semi-definite symmetric matrix, dimension nparams x nparams)
#   prop_center: center of proposal distribution (numeric vector, length nparams) 
#   prop_cov: covariance matrix of proposal distribution (positive semi-definite symmetric matrix, dimension nparams x nparams)
#   nit: number of SIR iterations to perform (integer, length 1)  
#   m: number of samples to generate for each iteration (integer vector, length nit) 
#   n: number of resamples to draw for each iteration (integer vector, length nit) 
#   par: use parallel computation (0 (no parallel), 1 (using parallel library), length 1) 
#   nodes: number of nodes to use when in parallel (integer, length 1)  
#   seed: specify random seed (integer, length 1)   
#   write: print results (logical, length 1)   
  
### FUNCTION TEST ##
#     name <- "test"
#     dir  <- getwd() 
#     sampling <- "MC"
#     nparams <- 2
#     true_center <- c(0,0)
#     true_cov <- diag(2)
#     prop_center <- c(1,1) 
#     prop_cov <- diag(2)
#     nit <- 1 
#     m <- 1000
#     n <- 100
#     par <- 1
#     nodes <- 2  
#     seed <- 123  
#     write <- FALSE
#   
#  Check bottlenecks 
#  profile <- lineprof(sir()) # groups by function, not line?
#  shine(profile)  
#  Rprof(sir())
###################  
  
### Load libraries
  
require(mvtnorm)
require(lhs)
# require(parallel)
# require(multidplyr)
  
### Define seed and output directory
  
set.seed(seed)
fulldir <- paste(dir,name,sep="/")
if(write==TRUE) {dir.create(fulldir)}

### Create stats of proposal distribution

stats_distrib           <- suppressWarnings(data.frame(rbind(prop_center,diag(prop_cov),prop_cov,rep(NA,nparams),rep(NA,nparams))))
names(stats_distrib)    <- paste0("param",seq(nparams))
stats_distrib$Variable  <- c("CENTER","VAR",rep("COV",nparams),"P975","P025")   
stats_distrib$Iteration <- 0
all_sim                 <- matrix(0,ncol=nparams+6,nrow=sum(m),dimnames=list(NULL,c(paste0("param",seq(nparams)),"vec","iteration","dmv_true","dmv_prop","ir","sir")))  # sim over all iterations

### Create function to get multivariate density, compatible with parallelization

appdv <- function(db, mean, sigma) { # create function that computes appdv for all or subsection of dataset
  y   <- apply(as.matrix(db[,-c(ncol(db),ncol(db)-1)]), 1, dmvnorm, mean=mean ,sigma=sigma) 
  return(y)
} 

appdv_true <- function(lim) { # create function that computes appdv for all or subsection of dataset
  y   <- apply(as.matrix(sim[,-c(ncol(sim),ncol(sim)-1)])[lim[1]:lim[2],], 1, dmvnorm, mean=true_center ,sigma=true_cov) 
  return(y)
} 

appdv_prop <- function(lim) { # create function that computes appdv for all or subsection of dataset
  y   <- apply(as.matrix(sim[,-c(ncol(sim),ncol(sim)-1)])[lim[1]:lim[2],], 1, dmvnorm, mean=prop_center ,sigma=prop_cov) 
  return(y)
}


### Loop SIR iterations

for (i in seq(nit)) {
  
  sim     <- matrix(0,ncol=nparams,nrow=m[i]) # table with parameter vectors for one iteration
    
  # 1a. Draw samples from multivariate normal distribution using MONTE-CARLO SAMPLING   
  
if (sampling=="MC") {
  sim     <- rmvnorm(m[i], prop_center, prop_cov)  # ncol=nparams, nrows=m[i] (1 parameter vector=1 row) 
}
  
  # 1b. Alternative 1: LATIN HYPERCUBE SAMPLING    

if (sampling=="LHS") {
    lh  <- randomLHS(m[i],nparams)                                                 # draw the hypercube
    for (j in seq(nparams)) {                                                      # loop so we can sample with different means and variances for each parameter
     sim[,j] <- qnorm(lh[,j],mean=prop_center[j],sd=sqrt(diag(prop_cov)[j]))       # transform the hypercube
    }
}
  
sim            <- cbind (sim,seq(m[i]),i)                 # add identifier for each vector
colnames(sim)  <- c(paste0("param",seq(nparams)),"vec","iteration")

# Compute their IR (in parallel or not)

if(par==0) { 
  dmv_true   <- appdv_true(c(1,nrow(sim)))
  dmv_prop   <- appdv_prop(c(1,nrow(sim)))
  }

if(par==1)  { 
#   cl         <- makePSOCKcluster(nodes)      # do it outside
#   clusterEvalQ(cl, { library(mvtnorm) })     # make sure all nodes have the right library
  clusterExport(cl=cl, varlist=c("true_center", "true_cov","prop_center", "prop_cov","sim"))
  intervals  <- round(seq(from = 1, to = nrow(sim), length.out = nodes + 1),0) # creates nodes intervals
  low        <- intervals[-length(intervals)] # create vector of min values to use as lower arguments to appdv function
  low[-1]    <- low[-1] + 1                   # all except first index need + 1
  up         <- intervals[-1]                 # create vector of max values to use as upper arguments to appdv function
  dmv_true   <- do.call(c,parLapply(cl = cl, X=Map(c,low,up) , fun = appdv_true))  
  dmv_prop   <- do.call(c,parLapply(cl = cl, X=Map(c,low,up) , fun = appdv_prop))  
  # stopCluster(cl)
}

# if(par==2)  { # using multipyr (github/hadley) but function not exported on nodes?
#   id        <- splitIndices(nrow(sim),nodes)
#   sim       <- cbind(sim,"index"=rep(seq(length(id)),lengths(id)))
#   cluster   <- create_cluster(nodes)
#   sim1      <- partition(as.data.frame(sim),cluster=cluster)
#   sim2      <- summarise(sim1, out1 = list(appdv()[[1]]))
#   sim3      <- collect(sim2)
# }        
  
  ir         <- dmv_true/dmv_prop 
  sim        <- cbind(sim,dmv_true,dmv_prop,ir)

# Resample N vectors based on IR and without replacement 

resamp    <- sample(sim[,"vec"], n[i], replace=FALSE, prob=sim[,"ir"])
sir       <- sim[,"vec"] %in% resamp   # add 0/1 column in sim (1=resampled vector)
sim       <- cbind(sim,sir)

# Compute mean and covmat for the resampled params

POST_CENTER <- unname(apply(as.matrix(sim[sim[,"sir"]==1,1:nparams]),2,mean))
POST_COV    <- cov(as.matrix(sim[sim[,"sir"]==1,1:nparams]))
POST_P975   <- unname(apply(as.matrix(sim[sim[,"sir"]==1,1:nparams]),2,quantile,p=0.975))
POST_P025   <- unname(apply(as.matrix(sim[sim[,"sir"]==1,1:nparams]),2,quantile,p=0.025))


stats_distrib.cur           <- suppressWarnings(data.frame(rbind(POST_CENTER,diag(POST_COV),POST_COV,POST_P025,POST_P975)))
names(stats_distrib.cur)    <- paste0("param",seq(nparams))
stats_distrib               <- rbind(stats_distrib,data.frame(cbind(stats_distrib.cur,
                                  "Variable"=c("CENTER","VAR",rep("COV",nparams),"P025","P975"),
                                  "Iteration"=rep(i,nparams+4))))


# Output files
if(write==TRUE) { write.csv(sim,paste0(fulldir,"/sim_iteration",i,".csv"),row.names=F)}
all_sim[(sum(m[1:i])-m[i]+1):sum(m[1:i]),] <- sim

# Reset proposal for next iteration

prop_center <- POST_CENTER # set posterior to new proposal  
prop_cov    <- POST_COV

}

if(write==TRUE) {write.csv(cbind(stats_distrib,name),paste(fulldir,"stats_distrib.csv",sep="/"),row.names=F)}

return(list("stats_distrib"=stats_distrib, "sim_all"=as.data.frame(all_sim),"name"=name))

}

### END
##################################################################################################


