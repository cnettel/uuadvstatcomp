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

sir <- function (name="SIR", dir=getwd(), sampling="MC", nparams, true_center, true_cov, prop_center, prop_cov, nit, m, n, parallel=FALSE, seed=123, write=TRUE) {
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
#   parallel: use parallel computation (logical, length 1) 
#   seed: specify random seed (integer, length 1)   
#   write: print results (logical, length 1)   
  
### FUNCTION TEST ##
#     name <- "test"
#     dir  <- getwd() 
#     sampling <- "MC"
#     nparams <- 1
#     true_center <- 0
#     true_cov <- as.matrix(1)
#     prop_center <- 0 
#     prop_cov <- as.matrix(1)
#     nit <- 1 
#     m <- 1000
#     n <- 100
#     parallel <- FALSE
#     seed <- 123  
#     write <- FALSE
###################  
  
require(mvtnorm)
require(lhs)
  
set.seed(seed)
fulldir <- paste(dir,name,sep="/")
if(write==TRUE) {dir.create(fulldir)}

### Create stats of proposal distribution

stats_distrib           <- suppressWarnings(data.frame(rbind(prop_center,diag(prop_cov),prop_cov,rep(NA,nparams),rep(NA,nparams))))
names(stats_distrib)    <- paste0("param",seq(nparams))
stats_distrib$Variable  <- c("CENTER","VAR",rep("COV",nparams),"P975","P025")   
stats_distrib$Iteration <- 0
all_sim                 <- matrix(0,ncol=nparams+6,nrow=sum(m),dimnames=list(NULL,c(paste0("param",seq(nparams)),"vec","iteration","dmv_true","dmv_prop","ir","sir")))  # sim over all iterations

### Loop SIR iterations

for (i in seq(nit)) {
  
  sim     <- matrix(0,ncol=nparams,nrow=m[i]) # table with parameter vectors for one iteration
    
  # 1a. Draw samples from multivariate normal distribution using MONTE-CARLO SAMPLING   
  
if (sampling=="MC") {
  sim <- rmvnorm(m[i], prop_center, prop_cov)  # ncol=nparams, nrows=m[i] (1 parameter vector=1 row) 
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

# Compute their IR (not in parallel - using apply)
  
dmv_true  <- apply(as.matrix(sim[,-c(ncol(sim),ncol(sim)-1)]), 1, dmvnorm,mean=true_center,sigma=true_cov)
dmv_prop  <- apply(as.matrix(sim[,-c(ncol(sim),ncol(sim)-1)]), 1, dmvnorm,mean=prop_center,sigma=prop_cov)
ir        <- dmv_true/dmv_prop
sim       <- cbind(sim,dmv_true,dmv_prop,ir)

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


