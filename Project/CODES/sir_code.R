##################################################################################################
### Advanced Statistical computing course
### Project code
### Author: Anne-Gaelle Dosne
### Date: November 2015
##################################################################################################

require(mvtnorm)

### Comments #####################################################################################
### WHAT THIS CODE DOES:
### INPUT: Define variables in beginning of code below. No external file needed.
### OUTPUT: 
### Create directory NAME for one simulation problem with: 
### - 1 csv file per iteration with all sampled and resampleds parameters and their IR 
### - 1 csv file for all iterations with posterior mean, SE and covmat + same for proposal  
##################################################################################################

##################################################################################################
### Define variables
##################################################################################################

# Problem-related parameters
NAME           <- "MVN_BIASINFL1_MC_NOPAR"
DIR            <- paste("C:/Users/anndo252/uuadvstatcomp/Project/SIMULATIONS",NAME,sep="/")
NPARAMS        <- 5                                        # dimensionality 
TRUE_CENTER    <- rep(0,NPARAMS)                           # center of true distribution
TRUE_COV       <- diag(NPARAMS)                            # covariance matrix of true distribution

# Initial proposal distribution parameters
PROP_CENTER    <- rep(1,NPARAMS)   # center of proposal distribution
PROP_COV       <- matrix(0,ncol=NPARAMS,nrow=NPARAMS)      # covariance matrix of proposal distribution
diag(PROP_COV) <- rep(1*2,NPARAMS)

stats_distrib           <- data.frame(rbind(PROP_CENTER,diag(PROP_COV),PROP_COV))
names(stats_distrib)    <- paste0("param",seq(NPARAMS))
stats_distrib$Variable  <- c("CENTER","VAR",rep("COV",NPARAMS))   
stats_distrib$Iteration <- 0

# SIR procedure parameters (stay fixed here)
NIT       <- 5                # number of SIR iterations to perform
M         <- rep(5000,NIT)    # number of samples to generate for each iteration
N         <- rep(1000,NIT)    # number of resamples to draw for each iteration

##################################################################################################
### Code (no modification needed) - SIR LOOP -
##################################################################################################

set.seed(123)

### Define function outside of loop 

irfun <- function (v){  # calculate IR for one vector
  ir <- dmvnorm(v,mean=TRUE_CENTER,sigma=TRUE_COV)/dmvnorm(v,mean=PROP_CENTER,sigma=PROP_COV)
  return(ir)
}

### Loop SIR iterations

for (i in seq(NIT)) {
  
  sim <- c() # table with parameter vectors
  
  # Draw samples from multivariate normal distribution using MONTE-CARLO SAMPLING   
  
sim <- rmvnorm(M[i], PROP_CENTER, PROP_COV)  # ncol=NPARAMS, nrows=M[i] (1 parameter vector=1 row) 
sim <- cbind (sim,seq(M[i]),i)                 # add identifier for each vector
colnames(sim)  <- c(paste0("param",seq(NPARAMS)),"vec","iteration")

# Compute their IR (not in parallel - using apply)
  
ir        <- apply(sim[,-c(ncol(sim),ncol(sim)-1)], 1, irfun)
sim       <- cbind(sim,ir)

# Resample N vectors based on IR and without replacement 

resamp    <- sample(sim[,"vec"], N[i], replace=FALSE, prob=sim[,"ir"])
sir       <- sim[,"vec"] %in% resamp   # add 0/1 column in sim (1=resampled vector)
sim       <- cbind(sim,sir)

# Compute mean and covmat for the resampled params

POST_CENTER <- unname(apply(sim[sim[,"sir"]==1,1:NPARAMS],2,mean))
POST_COV    <- cov(sim[sim[,"sir"]==1,1:NPARAMS])

stats_distrib <- rbind(stats_distrib,data.frame(cbind(rbind(POST_CENTER,diag(POST_COV),POST_COV),
                                  "Variable"=c("CENTER","VAR",rep("COV",NPARAMS)),
                                  "Iteration"=rep(i,NPARAMS+2))))


# Output files
write.csv(sim,paste0(DIR,"/sim_iteration",i,".csv"),row.names=F)

# Reset proposal for next iteration

PROP_CENTER <- POST_CENTER # set posterior to new proposal  
PROP_COV    <- POST_COV

}

write.csv(cbind(stats_distrib,NAME),paste(DIR,"stats_distrib.csv",sep="/"),row.names=F)

### END
##################################################################################################


