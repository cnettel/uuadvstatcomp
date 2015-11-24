##################################################################################################
### Advanced Statistical computing course
### Project code
### Author: Anne-Gaelle Dosne
### Date: November 2015
##################################################################################################

require(mvtnorm)

### Comments #####################################################################################
### WHAT THIS CODE DOES:
### INPUT:
### OUTPUT:
##################################################################################################

### Define variables

# Problem-related parameters
NAME           <- "PROBLEM1"
NPARAMS        <- 5                                        # dimensionality 
TRUE_CENTER    <- rep(0,NPARAMS)                           # center of true distribution
TRUE_COV       <- diag(NPARAMS)                            # covariance matrix of true distribution

# Initial proposal distribution parameters
PROP_CENTER    <- rep(0,NPARAMS)   # center of proposal distribution
PROP_COV       <- matrix(0,ncol=NPARAMS,nrow=NPARAMS)      # covariance matrix of proposal distribution
diag(PROP_COV) <- rep(1,NPARAMS)

# SIR procedure parameters (stay fixed here)
M         <- 5000             # number of samples to generate
N         <- 1000             # number of resamples to draw
NIT       <- 5                # number of SIR iterations to perform

# Draw samples from multivariate normal distribution using Monte-Carlo sampling

set.seed(123)
sim <- rmvnorm(M, PROP_CENTER, PROP_COV)  # ncol=NPARAMS, nrows=M (1 parameter vector=1 row)
sim <- cbind (seq(M),sim)                 # add identifier for each vector
colnames(sim)  <- c("vec",paste0("param",seq(NPARAMS)))

# Compute their IR (not in parallel - using apply)

irfun <- function (v){  # calculate IR for one vector
  ir <- dmvnorm(v,mean=TRUE_CENTER,sigma=TRUE_COV)/dmvnorm(v,mean=PROP_CENTER,sigma=PROP_COV)
  return(ir)
}
  
irs       <- apply(sim, 1, irfun)
sim       <- cbindsim,irs)

# Resample N vectors based on IR and without replacement 

resamp    <- sample(dat$model, NSAMP, replace=FALSE, prob=dat$probs)


### END
##################################################################################################


