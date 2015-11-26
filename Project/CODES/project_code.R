##################################################################################################
### Advanced Statistical computing course
### Project code
### Author: Anne-Gaelle Dosne
### Date: November 2015
##################################################################################################

### Comments #####################################################################################
### WHAT THIS CODE DOES:
### INPUT: Define variables in beginning of code below. No external file needed.
### OUTPUT: 
### Create directory NAME for one simulation problem with: 
### - 1 csv file per iteration with all sampled and resampleds parameters and their IR 
### - 1 csv file for all iterations with posterior mean, SE and covmat + same for proposal  
##################################################################################################

require(dplyr)
require(microbenchmark)
source("C:/Users/anndo252/uuadvstatcomp/Project/CODES/sir_function.R")  # load sir function

DIR            <- "C:/Users/anndo252/uuadvstatcomp/Project/SIMULATIONS"
sim_spec       <- read.csv(paste0(DIR,"/PROBLEM_specifications.csv"))   # read specification file when sir function arguments are stored for different scenarios

### Get arguments for a given scenario 

get_args       <- function (data,name) { # Read sir arguments from sim_spec for given problem

  sim_spec_i     <- filter(data,NAME==name)  
  SAMPLING       <- as.character(sim_spec_i$SAMPLING)
  # True distribution parameters
  NPARAMS        <- as.numeric(sim_spec_i$NPARAMS)                       # dimensionality 
  TRUE_CENTER    <- as.numeric(rep(sim_spec_i$TRUE_CENTER,NPARAMS))      # center of true distribution
  TRUE_COV       <- diag(as.numeric(rep(sim_spec_i$TRUE_VAR,NPARAMS)))   # covariance matrix of true distribution
  TRUE_COV[2,1]  <- TRUE_COV[1,2] <- TRUE_COV[NPARAMS,NPARAMS-1] <-  TRUE_COV[NPARAMS-1,NPARAMS] <- as.numeric(sim_spec_i$TRUE_COV)        # correlation between param1&2 and 2nd2last&last
  # Initial proposal distribution parameters
  PROP_CENTER    <- as.numeric(rep(sim_spec_i$PROP_CENTER,NPARAMS))      # center of proposal distribution
  PROP_COV       <- diag(as.numeric(rep(sim_spec_i$PROP_VAR,NPARAMS)))   # covariance matrix of proposal distribution
  # SIR procedure parameters (stay fixed here)
  NIT       <- as.numeric(sim_spec_i$NIT)                                # number of SIR iterations to perform
  M         <- rep(as.numeric(sim_spec_i$M),NIT)                         # number of samples to generate for each iteration
  N         <- rep(as.numeric(sim_spec_i$N),NIT)                         # number of resamples to draw for each iteration
  # Other function options
  PARALLEL         <- as.logical(sim_spec_i$PARALLEL)                           # number of resamples to draw for each iteration
  # Ouput of function
  out        <- list(name,NPARAMS,TRUE_CENTER,TRUE_COV,PROP_CENTER,PROP_COV,NIT,M,N,PARALLEL)
  names(out) <- c("name","NPARAMS","TRUE_CENTER","TRUE_COV","PROP_CENTER","PROP_COV","NIT","M","N","PARALLEL")
  return(out)
  
}

### Function get_args + sir for microbenchmark

getargs_and_sir <- function(data=sim_spec,name) {
  args_spec <- get_args(data,name)
  attach(args_spec)
  sir.res   <- sir(name=NAME,nparams=NPARAMS, true_center=TRUE_CENTER, true_cov=TRUE_COV, prop_center=PROP_CENTER, prop_cov=PROP_COV, nit=NIT, m=M, n=N, write=F)
}

### Microbenchmark to see if LHS take more time than MC

microbenchmark(getargs_and_sir(name="MVN_TRUE_MC_NOPAR"),getargs_and_sir(name="MVN_TRUE_LHS_NOPAR"))


