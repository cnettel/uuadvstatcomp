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
# require(devtools)
# find_rtools()
# require(stringi)
# install_github("hadley/lineprof")
# install_github("hadley/multidplyr")
# library(lineprof)
# library(multidplyr)
# require(shiny)
require(compiler)

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
  PARALLEL  <- as.numeric(sim_spec_i$PARALLEL)                           # parallel computing
  NODES     <- as.numeric(sim_spec_i$NODES)                              # number of nodes for parallel computing
  # Ouput of function
  out        <- list(name,NPARAMS,TRUE_CENTER,TRUE_COV,PROP_CENTER,PROP_COV,NIT,M,N,PARALLEL,NODES)
  names(out) <- c("name","NPARAMS","TRUE_CENTER","TRUE_COV","PROP_CENTER","PROP_COV","NIT","M","N","PARALLEL","NODES")
  return(out)
  
}

### Function get_args + sir for microbenchmark

getargs_and_sir <- function(data=sim_spec,name,nit=1, m=100, n=20, par=0, nodes=1) {
  args_spec <- get_args(data,name)
  sir.res   <- sir(name=name,nparams=args_spec$NPARAMS, true_center=args_spec$TRUE_CENTER, true_cov=args_spec$TRUE_COV, prop_center=args_spec$PROP_CENTER, prop_cov=args_spec$PROP_COV, nit=nit, m=m, n=n, par=par, nodes=nodes, write=F)
  # use only NIT=1 M=100  N=20 for the sake of comparing speed 
}

### Microbenchmark to see if LHS take more time than MC

microbenchmark(getargs_and_sir(name="MVN_TRUE_MC_NOPAR"),getargs_and_sir(name="MVN_TRUE_LHS_NOPAR"))

### Lineprof to see which parts of the sir take time
# 
# profile <- lineprof(sir(name="MVN_TRUE_MC_NOPAR"))
# shine(profile)

### Try to improve runtime 1: byte code compiler 

getargs_and_sir_comp <- cmpfun(getargs_and_sir)
# check                <- getargs_and_sir_comp(name="MVN_TRUE_MC_NOPAR")

microbenchmark(getargs_and_sir(name="MVN_TRUE_MC_NOPAR"),getargs_and_sir_comp(name="MVN_TRUE_MC_NOPAR")) # no gain from compiling

### Try to improve runtime 2: parallelization 

cl         <- makePSOCKcluster(2)
clusterEvalQ(cl, { library(mvtnorm) })     # make sure all nodes have the right library
#   clusterExport(cl=cl, varlist=c("true_center", "true_cov","prop_center", "prop_cov","sim"))
microbenchmark(getargs_and_sir(name="MVN_TRUE_LHS_NOPAR"),getargs_and_sir(name="MVN_TRUE_LHS_PAR",par=1)) # 20 and 40 times slower?!

stopCluster(cl)

cl         <- makePSOCKcluster(4)
