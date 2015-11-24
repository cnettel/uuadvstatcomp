##################################################################################################
### Advanced Statistical computing course
### Project postprocessing code
### Author: Anne-Gaelle Dosne
### Date: November 2015
##################################################################################################

require(ggplot2)
theme_set(theme_bw(base_size=16))
require(reshape)
require(dplyr)

### Comments #####################################################################################
### WHAT THIS CODE DOES:
### INPUT:
### OUTPUT:
##################################################################################################

DIR      <- "C:/Users/anndo252/uuadvstatcomp/Project/SIMULATIONS/"
PROBLEMS <- c("MVN_TRUE_MC_NOPAR","MVN_INFL1_MC_NOPAR","MVN_INFL2_MC_NOPAR","MVN_DEFL1_MC_NOPAR","MVN_DEFL2_MC_NOPAR",
              "MVN_BIAS1_MC_NOPAR","MVN_BIAS2_MC_NOPAR", "MVN_BIASINFL1_MC_NOPAR",
              "MVN_TRUE_LHS_NOPAR","MVN_BIASINFL1_LHS_NOPAR")

### Read in results

stats <- c()

for (i in PROBLEMS) {
  stats.cur <- read.csv(paste0(DIR,i,"/stats_distrib.csv"))
  stats     <- rbind(stats,stats.cur)
}

mstats      <- melt(stats, measure.vars=names(stats)[grep("param",names(stats))])
mstats$REF  <- 1
mstats[mstats$Variable=="CENTER",]$REF  <- 0
mstats      <- cbind(mstats,matrix(unlist(strsplit(as.character(mstats$NAME),"_")),ncol=4,byrow=TRUE,dimnames = list(NULL,c("DIST","SCENARIO","SAMP","PAR"))))

p1 <- ggplot(filter(mstats,Variable!="COV"), aes(x=Iteration,y=value,group=interaction(variable,SAMP),color=SAMP)) + 
  geom_point(alpha=0.2) +
  geom_line(alpha=0.2) +
  geom_smooth(aes(group=SAMP),se=F,size=1) +
  geom_hline(aes(yintercept=REF),linetype=2) +
  facet_grid(SCENARIO~Variable,scales="free_y")
p1

png(paste(DIR,"SUMMARY_PLOTS/convergence.png",sep=""),width = 800)
p1
dev.off()

### END
##################################################################################################


