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
# PROBLEMS <- c("MVN_TRUE_MC_NOPAR","MVN_INFL1_MC_NOPAR","MVN_INFL2_MC_NOPAR","MVN_DEFL1_MC_NOPAR","MVN_DEFL2_MC_NOPAR",
              # "MVN_BIAS1_MC_NOPAR","MVN_BIAS2_MC_NOPAR", "MVN_BIASINFL1_MC_NOPAR",
              # "MVN_TRUE_LHS_NOPAR","MVN_BIASINFL1_LHS_NOPAR")
PROBLEMS   <- dir(DIR,pattern="MVN")

### Read in results 1

stats <- c()

for (i in PROBLEMS) {
  stats.cur <- read.csv(paste0(DIR,i,"/stats_distrib.csv"))
  stats     <- rbind(stats,stats.cur)
}

mstats      <- melt(stats, measure.vars=names(stats)[grep("param",names(stats))])
mstats$REF  <- 1
mstats[mstats$Variable=="CENTER",]$REF  <- 0
mstats[mstats$Variable=="P025",]$REF  <- qnorm(0.025)
mstats[mstats$Variable=="P975",]$REF  <- qnorm(0.975)

mstats          <- cbind(mstats,matrix(unlist(strsplit(as.character(mstats$NAME),"_")),ncol=4,byrow=TRUE,dimnames = list(NULL,c("DIST","SCENARIO","SAMP","PAR"))))
mstats$Variable <- factor(mstats$Variable,levels=c("CENTER","VAR","P025","P975"))

p1 <- ggplot(filter(mstats,Variable!="COV"), aes(x=Iteration,y=value,group=interaction(variable,SAMP),color=SAMP)) + 
  geom_point(alpha=0.2) +
  geom_line(alpha=0.2) +
  geom_smooth(aes(group=SAMP),se=F,size=1) +
  geom_hline(aes(yintercept=REF),linetype=2) +
  facet_grid(SCENARIO~Variable,scales="free_y")
p1

png(paste(DIR,"SUMMARY_PLOTS/convergence.png",sep=""),width = 800, height=1500)
p1
dev.off()

### Correlations

stats_corr      <- stats[grep("COR",stats$NAME),]
stats_corr      <- stats_corr[stats_corr$Variable=="COV",]
stats_corr$id   <- 0
stats_corr$id[seq(2,nrow(stats_corr),by=length(grep("param",names(stats_corr))))] <- 1 # identify cor param1-2 in param1 colomn
stats_corr$id[seq(4,nrow(stats_corr),by=length(grep("param",names(stats_corr))))] <- 1 # identify cor param(n-1)-n in paramN column
stats_fin       <- filter(stats_corr[,-seq(2,length(grep("param",names(stats_corr)))-1)],id==1)
stats_fin$REF   <- 0.5
stats_fin[grep("NEG",stats_fin$NAME),"REF"]   <- -0.5
mstats_fin      <- melt(stats_fin, measure.vars=names(stats_fin)[grep("param",names(stats_fin))])
mstats_fin      <- cbind(mstats_fin ,matrix(unlist(strsplit(as.character(mstats_fin$NAME),"_")),ncol=4,byrow=TRUE,dimnames = list(NULL,c("DIST","SCENARIO","SAMP","PAR"))))

p1a <- ggplot(mstats_fin, aes(x=Iteration,y=value,group=interaction(variable,SAMP),color=SAMP)) + 
  geom_point(alpha=0.2) +
  geom_line(alpha=0.2) +
  geom_smooth(aes(group=SAMP),se=F,size=1) +
  geom_hline(aes(yintercept=REF),linetype=2) +
  facet_grid(SCENARIO~Variable,scales="free_y")
p1a

png(paste(DIR,"SUMMARY_PLOTS/correlations.png",sep=""))
p1a
dev.off()

### Read in results 2

rawres    <- c()
rawres.it <- c()

for (i in PROBLEMS) {
    raw        <- list.files(paste0(DIR,i),pattern="sim_iteration")
  for (j in raw) {
    rawres.it.cur  <- read.csv(paste0(DIR,i,"/",j))
    rawres.it      <- rbind(rawres.it,rawres.it.cur)
  }
    rawres.it$NAME <- i
    rawres         <- rbind(rawres,rawres.it)
    rawres.it      <- c()
}

rawres <- cbind(rawres,matrix(unlist(strsplit(as.character(rawres$NAME),"_")),ncol=4,byrow=TRUE,dimnames = list(NULL,c("DIST","SCENARIO","SAMP","PAR"))))

p2 <- ggplot(rawres, aes(x=as.factor(iteration),y=dmv_true,group=interaction(iteration,SAMP),color=SAMP)) + 
  geom_boxplot() +
  facet_grid(SCENARIO~.,scales="free_y") +
  scale_y_log10()
p2


png(paste(DIR,"SUMMARY_PLOTS/sampling_densities.png",sep=""),width = 800, height=800)
p2
dev.off()




### END
##################################################################################################


