#################################################
#                                               #
# Project (Advanced statistical computing)      #
#                                               #
#                                               #
#################################################

rm(list=ls())

pm<-read.table("C:/STUDY/data/network.txt", header=T)

N=1579
p=10
Ttnew<-pm
n<-100
ni<-10000

hra<-matrix(0, nrow=ni,ncol=n)
hrp<-matrix(0, nrow=p, ncol=n)


SAq0<-numeric()
SAq1<-numeric()
SAq2<-matrix(0, nrow=ni, ncol=p)
SAq3<-numeric()

for (i in 1:n)
{
  
  temperature7.tra<-fitness7.tra<-NULL
  subset<-numeric(0)
  
  ran.seven.seq<-sample(1:N,p,replace=F)
  obj.function.r7<-sum(as.numeric(apply(Ttnew[,ran.seven.seq],1,min)))
  iteration<-0
  Tem<-400
  count<-0
  beta<-0.5
  First_iteration7.tra<-c(ran.seven.seq,obj.function.r7)
  configuration.vector7.tra<-NULL
  while (iteration<ni){
    sam<-sample(1:p,1)
    substitution<-sample((1:N)[-ran.seven.seq[sam]],1)
    old<-ran.seven.seq
    ran.seven.seq[sam]<-substitution
    new.obj.function.r7<-sum(as.numeric(apply(Ttnew[,ran.seven.seq],1,min)))
    if (new.obj.function.r7<=obj.function.r7) {obj.function.r7<-new.obj.function.r7;beta<-0.5;count<-0}
    if (new.obj.function.r7>obj.function.r7){
      delta<-new.obj.function.r7-obj.function.r7
      unif.number<-runif(1,0,1)
      if (unif.number<exp(-delta/Tem)) {obj.function.r7<-new.obj.function.r7;beta<-0.5;count<-0}
      if (unif.number>=exp(-delta/Tem)) {count<-count+1;ran.seven.seq<-old}
    }
    if(count==10) {Tem<-Tem*(1+2)^beta; beta<-beta+0.5; count<-0}
    iteration<-iteration+1
    Tem<-Tem*0.50
    
    SAq0[iteration]<-obj.function.r7
    SAq1[iteration]<-min(SAq0[1:iteration])
    temperature7.tra<-c(temperature7.tra,Tem)
    configuration.vector7.tra<-c(configuration.vector7.tra,ran.seven.seq)
    
    SAq2[iteration,]<-ran.seven.seq
    
    SAq3<-SAq2[min(which(SAq0==SAq1)),]
    
  }
  
  
  hra[,i]<-SAq1
  hrp[,i]<-SAq3
}
mean(hra[ni,])/277725

################################################################
#                      Neighbourhood.                         ##
################################################################

rm(list=ls())
pm<-read.table("C:/STUDY/pascal/network.txt", header=T)
N=1579
p=10
Ttnew<-pm
n<-100
ni<-5000

hra<-matrix(0, nrow=ni,ncol=n)
hrp<-matrix(0, nrow=p, ncol=n)


SAq0<-numeric()
SAq1<-numeric()
SAq2<-matrix(0, nrow=ni, ncol=p)
SAq3<-numeric()

for (i in 1:n)
{
  
  temperature7.tra<-fitness7.tra<-NULL
  subset<-numeric(0)
  
  ran.seven.seq<-sample(1:N,p,replace=F)
  obj.function.r7<-sum(as.numeric(apply(Ttnew[,ran.seven.seq],1,min)))
  iteration<-0
  Tem<-400
  count<-0
  beta<-0.5
  First_iteration7.tra<-c(ran.seven.seq,obj.function.r7)
  configuration.vector7.tra<-NULL
  while (iteration<ni){
    sam<-sample(1:p,1)
    substitution<-sample((1:N)[-ran.seven.seq[sam]],1)
    old<-ran.seven.seq
    ran.seven.seq[sam]<-substitution
    new.obj.function.r7<-sum(as.numeric(apply(Ttnew[,ran.seven.seq],1,min)))
    if (new.obj.function.r7<=obj.function.r7) {obj.function.r7<-new.obj.function.r7;beta<-0.5;count<-0}
    if (new.obj.function.r7>obj.function.r7){
      delta<-new.obj.function.r7-obj.function.r7
      unif.number<-runif(1,0,1)
      if (unif.number<exp(-delta/Tem)) {obj.function.r7<-new.obj.function.r7;beta<-0.5;count<-0}
      if (unif.number>=exp(-delta/Tem)) {count<-count+1;ran.seven.seq<-old}
    }
    if(count==10) {Tem<-Tem*(1+2)^beta; beta<-beta+0.5; count<-0}
    iteration<-iteration+1
    Tem<-Tem*0.95
    
    SAq0[iteration]<-obj.function.r7
    SAq1[iteration]<-min(SAq0[1:iteration])
    temperature7.tra<-c(temperature7.tra,Tem)
    configuration.vector7.tra<-c(configuration.vector7.tra,ran.seven.seq)
    
    SAq2[iteration,]<-ran.seven.seq
    
    SAq3<-SAq2[min(which(SAq0==SAq1)),]
    
  }
  
  
  hra[,i]<-SAq1
  hrp[,i]<-SAq3
}
mean(hra[ni,])/277725