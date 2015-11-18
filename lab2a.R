rm(list=ls())

### 1. Optimization ###

SumFun <- function(x) {
  (x - 3)^2 + 2*(x - 3)^2 + 3*(x - 15)^2 + sin(100*x)
}      

optimise(SumFun, c(0,100))

optimise(SumFun, c(0,15))

optimise(SumFun, c(9,12))

optimise(SumFun, c(10,11))

#the term sin(100x) is causing problems 
#because the behavior of the function will change due to a small change of x.

SumFun <- function(x) {
  (x - 3)^2 + 2*(x - 3)^2 + 3*(x - 15)^2 + sin(x)
}      

optimise(SumFun, c(0,100))

optimise(SumFun, c(0,15))

optimise(SumFun, c(9,12))

optimise(SumFun, c(10,11))

### 2. Adding file lab2a.R in the repository C:\ uuadvstatcomp ###

### 3. Integrating a function ###

###############################
#                             #
#      Parallel               #
#                             #
###############################

Fun1 <- function(x) {
  fval <- x*sin(x)
  return (fval)
}

integrate(Fun1, lower = -7e5, upper = 7e5,
          subdivisions = 1e7, stop.on.error = TRUE)

system.time (integrate(Fun1, lower = -7e5, upper = 7e5,
                       subdivisions = 1e7, stop.on.error = TRUE))


########################################################################
library(parallel)

Ncores <- detectCores() - 1

clus <- makePSOCKcluster(Ncores)

clusterEvalQ(clus, Integral.function <- function(l,u,s){
  result <- integrate(function(x) {x*sin(x)}, lower = l, upper = u, 
                      subdivisions = s, stop.on.error = TRUE)
  return(result)}) 

Integral.function <- function(l,u,s){
  result <- integrate(function(x) {x*sin(x)}, lower = l, upper = u, 
                      subdivisions = s, stop.on.error = TRUE)
  return(result)}


clusterExport(clus,"Integral.function") 

inte1 <- parSapply(clus,-7e5:7e5, function(x) Integral.function(-7e5,7e5,1e7)) 
#The above code does not work due to the attributes in upper and lower
# Upper and lower should be functions
###############################################################################

library(parallel)

Fun1 <- function(x) {
  fval <- x*sin(x)
  return (fval)
}

Lower<-function(x){seq(from = (-7)*10^5,to = (7)*10^5,length.out = 5)[1:4][x]}

Upper<-function(x){seq(from = (-7)*10^5,to = (7)*10^5,length.out = 5)[2:5][x]}

Ncores <- detectCores() 

clus <- makePSOCKcluster(Ncores)

clusterExport(clus,"Fun1")

clusterExport(clus,"Lower")

clusterExport(clus,"Upper")

sum(unlist(parLapply(clus,1:Ncores,
                     function(x){ 
                       integrate(f = Fun1,lower = Lower(x),upper = Upper(x),
                                 subdivisions = 10^7)$value}
)))
#[1] 1356376

system.time(
  sum(unlist(parLapply(clus,1:Ncores,
                       function(x){ 
                         integrate(f = Fun1,lower = Lower(x),upper = Upper(x),
                                   subdivisions = 10^7)$value}
  )))
)

stopCluster(clus)

### 4. Functional Operators ###

   # Memoisation #
library(memoise)

fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}

fib2 <- memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})

fib3 <- memoise(fib)

system.time(fib(28))

system.time(fib2(28))

system.time(fib3(28))

### 5. Domain-specific Languages ###

    # qplot() #
library(ggplot2)

data(mpg, package= "ggplot2")

attach (mpg)

str(mpg)

qplot(displ, hwy) #engine displacement (displ) and highway miles per gallon (hwy)#

#plot the relation between displ and hwy for different factors of wheel drive using color=drv
qplot(displ, hwy , color =drv) 

#add statistics to the plot using geom (the following will add a smoother to a plot) 
qplot(displ, hwy, geom=c("point","smooth")) 

# check the linear relationship for the data:
qplot(displ, hwy, geom=c("point","smooth"),method="lm")

#check the linear relationship for different groups:
qplot(displ, hwy, geom=c("point","smooth"),method="lm",color=drv)

#one benefit of these plots is that you can modify them after they have been created.
#For example, we can use theme() to modify theme setting. We assign the graph to variable:  
gr <- qplot(displ, hwy, geom=c("point","smooth"),method="lm",color=drv)

#Then we can modify it. For example change the color of the rectangular elements to pink:  
gr + theme(panel.background = element_rect(fill = "pink"))



##########################################
#                                        #
#          5. Exercise                   #
#                                        #
##########################################

library(ggplot2)

#1. Load the data set diamonds from ggplot2.
data(diamonds)

attach(diamonds)

str(diamonds)

#2. Look at the variables carat and price in the dataframe and 
   #plot them against each other.
qplot(carat, price)

#3. Look at the relation between carat and price for different diamond colors.
qplot(carat, price, color = color)

qplot(log(carat), log(price), color = color)
#4. Add a smoother to the plot.
qplot(carat, price, color = color, geom=c("point","smooth"))

#got warning: geom_smooth: method="auto" and size of largest group is >=1000, 
 #so using gam with formula: y ~ s(x, bs = "cs"). 
  #Use 'method = x' to change the smoothing method.

library(mgcv)
qplot(carat, price, color = color, geom = c("point", "smooth"),
      method = "gam", formula = y ~ s(x, bs = "cs"))

#5. Change the title of legend of your plot to "New legends". 
   #Tips: look at the theme() help to find out how to change the title of legend.
egr <- qplot(carat, price, color = color) 
# Set aesthetic of legend key

# very low alpha value make it difficult to see legend key, 
egr<- qplot(carat, price, colour = color,
            alpha = I(1/100))

#so set an appropriate value is important. 
 #the following override.aes overwrites the alpha
egr <- egr+guides(colour = guide_legend(override.aes = list(alpha = 1)))

#egr + theme(legend.title = element_text(title ="New legends")) #Wrong

#egr + scale_fill_discrete(name="New legends") # Wrong

egr+ labs(color = "New legends") # Did the work correctlly

#########################################################



########################################################
#6. Find out how to draw a boxplot to check 
  #the distribution of price/carat for different colors.

qplot(factor(color), price/carat, color= color, geom= "jitter")

qplot(factor(color), price/carat, color= color, geom= "boxplot")

### 6. Getting a SUPR account (Done! Account is set up by using xzh@du.se) ###