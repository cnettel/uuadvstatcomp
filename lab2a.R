f<-function(x){
  y<-sum((x - 3)^2 + 2 * (x - 3) ^ 2 + 3 * (x - 15) ^ 2 + sin(100*x)) # the sin creates a local min
 return(y)
}

f(3)

optimise(f,c(0,15))
optimise(f,c(0,100))
optimise(f,c(9,12))
optimise(f,interval = c(10,11))

ff<-function(x) {
  y<-x*sin(x)
  return (y)
}

ff(2)

integrate(ff,lower = -7e5,upper = 7e5,subdivisions =1e7 )

system.time(integrate(ff,lower = -7e5,upper = 7e5,subdivisions =1e7))


integrate(ff,lower = -7e5,upper = 7e5,subdivisions =1e7 )

z<-c(-7e+05:-350000)
b<-c(-350000:0)
a<-c(0:350000)
c<-c(350000:7e+05)



library(parallel)
cl<-makePSOCKcluster(4)
chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
chunk(d,5)



f2<-function(lim){
    y<-integrate(function(x) {x*sin(x)},
   lower=lim[1],
   upper=lim[2],
 subdivisions=1E7)
 return(y$value)
}

d<-seq(-7e5,7e5,length.out = 5)
min<-d[-length(d)]
max<-d[-1]



system.time(parLapply(cl=cl,X = Map(c,min,max),fun = f2))
