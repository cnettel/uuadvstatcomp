f<-function(x){
  y<-sum((x - 3)^2 + 2 * (x - 3) ^ 2 + 3 * (x - 15) ^ 2 + sin(100*x))
 return(y)
}

f(3)

optimise(f,c(0,15))
optimise(f,c(9,12))
optimise(f,interval = c(10,11))
