f<-function(x){
  y<-sum((x - 3)^2 + 2 * (x - 3) ^ 2 + 3 * (x - 15) ^ 2 + sin(100*x))
 return(y)
}

f(3)

optimise(f,c(0,15))
optimise(f,c(9,12))
optimise(f,interval = c(10,11))

ff<-function(x) {
  y<-x*sin(x)
  return (y)
}

ff(2)

integrate(ff,lower = -7e5,upper = 7e5,subdivisions =1e7 )

