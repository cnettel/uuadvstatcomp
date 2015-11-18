#----------- OPTIMIZE -----------#
f <- function (x) {(x - 3)^2 + 2 * (x - 3)^2 + 3 * (x - 15)^2 + sin(100*x)}
optimize(f,c(0,15))
optimize(f,c(9,12))
optimize(f,c(10,11))

xx <- seq(8,12,length=50)
plot(xx,f(xx))

#----------- INTEGRATE -----------#
fInt <- function (x) { x*sin(x) }
system.time(intVal <- integrate(fInt, -7e5, 7e5, subdivisions = 1e8), gcFirst = TRUE )
print(intVal)
library(parallel)
cl <- makePSOCKcluster(4)
system.time(intVal <- parLapply(cl, (-7:6)*1e5, (function(x) { integrate(function (x) { x*sin(x) }, x, x+1e5, subdivisions = 1e8)$value})))
print(Reduce("+",intVal))
cl <- makePSOCKcluster(8)
system.time(intVal <- parLapply(cl, (-7:6)*1e5, (function(x) { integrate(function (x) { x*sin(x) }, x, x+1e5, subdivisions = 1e8)$value})))
print(Reduce("+",intVal))
cl <- makePSOCKcluster(16)
system.time(intVal <- parLapply(cl, (-7:6)*1e5, (function(x) { integrate(function (x) { x*sin(x) }, x, x+1e5, subdivisions = 1e8)$value})))
print(Reduce("+",intVal))

#----------- FUNCTIONAL OPERATORS (Memoisation) -----------#
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
forget(fib2)
forget(fib3)
