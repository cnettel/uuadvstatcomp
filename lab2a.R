f <- function (x) {(x - 3)^2 + 2 * (x - 3)^2 + 3 * (x - 15)^2 + sin(100*x)}
optimize(f,c(0,15))
optimize(f,c(9,12))
optimize(f,c(10,11))