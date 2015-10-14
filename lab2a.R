###########################################
## Optimization

# Function to minimize
f <- function(x) {
  (x - 3)^2 + 2 * (x - 3)^2 + 3 * (x - 15)^2 + sin(100*x)
}

# Metafunction, minimizes f in the range supplied
optrange <- function(xrange) {
  optimise(f, xrange)
}

# The results
opt1 <- optrange(c(0, 15))
opt2 <- optrange(c(9, 12))
opt3 <- optrange(c(10, 11))

# Create a function for plotting the results
optplot <- function(optobj, ...) {
  points(optobj$minimum, optobj$objective, ...)
}

# Plot curve and points
# Quite obviously, they are local minima!
par(mfrow = c(1,2))
curve(f, from = 0, to = 20)
optplot(opt1)
optplot(opt2, pch = 2)
optplot(opt2, pch = 3)

curve(f, from = 9.2, to = 9.8)
optplot(opt1)
optplot(opt2, pch = 2)
optplot(opt2, pch = 3)
par(mfrow = c(1,1))

###########################################
## Integration

g <- function(x) {
  x * sin(x)
}

# Takes about 8 seconds, but smaller values of subdivisions makes the function not work
integral <- system.time(integrate(g, -7e5, 7e5, subdivisions = 1e7))

