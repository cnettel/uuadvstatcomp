#####################################################
### Advanced Statistical Computing Course 
### Lab 2
### Winter 2015
### Report by Anne-Gaelle Dosne
#####################################################

### Optimization

myfun <- function(x) {               # create a univariate function
  (x - 3)**2 + 2 * (x - 3)**2 + 3 * (x - 15)**2 + sin(100*x)
}

myfun(3)                                     # test the function
opt1 <- optimize(f=myfun, interval=c(0,100)) # optimize it over the interval from 0 to 100 (local minimization): min: 9.03
myfun(opt1$minimum)                          # returns function value at minimum (~215)
optimize(f=myfun, interval=c(0,15))          # min: 9.28
optimize(f=myfun, interval=c(9,12))          # min: 9.53
optimize(f=myfun, interval=c(10,11))         # min: 10.16
optimize(f=myfun, interval=c(-1000,1000))    # min: 8.71
# The optimum changes a little every time but is close to 9 whenever 9 is in the interval. 
# The value of the minimum varies by up to 0.5.
# There is no guarantee that this optimum is the global optimum, but it is the best over -1000,1000.

### Integrate a function

multsin <- function(x) {x*sin(x)} # create a new function

system.time(integrate(multsin,lower=-7E5,upper=7E5,subdivisions=1E7)) # integrate it over large range and small steps
# takes ~13 seconds
