repeat {
dice <- sample(1:6,replace=T,size=3)

if (sum(dice,na.rm=T) == 18) break()
}


