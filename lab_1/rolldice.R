repeat {
	dice <- sample(1:6, 3, replace = TRUE)
	if (sum(dice) == 18) {
	  print('Done!')
	  break()
	  }
}