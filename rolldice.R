repeat {
	dice <- sample(1:6, 3)

	if (sum(dice) == 18) break()
}