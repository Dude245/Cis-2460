# first load rpmfSimple.R, binarySearch.R, rangedBinarySearch.R, rpmfBinary.R and rpmfSortedBinary.R

rpmf.generator <- function(pmf, values = NULL, type = "sorted.binary"){
	switch(type,
		naive = rpmf.generator.naive(pmf, values),
		linear = rpmf.generator.linear(pmf, values),
		binary = rpmf.generator.binary(pmf, values),
		sorted.linear = rpmf.generator.sorted.linear(pmf, values),
		sorted.binary = rpmf.generator.sorted.binary(pmf, values)
	)
}

# rpmf <- rpmf.generator(pmf1, values1)
# r1 <- rpmf(100)    	# produces 100 random values from the pmf distribution given in the generator
# r2 <- rpmf()			# produces 1 random value from the pmf distribution given in the generator
# r3 <- rpmf(1)			# also produces 1 random value from the pmf distribution given in the generator