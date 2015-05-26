# load binarySearch.R and rangedBinarySearch.R first

pmf2cdf <- function(pmf){
	n <- length(pmf)
	cdf <- vector(length = n)
	cdf[[1]] <- pmf[[1]]
	for(i in 2:n)
		cdf[[i]] <- cdf[[i-1]] + pmf[[i]]
	cdf
}

rpmf.generator.binary <- function(pmf, values = NULL){
	n <- length(pmf)
	cdf <- pmf2cdf(pmf)
	bSearch <- rangedBinarySearch.generator(values, cdf, bottom.value = 0, top.value = 1)

	function(n = 1){
		r <- runif(n)
		sapply(r, bSearch)
	}
}