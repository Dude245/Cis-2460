random.pmf <- function(n, distr = runif){
	upmf <- runif(n)
	normalize(upmf)
}

normalize <- function(upmf){	# upmf = unnormalized pdf
	norml <- sum(upmf)
	normalize <- function(x){x / norml}
	sapply(upmf, normalize)
}

rpmf.generator.naive <- function(pmf, values = NULL){
	if(is.null(values))
		values <- 1:length(pmf)
	rpmf <- function(r){
			sum <- i <- 0
			while(sum < r){
				i <- i + 1
				sum = sum + pmf[i]
			}
			values[[i]]
		}

	function(n = 1){
		r <- runif(n)
		sapply(r, rpmf)
	}
}

pmf2cdf <- function(pmf){
	n <- length(pmf)
	cdf <- vector(length = n)
	cdf[[1]] <- pmf[[1]]
	for(i in 2:n)
		cdf[[i]] <- cdf[[i-1]] + pmf[[i]]
	cdf
}

rpmf.generator.linear <- function(pmf, values = NULL){
	N <- length(pmf)
	if(is.null(values))
		values <- 1:N
	cdf <- pmf2cdf(pmf)
	
	rpmf <- function(r){
		for(i in 1:N )
			if (cdf[[i]] > r) return(values[[i]])
		return(values[[N]])
	}
	
	function(n = 1){
		r <- runif(n)
		sapply(r, rpmf)
	}
} 

rpmf.generator.sorted.linear <- function(pmf, values = NULL){
	pmf.order <- order(pmf, decreasing = TRUE)
	pmf.sorted <- pmf[pmf.order]
	print(pmf.sorted)
	values.sorted <- values[pmf.order]
	print(values.sorted)
	rpmf.generator.linear(pmf.sorted, values.sorted)
}