compare.binarySearch <- function(target, value, equal = `==`, greater = `>`) {
	if(equal(target, value)) 
		"found" 
	else if(greater(target, value))
		"higher"
	else
		"lower"	
}

binarySearch.generator <- function(key.list, data.list = NULL, compare = compare.binarySearch) { 
	# generalized for ranged binary search - when only a single valued key, will behave the same as a vector
	if(!is.data.frame(key.list))
		key.list <- data.frame(key = key.list)  	
	n <- length(row.names(key.list))
	
	# set up data - just be the index values if data is missing
	if(is.null(data.list))
		data.list <- 1:n
	# used to convert back to original type of list / vector if data is not a data.frame
	class.data <- class(data.list)	
	# generalize data.list to handle data frames	
	if(!is.data.frame(data.list)){
		data.list <- data.frame(data = data.list)
	}
	
	b.search <- function(target, low, high){
		if (high < low)
			return(NULL)
		else {
			mid <- (high + low) %/% 2
			switch(compare(target, key.list[mid,]),
				found = return(as(data.list[mid,], class.data)),
				lower = return(b.search(target, low, mid - 1)),
				higher = return(b.search(target, mid + 1, high))
			)
		}
	}
	
	# returns the function
	function(target) {
		b.search(target, 1, n)
	}
}	

binarySearch <- function(target, key.list, data.list = NULL, compare = compare.binarySearch, key = "key") { 
	bSearch <- binarySearch.generator(key.list, data.list, compare, key)
	bSearch(target)
}