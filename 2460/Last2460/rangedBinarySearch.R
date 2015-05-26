compare.range <- function(target, range){ 
	top <- range[["top"]]
	bottom <- range[["bottom"]]
	is.between <- (bottom < target && target <= top)
	
	if(is.between) 
		"found" 
	else if(target > top)
		"higher"
	else
		"lower"
}

key.range <- function(row){
	row[1:2]
}

rangedBinarySearch.generator <- function(data.list = NULL, top.list = NULL, bottom.list = NULL, 
										 top.value = Inf, bottom.value = -Inf){
	# set up top.list and bottom.list: deriving one from the other if need be
	if(is.null(bottom.list)) {
		n <- length(top.list)
		bottom.list <- c(bottom.value, top.list[1:(n-1)])
	}else{
		n <- length(bottom.list)
		if(is.null(top.list)){
			top.list <- c(bottom.list[2:n], top.value)
		}
	}

	# setting up data.list, creating it from the array index if missing
	if (is.null(data.list))
		data.list <-  1:n

	key.list <- data.frame(bottom = bottom.list, top = top.list)

	binarySearch.generator(key.list, data.list, compare = compare.range)
}

rangedBinarySearch <- function(data.list = NULL, target = NULL, top.list = NULL, bottom.list = NULL,
							   top.value = Inf, bottom.value = -Inf){
	bSearch <- rangedBinarySearch.generator(data.list, top.list, bottom.list, top.value, bottom.value, compare, data)
	bSearch(target)
}