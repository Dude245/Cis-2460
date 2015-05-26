# load binarySearch.R, rangedBinarySearch.R and rpmfBinary.R first

binarySearchOrder <- function(n){
	bSearchOrdering <- vector(mode = "numeric", length = n)
	height <- ceiling(log(n,2))
	leafNumber <- 2^(height-1)
	
	bscopy.node <- function(mid, breadth.order, depth){
		if(identical(depth, height)){
			breadth.order <- leafNumber
			leafNumber <<- leafNumber + 1
		}
		bSearchOrdering[[mid]] <<- breadth.order
	}

	createBSearchOrder <- function(low, high, breadth.order, depth){
		if(low <= high){
			mid <- (high + low) %/% 2
			bscopy.node(mid, breadth.order, depth)
			createBSearchOrder(low, mid - 1, 2 * breadth.order, depth + 1)
			createBSearchOrder(mid + 1, high, 2 * breadth.order + 1, depth + 1)
		} # else 
			# base case: outside tree so nothing to copy
	}
	
	createBSearchOrder(1, n, 1, 1)
	bSearchOrdering
}

rpmf.generator.sorted.binary <- function(pmf, values = NULL){
	n <- length(pmf)
	if(is.null(values)) values <- 1:n
	randVar <- data.frame(pmf, values)
	
	pmf.order <- order(-randVar$pmf)
	randVar.pmfOrder <- randVar[pmf.order,]								# first sort by pmf (decreasing)
	randVar.bSearchOrder <- randVar.pmfOrder[binarySearchOrder(n),]		# then sort into binary search order
	
	# use pmf and values in the binary search ordering
	rpmf.generator.binary(randVar.bSearchOrder$pmf, randVar.bSearchOrder$values)
}