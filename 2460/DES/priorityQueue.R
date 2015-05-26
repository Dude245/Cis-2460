## (c) Copyright Mark Wineberg 2011
## mwineber@uoguelph.ca

###
### priorityQueue / Heap implmentation (c) Mark Wineberg 2011 
###
### based on the priority-queue / heap algorithm from Cormen, Leiserson, et. al. Introduction to Algorithms 2nd edition
### a modification of the Queue Implementation by Barry Rowlingson 2010 (although almost no original code is left)
###
### creates a priorityQueue object. the 'statistics' flag adds basic logging
###

#####   heapNode object   #####

newHeapNode <- function(key, data = NULL, position = 0){
	data					# because data and key are not used in this function, lazy eval may produce weird behaviour
	key						# so touch to force eval
	hn <- environment()
	class(hn) <- c("heapNode", "priorityQueueNode")
	hn
}

newPriorityQueueNode <- newHeapNode

### S3 Generic Functions for heapElements
key <- function(hn) {UseMethod("key")}
setKey <- function(hn, key) {UseMethod("setKey")}
data <- function(hn) {UseMethod("data")}
setData <- function(hn, data) {UseMethod("setPosition")}
getFromData <- function(hn, contentName) {UseMethod("getFromData")}
position <- function(hn) {UseMethod("position")}
setPosition <- function(hn, i) {UseMethod("setPosition")}
isNull <- function(hn) {UseMethod("isNull")}

key.heapNode <- function(hn) {
	hn[["key"]]
}

setKey.heapNode <- function(hn, key) {
	hn[["key"]] <- key
	hn
}

data.heapNode <- function(hn) {
	hn[["data"]]
}

getFromData.heapNode <- function(hn, contentName){
	hn[["data"]][[contentName]]
}

setData.heapNode <- function(hn, data) {
	hn[["data"]] <- data
	hn
}

position.heapNode <- function(hn) {
	hn[["position"]]
}

setPosition.heapNode <- function(hn, i) {
	hn[["position"]] <- i
	hn
}

isNull.heapNode <- function(hn) {
	is.null(key(hn))
}

###### heap data structure  ######


newHeap <- function(h = list(), heapSize = 0, stats = list(maxHeapSize = 0, insertCount = 0, extractCount = 0)){	
	e <- environment()
	class(e) <- c("heap", "priorityQueue", "environment")
	e
}

newPriorityQueue <- newHeap

### Simple Statistic Functions

## Generic functions for Simple Statistics
getStats <- function(heap) {UseMethod("getStats")}
setStats <- function(heap, stats) {UseMethod("setStats")}
resetStats <- function(heap){UseMethod("resetStats")}
insertCount <- function(heap) {UseMethod("insertCount")}
incInsertCount <- function(heap) {UseMethod("incInsertCount")}
extractCount <- function(heap) {UseMethod("extractCount")}
incExtractCount <- function(heap) {UseMethod("incExtractCount")}
largestHeapSize <- function(heap) {UseMethod("largestHeapSize")}
updateMaxHeapSize <- function(heap, newHeapSize) {UseMethod("updateMaxHeapSize")}

### Methods for Simple Statistics

getStats.heap <- function(heap) {
	heap[["stats"]]
}

setStats.heap <- function(heap, stats) {
	heap[["stats"]] <- stats
}

resetStats.heap <- function(heap){
	stats <- list(maxHeapSize = heapSize, insertCount = 0, extractCount = 0)
	setStats(heap, stats)
	return(stats)
}

insertCount.heap <- function(heap) {
	getStats(heap)[["insertCount"]]
}

incInsertCount.heap <- function(heap) {
	stats <- getStats(heap)
	
	stats[["insertCount"]] <- stats[["insertCount"]] + 1
	
	setStats(heap, stats)
	stats[["insertCount"]]
}

extractCount.heap <- function(heap) {
	getStats(heap)[["extractCount"]]	
}

incExtractCount.heap <- function(heap) {
	stats <- getStats(heap)
	
	stats[["extractCount"]] <- stats[["extractCount"]] + 1
	
	setStats(heap, stats)
	stats[["extractCount"]] 
}

largestHeapSize.heap <- function(heap) {
	getStats(heap)[["maxHeapSize"]]	
}

updateMaxHeapSize.heap <- function(heap, newHeapSize) {
	stats <- getStats(heap)
	
	if(newHeapSize > stats[["maxHeapSize"]])
		stats[["maxHeapSize"]] <- newHeapSize
		
	setStats(heap, stats)
	stats[["maxHeapSize"]]
}

### S3 Generic Functions: Internal Methods - Implementation Dependent

getHeapSize <- function(heap) {UseMethod("getHeapSize")}
getPQueueSize <- getHeapSize
setHeapSize <- function(heap, newSize) {UseMethod("setHeapSize")}
incHeapSize <- function(heap) {UseMethod("incHeapSize")}
decHeapSize <- function(heap) {UseMethod("decHeapSize")}
getH <- function(heap) {UseMethod("getH")}
#  setH <- function(heap, newH) {UseMethod("setH")}   # not implemented since list copied if passed as parameter which will significantly slow implementation
isEmpty <- function(heap) {UseMethod("isEmpty")}
inside <- function(heap, i) {UseMethod("inside")}
outside <- function(heap, i) {UseMethod("outside")}
getNode <- function(heap, i) {UseMethod("getNode")}	
getTopNode <- function(heap) {UseMethod("getTopNode")}
getLastNode <- function(heap) {UseMethod("getLastNode")}
setNode <- function(heap, node, i) {UseMethod("setNode")}
removeTopNode <- function(heap) {UseMethod("removeTopNode")}
removeLastNode <- function(heap) {UseMethod("removeLastNode")}
appendNode <- function(heap, node) {UseMethod("appendNode")}
topNode <- function(heap, node) {UseMethod("topNode")}
makeTopNode <- function(heap, node){UseMethod("makeTopNode")}
parentNode <- function(heap, node) {UseMethod("parentNode")}
leftNode <- function(heap, node) {UseMethod("leftNode")}
rightNode <- function(heap, node) {UseMethod("rightNode")}
swapNodes <- function(heap, parent, child) {UseMethod("swapNodes")}
bubbleDown <- function(heap, node, is.lighter) {UseMethod("bubbleDown")}
bubbleUp <- function(heap, node, is.lighter)  {UseMethod("bubbleUp")}

###
### S3 Method: Internal Methods - Implementation Dependent
###

getHeapSize.heap <- function(heap) {
	heap[["heapSize"]]
}

setHeapSize.heap <- function(heap, newSize) {
	heap[["heapSize"]] <- newSize
}

incHeapSize.heap <- function(heap) {
	newSize <- getHeapSize(heap) + 1
	setHeapSize(heap, newSize)
	newSize
}

decHeapSize.heap <- function(heap) {
	heapSize <- getHeapSize(heap)
	
	if (heapSize > 0)
		newSize <- heapSize - 1
	else
		newSize <- heapSize
		
	setHeapSize(heap, newSize)
	newSize
}

getH.heap <- function(heap) {
	get("h", heap)
}

isEmpty.heap <- function(heap) {
	getHeapSize(heap) == 0
}

inside.heap <- function(heap, i) {
	(1 <= i) && (i <= getHeapSize(heap))
}

outside.heap <- function(heap, i) {
	!inside(heap, i)
}

getNode.heap <- function(heap, i) {	
	if(outside(heap, i))
		newHeapNode(NULL)
	else 
		heap[["h"]][[i]]
}

getTopNode.heap <- function(heap) {
	getNode(heap, 1)
}

getLastNode.heap <- function(heap) {
	getNode(heap, getHeapSize(heap))		
}

setNode.heap <- function(heap, node, i) {		# definitely an internal function only, *** use only with extreme caution ***

	setPosition(node, i)						# will set node position to i, whether i is inside or outside the heap

	if(inside(heap, i))
		heap[["h"]][[i]] <- node	
}

removeTopNode.heap <- function(heap) {			# heap unstable after this operation. Only to be called by extractNode() which reforms heap
		top <- getTopNode(heap)
		setPosition(top, 0)
		return(top)
}

removeLastNode.heap <- function(heap) {
	lastNode <- getLastNode(heap)
	setPosition(lastNode, 0)
	decHeapSize(heap)	
	return(lastNode)
}

appendNode.heap <- function(heap, node) {		#### update to have ***fast*** dynamically increasing heap size
	incHeapSize(heap)	
	heapSize <- getHeapSize(heap)
	updateMaxHeapSize(heap, heapSize)			# for simple statistics
	setNode(heap, node, heapSize)				# problem if new heapSize is greater than length(h) - list gets recopied leads to n^2 instead of n behavior
}

topNode.heap <- function(heap, node){
	position(node) == 1
}

makeTopNode.heap <- function(heap, node){			# heap unstable after this operation. Only to be called by extractNode() which reforms heap
	setNode(heap, node, 1)
}

parentNode.heap <- function(heap, node)  {
	p <- position(node) %/% 2		# integer division by 2
	getNode(heap, p)
}

leftNode.heap <- function(heap, node)  {
	l <- 2 * position(node)
	getNode(heap, l)
}

rightNode.heap <- function(heap, node)  {
	r <- 2 * position(node) + 1
	getNode(heap, r)
}

swapNodes.heap <- function(heap, parent, child) {
	pPos <- position(parent)
	cPos <- position(child)
	setNode(heap, parent, cPos)
	setNode(heap, child, pPos)
	heap
}


###
### S3 Methods: internal methods - implementation independent
###


bubbleDown.heap <- function(heap, node, is.lighter = `<`)  {
	left <- leftNode(heap, node)
	right <- rightNode(heap, node)
	
	if(!isNull(left) && is.lighter(key(left), key(node)))
		higher <- left
	else
		higher <- node
		
	if(!isNull(right) && is.lighter(key(right), key(higher)))
		higher <- right

	if (!identical(higher, node)) {
		swapNodes(heap, node, higher)
		bubbleDown(heap, node, is.lighter)
	}	
}

bubbleUp.heap <- function(heap, node, is.lighter = `<`)  {
	newParent <- parentNode(heap, node)
	while(!topNode(heap, node) && is.lighter(key(node), key(newParent))) {	# node is lighter than parent
		swapNodes(heap, newParent, node)									# so should be higher therefore swap
		newParent <- parentNode(heap, node)									# find node's new parent
	}	
}

### S3 Generic Functions: External Methods

getTop <- function(heap) {UseMethod("getTop")}
extractTop <- function(heap, is.lighter) {UseMethod("extractTop")}
insert <- function(heap, node,  is.lighter) {UseMethod("insert")}
enqueue <- function(heap, node,  is.lighter) {UseMethod("enqueue")}
dequeue <- function(heap, node,  is.lighter) {UseMethod("dequeue")}
nextInQueue <- function(heap, node,  is.lighter) {UseMethod("nextInQueue")}

###
### S3 Methods: External Methods (implementation ***independent***   good property for external methods to have if possible)
###

getTop.heap <- function(heap) {
	getTopNode(heap)
}

extractTop.heap<- function(heap, is.lighter = `<`) {
	if(isEmpty(heap))
		top <- newHeapNode(NULL)
	else {
		top <- removeTopNode(heap)		
		newTop <- removeLastNode(heap)
		makeTopNode(heap, newTop)
		bubbleDown(heap, newTop, is.lighter)
		incExtractCount(heap)						# for simple heap statistics
	}		
	return(top)
}

insert.heap <- function(heap, node, is.lighter = `<`) {
	appendNode(heap, node)
	bubbleUp(heap, node, is.lighter)
	incInsertCount(heap)							# for simple heap statistics	
}

enqueue.priorityQueue <- function(pq, element, higher.priority = `<`){
	insert(pq, element, higher.priority)
}

dequeue.priorityQueue <- function(pq, higher.priority = `<`){
	extractTop(pq, higher.priority)
}

nextInQueue.priorityQueue <- function(pq){
	getTop(pq)
}




### print methods
print.heapNode <- function(node, ...){
	if(isNull(node))
		cat("<NULL>\n")
	else	
		cat("<", key(node), ", ", data(node), ">\n", sep = "")
}

print.heap <- function(heap, ...){
	if(isEmpty(heap)){
		print("Heap Empty")
	} else {
		h <- getH(heap)
		for(i in 1:getHeapSize(heap)) {
			node <- h[[i]]
			cat("[", i, "] ", sep = "")
			print(node)
		}
	}
}

summary.heap <- function(heap,...){
	cat("Heap with ", getHeapSize(heap), " elements\n")
	cat(insertCount(heap), "inserted,", extractCount(heap), "extracted. Largest heap size=", largestHeapSize(heap), "\n") 
}

heapTest1 <- function(){
	hN <- list()
	hN[[1]] <- newHeapNode(20, "a")
	hN[[2]] <- newHeapNode(4, "b")
	hN[[3]] <- newHeapNode(12, "c")
	hN[[4]] <- newHeapNode(7, "d")
	hp <- newHeap()
	for (i in 1:4){ 
		insert(hp, hN[[i]])
		cat("\nInsert", i, "\n")
		print(hp)
	}
}

heapTest2 <- function(){
	hN <- list()
	hN[[1]] <- newHeapNode(20, "a")
	hN[[2]] <- newHeapNode(4, "b")
	hN[[3]] <- newHeapNode(12, "c")
	hN[[4]] <- newHeapNode(7, "d")
	hp <- newHeap()
	for (i in 1:4){ 
		insert(hp, hN[[i]])
	}
	print("Heap")
	print(hp)
	while(!isEmpty(hp)){
		top <- extractTop(hp)
		cat("\nExtracted Top\n[1] ")
		print(top)
		cat("Heap\n")
		print(hp)
	}
	
	# extract once more
	top <- extractTop(hp)
	cat("\nExtracted Top\n[1] ")
	print(top)
	cat("Heap\n")
	print(hp)
}

heapTest3 <- function(n = 20, p = 0.5, printrun = FALSE){
	hp <- newHeap()
	for(i in 1:n){
		if(runif(1) < p) {
			newNode <- newHeapNode(runif(1), i)
			insert(hp, newNode)
			if(printrun){
				cat("\nInsert ")
				print(newNode)
				cat("Heap\n")
				print(hp)
			}
		} else {
			top <- getTop(hp)
			extractTop(hp)
			if(printrun){
				cat("\nExtract Top \n[1] ")
				print(top)
				cat("Heap\n")
				print(hp)
			}
		}
	}
	summary(hp)
}

pQueueTest3 <- function(n = 20, p = 0.5, printrun = FALSE){
	pq <- newPriorityQueue()
	for(i in 1:n){
		r <- runif(1)
		if(r < p) {
			newNode <- newPriorityQueueNode(runif(1), i)
			enqueue(pq, newNode)
			if(printrun){
				cat("\nEnqueue ")
				print(newNode)
				cat("in Priority Queue\n")
				print(pq)
			}
		} else {
			first <- nextInQueue(pq)
			dequeue(pq)
			if(printrun){
				cat("\nExtract first in queue \n[1] ")
				print(first)
				cat("Priority Queue\n")
				print(pq)
			}
		}
	}
	summary(pq)
}