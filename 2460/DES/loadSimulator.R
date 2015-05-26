basePath <- "C:/Users/Nathan/Desktop/2460/DES/"  # or where ever you put the simulator.

loadList <- c("state_stats_Functions.R", "DESystemFunctions.R", 
              "priorityQueue.R", "futureEventScheduler.R", "schedulerPrintFunctions.R",
              "discreteEventSimulator.R")

for(fileName in loadList){
	source(paste(basePath, fileName, sep=""))
}
