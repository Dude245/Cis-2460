#############################################################################################
### Load and run
basePath <- "C:/Users/Nathan/Desktop/2460/DES/"  # or where ever you put the simulator.

### load simulator (copied from loadSimulator.R)
loadList <- c("state_stats_Functions.R", "DESystemFunctions.R", 
              "priorityQueue.R", "futureEventScheduler.R", "schedulerPrintFunctions.R",
              "discreteEventSimulator.R")

for(fileName in loadList){
	source(paste(basePath, fileName, sep=""))
}

### load singleServerSysterm.R
source(paste(basePath, "singleTiringServerSystem.R", sep=""))

### run DE simulator and store results in 'results'
results <- discreteEventSimulator(new.singleTiringServerSystem(), endTime = 100 , verbose = TRUE)



#############################################################################################
### Re-Run (after full system already loaded)

sss <- new.singleServerSystem()
results <- discreteEventSimulator(sss, startTime= 0, endTime = 100, verbose = TRUE)
print.stats(results)

## or you can run

results <- discreteEventSimulator(new.singleTiringServerSystem(), endTime = 100, verbose = TRUE)
print.stats(results)

# note: startTime = 0 is the default value. That is why I have been leaving it off in some calls
#       I have included it in in other calls just to remind you that it is there and can be used
#      for other assignments



################################################################
### Run with multiple reps (after full system already loaded)

reps <- 10
results <- vector("list", reps)												  	  
for(i in 1:reps){
	results[[i]] <- discreteEventSimulator(new.singleTiringServerSystem(), endTime = 100)
}

for(i in 1:length(results)){
	print.stats(results[[i]], header = paste("i=", i, ": ", sep=""), after = "\n")
}



#############################################################################################
### Run with multiple reps (after full system already loaded) using simulationReplication.R

singleServerSimulator <- function(){
	discreteEventSimulator(new.singleTiringServerSystem(), startTime = 50, endTime = 200)
}

results <- simulation.replications(15, singleServerSimulator)

for(i in 1:length(results)){
	print.stats(results[[i]], header = paste("i=", i, ": ", sep=""), after = "\n")
}
