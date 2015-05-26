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
arrival.eventUpdate <- function(state, scheduler, verbose){
  with(state, {
    if(Ls > 0){									# if server is busy
      Lq = Lq + 1								#	  increment queue (the new customer join the line)
    } else {									# else server can take the new customer immediately
      Ls = 1
      
      #     serve becomes busy and a departure event is generated
      #Generate 100 numbers based on half of the last one, to twice the last value
      avgST<-sample((prevST/2):(prevST*2), 100, replace = TRUE)
      #Get the middle value
      avgST<-median(avgST)
      ST = round(random.serviceTime() * prevST / avgST)  
      
      # Schedule departure of the customer from the server. 
      # Store the serviceTime as prevST in the future event to calculate the service time when the next customer is served in the future.
      schedule.futureEvent(scheduler, "D", ST, list(prevST = ST))
    }
    
    # Schedule next arrival event: when one arrival occurs, next arrival generated and scheduled on the FEL as a futureEvent
    schedule.futureEvent(scheduler, "A", random.interArrivalTime())
  })
  
  print.all.scheduler(scheduler, verbose, after = "\n")
  print.state(state, verbose, header = "     ", after = '\n')
}

new.singleTiringServerSystem <- function(){
  new.discreteEventSystem(firstEvent.name = "A", measure = identity,
                          state = new.state(Ls = 0, Lq = 0, prevST = 7.5, avgST = 7.5),
                          stats = new.stats(N = 0, NW = 0, WS = 0, WQ = 0, T0 = 0, Tmax = 0),
                          updateFn.stats.default = base.statsUpdate,
                          updateFns = new.updateFns(A = c(arrival.eventUpdate, arrival.statsUpdate), 
                                                    D = departure.eventUpdate, 
                                                    E = c(end.eventUpdate, end.statsUpdate)))
}

results <- discreteEventSimulator(new.singleTiringServerSystem(), endTime = 100, verbose = TRUE)
print.stats(results)