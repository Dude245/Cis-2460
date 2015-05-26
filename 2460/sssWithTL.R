###
##A3
##
#   average service rate per hour 
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
source(paste(basePath, "singleServerSystem.R", sep=""))
base.statsUpdate <- function(stats, verbose){  # this is the default stats update
  with(stats, {
    WS <- WS + timeDiff * Ls
    WQ <- WQ + timeDiff * Lq
    if(Ls == 0)
      T0 <- T0 + timeDiff
    Tmax <- Tmax + timeDiff
    ##mycode
    if(is.na(T[N])==TRUE)
    { T[N]=0
      
    }
    T[N]<-T[N]+timeDiff
    ##mycode
  })
  
  print.stats(stats, verbose, header = "     ", after = "\n\n")
}
print.stats <- function(stats, verbose = TRUE, header = "", after = "") {
  if(verbose){
    cat(header)
    
    with(stats, {
      
      cat("stats(N = ", N, ", NW = ", NW, ", WS = ", WS, ", WQ = ", WQ, ", T0 = ", T0,  sep = "") 
      cat(", Tmax = ", Tmax,"\n") #THis is where I add my custom values
      for(i in 1:length(T))
      {
        cat("T(",i,")=",T[i],", ",sep="")
      }
      cat("\n\n")
      for(i in 1:length(T))
      {
        cat("T(",i,")%=",(T[i]/Tmax),", ",sep="")
      }
      
    })
    cat(")")
    cat(after)
  }
}

my.singleServerSystem <- function(){
  new.discreteEventSystem(firstEvent.name = "A",
                          state = new.state(Ls = 0, Lq = 0),
                          stats = new.stats(N = 0, NW = 0, WS = 0, WQ = 0, T0 = 0, Tmax = 0,T=0),
                          updateFn.stats.default = base.statsUpdate,
                          updateFns = new.updateFns(A = c(arrival.eventUpdate, arrival.statsUpdate), 
                                                    D = departure.eventUpdate, 
                                                    E = c(end.eventUpdate, end.statsUpdate)))
}
results <-discreteEventSimulator(my.singleServerSystem(), endTime = 100 , verbose = FALSE)
print.stats(results)
results <-discreteEventSimulator(my.singleServerSystem(), endTime = 500 , verbose = FALSE)
print.stats(results)
results <-discreteEventSimulator(my.singleServerSystem(), endTime = 1000 , verbose = FALSE)
print.stats(results)
results <-discreteEventSimulator(my.singleServerSystem(), endTime = 5000 , verbose = FALSE)
print.stats(results)