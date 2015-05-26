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
myFn <-function(stats)
{
  #Do ALL the Maths
  stats$wq<-(stats$WQ/stats$N)
  stats$wq<-round(stats$wq, 2)
  stats$pw=(stats$NW/stats$N)
  #Round all the numbers to keep it pretty
  stats$pw<-round(stats$pw, digits = 1)
  stats$P0=(stats$T0/stats$Tmax)
  stats$P0<-round(stats$P0, digits = 2)
  stats$PBS=(1/stats$P0)
  stats$PBS<-round(stats$PBS, digits = 2)
  stats$ws=(stats$WS/stats$N)
  stats$ws<-round(stats$ws, digits = 2)
  stats$AVM=(1/stats$ws)
  stats$AVM<-round(stats$AVM, digits = 2)
  stats$AVH=(stats$N/stats$Tmax)
  stats$AVH<-round(stats$AVH, digits = 2)
  #Return the whole shebang
  stats
}
print.stats <- function(stats, verbose = TRUE, header = "", after = "") {
  if(verbose){
    cat(header)
    
    with(stats, {

      cat("stats(N = ", N, ", NW = ", NW, ", WS = ", WS, ", WQ = ", WQ, ", T0 = ", T0,  sep = "") 
      cat(", Tmax = ", Tmax,"\n", #THis is where I add my custom values
          "     wq = ",wq," pw = ",pw,", P0 = ",P0,
          ", PBS = ",PBS,", ws = ",ws,", AVM = ",AVM," AVH = ",AVH,sep = "")
      
    })
    cat(")")
    cat(after)
  }
}

my.singleServerSystem <- function(){
  new.discreteEventSystem(firstEvent.name = "A",
                          measure = myFn,
                          state = new.state(Ls = 0, Lq = 0),
                          stats = new.stats(N = 0, NW = 0, WS = 0, WQ = 0, T0 = 0, Tmax = 0,
                                            wq=0,pw=0,P0=0, PBS=0,ws=0, AVM=0,AVH=0),
                          updateFn.stats.default = base.statsUpdate,
                          updateFns = new.updateFns(A = c(arrival.eventUpdate, arrival.statsUpdate), 
                                                    D = departure.eventUpdate, 
                                                    E = c(end.eventUpdate, end.statsUpdate)))
}
results <-discreteEventSimulator(my.singleServerSystem(),startTime=00, endTime = 500 , verbose = FALSE)
print.stats(results)
results <-discreteEventSimulator(my.singleServerSystem(),startTime=00, endTime = 1000 , verbose = FALSE)
print.stats(results)
results <-discreteEventSimulator(my.singleServerSystem(),startTime=00, endTime = 5000 , verbose = FALSE)
print.stats(results)