#############################################################################################
### Load and run

basePath <- basePath <- "C:/Users/Nathan/Desktop/2460/DES/"  # or where ever you put the simulator.

### load simulator (copied from loadSimulator.R)
loadList <- c("state_stats_Functions.R", "DESystemFunctions.R", 
              "priorityQueue.R", "futureEventScheduler.R", "schedulerPrintFunctions.R",
              "discreteEventSimulator.R")

for(fileName in loadList){
  source(paste(basePath, fileName, sep=""))
}

### load singleServerSysterm.R
source(paste(basePath, "singleServerSystem.R", sep=""))
create.interArrivalTime <- function(){
  #sample(1:20, 1, replace = TRUE)
  rexp(1,1/12)
}

create.serviceTime <- function(){
  sample(3:12, 1, replace = TRUE)
}
myFn <-function(stats)
{
  items <- rpois(1, 20)
  random<-sample(1:100,1,replace=FALSE)
  if(random<=20)
  {
    
    stats$WQ<-(stats$WQ+(rgamma(1, items, 1/2.5)))
  }
  random<-sample(1:100,1,replace=FALSE)
  if(random<=65)
  {
  
    stats$WQ<-(stats$WQ+(rexp(1,1/15)))
    random<-sample(1:100,1,replace=FALSE)
    if(random<=40)
    {
      stats$WQ<-(stats$WQ+(rgamma(1, items, 1/2.5)))
      random<-sample(1:100,1,replace=FALSE)
      if(random<=40)
      {
        #DO NOthing
      }
    else
    {
      #Buy Things
      pois<-rpois(1,10)
      pois=pois*.2
      pois=pois+3
     stats$WQ<-(stats$WQ+(pois))
    }
    
    }
  }
  #Do ALL the Maths
  
  
  #Return the whole shebang
  stats
}
base.statsUpdate <- function(stats, verbose){  # this is the default stats update
  with(stats, {
    WS <- WS + timeDiff * Ls
    WQ <- WQ + timeDiff * Lq
    if(Ls == 0)
      T0 <- T0 + timeDiff
    Tmax <- Tmax + timeDiff
  })
  
  print.stats(stats, verbose, header = "     ", after = "\n\n")
}

arrival.statsUpdate <- function(stats, verbose){
  with(stats, {
    N <- N + 1
    if(Lq > 0) {
      NW <- NW + 1
    }
  })
}
  
Cam<- function(){
  new.discreteEventSystem(firstEvent.name = "A", measure = myFn,
                          state = new.state(Ls = 0, Lq = 0, prevST = 7.5, avgST = 7.5),
                          stats = new.stats(N = 0, NW = 0, WS = 0, WQ = 0, T0 = 0, Tmax = 0,TE0=0),
                          updateFn.stats.default = base.statsUpdate,
                          updateFns = new.updateFns(A = c(arrival.eventUpdate, arrival.statsUpdate), 
                                                    D = departure.eventUpdate, 
                                                    E = c(end.eventUpdate, end.statsUpdate)))
}
### run DE simulator and store results in 'results'
for(i in 1:10)
{
results <- discreteEventSimulator(Cam(), endTime = 600 , verbose = FALSE)
print.stats(results)

print('')
}
