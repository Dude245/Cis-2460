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
  P<-.3
 #Gloabalish Items
  items <- rpois(1, 20)
  
    #20% of people look
    random<-sample(1:100,1,replace=FALSE)
    if(random<=20)
    {
      
      stats$WQ<-(stats$WQ+(rgamma(1, items, 1/2.5)))
    }
    random<-sample(1:100,1,replace=FALSE)
    #65% go to a teller
    if(random<=65)
    {
      #Percent change of a loop for a normal day
      while((stats$WQ)<=P*600 && P>=0)
      {
      stats$WQ<-(stats$WQ+(rexp(1,1/15)))
      random<-sample(1:100,1,replace=FALSE)
      if(random<=40)
      {
        #40% of 60% look at things
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
  }  
  
  #Return the whole shebang
  stats
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
total<-0
for(i in 1:10)
{
  total<-results$T0+total
  results <- discreteEventSimulator(Cam(), endTime = 600 , verbose = FALSE)
  print.stats(results)
  print('')
}
print(total/10)
