####################################################################################################
### The single tiring server simulator system 
#
#   - similar to the single server system, but with the service time is affected by the how long the previous service event took
#           (the server is more more tired the longer the previous service time lasted and this is directly related to
#           the length of the previous task and not to the length of time the server has been serving overall). 
#
#   - the extensive comments from singleServerSystem.R have been removed to concentrate more on the system
#
#   IMPORTANT!!!   <---
#   - this *entire* file must be loaded for the simulation to work as a single-tired-server simulation
#          *especially* if singleServerStats.R was loaded previously
#
### To use the single server simulator system once in the discrete event simulator (in full verbose mode), 
###      enter the following function in the console
#
#    results <- discreteEventSimulator(new.singleTiringServerSystem(), endTime = 100 , verbose = TRUE)
#    print.stats(results)
#
### To use the single tiring server simulator system in the discrete event simulator repeatedly,say 2000 times.  
#        call the above in a for loop, storing the results in a list. or use the simulation.replications() 
#        function as in the example below (wich uses reps = 15). In either case the results will be a list 
#        of stats environments that can be printed with print.stats()).
#        Below is an example with a rep = 15.
#
#    singleTiringServerSimulator <- function(){
#       discreteEventSimulator(new.singleTiringServerSystem(), endTime = 100)
#    }
#
#    results <- simulation.replications(15, singleTiringServerSimulator)
#
#    for(i in 1:length(results)){
#       print.stats(results[[i]], header = paste("i=", i, ": ", sep=""), after = "\n")
#    }
#

######################################################################################
#  The single tiring server system
#
#     The simulator system for the simple single-tiring-server simulator, 
#          an extension from the single-server simulator from Chapter 3 of the text book

new.singleTiringServerSystem <- function(){
	new.discreteEventSystem(firstEvent.name = "A", measure = identity,
							state = new.state(Ls = 0, Lq = 0, prevST = 7.5, avgST = 7.5),
						  	stats = new.stats(N = 0, NW = 0, WS = 0, WQ = 0, T0 = 0, Tmax = 0),
						  	updateFn.stats.default = base.statsUpdate,
						  	updateFns = new.updateFns(A = c(arrival.eventUpdate, arrival.statsUpdate), 
													  D = departure.eventUpdate, 
												  	  E = c(end.eventUpdate, end.statsUpdate)))
}

### The random variables used in the simulator - interArrivalTime and serviceTime

random.interArrivalTime <- function(){
	sample(1:8, 1, replace = TRUE)
}

random.serviceTime <- function(){
	sample(3:12, 1, replace = TRUE)
}


### The state used for the single server system and accessor functions
#
# Event State (inside the state environment)
#	 Ls = number of customers with servers (Ls == 0 or Ls == 1 for a single server system)
#	 Lq = number of customers waiting
#    avg.serviceTime = the typical (average) service time (a constant = 7.5 for the random number generator used)
#    prev.serviceTime = the time the server took to serve the previous customer

### The event update routines -- must return a updateStatsInfo object 
###                              (using a new.singleServer.updateStatsInfo(state) call)

arrival.eventUpdate <- function(state, scheduler, verbose){
	with(state, {
		if(Ls > 0){									# if server is busy
			Lq = Lq + 1								#	  increment queue (the new customer join the line)
		} else {									# else server can take the new customer immediately
			Ls = 1									#     serve becomes busy and a departure event is generated
			
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

departure.eventUpdate <- function(state, scheduler, verbose){
	with(state, {
		if(Lq == 0){                     # if no new customer
			Ls = 0						 #     server is idle 
		} else {                         # else server takes a new customer from queue
			Lq = Lq - 1   				 #     decrement the queue size and schedule a future departure
			                                  
			ST = round(random.serviceTime() * prevST / avgST)  
			   
			# Schedule departure of the customer from the server. 
			# Store the serviceTime as prev.serviceTime in the future event to calculate the service time when the next customer is served in the future.
			schedule.futureEvent(scheduler, "D", ST, list(prevST = ST))
		}
	})
	
	print.all.scheduler(scheduler, verbose, after = "\n")
	print.state(state, verbose, header = "     ", after = '\n')
}

end.eventUpdate <- function(state, scheduler, verbose){    
	print.all.scheduler(scheduler, verbose, after = "\n")
	print.state(state, verbose, header = "     ", after = '\n')
}


### Statistics section

# Inside the stats environment
#
# Basic Data
#    N  = Total Number of customers
#    NW = Total Number of customers who have to queue
#    WS = total service time
#    WQ = total wait time
#    T0 = total idle time (since this is a single server system)  
#    Tmax = total clock time
#
# Event State (a copy of the current state of the simulation is automatically added to stats) 
#	 Ls = number of customers with servers ( Ls == 0 or Ls == 1 for a single server system)
#	 Lq = number of customers waiting
#
# Event Duration (i.e. time between the current event and the next event to be processed)
#    timeDiff = current event duration

base.statsUpdate <- function(stats, verbose){	# this is the default stats update
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
	
	# the base statistics update also needs to be run
	base.statsUpdate(stats, verbose)		
}

end.statsUpdate <- function(stats, verbose){
	print.stats(stats, verbose, header = "     ", after = "\n")
}


#### Print Section - for debugging - to be used in the update routines created in this file

print.state <- function(state, verbose = TRUE, header = "", after = "") {
	if(verbose){
		cat(header)
		with(state, {
			cat("state(LS = ", Ls, ", LQ = ", Lq, ", prevST = ", prevST, ", ST = ", ST ,sep = "")
		})
		cat(")")
		cat(after)
	}
}

print.stats <- function(stats, verbose = TRUE, header = "", after = "") {
	if(verbose){
		cat(header)
		with(stats, {
			cat("stats(N = ", N, ", NW = ", NW, ", WS = ", WS, ", WQ = ", WQ, ", T0 = ", T0,  sep = "") 
			cat(", Tmax = ", Tmax, sep = "")
		})
		cat(")")
		cat(after)
	}
}

print.all.scheduler <- function(scheduler, verbose, header = "", after = ""){
	if(verbose){
		cat(header)
		print.clock(scheduler, header = "Clock = ")
		print.nextEvent.clockTime(scheduler, header = " -> ")
		print.time.to.nextEvent(scheduler, header = "  (timeDiff = ", after = ")")
		print.currentEvent.name(scheduler, header = "\n     Event = ")
		print.nextEvent.name(scheduler, header = "  (Next = ", after = ")\n")
		print.eventsScheduledThisUpdate(scheduler, header = "     Future events = ")
		cat(after)
	}
}