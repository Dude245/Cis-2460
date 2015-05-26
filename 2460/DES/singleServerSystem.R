####################################################################################################
### The single server simulator system 
#
#   - to be used with the discrete event simulator 
#   - analogous to how the montyHall system was used by the monte carlo simulator
#
#
### To use the single server simulator system once in the discrete event simulator (in full verbose mode), 
###      enter the following function in the console
#
#    results <- discreteEventSimulator(new.singleServerSystem(), endTime = 100 , verbose = TRUE)
#    print.stats(results)
#
### To use the single server simulator system in the discrete event simulator repeatedly,say 2000 times.  
#        call the above in a for loop, storing the results in a list. or use the simulation.replications() 
#        function as in the example below (wich uses reps = 15). In either case the results will be a list 
#        of stats environments that can be printed with print.stats()).
#        Below is an example with a rep = 15.
#
#    singleServerSimulator <- function(){
#       discreteEventSimulator(new.singleServerSystem(), endTime = 100)
#    }
#
#    results <- simulation.replications(15, singleServerSimulator)
#
#    for(i in 1:length(results)){
#       print.stats(results[[i]], header = paste("i=", i, ": ", sep=""), after = "\n")
#    }
#

######################################################################################
#  The single server system
#
#     The simulator system for the simple single-server simulator from Chapter 3 of the text book

new.singleServerSystem <- function(){
	new.discreteEventSystem(firstEvent.name = "A", measure = identity,
							state = new.state(Ls = 0, Lq = 0),
						  	stats = new.stats(N = 0, NW = 0, WS = 0, WQ = 0, T0 = 0, Tmax = 0),
						  	updateFn.stats.default = base.statsUpdate,
						  	updateFns = new.updateFns(A = c(arrival.eventUpdate, arrival.statsUpdate), 
													  D = departure.eventUpdate, 
												  	  E = c(end.eventUpdate, end.statsUpdate)))
}

### The random variables used in the simulator - interArrivalTime and serviceTime

create.interArrivalTime <- function(){
	sample(1:8, 1, replace = TRUE)
}

create.serviceTime <- function(){
	sample(3:12, 1, replace = TRUE)
}


### The state used for the single server system and accessor functions
#
# Event State (inside the state environment)
#	 Ls = number of customers with servers ( Ls == 0 or Ls == 1 for a single server system)
#	 Lq = number of customers waiting

queueIsEmpty <- function(state){
	state$Lq == 0
}

incQueue <- function(state){
	state$Lq <- state$Lq + 1
}

decQueue <- function(state){
	state$Lq <- state$Lq - 1
}

serverIsIdle <- function(state){
	state$Ls == 0
}

serverIsBusy <- function(state){
	!serverIsIdle(state)
}

serverBecomesBusy <- function(state){
	state$Ls <- state$Ls + 1
}

serverBecomesFree <- function(state){
	state$Ls <- 0
}

#### Scheduling future events: after creation they are given to the scheduler to be placed on the FEL

schedule.arrivalEvent <- function(scheduler){
	schedule.futureEvent(scheduler, "A", create.interArrivalTime())
}

schedule.departureEvent <- function(scheduler){
	schedule.futureEvent(scheduler, "D", create.serviceTime())
}

### The event update routines -- must return a updateStatsInfo object 
###                              (using a new.singleServer.updateStatsInfo(state) call)

# arrival.eventUpdate with full comments describing when state is updated and when events are generated
#   - this has been commented out
#   - a more condensed version follows
#
# arrival.eventUpdate <- function(state, scheduler, verbose){
#	# process the new customer who just arrived with this arrival event
#	if(serverIsBusy(state)){					# if server is busy, have the new customer join the line
#		# update states: two states - qLength and serverBusy
#		# do not need to update server as server remains busy
#		incQueue(state)
#
#		# Generate Events
#		# 		- no events to generate		
#	}else{							# else server can take the new customer immediately
#		# update states: two states - qLength and serverBusy
#		# do not need to update queue as queue remains empty
#		serverBecomesBusy(state)
#
#		# Generate Events
#		# 	- new departure event created for when server will finish serving the new customer
#		schedule.departureEvent(scheduler)	
#	}
#	
#	# Generate next arrival event
#	# when one arrival occurs, next arrival generated and scheduled
#	schedule.arrivalEvent(scheduler)
#	
#	print.all.scheduler(scheduler, verbose, after = "\n")
#	print.state(state, verbose, header = "     ", after = '\n')
# }

arrival.eventUpdate <- function(state, scheduler, verbose){
	if(serverIsBusy(state)){			# if server is busy, have the new customer join the line
		incQueue(state)
	} else {							# else server can take the new customer immediately
		serverBecomesBusy(state)
		schedule.departureEvent(scheduler)	
	}
	
	# Generate next arrival event when one arrival occurs, next arrival generated and scheduled
	schedule.arrivalEvent(scheduler)
	
	print.all.scheduler(scheduler, verbose, after = "\n")
	print.state(state, verbose, header = "     ", after = '\n')
}


# departure.eventUpdate with full comments describing when state is updated and when events are generated
#   - this has been commented out
#   - a more condensed version follows
#
# departure.eventUpdate <- function(state, scheduler, verbose){
#	# process the server as the customer has now left 
#	# (the customer does not have to be considered as the customer has exited the system)
#	if(queueIsEmpty(state)){                                   # if no new customer, server is idle
#		# update states: two states - qLength and serverBusy
#		# do not need to update queue as queue remains empty
#		serverBecomesFree(state)
#
#		# Generate Events
#		# 		- no events to generate		
#	}else{                                                  # else server takes a new customer from queue 
#		# update states: two states - qLength and serverBusy
#		# do not need to update server as server remains busy
#		decQueue(state)                                     # queue shrinks by 1 since customer taken from queue
#
#		# Generate Events
#		#		- since new customer taken from queue, new departure event created
#		#		  for when server will finish serving the new customer
#		schedule.departureEvent(scheduler)	
#	}
#	
#	print.all.scheduler(scheduler, verbose, after = "\n")
#	print.state(state, verbose, header = "     ", after = '\n')
# }

departure.eventUpdate <- function(state, scheduler, verbose){
	if(queueIsEmpty(state)){                 # if no new customer, server is idle
		serverBecomesFree(state)
	} else {                                 # else server takes a new customer from queue and schedules a future departure
		decQueue(state)                                     
		schedule.departureEvent(scheduler)	
	}
	
	print.all.scheduler(scheduler, verbose, after = "\n")
	print.state(state, verbose, header = "     ", after = '\n')
}


# end.eventUpdate with full comments describing when state is updated and when events are generated
#   - this has been commented out
#   - a more condensed version follows
#
# end.eventUpdate <- function(state, scheduler, verbose){    
#	# Must be associated with event name 'E'
#	
#	# Can include state cleanup routines - not needed here
#	# Will not schedule any new events as they would not be run - the simulation has ended 
#	
#	# The 'E' event need not be written (not even this stub) if cleanup is not needed. System automatically creates one.
#	# In this case the default statsUpdate is used for the 'E' statsUpdate
#	
#	# If a different 'E' statsUpdate is required, but no cleanup state change is need, 
#	#    - this stub must be "written" and associated with the 'E' event along with the statsUpdate function
#	#    - this is the case here, see the statsUpdate section for details
#	
#	print.all.scheduler(scheduler, verbose, after = "\n")
#	print.state(state, verbose, header = "     ", after = '\n')
# }

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
	# There are no updates as an 'end' event has no effect
	# However we can't use the updateFn.stats.default as that runs the base.statsUpdate,
	#     which would compute stats based on a duration from the end evnet to the next scheduled event
	#     that will never happen and so produce incorrect statistics
	# Consequently we need an explicity end.statsUpdate that actively does "no updates"
	
	# We will print the final stats if in 'verbose' mode

	print.stats(stats, verbose, header = "     ", after = "\n")
}


#### Print Section - for debugging - to be used in the update routines created in this file

print.state <- function(state, verbose = TRUE, header = "", after = "") {
	if(verbose){
		cat(header)
		cat("state(LS = ", state$Ls, ", LQ = ", state$Lq, sep = "")
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