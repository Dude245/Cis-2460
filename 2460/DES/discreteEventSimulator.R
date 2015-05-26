# Load priorityQueue.R
# Load futureEventScheduler.R
# Load state_stats_Functions.R 
# Load DESystemFunctions.R 
# Load schedulerPrintFunctions.R (optional - for debugging)

#######################################################################################################################
### Main simulator routines - discreteEventSimulatorEnv() to be used with simulation.replications()

discreteEventSimulator <- function(DESystem, endTime = 100 , startTime = 0, verbose = FALSE){
	# Load the start and end times, and verbose flag, into the DESystem
	DESystem$startTime <- startTime
	DESystem$endTime <- endTime
	DESystem$verbose <- verbose
	DESystem$DESystem <- DESystem
	
	with(DESystem, {
		# DESSystem includes the following variable:
		#  - state (already initialized)
		#  - stats (already initialized)
		#  - firstEvent.name (usually initialized to 'A')
		#  - measure (if not supplied by the user, will by the identity function)
		
		# create the scheduler (includes the Future Events List (FEL) i.e a priority queue of events)
		scheduler <- new.scheduler(firstEvent.name, startTime, endTime)
		
		state$scheduler <- scheduler									# need if with(stats {...}) used in an update function 
		state$verbose <- verbose										#   and the schedulor and print functions are being called inside it

		# start simulation
		startSimulation(state)
				
		while (simulationActive(state)) {
			# time advance
			event <- dequeue.next.event(scheduler)						# get the new event from the schedule - the FEL is dequeued inside the scheduler
			time.advance(scheduler, event)           					# set the clock in the scheduler to the event time
		
			# setup and run eventUpdate
			add.values(state) <- stateInfo(event)
			eventUpdate <- eventFunction(DESystem, eventName(event))  	# get the eventUpdate for the new event from the simulator system
			eventUpdate(state, scheduler, verbose)                 		# run the event routine - both state and FEL will be effected/updatewd
		
			# setup stats for statsUpdate
			add.state.values(stats) <-  all.values(state, except = c("state", "scheduler", "DESystem"))		# copy all state values into stats
			stats$timeDiff <- time.to.nextEvent(scheduler, event)											# store time to nextEvent as timeDiff in stats
					
			# setup and run statsUpdate
			statsUpdate <- statsFunction(DESystem, eventName(event))  	# get the statsUpdate for the new event from the simulator system
			statsUpdate(stats, verbose)              					# run stats update
		}
		
		cleanup.stats(stats, state)
					
		return(measure(stats))
	})	
}

#######################################################################################################################
### Allows the colling of the discrete event simulator multiple times - see runSingleServerSystem.R for an example

simulation.replications <- function(reps = 1, simulator = monteCarloSimulation, 
									statistic = identity,  resultMode = "list", 
									printRep = function(replication){}) {
	result <- vector(resultMode, reps)
	for(i in 1:reps){
		result[[i]] <- simulator()
		printRep(result[[i]])
	} 
	return(statistic(result))
}