######################################################################################
#   Interface to Simulator used in Event Update Routines
######################################################################################

###############
### Event update routines 

# Written by the user
#	- can be called anything
#   - there has to be one associated with each event
# 	- will be passed the (state, scheduler, verbose flag) as argument
#	  so must have these function parameters in this order

arrival.eventUpdate <- function(state, scheduler, verbose){
	# body
}

###############
### Creating and manipulating state 

# To create new state function to be used by the simulator 
#   (and which is ultimately passed to your event update routine)
#   use new.state()

new.state(Ls = 0, Lq = 0)

# -  States are just environments with variables that use the same names (and initial values) supplied in new.state()
# -  State variables can be accessed and modified in the same way you would any environment variables

if(state$Ls > 0)
	state$Lq <- state$Lq + 1

# You can use the 'with' function to access the state variables without needing the state$
# You can still access any state and scheduler functions 
#   (as well as any functions defined at the top level of R), 
#   but you must use the symbols `state`, and `scheduler` (you do not need the backquotes)
#	no matter what parameter name you used in your event update routine
# To reduce confusion, it is recommended that you use the parameter names 
#      `state`, `scheduler` and `verbose` (without the backquotes)
#      unlike in the example below 

arrival.eventUpdate <- function(myState, myScheduler, verbose){
	(with myState, {
		if(isBusy(state))
			Lq <- Lq + 1
			schedule.futureEvent(scheduler, "A", create.interArrivalTime())
	})
}

###############
###  Scheduling new future events: after creation they are placed on the priority queue

# The typical way of scheduling the future event is

schedule.futureEvent(scheduler, "A", create.interArrivalTime())

# The current clock time can be obtained from the scheduler

clock(scheduler)

# An alternate way of scheduling an event: calculating the clock time for the future event directly
#   - Not recommended, because usually unnecessary 
#        - In most cases the service time (or inter-arrival time) is all that is needed and the scheduler can 
#          calculate the clock time by itself. So the original approach above is all that is needed.
#   - The facility is there for more complex clock time calculations that might be required for a simulation
#   - Note: The at.time parameter name must be used to set the clock time of the future event directly
#           If used, the scheduled.futureEvent() function will ignore any value given through the duration parameter
#              i.e. the at.time parameter has precidence

schedule.futureEvent(scheduler, "D", at.time = calculateClockTime(clock(scheduler), otherInfo))

# Storing "state information" when scheduling a future event
#   - There is the option (not needed in a simple single server system) for information to be stored in a future event
#     and then when the event is dequeued from the FEL by the scheduler, that infomation is placed in the state and
#     can be accessed by the current event's stateUpdate (and statsUpdate)
#   - The 'stateInfo' is made available to the scheduler through a fourth parameter in the schedule.futureEvent() function
#        - The information is passed as a list of the form list(stateName = stateInfo) where stateInfo can be any type of information
#          which will be stored as a state variable called 'stateName' when the future event is dequeued and loaded 
#        - The state variable 'stateName' should be declared as a state variable in new.state so it can be given an initial value
#   - As an example 
	      - it may be that the service time is affected by the how long the previous service event took
#           (the server is more more tired the longer the previous service time lasted and this is directly related to
#           the length of the previous task and not to the length of time the server has been serving overall). 
#         - The following code inside a departure eventUpdate allows for this to be calculated
       
departure.eventUpdate <- function(state, scheduler, verbose){
	with(state, {
		if(Lq == 0){                     # if no new customer, server is idle
			Ls = 0
		} else {                         # else server takes a new customer from queue and schedules a future departure
			Lq = Lq - 1                                     
	
			# note: ST is now also stored in 'state' and so will be available to the print functions and updateStats routine when run
			ST = round(random.serviceTime() * prevST / avgST)  
			   
			schedule.futureEvent(scheduler, "D", ST, list(prevST = ST))
		}
	})
}


######################################################################################
#   Interface to Simulator used in Stats Update Routines
######################################################################################

# creating the stats object complete with stats variables
new.stats(N = 0, NW = 0, WS = 0, WQ = 0, T0 = 0, Tmax = 0)   

# Modifying the variables in the stats object is done in statsUpdate routines, which are user defined
#    - There is one per event, although a default statsUpdate can be defined
#    - The time between this event and the next is accesible through an automatically 
#	       set variable 'timeDiff' inside stats
#    - Note: The user does not declare 'timeDiff' in new.stats()
# The 'with' function allows direct updating inside the stats object. 
#    - The stats variables can be directly referenced by name 

base.statsUpdate <- function(stats, verbose){	# this is the default stats update
	with(stats, {
		WS <- WS + timeDiff * Ls
		WQ <- WQ + timeDiff * Lq
		if (Ls == 0)
			T0 <- T0 + + timeDiff
		Tmax <- Tmax + timeDiff
	})
}

######################################################################################
#    Print routines for debugging the user defined discrete event system
######################################################################################

####	Used in event update routines

# Defined with the system
print.clock(scheduler, line.breaks = 0, header = "", after = "")
print.currentEvent(scheduler, line.breaks = 0, header = "", after = "")
print.currentEvent.name(scheduler, line.breaks = 0, header = "", after = "")
print.currentEvent.clockTime(scheduler, line.breaks = 0, header = "", after = "")
print.nextEvent(scheduler, line.breaks = 0, header = "", after = "")
print.nextEvent.name(scheduler, line.breaks = 0, header = "", after = "")
print.nextEvent.clockTime(scheduler, line.breaks = 0, header = "", after = "")
print.time.to.nextEvent(scheduler, line.breaks = 0, header = "", after = "")
print.eventsScheduledThisUpdate(scheduler, line.breaks = 0, sep = ", ", header = "", after = "")

# Defined by user
print.state(state, ...)

####	Used in stats update routines

# Defined by user
print.stats(stats, ...)



######################################################################################
#   Interface to Simulator - creating the simulator system and running the simulator
######################################################################################

#### The simulator system for the simple single-server simulator from Chapter 3 of the text book

singleServerSystem <- new.discreteEventSystem(firstEvent.name = "A", measure = identity,
							state = new.state(Ls = 0, Lq = 0),
						  	stats = new.stats(N = 0, NW = 0, WS = 0, WQ = 0, T0 = 0, Tmax = 0),
						  	updateFn.stats.default = base.statsUpdate,
						  	updateFns = new.updateFns(A = c(arrival.eventUpdate, arrival.statsUpdate), 
													  D = departure.eventUpdate, 
												  	  E = c(end.eventUpdate, end.statsUpdate)))												  	  

##### Call to the simulator using the singleServerSystem

result <- discreteEventSimulator(singleServerSystem, endTime = 100 , startTime = 0, verbose = TRUE)

# Note: discreteEventSimulator() returns whatever the user supplied measure() returns
#		- if measure is not supplied, discreteEventSimulator() returns the stats environment unalterned
#		- Typically measure will modify the stats environment, 
#			calculating statistics from the basic stats data and adding it to the stats environment,
#			and then return the stats environment for analysis

