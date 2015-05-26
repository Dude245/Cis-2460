# Public Functions available to the simulation user

# Runs the simulator - uses a DE simulator system defined by new.discreteEventSystem
# Defined in discreteEventSimulator.R

stats <- discreteEventSimulator(DESystem, endTime = 100 , startTime = 0, verbose = FALSE)

# Note: discreteEventSimulator() actually returns whatever the user supplied measure() returns
#		- if measure is not supplied, discreteEventSimulator() returns the stats environment unalterned
#		- Typically measure will modify the stats environment, 
#			calculating statistics from the basic stats data and adding it to the stats environment,
#			and then return the stats environment for analysis

# Creates a DE simulator system to be used by the discreteEventSimulator
# Defined in DESystemFunctions.R

DESystem <- new.discreteEventSystem <- function(state = NULL, stats = NULL, firstEvent.name = character(), 
												measure = identity, update.stats.default = NULL, updateFns = list())
			 
# Scheduling a future event on the scheduler
# Defined in futureEventScheduler.R

schedule.futureEvent(scheduler, eventName, duration = 0, stateInfo = list(), at.time = NULL)
clock(scheduler)

# Creating and initializing the state and stats to be used in the discrete event system
# Defined in state_stats_Functions.R

state <- new.state(stateName = stateValue, ...)
stats <- new.stats(statsName = statsValue, ...)

#### Print routines for debugging the user defined discrete event system

####	Used in event update routines

# Defined in schedulerPrintFunctions.R
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
