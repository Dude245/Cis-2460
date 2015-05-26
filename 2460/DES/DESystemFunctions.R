#### Discrete Event System (i.e. DESystem) functions

######################
### Public function - available to the user
#
# new.discreteEventSystem
#		state {type environment} - the initial state for the DESystem
#		stats {type environment} - the initial stats environment for the DESystem
#		update.stats.default {type function, default: NULL} 
#			- the update stats function used if one is not provided with an event in update.functions
#		update.functions {type list, default: empty} 
#           - a list of event names associated with an (event update function, statsInfo update function) pair
#           - to be added to updateFn.table
#			- if only the event update function is supplied, the update.stats.default function will be used
#		firstEvent.name {type character, default: empty} - first event to be placed on queue at time 0)
#		measure {type function, default: identity} 
#			- produces the actual random variable from the stats record that will be statistically analyzed
#			- default (identity) is to return the stats table that holds all relevant stats variables for further analysis


new.discreteEventSystem <- function(state = NULL, stats = NULL, firstEvent.name = character(), measure = identity,  
						 			updateFn.stats.default = NULL, updateFns = list()){
	state; stats; firstEvent.name; measure
	updateFns <- updateEndFn(updateFns)
	updateFns <- mergeWithDefault(updateFns, updateFn.stats.default)
	environment()
}

# new.updates is a synonym for list
#    - Associated with the update.functions parameter 
#      just as new.state() and new.stats() are associated with the state and stats parameter
#      of new.discreteEventSystem() repsectively

new.updateFns <- list

######################
### Accessor functions for update pairs, i.e. the pair c(eventUpdate, statsUpdate) associated with an event
#
# private functions to DESSystemFunctions.R

eventUpdateFn <- function(updatePair){
	if(is.list(updatePair))
		updatePair[[1]]
	else
		updatePair
}

# if the updatePair only has an event update (is a singleton), then use the passed in default
statsUpdateFn <- function(updatePair, default = function(stats, verbose){}){
	if(is.list(updatePair))
		updatePair[[2]]
	else
		default
}

######################
### Setup functions used in new.discreteEventSystem()

mergeWithDefault <- function(update.functions, update.stats.default){
	mergeFn <- function(updatePair){
		if (length(updatePair) > 1){
			updatePair
		}else{
			if(is.null(update.stats.default))
				stop("An update.stats.default function is required. None was provided.")
			else
				c(updatePair, update.stats.default)
		}
	}
	
	lapply(update.functions, mergeFn)
}

# updateEndFn
#		The end event is an event function that ends the simulation (event name = "E").
#		Code necessary for the working of the simulator engine needs to be added 
#         to the event update code (possibly) provided by the user
#		If the user did not provide an 'E' event update, one is added here

updateEndFn <- function(update.functions){
	end.update <- update.functions[["E"]]
	user.end.eventUpdate <- eventUpdateFn(end.update)
	
	default.end.eventUpdate <- function(state, scheduler, verbose){
		endSimulation(state)
	}

	end.eventUpdate <- function(state, scheduler, verbose){
		default.end.eventUpdate(state, scheduler, verbose)				# adds the simulation ending functionality
		user.end.eventUpdate(state, scheduler, verbose)					# to the user defined part of end event
	}
	
	if(is.null(end.update))
		update.functions[["E"]] <- default.end.eventUpdate				# ensures end.event exists
	else if(length(end.update) == 1)
		update.functions[["E"]] <- end.eventUpdate			
	else {	
		end.statsUpdate. <- statsUpdateFn(end.update)
		update.functions[["E"]] <- c(end.eventUpdate, end.statsUpdate)	# ensures end.event exists
	}
	update.functions
}

######################
### Simulator Engine Public functions 
#      - available elsewhere in the simulator engine 
#      - not to the user

# Returns the update function pair associated with the event name in the DESystem
updateFunction <- function(DESystem, eventName){
	(DESystem$updateFns)[[eventName]]
}

# Returns the eventUpdate function associated with the event name in the DESystem
eventFunction <- function(DESystem, eventName){
	eventUpdateFn(updateFunction(DESystem, eventName))
}

# Returns the statsUpdate function associated with the event name in the DESystem
statsFunction <- function(DESystem, eventName){
	statsUpdateFn(updateFunction(DESystem, eventName))
}

#########################
# extra functions for debugging simulator engine 
#    - allows the programmer to see all event and stats update functions 

updateFunctions <- function(DESystem){
	updateFn.table <- DESystem$updateFn.table
	as.list(updateFn.table, FALSE)				# 	returns all events except those whose name begin with a '.' 
}

eventFunctions <- function(DESystem){
	update.fns <- updateFunctions(DESystem)
	lapply(update.fns, eventUpdateFn)
}

statsFunctions <- function(DESystem){
	update.fns <- updateFunctions(DESystem)
	lapply(update.fns, statsUpdateFn)
}
