
#### State and Stats functions
#
# state and stats are both environments

##################
### Public Function 
#     - available to the user

# new.state, new.stats
#    Creates a new env table initialized to values associated with the named arguments in the call function
#    e.g. state <- newState(var1 = value1, var2 = value2)
#    e.g. stats <- newStats(var1 = value1, var2 = value2)

new.state <- function(...){
	state <- environment()				# allows functions defined at the top level of R to run inside   with(state ...
	add.values(state) <- list(...)		# adds the variable value pairs, one-by-one - forces strict evalutation
	state$state <- state				# places a pointer to the state env inside the state env
										#    allows functions on state to be run inside   with(state ...
	state
}

new.stats <- function(...){
	stats <- environment()				# allows functions defined at the top level of R to run inside   with(stats ...
	add.values(stats) <- list(...)		# adds the variable value pairs, one-by-one - forces strict evalutation
	stats$stats <- stats				# places a pointer to the stats env inside the stats env
										#    allows functions on stats to be run inside   with(stats ...
	stats
}

##################
### Functions on states as used by the event simulator engine
#      - not available to the user

# Return all values from the state or stats environment as a list
# Similar to as.list() but adds the ability to prevent the copying of values
#   that are on the 'except' list

all.values <- function(env, except = list()){
	equals.removeValues <- function(x){
		sum(except == x) > 0
	}

	valuesList <- as.list(env)
	
	if(length(except) > 0){
		ri <- sapply(names(valuesList), equals.removeValues)
		valuesList <- valuesList[!ri]
	}
	
	valuesList
}


# Add values to state or stats from a list
#    Takes a list of variable-name / value pairs and assigns the value to the variable name 
#       in the state or stats environment
#    e.g. add.values(state) <- list(var1 = value1, var2 = value2)

`add.values<-` <- function(env, value = list()){
	n <- length(value)
	if(n > 0){
		varNames <- names(value)
		for(i in 1:n){
			varName <- varNames[[i]]
			env[[varName]] <- value[[i]]
		}
	}
	env
}

# Same as add.values<- but adds a variable 'state' that holds the state values so accessor state functions
#    can be used inside a with(env, {...}) call; typically used for adding state info into a 'stats' environment
`add.state.values<-` <- function(env, value = list()){
	add.values(env) <- value
	env$state <- value
	env
}


# Cleanup function -> removes variables from stats that are useful for statsUpdate 
#                     (especially inside with(stats, {...}))
#                     but are not actual stats variables

cleanup.stats <- function(stats, state){
	state.names <- names(all.values(state, except = "scheduler"))
	rm(list = state.names, envir = stats)
	rm(list = c("stats", "timeDiff"), envir = stats)
}


# functions that control the "simulation active" variable in state
#     note: this variable is not returned when allStateValues(state) is called

simulationActive <- function(state){
	state$`.*active*`
}

startSimulation <- function(state){
	state$`.*active*` <- TRUE
}

endSimulation <- function(state){
	state$`.*active*` <- FALSE
}