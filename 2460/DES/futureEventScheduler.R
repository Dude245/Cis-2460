# Load priorityQueue.R to use

########################################################################################
#### Future event - creation and accessing

new.futureEvent <- function(eventName, eventTime, stateInfo = list()) {
	newPriorityQueueNode(eventTime, list(eventName = eventName, stateInfo = stateInfo))
}

eventTime <- function(event){
	key(event)
}

eventName <- function(event){
	with(event, {
		data[["eventName"]]
	})
}

stateInfo <- function(event){
	with(event, {
		data[["stateInfo"]]
	})
}

########################################################################################
#### Scheduler - creation and accessing
####	Includes:
####		- FEL (future events list): where future events are scheduled and 
####		- clock: which keeps track of the time for scheduling
####		- current.event: holds the current event - used for printing purposes for debugging

### public functions - internal to the simulator

new.scheduler <- function(firstEvent.name, startTime, endTime){
	scheduler <- new.env()
	
	scheduler[["FEL"]] <- newPriorityQueue()
	scheduler[["clock"]] <- startTime
	# a list of future events that were scheduled during the current event update (for debugging)
	scheduler[["eventsScheduledThisUpdate"]] <- list()
	scheduler[["current.event"]] <- NULL
	
	#initialization
	schedule.initialEvent(scheduler, firstEvent.name, startTime)
	schedule.endEvent(scheduler, endTime)
	
	#return scheduler
	scheduler
}

time.advance <- function(scheduler, event){
	set.clock(scheduler, eventTime(event))
	clear.eventsScheduledThisUpdate(scheduler)	  # cleared of events that old eventUpdate scheduled (for debugging) 
}

time.to.nextEvent <- function(scheduler, event = NULL){
	next.event <- view.next.event(scheduler)	  # does not dequeue next.event from FEL - just provides it for information
	if(is.null(event))
		event <- current.event(scheduler)
	eventTime(next.event) - eventTime(event)
}

dequeue.next.event <- function(scheduler){
	event <- dequeue(FEL(scheduler))
	set.current.event(scheduler, event)
	event
}

### public function - for the user

schedule.futureEvent <- function(scheduler, eventName, duration = 0, stateInfo = list(), at.time = NULL){
	if(is.null(at.time))
		eventClockTime <- clock(scheduler) + duration
	else
		eventClockTime <- at.time
	futureEvent <- new.futureEvent(eventName, eventClockTime, stateInfo)
	addEvent.eventsScheduledThisUpdate(scheduler, futureEvent)		# for debugging
	enqueue(FEL(scheduler), futureEvent)
	futureEvent
}

clock <- function(scheduler){
	scheduler[["clock"]]
}


### functions for use internal to the futureEvent.scheduler

FEL <- function(scheduler){
	scheduler[["FEL"]]
}

set.clock <- function(scheduler, time){
	scheduler[["clock"]] = time
}

current.event <- function(scheduler, event){
	scheduler[["current.event"]]
}

set.current.event <- function(scheduler, event){
	scheduler[["current.event"]] = event
}

view.next.event <- function(scheduler){
	nextInQueue(FEL(scheduler))
}

schedule.initialEvent <- function(scheduler, eventName, startTime) {
	initial.event <- new.futureEvent(eventName, startTime)
	enqueue(FEL(scheduler), initial.event)
}

schedule.endEvent <- function(scheduler, endTime) {
	end.event <- new.futureEvent("E", endTime)
	enqueue(FEL(scheduler), end.event)
}

eventsScheduledThisUpdate <- function(scheduler){
	scheduler[["eventsScheduledThisUpdate"]]
}

addEvent.eventsScheduledThisUpdate <- function(scheduler, event){
	scheduler[["eventsScheduledThisUpdate"]] = c(eventsScheduledThisUpdate(scheduler), event)
}

clear.eventsScheduledThisUpdate <- function(scheduler){
	scheduler[["eventsScheduledThisUpdate"]] = list()
}
