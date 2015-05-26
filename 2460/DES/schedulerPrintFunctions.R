# Load futureEventScheduler.R to use

########################################################################################
### Print functions for scheduler - for user debugging 
###		unlike printState and printStats which are user defined, the events, futureEvents etc.
###		are application independent and so can be defined in general.
###
###		This gives the user the ability to print events while debugging their own simulator code.
###     These print functions can be called inside event update routines where the scheduler is available
###
###     Note: Printing the FEL is not provided because the FEL is stored as a heap, which is not readible
###			  To print the FEL, at every timestep a copy of the FEL would have to be produced 
###             and then advanced to the end. This has not yet been programmed (it would be slow)

# internal print routines

print.lineBreaks <- function(line.breaks){
	if (line.breaks > 0)
		for(i in 1:line.breaks) cat("\n")
}

print.event <- function(futureEvent){
	eventName <- eventName(futureEvent)
	scheduledTime <- eventTime(futureEvent)
	cat("(", eventName, ", ", scheduledTime, ")", sep = "")
}

print.eventList <- function(events.list, within.sep = ", "){
	if(length(events.list) > 0){
		sep <- ""
		for(event in events.list){
			cat(sep); sep <- within.sep
			print.event(event)
		}
	} else {
		cat("no new events generated")
	}
}

### public function - for the user

print.clock <- function(scheduler, line.breaks = 0, header = "", after = ""){
	cat(header)
	cat(clock(scheduler))
	cat(after)
	print.lineBreaks(line.breaks)
}

print.currentEvent <- function(scheduler, line.breaks = 0, header = "", after = ""){
	cat(header)
	print.event(current.event(scheduler))
	cat(after)
	print.lineBreaks(line.breaks)
}

print.currentEvent.name <- function(scheduler, line.breaks = 0, header = "", after = ""){
	cat(header)
	cat(eventName(current.event(scheduler)))
	cat(after)
	print.lineBreaks(line.breaks)
}

print.currentEvent.clockTime <- function(scheduler, line.breaks = 0, header = "", after = ""){
	cat(header)
	cat(eventTime(current.event(scheduler)))
	cat(after)
	print.lineBreaks(line.breaks)
}

print.nextEvent <- function(scheduler, line.breaks = 0, header = "", after = ""){
	cat(header)
	print.event(view.next.event(scheduler))
	cat(after)
	print.lineBreaks(line.breaks)
}

print.nextEvent.name <- function(scheduler, line.breaks = 0, header = "", after = ""){
	cat(header)
	cat(eventName(view.next.event(scheduler)))
	cat(after)
	print.lineBreaks(line.breaks)
}

print.nextEvent.clockTime <- function(scheduler, line.breaks = 0, header = "", after = ""){
	cat(header)
	cat(eventTime(view.next.event(scheduler)))
	cat(after)
	print.lineBreaks(line.breaks)
}

print.time.to.nextEvent <- function(scheduler, line.breaks = 0, header = "", after = ""){
	cat(header)
	cat(time.to.nextEvent(scheduler))
	cat(after)
	print.lineBreaks(line.breaks)
}
	
print.eventsScheduledThisUpdate <- function(scheduler, line.breaks = 0, header = "", sep = ", ", after = ""){
	event.list <- eventsScheduledThisUpdate(scheduler)
	cat(header)
	print.eventList(event.list, sep)
	cat(after)
	print.lineBreaks(line.breaks)
}
