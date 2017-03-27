########################################################################
## Transform the events data into the format suitable for process mining
########################################################################

all.events <- read.csv(file = "datasets/data2u_sem2_14_events_labeled.csv", stringsAsFactors = F)
str(all.events)
## remove irrelevant columns
all.events <- all.events[,-c(1,3,7)]

###############################
## FILTERING OF THE SOURCE DATA
###############################

## remove activities with action-id equal to "activity-collapse-expand" or "activity-duration"
## these are (currently) not considered
filtered.events <- all.events[!(all.events$action_id %in% c("activity-collapse-expand", "activity-duration")),]
## check that these are really excluded
length(which(filtered.events$action_id == "activity-duration"))
length(which(filtered.events$action_id == "activity-collapse-expand"))

## exclude "resource-view" actions where topic is not one of the course subject topics nor one
## of these: "ORG", "DBOARD"
relevant.topics <- c('CST', 'COD', 'DRM', 'CDL', 'SDL', 'ARC', 'ISA', 
                    'ASP', 'ADM', 'HLP', 'ORG', 'DBOARD')
to.remove <- which((filtered.events$action_id == "resource-view") & 
                     !(filtered.events$topic %in% relevant.topics))
filtered.events <- filtered.events[-to.remove,]

## exclude "embedded-video" actions where the 1st element of the payload is not "PLAY"
## example value of the payload for this action type: "{\"PLAY\":\"F0Ri2TpRBBg\"}"

## f. for transforming the payload value of the embedded-video action 
## into a vector of two elements the payload consists of
video.payload.split <- function(x) { 
  temp <- gsub("\\{\"(\\w+)\":\"(.+)\"\\}", "\\1 \\2", x)
  result <- strsplit(temp, " ") 
  result[[1]]
}

## first, create a new vector by extracting the indices of the observations
## where action_id is "embedded-video" and the 1st element of the payload column is "PLAY"
indices.to.remove <- vector()
counter <- 1
video.events <- filtered.events[filtered.events$action_id=="embedded-video",]
for (i in 1:nrow(video.events)) {
  payload.vec <- video.payload.split(video.events[i,4])
  if (payload.vec[1]!="PLAY") {
    indices.to.remove[counter] <- row.names(video.events[i,])
    counter <- counter + 1
  }
}
## remove observations with indices in indices.to.remove
indices.to.remove <- as.integer(indices.to.remove)
filtered.events <- filtered.events[ !(row.names(filtered.events) %in% indices.to.remove), ]


#################################################################################
## USE PAYLOAD TO ADD NEW VARIABLES REQUIRED FOR THE MAPPING TO THE TARGER FORMAT
#################################################################################

## f. for transforming the payload value of the "embedded-question" action 
## into a vector of two elements the payload consists of
mcq.payload.split <- function(x) { 
  temp <- gsub("\\{\"([a-zA-Z0-9_-]+)\":[\"]?([-]?\\d)[\"]?\\}", "\\1 \\2", x)
  result <- strsplit(temp, " ") 
  result[[1]]
}

## f. for transforming the payload value of the "exco-answer" action 
## into a vector of two elements the payload consists of
exco.payload.split <- function(x) { 
  temp <- gsub("\\{\"([^:]+)\":\\s\"([a-z]+)\"\\}", "\\1 \\2", x)
  result <- strsplit(temp, " ") 
  result[[1]]
}

filtered.events$payload.part <- vector(mode = "character", length = nrow(filtered.events))
for (i in 1:nrow(filtered.events)) {
  if (filtered.events$action_id[i] == "embedded-question") {
    temp <- mcq.payload.split(filtered.events$payload[i])
    filtered.events$payload.part[i] <- temp[2] # this will be "1", "0", or "-1"
  } else {
    if (filtered.events$action_id[i] == "exco-answer") {
      temp <- exco.payload.split(filtered.events$payload[i])
      filtered.events$payload.part[i] <- temp[2] # this will be either "correct" or "incorrect"
    }
  }
}

## TODO: store the filtered data

######################################################
## TRANSFORMATION OF EVENT DATA INTO THE TARGET FORMAT
######################################################

## remove the payload as it's not needed any more
target.trace <- filtered.events[,-4]

colnames(target.trace)[1:2] <- c('TIMESTAMP','CASE_ID')
target.trace$ACTIVITY_NAME <- vector(mode = 'character',length = nrow(target.trace))

subject.topics <- c('CST', 'COD', 'DRM', 'CDL', 'SDL', 'ARC', 'ISA', 'ASP', 'ADM', 'HLP')
for (i in 1:nrow(target.trace)) {
 
  # ORIENT activity: action-id:resource-view + topic:ORG
  if (target.trace$action_id[i]=="resource-view" & target.trace$topic[i]=="ORG") {
    target.trace$ACTIVITY_NAME[i] <- "ORIENT"
    next
  }
  
  # EXE_CO activity: action-id: exco-answer + payload: 2nd element = "correct"
  if (target.trace$action_id[i]=="exco-answer" & target.trace$payload.part[i]=="correct") {
    target.trace$ACTIVITY_NAME[i] <- "EXE_CO"
    next
  }
  
  # EXE_IN activity: action-id: exco-answer + payload: 2nd element = "incorrect"
  if (target.trace$action_id[i]=="exco-answer" & target.trace$payload.part[i]=="incorrect") {
    target.trace$ACTIVITY_NAME[i] <- "EXE_IN"
    next
  }
  
  # MCQ_CO activity: action-id: embedded-question + payload: 2nd element = 1
  if (target.trace$action_id[i]=="embedded-question" & target.trace$payload.part[i]=="1") {
    target.trace$ACTIVITY_NAME[i] <- "MCQ_CO"
    next
  }
  
  # MCQ_IN activity: action-id: embedded-question + payload: 2nd element = 0
  if (target.trace$action_id[i]=="embedded-question" & target.trace$payload.part[i]=="0") {
    target.trace$ACTIVITY_NAME[i] <- "MCQ_IN"
    next
  }
  
  # MCQ_SR activity: action-id: embedded-question + payload: 2nd element = -1
  if (target.trace$action_id[i]=="embedded-question" & target.trace$payload.part[i]=="-1") {
    target.trace$ACTIVITY_NAME[i] <- "MCQ_SR"
    next
  }
  
  # DBOARD_ACCESS activity: action-id: dboard-view
  if (target.trace$action_id[i]=="dboard-view") {
    target.trace$ACTIVITY_NAME[i] <- "DBOARD_ACCESS"
    next
  }
  
  # # HOF_ACCESS activity: action-id:resource-view + topic:HOF
  # if (target.trace$action_id[i]=="resource-view" & target.trace$topic[i]=="HOF") {
  #   target.trace$ACTIVITY_NAME[i] <- "HOF_ACCESS"
  #   next
  # }
  
  # VIDEO_PLAY activity: action-id: embedded-video + payload: 1st element = “PLAY”
  # since in the filtering phase all other forms of interaction with videos except PLAY
  # have been removed, this mapping can be based only on the action-id
  if (target.trace$action_id[i]=="embedded-video") {
    target.trace$ACTIVITY_NAME[i] <- "VIDEO_PLAY"
    next
  }
  
  # CONTENT_ACCESS activity: action-id: resource-view + 
  # topic: “CST” | “COD” | “DRM” | “CDL” | “SDL” | “ARC” | “ISA” | “ASP” | “ADM” | “HLP” 
  if (target.trace$action_id[i]=="resource-view" & target.trace$topic[i] %in% subject.topics) {
    target.trace$ACTIVITY_NAME[i] <- "CONTENT_ACCESS"
    next
  }
  
}

## keep only the columns/variables that are needed
target.trace <- target.trace[,c(1,2,4,5,7)]
str(target.trace)
colnames(target.trace)[3:4] <- c("WEEK", "TOPIC")
## change the order of columns, to have:
## CASE_ID, ACTIVITY_NAME, TIMESTAMP, WEEK, TOPIC
target.trace <- target.trace[,c(2,5,1,3,4)]
## store the generated trace data
write.csv(target.trace, file = "Intermediate_files/trace_data_w0-16.csv", row.names = F)


###############################################################################
## CREATE NEW TRACE FORMAT WITH SESSIONS REPRESENTING CASES; SO, THE FORMAT IS:
## CASE_ID (SESSION_ID), ACTIVITY, TIMESTAMP, RESOURCE_ID (USER_ID), WEEK
##
## session is defined as a continuous sequence of events/activities where any
## two events are separated not more than 30 minutes
###############################################################################

## load the trace data (without sessions)
target.trace <- read.csv(file = "Intermediate_files/trace_data_w0-16.csv",
                         stringsAsFactors = F)
str(target.trace)
target.trace$TIMESTAMP <- as.POSIXct(target.trace$TIMESTAMP)

## rename the colums to prevent confusion
colnames(target.trace) <- c('user_id', 'activity', 'timestamp', 'week', 'topic')
str(target.trace)

## order the trace data first based on the user_id, then based on the timestamp
target.trace <- target.trace[ order(target.trace$user_id, target.trace$timestamp),]
head(target.trace)

## add the session_id column
target.trace$session_id <- vector(mode = "numeric", length = nrow(target.trace))
## session counter, and also session id
s <- 1
target.trace$session_id[1] <- s
for (i in 1:(nrow(target.trace)-1)) {
  ## if the two consecutive events, i and (i+1) do not relate to the same user, 
  ## consider that a new session has started
  if ( target.trace$user_id[i] != target.trace$user_id[i+1] ) {
    s <- s + 1
  } else {
    ## the two events are related to the same user; now check if they are not 
    ## separated more than 30 mins 
    td <- difftime(time1 = target.trace$timestamp[i],
                   time2 = target.trace$timestamp[i+1],
                   units = "mins")
    ## if the time diff is >= 30 mins, consider that a new session has started
    if (abs(td) >= 30) s <- s + 1   
  }
  target.trace$session_id[i+1] <- s  
}

## rename the columns
colnames(target.trace) <- c('RESOURCE_ID', 'ACTIVITY', 'TIMESTAMP','WEEK','TOPIC', 'CASE_ID')
## change the order of the columns
target.trace <- target.trace[,c(6,1,3,2,4,5)]
## write to a file
write.csv(target.trace, file = "Intermediate_files/trace_data_with_sessions_w0-16.csv",
          quote = F, row.names = F)


## filter out sessions that consists of only one event
one.event.sessions <- vector()
event.count <- 1
j <- 1
for(i in 1:(nrow(target.trace)-1)) {
  if ( target.trace$CASE_ID[i] == target.trace$CASE_ID[i+1] )
    event.count <- event.count + 1
  else {
    if ( event.count == 1 ) {
      one.event.sessions[j] <- target.trace$CASE_ID[i]
      j <- j + 1
    }
    event.count <- 1
  }
}

filtered.traces <- target.trace[!(target.trace$CASE_ID %in% one.event.sessions), ]

## write data without one-event sessions 
write.csv(filtered.traces, file = "Intermediate_files/trace_data_with_sessions_(no-1-event)_w0-16.csv",
          quote = F, row.names = F)


#######################################################################
## EXTRACT TRACE DATA ONLY FOR:
## - THE BEST PERFORMING STUDENTS (the top 25% or 10% of students 
##   both on midterm and final exams)
## - THE WORST PERFORMING STUDENTS (the bottom 25% or 10% of students 
##   both on midterm and final exams)
#######################################################################

## load the f. for identifying the best abd worst performing students
source("util_functions.R")
counts.data <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv") 
## extract exam scores
scores <- counts.data[, c('user_id', 'SC_MT_TOT', 'SC_FE_TOT')]
selection <- best.and.worst.performers(scores)

## load the trace data
target.trace <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv",
                         stringsAsFactors = F)

## extract trace data for top 10% students
top10perc.trace <- target.trace[target.trace$RESOURCE_ID %in% selection$top10,]
table(top10perc.trace$ACTIVITY)
table(top10perc.trace$WEEK)
write.csv(x = top10perc.trace, 
          file = "Intermediate_files/top10perc_students_trace_data_with_sessions_w0-16.csv",
          row.names = F, quote = F)

## extract trace data for the bottom 10% students
bottom10perc.trace <- target.trace[target.trace$RESOURCE_ID %in% selection$top10$worst10,]
table(bottom10perc.trace$WEEK)
table(bottom10perc.trace$ACTIVITY)
write.csv(bottom10perc.trace, 
          file = "Intermediate_files/bottom10perc_students_trace_data_with_sessions_w0-16.csv",
          row.names = F, quote = F)

## extract trace data for the bottom 25% students
bottom25perc.trace <- target.trace[target.trace$RESOURCE_ID %in% selection$worst25,]
table(bottom25perc.trace$WEEK)
table(bottom25perc.trace$ACTIVITY)
write.csv(bottom25perc.trace, 
          file = "Intermediate_files/bottom25perc_students_trace_data_with_sessions_w0-16.csv",
          row.names = F, quote = F)
