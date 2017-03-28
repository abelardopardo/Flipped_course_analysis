############################################################################
## TRANSFORM THE EVENTS DATA INTO THE FORMAT REQUIRED FOR THE SUBSEQUENT
## ANALYSIS (E.G. ANALYSIS OF LEARNING SEQUENCES, HIDDEN MARKOV MODELS)
############################################################################

all.events <- read.csv(file = "dataset/data2u_sem2_16_events_labeled.csv", 
                       stringsAsFactors = F)
str(all.events)

## remove irrelevant columns
all.events <- all.events[,-c(1,4)]
## change the order of columns
all.events <- all.events[,c(4,1,3,2,5,6)]
## rename the columns
colnames(all.events)[2] <- 'timestamp'


#############################
## FILTERING THE SOURCE DATA
#############################

## retain the data only for the active weeks of the course: weeks 2-13
all.events <- subset(all.events, week %in% c(2:13))

unique(all.events$action_id)
## remove activities with action-id equal to "activity-collapse-expand", "activity-duration",
## or "form-submit" - these are not required for futher analysis
filtered.events <- subset(all.events, 
                          !(action_id %in% c("activity-collapse-expand", "activity-duration", "form-submit")))
## check that these are really excluded
length(which(filtered.events$action_id == "activity-duration"))
length(which(filtered.events$action_id == "activity-collapse-expand"))
length(which(filtered.events$action_id == "form-submit"))

sort(unique(filtered.events$topic))
## exclude "resource-view" actions where topic is not one of the course subject topics, marked with:
## CST, COD, DRM, CDL, SDL, ARC, ISA, ASP, ADM, HLP
## or ORG
relevant.topics <- c('CST', 'COD', 'DRM', 'CDL', 'SDL', 'ARC', 'ISA', 
                    'ASP', 'ADM', 'HLP', 'ORG')
to.remove <- which((filtered.events$action_id == "resource-view") & 
                     !(filtered.events$topic %in% relevant.topics))
length(to.remove)/nrow(filtered.events)
## 17.28% percent of observations will be removed
filtered.events <- filtered.events[-to.remove,]
## let's check
unique(filtered.events$topic[filtered.events$action_id == "resource-view"])

## examine the paylod of 'embedded-video' events 
filtered.events$payload[filtered.events$action_id == "embedded-video"][1200:1220]
## exclude "embedded-video" events the payload does not contain "PLAY" or "PAUSED"
to.remove <- which((filtered.events$action_id == "embedded-video") & 
                        grepl("PLAY|PAUSED", filtered.events$payload) == F)
length(to.remove)/nrow(filtered.events)
## 20837 or 1.9% of events will be filtered out
filtered.events <- filtered.events[-to.remove,]

#################################################################################
## USE PAYLOAD TO ADD NEW VARIABLES REQUIRED FOR THE MAPPING TO THE TARGER FORMAT
#################################################################################

source(file="event_payload_extract_functions.R")

## apply the above functions to create a new variable - response - that will store
## students response to an MCQ or an exercise
filtered.events$response <- NA
## add response to MCQs
filtered.events$response[filtered.events$action_id=="embedded-question"] <- 
        mcq.answer.extract(filtered.events$payload[filtered.events$action_id=="embedded-question"])
## add response to exercises
filtered.events$response[filtered.events$action_id=="exco-answer"] <- 
        exco.outcome.extract(filtered.events$payload[filtered.events$action_id=="exco-answer"])
table(filtered.events$response)

## apply the f. to add the type of assessment to each exercise
filtered.events$assess_type <- NA
filtered.events$assess_type[filtered.events$action_id=="exco-answer"] <- 
  exco.assessment.type(filtered.events$payload[filtered.events$action_id=="exco-answer"])
filtered.events$assess_type <- factor(filtered.events$assess_type)
table(filtered.events$assess_type)


## examine the payload of the studykit-view events
filtered.events$payload[filtered.events$action_id=="studykit-view"][200:220]
# don't know how to make use of these values - should be checked with Abelardo

## examine the payload of the dboard-view events
filtered.events$payload[filtered.events$action_id=="dboard-view"][1:20]
# the meaning is unclear - should be checked with Abelardo; might be interesting/relevant
# for further analysis

## examine the payload of the embedded-video events
filtered.events$payload[filtered.events$action_id=="embedded-video"][100:120]
# the timing of the video actions might be relevant for some subsequent analysis

## examine the payload of the exco-view events
filtered.events$payload[filtered.events$action_id=="exco-view"][100:120]
# the meaning is unclear - should be checked with Abelardo; might be interesting/relevant
# for further analysis

## examine the payload of the xy-click events
filtered.events$payload[filtered.events$action_id=="xy-click"][100:120]
# these data might be very useful - should be explored

## examine the payload of the exco-done events
filtered.events$payload[filtered.events$action_id=="exco-done"][100:120]
# this doesn't seem much useful

## examine the payload of the exco-reset events
filtered.events$payload[filtered.events$action_id=="exco-reset"][2000:2020]
# doesn't seem to be much useful

str(filtered.events)

## format the data before saving it
filtered.events$timestamp <- as.POSIXlt(filtered.events$timestamp, tz = "Australia/Sydney")
filtered.events$action_id <- factor(filtered.events$action_id)
filtered.events$topic <- factor(filtered.events$topic)

## store the filtered data
saveRDS(object = filtered.events, file = "Intermediate_results/filtered_events_w2-13.RData")
write.csv(x = filtered.events, file = "Intermediate_results/filtered_events_w2-13.csv", 
          row.names = F, quote = F)


#######################################################
## TRANSFORMATION OF EVENT DATA INTO THE TARGET FORMAT:
## USER_ID, TIMESTAMP, ACTION, WEEK, TOPIC
## ACTION CAN HAVE ONE OF THE FOLLOWING VALUES:
## EXE_S_CO, EXE_S_IN, EXE_F_CO, EXE_F_IN, 
## VEQ_CO, VEQ_IN, VEQ_SH, 
## EQT_CO, EQT_IN, EQT_SH, 
## VIDEO_PL, VIDEO_PA,
## DBOARD_ACCESS, ORIENT 
##
#######################################################

filtered.events <- readRDS(file = "Intermediate_results/filtered_events_w2-13.RData")

filtered.events$ACTION <- NA

## ORIENT activity: action-id:resource-view + topic:ORG
filtered.events$ACTION[filtered.events$action_id=="resource-view" & 
                       filtered.events$topic=="ORG" &
                       grepl("Organization/Syllabus|Organization/Strategy|Course_activities/A_word_about_your_strategy", filtered.events$payload)] <- "ORIENT"
## also access to the pages with the studykit can be considered as metacognitive orientation action
filtered.events$ACTION[filtered.events$action_id=="studykit-view"] <- "ORIENT"
length(which(filtered.events$ACTION=="ORIENT"))  
# 4371

## EXE_S_CO activity: action-id: exco-answer + response = "correct" + assessment type = "summative"
filtered.events$ACTION[filtered.events$action_id=="exco-answer" & 
                       filtered.events$response=="correct" &
                       filtered.events$assess_type=="summative"] <- "EXE_S_CO"
length(which(filtered.events$ACTION=="EXE_S_CO"))    
## 88472

# EXE_S_IN activity: action-id: exco-answer + response = "incorrect" + assessment type = "summative"
filtered.events$ACTION[filtered.events$action_id=="exco-answer" & 
                         filtered.events$response=="incorrect" &
                         filtered.events$assess_type=="summative"] <- "EXE_S_IN"
length(which(filtered.events$ACTION=="EXE_S_IN"))    
# 136345

## EXE_F_CO activity: action-id: exco-answer + response = "correct" + assessment type = "formative"
filtered.events$ACTION[filtered.events$action_id=="exco-answer" & 
                         filtered.events$response=="correct" &
                         filtered.events$assess_type=="formative"] <- "EXE_F_CO"
length(which(filtered.events$ACTION=="EXE_F_CO"))    
# 36098

# EXE_F_IN activity: action-id: exco-answer + response = "incorrect" + assessment type = "formative"
filtered.events$ACTION[filtered.events$action_id=="exco-answer" & 
                         filtered.events$response=="incorrect" &
                         filtered.events$assess_type=="formative"] <- "EXE_F_IN"
length(which(filtered.events$ACTION=="EXE_F_IN"))    
# 24313

## MCQ associated with a video (VEQ): action-id: embedded-question + payload contains 'videoeqt'
## if response was correct (response==1), it's VEQ_CO event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="1" &
                         grepl("videoeqt", filtered.events$payload)] <- "VEQ_CO"
length(which(filtered.events$ACTION=="VEQ_CO"))
# 28268

## if response was incorrect (response==0), it's VEQ_IN event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="0" &
                         grepl("videoeqt", filtered.events$payload)] <- "VEQ_IN"
length(which(filtered.events$ACTION=="VEQ_IN"))
# 18560

## if student asked to see the response (response==-1), it's VEQ_SR event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="-1" &
                         grepl("videoeqt", filtered.events$payload)] <- "VEQ_SR"
length(which(filtered.events$ACTION=="VEQ_SR"))
# 6672

## MCQ embedded in course readings (not associated with a video) (EQT): 
## action-id: embedded-question + payload NOT contains 'videoeqt'
## if response was correct (response==1), it's EQT_CO event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="1" &
                         grepl("videoeqt", filtered.events$payload)==F] <- "EQT_CO"
length(which(filtered.events$ACTION=="EQT_CO"))
# 33063

## if response was incorrect (response==0), it's EQT_IN event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="0" &
                         grepl("videoeqt", filtered.events$payload)==F] <- "EQT_IN"
length(which(filtered.events$ACTION=="EQT_IN"))
# 18184

## if student asked to see the response (response==-1), it's EQT_SR event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="-1" &
                         grepl("videoeqt", filtered.events$payload)==F] <- "EQT_SR"
length(which(filtered.events$ACTION=="EQT_SR"))
# 6773

## DBOARD_ACCESS activity: action-id: dboard-view
filtered.events$ACTION[filtered.events$action_id=="dboard-view"] <- "DBOARD_ACCESS"
length(which(filtered.events$ACTION=="DBOARD_ACCESS"))                       
# 3836

## VIDEO_PL and VIDEO_PA: action-id: embedded-video + payload contains “PLAY” or "PAUSED"
filtered.events$ACTION[filtered.events$action_id=="embedded-video" &
                       grepl("PLAY", filtered.events$payload)] <- "VIDEO_PL"
length(which(filtered.events$ACTION=="VIDEO_PL"))  
# 165647
filtered.events$ACTION[filtered.events$action_id=="embedded-video" &
                         grepl("PAUSED", filtered.events$payload)] <- "VIDEO_PA"
length(which(filtered.events$ACTION=="VIDEO_PA"))  
# 97601

unique(filtered.events$topic)
subject.topics <- c('CST', 'COD', 'DRM', 'CDL', 'SDL', 'ARC', 'ISA', 'ASP', 'ADM', 'HLP')
## CONTENT_ACCESS activity: action-id: resource-view + topic in subject.topics
filtered.events$ACTION[filtered.events$action_id=="resource-view" & 
                         filtered.events$topic %in% subject.topics] <- "CONTENT_ACCESS"
length(which(filtered.events$ACTION=="CONTENT_ACCESS"))
# 90334


str(filtered.events)
table(filtered.events$ACTION)
length(which(is.na(filtered.events$ACTION)))
# there are 334452 events with no type defined 
table(filtered.events$action_id[is.na(filtered.events$ACTION)])
# the kinds of events tht were not considered plus some resource-view events
## remove them
filtered.events <- filtered.events[!is.na(filtered.events$ACTION),]
## keep only the columns/variables that are needed
trace.data <- filtered.events[,c(1,2,5,6,9)]
str(trace.data)
## change the name of the columns
colnames(trace.data) <- c('USER_ID', 'TIMESTAMP', 'WEEK', 'TOPIC', 'ACTION')
## change the order of columns, to have:
## USER_ID, ACTION, TIMESTAMP, WEEK, TOPIC
trace.data <- trace.data[,c(1,5,2:4)]
## store the generated trace data
write.csv(trace.data, file = "Intermediate_results/trace_data_w2-13.csv", row.names = F, quote = F)
saveRDS(trace.data, file = "Intermediate_results/trace_data_w2-13.RData")

###############################################################################
## EXTEND THE TRACE DATA WITH SESSIONS 
## THE NEW TRACE FORMAT:
## STUDENT_ID, SESSION_ID, TIMESTAMP, ACTION, WEEK, TOPIC 
##
## session is defined as a continuous sequence of events/actions where any
## two events are separated not more than 10 minutes 
## (96th percentile of the time gaps between two consecutive events)
###############################################################################

## load the trace data
trace.data <- readRDS(file = "Intermediate_results/trace_data_w2-13.RData")
str(trace.data)

## order the trace data first based on the user id, then based on the week and timestamp
trace.data <- trace.data[ order(trace.data$USER_ID, trace.data$WEEK, trace.data$TIMESTAMP),]
head(trace.data)

## compute the time gap between any two consecutive events
## this is required for determing learning sessions
trace.data <- compute.event.timegap(trace.data) 
summary(trace.data$TIME_GAP)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.000    0.042    0.125    9.450    0.543 1440.000     9989   (in minutes)
quantile(trace.data$TIME_GAP, probs = seq(0.9, 1, 0.01), na.rm = T)
#   94%         95%       96%         97%       98%      99%    
# 5.13495    6.80786    9.62098   14.94177   31.2652  220.5235   (in minutes)

## use the time gap of 10 mins (96th percentile) as the 'session delimiter'
## to split events into sessions
session.id <- 0
for(i in 1:nrow(trace.data)) {
  ## TIME_GAP is NA if: i) 2 consecutive events originate from different students, or
  ## ii) the gap between the 2 events is overly large (> 24h)
  if ( is.na(trace.data$TIME_GAP[i]) || trace.data$TIME_GAP[i] >= 10 )
    session.id <- session.id + 1
  trace.data$SESSION_ID[i] <- session.id
}
length(unique(trace.data$SESSION_ID))
# 39159 

str(trace.data)
head(trace.data)

## write data to a file
write.csv(trace.data, file = "Intermediate_results/trace_data_with_sessions_w2-13.csv",
          quote = F, row.names = F)
saveRDS(trace.data, file = "Intermediate_results/trace_data_with_sessions_w2-13.RData")


####################
## UTILITY FUNCTIONS
####################

## the f. computes time gap between two subsequent learning events
## the purpose is to identify the length of the time gap that is indicative 
## of a session's end; therefore, overly long (more than a day) time gaps are not considered
## the f. returns the input dataframe with the new TIME_GAP column added  
compute.event.timegap <- function(traces) {
  traces$TIME_GAP <- vector(mode = "numeric", length = nrow(traces))
  traces$TIME_GAP[1] <- NA
  current.stud <- traces$USER_ID[1];
  for(i in 2:nrow(traces)) {
    if (traces$USER_ID[i]==current.stud) {
        traces$TIME_GAP[i] <- difftime(time1 = traces$TIMESTAMP[i], 
                                       time2 = traces$TIMESTAMP[i-1], 
                                       units = "mins" )
      ## consider that a day long gap between two consecutive events indicate the end
      ## of one and begining of a new session; to indicate that, set TIME_GAP to NA 
      ## with meaning "not applicable" or "not meaningful"
      if (traces$TIME_GAP[i] > 1440) 
          traces$TIME_GAP[i] <- NA
    } else {
          traces$TIME_GAP[i] <- NA  
          current.stud <- traces$USER_ID[i]
    }  
  }
  return (traces)
}

