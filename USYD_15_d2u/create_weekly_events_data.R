############################################################################
## TRANSFORM THE EVENTS DATA INTO THE FORMAT REQUIRED FOR THE SUBSEQUENT
## ANALYSIS (E.G. ANALYSIS OF LEARNING SEQUENCES, HIDDEN MARKOV MODELS)
## CREATE SEPARATE EVENT DATA FILES FOR EACH WEEK OF THE COURSE 
############################################################################

all.events <- read.csv(file = "dataset/data2u_sem2_15_events_labeled.csv", 
                       stringsAsFactors = F)
str(all.events)
## remove irrelevant columns
all.events <- all.events[,-c(1,6)]

#############################
## FILTERING THE SOURCE DATA
#############################

## retain the data only for the active weeks of the course: weeks 2-13
all.events <- subset(all.events, week %in% c(2:13))

unique(all.events$action_id)
## remove activities with action-id equal to "activity-collapse-expand" or "activity-duration"
## these are not required for futher analysis
filtered.events <- subset(all.events, !(action_id %in% c("activity-collapse-expand", "activity-duration")))
## check that these are really excluded
length(which(filtered.events$action_id == "activity-duration"))
length(which(filtered.events$action_id == "activity-collapse-expand"))

sort(unique(filtered.events$topic))
## exclude "resource-view" actions where topic is not one of the course subject topics, marked with:
## CST, COD, DRM, CDL, SDL, ARC, ISA, ASP, ADM, HLP
## or one of these: ORG, DBOARD, STRAT
relevant.topics <- c('CST', 'COD', 'DRM', 'CDL', 'SDL', 'ARC', 'ISA', 
                    'ASP', 'ADM', 'HLP', 'ORG', 'DBOARD', 'STRAT')
to.remove <- which((filtered.events$action_id == "resource-view") & 
                     !(filtered.events$topic %in% relevant.topics))
length(to.remove)/nrow(filtered.events)
## 22.3% percent of observations will be removed
filtered.events <- filtered.events[-to.remove,]
## let's check
unique(filtered.events$topic[filtered.events$action_id == "resource-view"])

## examine the paylod of 'embedded-video' events
filtered.events$payload[filtered.events$action_id == "embedded-video"][1:20]
## exclude "embedded-video" events the payload does not contain "PLAY" or "PAUSED"
to.remove <- which((filtered.events$action_id == "embedded-video") & 
                        grepl("PLAY|PAUSED", filtered.events$payload) == F)
head(to.remove)
length(to.remove)/nrow(filtered.events)
## 16046 or 2.6% of events will be filtered out
filtered.events <- filtered.events[-to.remove,]

#################################################################################
## USE PAYLOAD TO ADD NEW VARIABLES REQUIRED FOR THE MAPPING TO THE TARGER FORMAT
#################################################################################

## examine the paylod of 'embedded-question' events
filtered.events$payload[filtered.events$action_id == "embedded-question"][2100:2150]

## f. for extracting the value (-1,0,1) of the 'answer' attribute from the 
## payload variable of the "embedded-question" action 
mcq.answer.extract <- function(x) { 
  temp <- gsub("\\{\"answer\": [\"]?([-]?\\d)[\"]?,.+\\}", "\\1", x)
  temp
}

## examine the paylod of 'exco-answer' events
filtered.events$payload[filtered.events$action_id == "exco-answer"][100:120]
## f. for extracting the value of the 'outcome' attribute from the payload 
## of the "exco-answer" action 
exco.outcome.extract <- function(x) { 
  result <- gsub("\\{\"outcome\":\\s\"([a-z]+)\", .+\\}", "\\1", x)
  result
}

filtered.events$response <- vector(mode = "character", length = nrow(filtered.events))
for (i in 1:nrow(filtered.events)) {
  if (filtered.events$action_id[i] == "embedded-question") {
    filtered.events$response[i] <- mcq.answer.extract(filtered.events$payload[i]) # this will be "1", "0", or "-1"
  } else if (filtered.events$action_id[i] == "exco-answer") {
      filtered.events$response[i] <- exco.outcome.extract(filtered.events$payload[i]) # this will be either "correct" or "incorrect"
  }
}

## f. for extracting the type of assessment from the payload 
## of the "exco-answer" action; it can be "summative" or "formative"
exco.assessment.type <- function(x) { 
  result <- gsub("\\{.+\"assessment\":\\s\"([a-z]+)\", .+\\}", "\\1", x)
  result
}

filtered.events$assess_type <- NA
filtered.events$assess_type[filtered.events$action_id=="exco-answer"] <- 
  exco.assessment.type(filtered.events$payload[filtered.events$action_id=="exco-answer"])
filtered.events$assess_type <- factor(filtered.events$assess_type)

str(filtered.events)

## format the data before saving it
filtered.events$received <- as.POSIXlt(filtered.events$received, tz = "Australia/Sydney")
colnames(filtered.events)[1] <- 'timestamp'
filtered.events$action_id <- factor(filtered.events$action_id)

## store the filtered data
saveRDS(object = filtered.events, file = "Intermediate_results/filtered_events_w2-13.RData")


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

colnames(filtered.events)[1:2] <- c('TIMESTAMP','USER_ID')

unique(filtered.events$topic)
subject.topics <- c('CST', 'COD', 'DRM', 'CDL', 'SDL', 'ARC', 'ISA', 'ASP', 'ADM', 'HLP')

filtered.events$ACTION <- NA

## ORIENT activity: action-id:resource-view + topic:ORG
filtered.events$ACTION[filtered.events$action_id=="resource-view" & 
                       filtered.events$topic=="ORG" &
                       grepl("Organization/Syllabus|Organization/Strategy", filtered.events$payload)] <- "ORIENT"
length(which(filtered.events$ACTION=="ORIENT"))  
# 6185

## EXE_S_CO activity: action-id: exco-answer + response = "correct" + assessment type = "summative"
filtered.events$ACTION[filtered.events$action_id=="exco-answer" & 
                       filtered.events$response=="correct" &
                       filtered.events$assess_type=="summative"] <- "EXE_S_CO"
length(which(filtered.events$ACTION=="EXE_S_CO"))    
## 65457

# EXE_S_IN activity: action-id: exco-answer + response = "incorrect" + assessment type = "summative"
filtered.events$ACTION[filtered.events$action_id=="exco-answer" & 
                         filtered.events$response=="incorrect" &
                         filtered.events$assess_type=="summative"] <- "EXE_S_IN"
length(which(filtered.events$ACTION=="EXE_S_IN"))    
# 94092

## EXE_F_CO activity: action-id: exco-answer + response = "correct" + assessment type = "formative"
filtered.events$ACTION[filtered.events$action_id=="exco-answer" & 
                         filtered.events$response=="correct" &
                         filtered.events$assess_type=="formative"] <- "EXE_F_CO"
length(which(filtered.events$ACTION=="EXE_F_CO"))    
# 29864

# EXE_F_IN activity: action-id: exco-answer + response = "incorrect" + assessment type = "formative"
filtered.events$ACTION[filtered.events$action_id=="exco-answer" & 
                         filtered.events$response=="incorrect" &
                         filtered.events$assess_type=="formative"] <- "EXE_F_IN"
length(which(filtered.events$ACTION=="EXE_F_IN"))    
# 24865

## MCQ associated with a video (VEQ): action-id: embedded-question + payload contains 'videoeqt'
## if response was correct (response==1), it's VEQ_CO event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="1" &
                         grepl("videoeqt", filtered.events$payload)] <- "VEQ_CO"
length(which(filtered.events$ACTION=="VEQ_CO"))
# 26306

## if response was incorrect (response==0), it's VEQ_IN event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="0" &
                         grepl("videoeqt", filtered.events$payload)] <- "VEQ_IN"
length(which(filtered.events$ACTION=="VEQ_IN"))
# 17162

## if student asked to see the response (response==-1), it's VEQ_SR event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="-1" &
                         grepl("videoeqt", filtered.events$payload)] <- "VEQ_SR"
length(which(filtered.events$ACTION=="VEQ_SR"))
# 5977

## MCQ embedded in course readings (not associated with a video) (EQT): 
## action-id: embedded-question + payload NOT contains 'videoeqt'
## if response was correct (response==1), it's EQT_CO event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="1" &
                         grepl("videoeqt", filtered.events$payload)==F] <- "EQT_CO"
length(which(filtered.events$ACTION=="EQT_CO"))
# 34935

## if response was incorrect (response==0), it's EQT_IN event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="0" &
                         grepl("videoeqt", filtered.events$payload)==F] <- "EQT_IN"
length(which(filtered.events$ACTION=="EQT_IN"))
# 19460

## if student asked to see the response (response==-1), it's EQT_SR event
filtered.events$ACTION[filtered.events$action_id=="embedded-question" & 
                         filtered.events$response=="-1" &
                         grepl("videoeqt", filtered.events$payload)==F] <- "EQT_SR"
length(which(filtered.events$ACTION=="EQT_SR"))
# 7178

## DBOARD_ACCESS activity: action-id: dboard-view
filtered.events$ACTION[filtered.events$action_id=="dboard-view"] <- "DBOARD_ACCESS"
length(which(filtered.events$ACTION=="DBOARD_ACCESS"))                       
# 13042

## VIDEO_PL and VIDEO_PA: action-id: embedded-video + payload contains “PLAY” or "PAUSED"
filtered.events$ACTION[filtered.events$action_id=="embedded-video" &
                       grepl("PLAY", filtered.events$payload)] <- "VIDEO_PL"
length(which(filtered.events$ACTION=="VIDEO_PL"))  
# 92164
filtered.events$ACTION[filtered.events$action_id=="embedded-video" &
                         grepl("PAUSED", filtered.events$payload)] <- "VIDEO_PA"
length(which(filtered.events$ACTION=="VIDEO_PA"))  
# 66590

## CONTENT_ACCESS activity: action-id: resource-view + 
## topic: “CST” | “COD” | “DRM” | “CDL” | “SDL” | “ARC” | “ISA” | “ASP” | “ADM” | “HLP” 
filtered.events$ACTION[filtered.events$action_id=="resource-view" & 
                         filtered.events$topic %in% subject.topics] <- "CONTENT_ACCESS"
length(which(filtered.events$ACTION=="CONTENT_ACCESS"))
# 94924


str(filtered.events)
table(filtered.events$ACTION)
length(which(is.na(filtered.events$ACTION)))
## there are 4403 events with no type defined
table(filtered.events$action_id[is.na(filtered.events$ACTION)])
# all are resource-view events; examine them...
filtered.events$payload[is.na(filtered.events$ACTION)][1500:1520]
## remove them
filtered.events <- filtered.events[!is.na(filtered.events$ACTION),]
## keep only the columns/variables that are needed
trace.data <- filtered.events[,c(1,2,5,6,9)]
## change the order of columns, to have:
## USER_ID, ACTION, TIMESTAMP, WEEK, TOPIC
str(trace.data)
trace.data <- trace.data[,c(2,5,1,3,4)]
colnames(trace.data)[4:5] <- c('WEEK', 'TOPIC')
## store the generated trace data
write.csv(trace.data, file = "Intermediate_results/trace_data_w2-13.csv", row.names = F)
saveRDS(trace.data, file = "Intermediate_results/trace_data_w2-13.RData")

###############################################################################
## EXTEND THE TRACE DATA WITH SESSIONS 
## THE NEW TRACE FORMAT:
## STUDENT_ID, SESSION_ID, TIMESTAMP, ACTION, WEEK, TOPIC 
##
## session is defined as a continuous sequence of events/actions where any
## two events are separated not more than 9 minutes 
## (95th percentile of the time gaps between two consecutive events)
###############################################################################

## load the trace data
trace.data <- readRDS(file = "Intermediate_results/trace_data_w2-13.RData")
str(trace.data)

## order the trace data first based on the user id, then based on the week and timestamp
trace.data <- trace.data[ order(trace.data$USER_ID, trace.data$WEEK, trace.data$TIMESTAMP),]
head(trace.data)

## compute the time gap between any two consecutive events
## this is required for determing learning sessions
trace.data <- compute.event.timegap(trace.data) ## STOPPED HERE
summary(trace.data$TIME_GAP)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.000    0.049    0.132   11.520  0.575   1440.000     7883  (in minutes)
quantile(trace.data$TIME_GAP, probs = seq(0.9, 1, 0.025), na.rm = T)
#   90%       92.5%       95%       97.5%        100% 
# 2.615142  4.203685    8.410306   34.448656  1439.811968 

## use the time gap of 9 mins (95th percentile) as the 'session delimiter'
## to split events into sessions
session.id <- 0
for(i in 1:nrow(trace.data)) {
  ## TIME_GAP is NA if: i) 2 consecutive events originate from different students, or
  ## ii) the gap between the 2 events is overly large (> 24h)
  if ( is.na(trace.data$TIME_GAP[i]) || trace.data$TIME_GAP[i] >= 9 )
    session.id <- session.id + 1
  trace.data$SESSION_ID[i] <- session.id
}
length(unique(trace.data$SESSION_ID))
# 36314 

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

