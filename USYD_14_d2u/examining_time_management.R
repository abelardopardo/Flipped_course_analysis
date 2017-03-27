events <- read.csv(file = "Intermediate_files/trace_data_with_sessions_(no-1-event)_w0-16.csv", 
                   stringsAsFactors = F)
str(events)

## limit events to the weeks 2-13
events <- subset(events, WEEK %in% c(2:13))

table(events$TOPIC)
prop.table((table(events$TOPIC)))
## 22% of events do not have topic!
events$TOPIC <- factor(events$TOPIC)

unique(events$ACTIVITY)
events$ACTIVITY <- factor(events$ACTIVITY)
table(events$ACTIVITY)

## associate each event with the week its topic refers to
events$topic.week <- vector(length = nrow(events))
events$topic.week[events$TOPIC=='CST'] <- 1
events$topic.week[events$TOPIC=='COD'] <- 2
events$topic.week[events$TOPIC=='DRM'] <- 3
events$topic.week[events$TOPIC=='CDL'] <- 4
events$topic.week[events$TOPIC=='SDL'] <- 5
events$topic.week[events$TOPIC=='ARC'] <- 7
events$topic.week[events$TOPIC=='ISA'] <- 8
events$topic.week[events$TOPIC=='ASP'] <- 9
events$topic.week[events$TOPIC=='ADM'] <- 10
## HLP is the topic of weeks 11 and 12
events$topic.week[events$TOPIC=='HLP'] <- 11.5 
## for events that are not associated with a course specific topic 
## (ie. where topic is unknown (-)) and also for events of the type 
## 'ORG' and 'DBOARD', set topic.week to 0, so they can be more easily distiguished later 
events$topic.week[events$TOPIC %in% c('ORG', 'DBOARD', '-')] <- 0

table(events$topic.week)

## the difference between the week for which the topic is scheduled
## and the week when topic-related event took place
events$week.diff <- NA
for(i in 1:nrow(events))
  if (events$topic.week[i] > 0) events$week.diff[i] <- events$topic.week[i] - events$WEEK[i]

events$week.diff[120:140]
summary(events$week.diff)

## add the study mode attribute
events$study.mode <- 'unknown'
## student's events related to the week's topic - >student is preparing for the week's lecture
events$study.mode[!is.na(events$week.diff) & 
                    abs(events$week.diff) <= 0.5] <- 'preparing'
## student is 'revisiting' the topic, that is doing a topic-related activity in some week
## after the week for which the topic was scheduled 
events$study.mode[!is.na(events$week.diff) & 
                    events$week.diff <= -1] <- 'revisiting'
## student is working ahead (on a topic from one of the following weeks)
events$study.mode[!is.na(events$week.diff) & 
                    events$week.diff >= 1] <- 'ahead'

table(events$study.mode)
round(prop.table(table(events$study.mode)), digits = 4)

## examine 'revisiting' in more detail and differentiate between 'revisiting' and 
## 'catching up with the course' - the former means the student did certain topic before
## and is coming back to it, the latter meaning the student is doing the topic for the 1st time
revisits <- subset(events, study.mode == "revisiting")
catching.up.indices <- vector()
k <- 1
for(i in 1:nrow(revisits)) {
  stud.id <- revisits$RESOURCE_ID[i]
  week <- revisits$WEEK[i]
  topic <- revisits$TOPIC[i]
  # check if the student had activities related to the give topic in some of the previous weeks
  earlier.events <- events[events$RESOURCE_ID==stud.id & events$TOPIC==topic & events$WEEK < week, ]
  if ( nrow(earlier.events) == 0 ) {
    catching.up.indices[k] <- row.names(revisits[i,])
    k <- k + 1
  }
}

## substitute the study.mode attribute of the identified catching up events 
## (those with the indices in catching.up.indices) with "catching-up"
events$study.mode[ row.names(events) %in% catching.up.indices ] <- "catching-up"
table(events$study.mode)
events$study.mode <- factor(events$study.mode)

## remove the week.diff column, not required further
events <- events[,-8]
colnames(events)[c(7,8)] <- c("TOPIC_WEEK", "STUDY_MODE")

## write to a file
write.csv(events, file = "Intermediate_files/study_mode_weeks2-13_v01-02-2017.csv", row.names=F, quote = F)

