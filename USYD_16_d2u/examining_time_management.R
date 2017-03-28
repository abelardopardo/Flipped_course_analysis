events <- readRDS(file = "Intermediate_results/trace_data_with_sessions_w2-13.RData")
str(events)

table(events$TOPIC)
round(prop.table((table(events$TOPIC))), digits = 4)
events$TOPIC <- factor(events$TOPIC)

unique(events$ACTION)
events$ACTION <- factor(events$ACTION)
round(prop.table(table(events$ACTION)), digits = 3)

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
## for the topic of the type 'ORG', 'DBOARD' and 'TEA', 
## set topic.week to 0, so they can be more easily distiguished later 
events$topic.week[events$TOPIC %in% c('ORG', 'DBOARD', 'TEA')] <- 0

table(events$topic.week)

## the difference between the week for which the topic is scheduled
## and the week when topic-related event took place
events$week.diff <- NA
diff.indices <- which(events$topic.week > 0)
events$week.diff[diff.indices] <- events$topic.week[diff.indices] - events$WEEK[diff.indices]

events$week.diff[1020:1040]
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
# ahead   preparing   revisiting  unknown 
# 0.0484     0.4880     0.4522     0.0115

## examine 'revisiting' in more detail and differentiate between 'revisiting' and 
## 'catching up with the course' - the former means the student did certain topic before
## and is coming back to it, the latter meaning the student is doing the topic for the 1st time
revisits <- subset(events, study.mode == "revisiting")
catching.up.indices <- vector()
k <- 1
for(i in 1:nrow(revisits)) {
  stud.id <- revisits$USER_ID[i]
  week <- revisits$WEEK[i]
  topic <- revisits$TOPIC[i]
  # check if the student had activities related to the give topic in some of the previous weeks
  earlier.events <- events[events$USER_ID==stud.id & events$TOPIC==topic & events$WEEK < week, ]
  if ( nrow(earlier.events) == 0 ) {
    catching.up.indices[k] <- row.names(revisits[i,])
    k <- k + 1
    print(paste("k=", k))
  }
}

## substitute the study.mode attribute of the identified catching up events 
## (those with the indices in catching.up.indices) with "catching-up"
events$study.mode[ row.names(events) %in% catching.up.indices ] <- "catching-up"
table(events$study.mode)
events$study.mode <- factor(events$study.mode)

str(events)
## remove the week.diff and topic.week columns, not required further
events <- events[,-c(8,9)]
## change the order of columns
events <- events[,c(1,7,2:5,8,6)]
colnames(events)[7] <- "STUDY_MODE"

## write to a file
write.csv(events, file = "Intermediate_results/study_mode_weeks2-13.csv", row.names=F, quote = F)
saveRDS(events,file = "Intermediate_results/study_mode_weeks2-13.RData")
