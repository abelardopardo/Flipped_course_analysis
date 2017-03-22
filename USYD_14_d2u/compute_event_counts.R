#####################################################################################
## compute the frequency, per student and per week, for the following kinds of events
## MC.EVAL: DBOARD.VIEW - student views the dashboard (DBOARD.VIEW) 
## MC.ORIENT: access to the schedule and the learning objective pages
## CONTENT.ACCESS - student accesses a page containing the course content
## as the input, use the data about session based learning traces 
#####################################################################################

traces <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv", 
                   stringsAsFactors = F)
str(traces)

## keep the data (traces) for weeks 2-13, as those were the key weeks of the course 
## (the time period considered in the analysis)
traces <- subset(traces, traces$WEEK %in% c(2:13))

## substitute DBOARD_ACCESS and HOF_ACCESS with MC_EVAL
## also ORIENT with MC_ORIENT
activity.rev <- as.vector(sapply(traces$ACTIVITY, function(x) {
  if (x == "DBOARD_ACCESS") x <- "MC_EVAL"
  else if (x == "ORIENT") x <- "MC_ORIENT"
  x
}))
traces$ACTIVITY <- activity.rev

## load one of the files with weekly counts (computed by Abelardo)
## to extract the order of student ids, so that these additional
## counts are ordered in the same way
existing.counts <- read.csv("datasets/data2u_sem2_14_student_all_variables.csv")
user.ids <- existing.counts$user_id
users.cnt <- length(user.ids)

weeks <- sort(unique(traces$WEEK))

counts.df <- data.frame()
for (i in 1:length(weeks)) {
  
  mc.orient.cnt <- vector(mode = 'numeric', length = users.cnt)
  mc.eval.cnt <- vector(mode = 'numeric', length = users.cnt)
  cont.access.cnt <- vector(mode = 'numeric', length = users.cnt)
  
  for (j in 1:users.cnt) {
    user.week.traces <-  traces[traces$RESOURCE_ID == user.ids[j] & traces$WEEK == weeks[i], ]
    mc.orient.cnt[j] <- nrow( user.week.traces[user.week.traces$ACTIVITY == 'MC_ORIENT',] )
    mc.eval.cnt[j] <- nrow( user.week.traces[user.week.traces$ACTIVITY == 'MC_EVAL',])
    cont.access.cnt[j] <- nrow( user.week.traces[user.week.traces$ACTIVITY == 'CONTENT_ACCESS',])
  }
  
  df <- as.data.frame(cbind(user_id = user.ids, 
                            mc.orient = mc.orient.cnt, 
                            mc.eval = mc.eval.cnt,
                            content.access = cont.access.cnt,
                            week = rep(weeks[i], times=users.cnt)))
  counts.df <- as.data.frame(rbind(counts.df, df))
}
str(counts.df)

write.csv(x = counts.df, file = "Intermediate_files/mc-orient_mc-eval_content-access_weekly_counts.csv", 
          quote = F, row.names = F)

#####################################################################################
## compute the frequency, per student and per week, for the following kinds of events
## ORG - Access to the schedule and the learning objective pages
## DBOARD - Access to the dashboard screen
## HOF - Access to the pages showing the Hall of Fame <- UPDATE: remove, not relevant
#####################################################################################

all.events <- read.csv(file = "datasets/data2u_sem2_14_events_labeled.csv", stringsAsFactors = F)
str(all.events)
## keep only relevant columns
all.events <- all.events[, c("received", "user_id", "action_id", "week", "topic")]
head(all.events)

## remove rows where topic == '-'
all.events <- all.events[all.events$topic != "-", ]

table(all.events$topic)

## keep only events of the kind (topic): ORG, DBOARD, HOF
all.events <- all.events[all.events$topic %in% c('ORG', 'DBOARD'), ]

## keep only events that took place in weeks 2-13
weeks <- unique(all.events$week)
all.events <- all.events[all.events$week %in% c(2:13), ]
head(all.events)

user.ids <- unique(all.events$user_id)
users.cnt <- length(user.ids)
weeks <- unique(all.events$week)

events.df <- data.frame()

for (i in 1:length(weeks)) {
  
  org.events.cnt <- vector(mode = 'numeric', length = users.cnt)
  dboard.events.cnt <- vector(mode = 'numeric', length = users.cnt)

  for (j in 1:users.cnt) {
    user.week.events <-  all.events[all.events$user_id == user.ids[j] & all.events$week == weeks[i], ]
    org.events.cnt[j] <- nrow( user.week.events[user.week.events$topic == 'ORG',] )
    dboard.events.cnt[j] <- nrow( user.week.events[user.week.events$topic == 'DBOARD',])
  }
  
  df <- as.data.frame(cbind(user_id = user.ids, 
                            org.events = org.events.cnt, 
                            dboard.events = dboard.events.cnt,
                            week = rep(weeks[i], times=users.cnt)))
  events.df <- as.data.frame(rbind(events.df, df))
}

str(events.df)
tail(events.df)

nrow( events.df[events.df$org.events > 0, ])
## in 787 observations (23.33%)
nrow( events.df[events.df$dboard.events > 0, ])
## 972 observations (28.82%)

write.csv(x = events.df, file = "Intermediate_files/org_dboad_weekly_counts.csv", 
          quote = F, row.names = F)


