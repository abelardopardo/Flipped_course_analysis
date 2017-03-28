events <- readRDS(file = "Intermediate_results/filtered_events_w2-13.RData")
str(events)

table(events$action_id)
## select only dboard-view events
db.views <- subset(events, action_id=="dboard-view") 

str(db.views)
## remove columns that are not needed
db.views <- db.views[,-c(3,7,8)]

db.views$payload[1:5]

#######################################################################################
## extract paylod data and use it to create new variables that describe
## dboard-view events in more detail:
## - db_week - the week of the data that the dashboard is presenting
## - db_current - binary variable with values: 1 - current week 
##   (student is checking his/her performance in the current week), 
##   0 - earlier week (student is reviewing his/her performance in 
##   one of the previous course weeks)
## - vid_stud - the percentage of videos watched by the student
## - vid_class - the average perc. videos watched by the class
## - veq_stud - the percentage of questions next to videos done by the student
## - veq_class - the avg. perc. of questions next to videos done by the class
## - eqt_stud - the perc. of questions embedded in course notes done by the student
## - eqt_class - the avg. perc. of questions embedded in course notes done by the class
## - exe_stud - the percentage of exercises done by the student
## - exe_class - the avg. percentage of exercises done by the class
########################################################################################

source(file = "event_payload_extract_functions.R")

for(i in 1:nrow(db.views)) {
  payload.dat <- dboard.extract(db.views$payload[i])
 
  db.views$db_week[i] <- as.integer(payload.dat[[1]]) + 1 # +1 because weeks in the payload field start from 0 
  
  db.views$db_current[i] <- 1
  if ( db.views$db_week[i] < db.views$week[i] )
    db.views$db_current[i] <- 0
  
  db.views$vid_stud[i] <- as.integer(payload.dat[[2]][1])
  db.views$vid_class[i] <- as.integer(payload.dat[[2]][2])
  db.views$veq_stud[i] <- as.integer(payload.dat[[2]][3])
  db.views$veq_class[i] <- as.integer(payload.dat[[2]][4])
  db.views$eqt_stud[i] <- as.integer(payload.dat[[2]][5])
  db.views$eqt_class[i] <- as.integer(payload.dat[[2]][6])
  db.views$exe_stud[i] <- as.integer(payload.dat[[2]][7])
  db.views$exe_class[i] <- as.integer(payload.dat[[2]][8])
}

str(db.views)
## remove the payload - no longer needed
db.views <- db.views[,-3]

#######################################################################################
## Examine the student's engagement level in comparison with the class average
## Introduce a new variable - db_engage_lvl - with the following values:
## - below_avg - if the student’s percentages on the majority of the activities 
##   (at least 3 out of 4) are below the class average; 
## - above_avg - if the student’s percentages on the majority of the activities 
##   (at least 3 out of 4) are above the class average
## - avg - in the other cases 
#######################################################################################

for(i in 1:nrow(db.views)) {
  db.views$db_engage_lvl[i] <- "avg"
  dat <- db.views[i,]
  above.cnt <- (dat$vid_stud > dat$vid_class) + (dat$veq_stud > dat$veq_class) +
               (dat$eqt_stud > dat$eqt_class) + (dat$exe_stud > dat$exe_class)
  if (above.cnt >= 3)
    db.views$db_engage_lvl[i] <- "above_avg"
  else {
    below.cnt <- (dat$vid_stud < dat$vid_class) + (dat$veq_stud < dat$veq_class) +
                 (dat$eqt_stud < dat$eqt_class) + (dat$exe_stud < dat$exe_class)
    if (below.cnt >= 3)
      db.views$db_engage_lvl[i] <- "below_avg"
  }
}

table(db.views$db_engage_lvl)
# above_avg       avg     below_avg 
#      1185      1158     1493
round(prop.table(table(db.views$db_engage_lvl)), digits = 3)
# above_avg       avg     below_avg 
#     0.309     0.302     0.389

db.views$db_engage_lvl <- factor(db.views$db_engage_lvl)

## store the data
write.csv(db.views, file = "Intermediate_results/processed_dboad-view_events.csv", 
          quote = F, row.names = F)
saveRDS(db.views, file = "Intermediate_results/processed_dboad-view_events.RData")


## examine db_engage_lvl at the level of individual students
## for each student determine the most dominant db_engage_lvl value
## examine the distribution of students across the 3 db_engage_lvl values 
str(db.views)
stud.eng.dist <- with(db.views, as.matrix(table(user_id, db_engage_lvl)))
head(stud.eng.dist)
stud.eng.dist <- as.data.frame(stud.eng.dist)
n.stud <- length(unique(stud.eng.dist$user_id))
stud.eng.dist.df <- data.frame(user_id=stud.eng.dist$user_id[1:n.stud],
                               above_avg=stud.eng.dist$Freq[stud.eng.dist$db_engage_lvl=="above_avg"],
                               avg=stud.eng.dist$Freq[stud.eng.dist$db_engage_lvl=="avg"],
                               below_avg=stud.eng.dist$Freq[stud.eng.dist$db_engage_lvl=="below_avg"],
                               row.names = NULL)
str(stud.eng.dist.df)
stud.eng.dist.df$user_id <- as.character(stud.eng.dist.df$user_id)

## for each student, find the most dominant engagement level
for(i in 1:n.stud) {
  max.cat <- "avg"
  max.val <- max(stud.eng.dist.df[i,c(2:4)])
  if(stud.eng.dist.df$above_avg[i] == max.val)
    max.cat <- "above_avg"
  else if (stud.eng.dist.df$below_avg[i] == max.val)
    max.cat <- "below_avg"
  stud.eng.dist.df$eng_cat[i] <- max.cat
}

str(stud.eng.dist.df)
table(stud.eng.dist.df$eng_cat)
# above_avg   avg below_avg 
# 90          113       155 
round(prop.table(table(stud.eng.dist.df$eng_cat)), digits = 3)
# above_avg       avg below_avg 
#     0.251     0.316     0.433


######################################################################################
## Examine the students' access to the dashboard w.r.t. the week the dashborad is 
## presenting the data for - the current week (i.e. the week of the event) or some of 
## the previous course weeks
######################################################################################

db.views <- readRDS(file = "Intermediate_results/processed_dboad-view_events.RData")

table(db.views$db_current)
#   0    1 
# 337 3499 
round(prop.table(table(db.views$db_current)), digits = 3)
#   0     1 
# 0.088 0.912

## examine the distribution of db-view events across the weeks the dashboard
## is displaying the data for - in cases when the displayed week is one of the
## previous course weeks
table(db.views$db_week[db.views$db_current==0])
#   1   2   3   4   5   6   7   8   9  10  11
# 213  37  35  24   8   3   2   5   3   3   4

## do the same but when the displayed data is for the current week 
table(db.views$db_week[db.views$db_current==1])
w.dist <- as.matrix(round(prop.table(table(db.views$db_week[db.views$db_current==1])), 
                          digits = 3))
require(knitr)
kable(t(w.dist), format = "pandoc")
