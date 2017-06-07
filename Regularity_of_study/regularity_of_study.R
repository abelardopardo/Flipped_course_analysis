# install.packages('tidyverse')
library(tidyverse)

source("regularity_of_study_functions.R")


all_events <- read_csv(file = "dataset/data2u_sem2_16_events_labeled.csv")
str(all_events)

## remove irrelevant columns
all_events <- all_events %>%
  select(-c(1,4))

## keep data for weeks 2 to 13
all_events <- all_events %>%
  filter( week >=2 & week <=13 )
table(all_events$week)

## change the order of columns
all_events <- all_events %>%
  select(user_id, action_id, received, week, topic, payload)

colnames(all_events)[3] <- "timestamp"

## examine the kinds of events / actions
unique(all_events$action_id)
prop.table(table(all_events$action_id))

## sort the event data by students, and then for each student by week and the timestamp 
all_events <- arrange(all_events, user_id, week, timestamp)


#######################################################
## prepare a dataset with study sessions related data
#######################################################

## determine study sesssions based on the time gap between two subsequent actions; 
## use 15 minutes as the session threshold, that is, if the timegap between actions i and j 
## is longer than 15 min, consider that i is the last event in the previous session and j is the
## first event in the next session
all_events <- compute.study.sessions(all_events, 15)
str(all_events)

## store the trace data with the computed time gap and session data (to avoid re-computation)
write_csv(all_events, "Intermediate_results/all_events_with_sessions_w2to13.csv")

## detect sessions that are overly short, that is, those that lasted less than 30 seconds 
result <- detect.overly.short.sessions(all_events, 0.5)
events <- result[1:9]
## events data frame with the action length added (as the last column)
events.df <- do.call(cbind.data.frame, events)
str(events.df)
events.df$payload <- as.character(events.df$payload)
## store the events data with the length of actions added (to avoid re-computation)
saveRDS(all_events, "Intermediate_results/all_events_with_sessions_and_action_len_w2to13.RData")
remove(events)

## vector of sessions that lasted less than half a minute
too.short.sessions <- as.vector(unlist(result[10:length(result)]))

## check the total number of sessions
events.df$session[nrow(events.df)]
# 53352 sessions
length(too.short.sessions)
# 14351
## check the proportion of overly short sessions
length(too.short.sessions)/events.df$session[nrow(events.df)]
# 0.27 - 27% of all sessions are short ones

## examine some of the short sessions
events.df %>%
  select( user_id, action_id, timestamp, topic, time_gap, session, action_len ) %>%
  filter( session %in% too.short.sessions[1:10] )

## remove too.short.sessions
filtered.events <- events.df %>%
  filter( !(session %in% too.short.sessions) )
## check te number of events before and after the filtering
nrow(filtered.events)
# 1,469,530
1-nrow(filtered.events)/nrow(events.df)
# 0.03 - 3% of events were removed; this suggests that 3% of events constituted 27% of sessions
# obviously the sessions were really sparse, that is, empty of any real engagement

str(filtered.events)
filtered.events$action_id <- as.character(filtered.events$action_id)
filtered.events$topic <- as.character(filtered.events$topic)

## create a data frame focused on study sessions - the subsequent analysis should be session-based
## variables to include:
## - session_id
## - user_id
## - start_time - the timestamp of the first event in the session
## - week
## - session_len - length of the session
sessions.df <- create.sessions.df(filtered.events) 
str(sessions.df)
sessions.df$start_time <- as.POSIXct(sessions.df$start_time, 
                                     origin = "1970-01-01 00:00.00 UTC", 
                                     tz = "UTC")
sessions.df$week <- as.integer(levels(sessions.df$week))[sessions.df$week]

## get the dominant / primary topic for each session
session.topics <- dominant.session.topic(filtered.events)
str(session.topics)
session.topics$session_id <- as.integer(session.topics$session_id)

## merge topics with other session data
session.data <- merge(x = sessions.df, y = session.topics, by = 'session_id',
                       all = TRUE)

## store the session data
saveRDS(session.data, "Intermediate_results/filtered_sessions_w2to13.RData")

###############################################################
# Regularity of intervals between consecutive sessions
#
# compute it as the Absolute Deviation Around the Median (MAD) of 
# the time period between two consecutive logins (sessions)
# MAD is used instead of SD, as the data (session time gaps) are
# not normally distributed (far from normal dist.)
#
# the time is expressed in minutes
###############################################################
session.data <- readRDS("Intermediate_results/filtered_sessions_w2to13.RData")

## compute, for each student, the median and MAD of inter-session time intervals
session.regularity <- regularity.session.intervals(session.data)
str(session.regularity)
summary(session.regularity)

## if a student had only one 'meaningful' session (ie. session longer than 30sec)
## both median and MAD are set to NA, as they cannot be computed 
## check how many such students
session.regularity %>%
  filter( is.na(median_s_gap) ) 
# 2 students: 1068, 1425

## will use the term "divergence" to refer to the MAD of inter-session time intervals
## of a particular student

## outliers in terms of the divergence / irregularity
boxplot(session.regularity$mad_s_gap)
length(boxplot.stats(session.regularity$mad_s_gap)$out)
# 27 (out of 486) students

## detect students whose divergence is above 90th percentile or below 10th percentile
mad_gap_quantiles <- quantile(session.regularity$mad_s_gap, probs = c(0.1, 0.9), 
                             na.rm = T, names = FALSE)
session.regularity %>%
  filter( (mad_s_gap < mad_gap_quantiles[1]) | (mad_s_gap > mad_gap_quantiles[2]) ) %>%
  nrow()
# 98

## store the computed regularity data
write_csv(session.regularity, "Intermediate_results/regularity_of_study/inter-session_time_intervals.csv")

## QUESTION: can we use certain level of divergence as indicator of irregularity? 
## only outliers? those in the top and bottom 10 percentile?

######################################################################
# Counts (frequencies), per student, that might be relevant:
# - total number of study sessions (throughout the course)
# - number of sessions per week and based on that:
#  -- number of inactive weeks (weeks with zero sessions)
#  -- SD / MAD of weekly session proportions
#  -- entropy of weekly sessions
#
# weeks 6 and 13 are excluded as these are exam preparation weeks
# when learning behavour is markedly different
######################################################################

session.data <- readRDS("Intermediate_results/filtered_sessions_w2to13.RData")
table(session.data$week)

weekly.props <- compute.weekly.counts(session.data %>% filter(week %in% c(2:5,7:12)))
# proportion of students with normal dist for weekly proportions: 0.78 -> use SD?
str(weekly.props)

table(weekly.props$inactive_weeks)
#   0   1   2   3   4   5   6   7   8   9 
# 410  47  14   3   2   1   2   1   2   4 
# 410 (84.36%) students were active all the weeks

## check for outliers
apply(weekly.props %>% select(weekly_prop_sd, weekly_prop_mad, weekly_entropy),
      2, function(x) length(boxplot.stats(x)$out))
# weekly_prop_sd weekly_prop_mad  weekly_entropy 
#     22               5              33 

## store the computed indicators
write_csv(weekly.props, "Intermediate_results/regularity_of_study/weekly_session_props.csv")


###############################################################################
# Compute number of sessions, per student, per each day of a week (Mon - Sun) 
# Also, SD, MAD, and entropy of session proportions per week day - these serve
# as indicators of regularity/irregularity in a student's weekday pattern 
# 
# the idea is to capture if a student is regular in terms of  
# the week day when he/she would engage with online resources
#
# weeks 6 and 13 are excluded (the same reason as the above)
##########################################################################

weekday.props <- make.weekdays.counts(session.data %>% filter(week %in% c(2:5,7:12)))
# proportion of students with normal dist for weekday props: 0.632

summary(weekday.props)

## check for outliers
apply(weekday.props %>% select(weekday_prop_sd, weekday_prop_mad, weekday_entropy),
      2, function(x) length(boxplot.stats(x)$out))
# weekday_prop_sd weekday_prop_mad  weekday_entropy 
#       11                4               17

## store the computed indicators
write_csv(weekday.props, "Intermediate_results/regularity_of_study/weekday_session_props.csv")


###################################################################
# Using previously computed study mode data 
# ('preparing', 'revisiting', 'catching-up', and (being) 'ahead')
# to create regularity indicators based on SD and MAD
# 
# weeks 6 and 13 are excluded (the same reason as the above)
###################################################################

study.mode.data <- read_csv(file = "Intermediate_results/study_mode_weeks2-13.csv")
str(study.mode.data)
table(study.mode.data$STUDY_MODE)

## for each student compute the proportions of events having 'revisiting', 'preparing', 
## 'catching-up', and 'ahead' study modes  
study.mode.data <- study.mode.data %>%
  filter(WEEK %in% c(2:5,7:12)) %>%
  select(USER_ID, SESSION_ID, WEEK, STUDY_MODE)
smode.indicators <- compute.smode.proportions(study.mode.data)

## check outliers
apply(smode.indicators[,-1], 2, function(x) length(boxplot.stats(x)$out))
# preparing_prop  revisits_prop catch_ups_prop  ahead_prop 
#     12              8             93             57 

## those with particularly high catch_up proportion might be the ones who
## are irregular and thus often have to do the "catch-up"
quantile(smode.indicators$catch_ups_prop, probs = seq(0.9, 1, 0.025))

## QUESTION: could we use 90+ percentile of catch-up proportion to identify irregulars?

## for each study mode, compute SD and MAD of the weekly proportions
## consider this as how regular students are in their weekly study modes
## (weeks 6 and 13 are excluded as exam preparation weeks)
smode.regularity <- weekly.studymode.regularity(study.mode.data)     
str(smode.regularity)
summary(smode.regularity)
smode.regularity$user_id[is.na(smode.regularity$weekly_preparing_prop_sd)]
# 878  964 1068 1425
## 4 students were active only during 1 of the considered weeks (2:5,7:12)
## so, for them, SD could not be computed, hence NA values

## check for outliers
apply(smode.regularity[,-1], 2, function(x) length(boxplot.stats(x)$out))
# weekly_catching-up_prop_* and weekly_ahead_prop_* have high number of outliers

## store the regularity of study mode indicators
write_csv(smode.regularity, "Intermediate_results/regularity_of_study/regularity_of_study_mode_proportions.csv")


##############################################################################
## Diversity of topics reviewed in a session, and how regular students were
## in this aspect of their learning behaviour
## 
## Compute it as MAD in the number of topics considered during a session
## (unique topic counts are not normally distributed)
## weeks 6 and 13 are excluded as these are the weeks when students 
## revise for the exams and thus are expected to be engaged with various topics   
##
## The idea is to have an indicator of how regular / irregular students were 
## in their tendency to mix topics or to stick to one topic only
##############################################################################

session.data <- readRDS("Intermediate_results/filtered_sessions_w2to13.RData")

events.data <- readRDS("Intermediate_results/all_events_with_sessions_and_action_len_w2to13.RData") 
str(events.data)

## keep only relevant columns and sessions that are not overly short (i.e. at least 30sec long)
## also, remove events from weeks 6 and 13
events.data <- events.data %>%
  select(user_id, week, topic, session) %>%
  filter( (session %in% session.data$session_id) & (week %in% c(2:5,7:12)) )

## examine topics
events.data %>% select(topic) %>% table()

## group some of the topics:
## - ORG, DBOARD, STRAT, STUDYKIT - metacognitive actions
## - HOME, W01 - W17, HOF, SEARCH - orientiring actions
events.data$topic[events.data$topic %in% c('ORG', 'DBOARD', 'STRAT', 'STUDYKIT')] <- 'METACOG'
events.data$topic[events.data$topic %in% c('HOME', 'HOF', 'SEARCH', 'TEA', 'EXAM',
                                           paste0('W0', 1:9), paste0('W', 10:13))] <- 'ORIENTIRING'
events.data %>% select(topic) %>% table()

## compute topic diversity indicator
topic.regularity <- topic.diversity(events.data)
# Proportion of students with normal dist. for topic ses. counts: 0
summary(topic.regularity)

## check for outliers
apply(topic.regularity[,-1], 2, function(x) length(boxplot.stats(x)$out))
# topic_cnt_median    topic_cnt_mad 
#       13               71

## store the indicators related to topic diversity 
write_csv(topic.regularity, "Intermediate_results/regularity_of_study/regularity_of_topic_diversity.csv")

#################################################################################
# Diversity in the types of resources used during a session and how regular 
# students were with respect to this aspect of their learning behaviour
# This diversity can be considered as an indicator of the "depth" of the study 
# session (the higher the diversity, the "deeper" the study session)
# 
# The idea is to have an indicator of how regular / irregular students were 
# in their tendency to have deep study sessions
#
# weeks 6 and 13 are excluded
##############################################################################

events.data <- readRDS(file = "Intermediate_results/all_events_with_sessions_and_action_len_w2to13.RData")
str(events.data)
table(events.data$action_id)

## keep only relevant columns and sessions that are not overly short (i.e. at least 30sec long)
events.data <- events.data %>%
  select(user_id, action_id, week, session) %>%
  filter( (session %in% session.data$session_id) & (week %in% c(2:5,7:12)) )

## add resource_type column based on the action types:
## - 'form-submit' and 'activity-duration' - ignore; in any case they form just < 0.09% of all the events
## - for 'exco-*' introduce exercise (EXE) as resource type
## - for 'embedded-question' introduce multiple choice question (MCQ) as resource type
## - for 'embedded-video' -introduce video (VIDEO) as resource type
## - for 'dboard-view', 'studykit-view', 'xy-click' introduce metacognitive toolkit (METACOG) as resource type
## - for 'resource-view' introduce RES as resource type
## - 'activity-collapse-expand' - ignore, as these are produced when users expand or collapse a 
##   subsection of an HTML page, and access to a HTML page is already captured by resource-view 
events.data <- events.data %>%
  filter( !(action_id %in% c('form-submit', 'activity-duration', 'activity-collapse-expand')) )

events.data$res_type <- "unknown"
events.data$res_type[events.data$action_id=='embedded-question'] <- 'MCQ'
events.data$res_type[events.data$action_id=='embedded-video'] <- 'VIDEO'
events.data$res_type[events.data$action_id=='resource-view'] <- 'RES'
events.data$res_type[events.data$action_id %in% c('dboard-view', 'studykit-view', 'xy-click')] <- 'METACOG'
events.data$res_type[events.data$action_id %in% c('exco-answer', 'exco-done', 'exco-reset', 'exco-view')] <- "EXE"

events.data %>% select(res_type) %>% table()

res.type.regularity <- resource.diversity(events.data)
summary(res.type.regularity)
## check for outliers
apply(res.type.regularity[,-1], 2, function(x) length(boxplot.stats(x)$out))
# no outliers

## store the indicators of regularity in resource type diversity 
write_csv(res.type.regularity, 
          "Intermediate_results/regularity_of_study/regularity_of_resource_type.csv")


#################################################################################
# Compute proportions of 
# - 'preparation' sessions, that is, sessions with the main_topic being the topic
#    of the week's lecture
# - 'preparation' sessions done in 24h before a week's lecture; lectures took 
#   place on Friday at noon, so, looking at 'preparation' sessions that happened 
#   between Thur noon and Fri noon
# Also, SD of these proportions computed at the weekly level
#
# weeks 6 and 13 are excluded
#################################################################################

session.data <- readRDS("Intermediate_results/filtered_sessions_w2to13.RData")
table(session.data$main_topic)

## consider only sessions where the main topic was one of the course topics
## or W01-W05, W07-W12 topics
relevant.topics <- c('ADM','ARC','ASP','CDL','COD','CST','HLP','ISA','SDL','DRM',
                     paste0('W0', c(2:5,7:9)), paste0('W', 10:12))
session.data <- session.data %>%
  filter(main_topic %in% relevant.topics)

ontopic.last.min <- ontopic.and.last.minute.stats(session.data)
summary(ontopic.last.min)

## check outliers
apply(ontopic.last.min[,-1], 2, function(x) length(boxplot.stats(x)$out))
# on_topic_prop     last_min_prop on_topic_prop_mad last_min_prop_mad  on_topic_prop_sd last_min_prop_sd 
# 8                 8                 0                17                14               0

## store the indicators
write_csv(ontopic.last.min, 
          "Intermediate_results/regularity_of_study/on_topic_and_last_min_proportions.csv")

###########################################################################################
# gather all indicators of regularity of study that have been computed so far
###########################################################################################

weekly.props <- read_csv("Intermediate_results/regularity_of_study/weekly_session_props.csv")
weekday.props <- read_csv("Intermediate_results/regularity_of_study/weekday_session_props.csv")

str(weekly.props)
str(weekday.props)

regularity.data <- merge(x = weekly.props[, c(1,2,13:16)], y = weekday.props[,c(1,9:11)],
                         by = 'user_id', all = T)

ses.regularity <- read_csv("Intermediate_results/regularity_of_study/inter-session_time_intervals.csv")
regularity.data <- merge(x = regularity.data, y = ses.regularity[,c(1,3)],
                         by = 'user_id', all = T)

study.mode.regularity <- read_csv("Intermediate_results/regularity_of_study/regularity_of_study_mode_proportions.csv")
regularity.data <- merge(x = regularity.data, 
                         y = study.mode.regularity[,c(1,6:9)], 
                         by = 'user_id', all = TRUE)

colnames(regularity.data)[2:10] <- c('ses_tot', 'inactive_week_cnt','week_prop_sd','week_prop_mad', 'week_entropy', 
                                    'weekday_prop_sd','weekday_prop_mad', 'weekday_entropy', 'ses_timegap_mad')

topic.regularity <- read_csv("Intermediate_results/regularity_of_study/regularity_of_topic_diversity.csv")
regularity.data <- merge(x = regularity.data, y = topic.regularity, by = 'user_id', all = T)

res.type.regularity <- read_csv("Intermediate_results/regularity_of_study/regularity_of_resource_type.csv")
regularity.data <- merge(x = regularity.data, y = res.type.regularity, by = 'user_id', all = T)

ontopic.last.min <- read_csv("Intermediate_results/regularity_of_study/on_topic_and_last_min_proportions.csv")
regularity.data <- merge(x = regularity.data, y = ontopic.last.min, by = 'user_id', all = T)

str(regularity.data)

write_csv(regularity.data, 
          path = "Intermediate_results/regularity_of_study/regularity_indicators.csv")




#############################################################################
# Potential additional features:
#
# Assuming we know day/time when the preparation activities for a particular 
# week were made available to students, we can compute with how much delay
# a student started working on the preparation activities
# We can compute:
# - average delay
# - SD / MAD of the delay
#############################################################################


