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

weekly.props <- compute.weekly.counts(sessions = session.data %>% filter(week %in% c(2:5,7:12)),
                                      weeks = c(2:5,7:12))
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
# For each student, compute the number of days the student was active 
# (had at least one study session) in each week of the course
#
# To examine how regular a student was in weekly course engagements,
# compute SD and entropy of weekly counts
# 
# To examine how regular a student was throughout the course,
# compute the number of days (time gap) between two consecutive engagements
#############################################################################
session.data <- readRDS("Intermediate_results/filtered_sessions_w2to13.RData")

daily.counts <- make.daily.counts(session.data, c(2:13))
# proportion of students with normal dist for weekly counts of engaged days: 0.64

## add a column with the total number of active/engaged days
daily.counts$tot_cnt <- rowSums(daily.counts[,c(86:97)])

## add a column for SD of the weekly counts
daily.counts$weekly_cnt_sd <- apply(daily.counts[,c(86:97)], 1, sd)

## add a column for entropy of weekly counts
weekly.props <- as.data.frame(apply(daily.counts[,c(86:97)], 2, 
                                    function(x) x/daily.counts$tot_cnt))
entropy <- apply(weekly.props, 1, function(x) {-sum(x * log1p(x))})
summary(entropy)
daily.counts$weekly_entropy <- entropy

## store the data
write_csv(daily.counts, 
          "Intermediate_results/regularity_of_study/weekly_counts_of_daily_logins_w2-13.csv")


## compute time gap (in days) between two consecutive active days
daily.counts <- read.csv("Intermediate_results/regularity_of_study/weekly_counts_of_daily_logins_w2-13.csv")

daily.gaps <- gaps.between.consecutive.active.days(daily.counts[,c(1:85)])
# Proportion of students with normal dist for time gaps: 0.02

summary(daily.gaps[,-1])

## store the data
write_csv(daily.gaps, 
          "Intermediate_results/regularity_of_study/gaps_between_consecutive_logins_w2-13.csv")


#############################################################################
# For each student and each day of the course, compute the number of different 
# kinds of resources the student used on the given day
#
# the resources are determined based on the action type:  
# - 'form-submit' and 'activity-duration' actions are ignored
# - for 'exco-*', exercise (EXE) is introduced as the resource type
# - for 'embedded-question', multiple choice question (MCQ) is the resource type
# - for 'embedded-video', video (VIDEO) is the resource type
# - for 'dboard-view', 'studykit-view', and 'xy-click' metacognitive toolkit (METACOG) is the resource type
# - for 'resource-view', RES is the resource type
# - 'activity-collapse-expand' is ignored, as these are produced when users expand or collapse a 
#   subsection of an HTML page, and access to a HTML page is already captured by resource-view 
#############################################################################
session.data <- readRDS("Intermediate_results/filtered_sessions_w2to13.RData")

events.data <- readRDS("Intermediate_results/all_events_with_sessions_and_action_len_w2to13.RData") 
str(events.data)

## keep only relevant columns and sessions that are not overly short (i.e. at least 30sec long)
events.data <- events.data %>%
  select(user_id, action_id, week, timestamp, session) %>%
  filter( session %in% session.data$session_id )

## add resource_type column based on the action types
events.data <- events.data %>%
  filter( !(action_id %in% c('form-submit', 'activity-duration', 'activity-collapse-expand')) )

events.data$res_type <- "unknown"
events.data$res_type[events.data$action_id=='embedded-question'] <- 'MCQ'
events.data$res_type[events.data$action_id=='embedded-video'] <- 'VIDEO'
events.data$res_type[events.data$action_id=='resource-view'] <- 'RES'
events.data$res_type[events.data$action_id %in% c('dboard-view', 'studykit-view', 'xy-click')] <- 'METACOG'
events.data$res_type[events.data$action_id %in% c('exco-answer', 'exco-done', 'exco-reset', 'exco-view')] <- "EXE"

events.data %>% select(res_type) %>% table()

daily.res.counts <- daily.counts.of.resource.use(events.data, 2:13)
colnames(daily.res.counts)
## store the counts
write_csv(daily.res.counts, 
          "Intermediate_results/regularity_of_study/daily_counts_of_resource_use_w2-13.csv")


####################################################################################
# Compute some basic statistics about the different kinds of resources students used
# during their active days; the types of resources considered: 
# - video
# - exercises
# - MCQs
# - reading materials (RES)
# - metacognitive items (METACOG)
#
# Computed statistics:
# - number of resources (of the give type) used during the student's active days
# - median number of resources (of the give type) used during the student's active days
# - MAD of resources (of the give type) used during the student's active days
# - number of days when resources of the given type were used
# - proportion of days when resources of the given type were used versus total number of
#   active days
# note: when computing these statistics, only the days when a student was active are considered
#
# Weeks 6 and 13 are excluded from these computations, as during these weeks one 
# can expect  different behavioral patterns than usual
#########################################################################################

daily.res.counts <- read.csv("Intermediate_results/regularity_of_study/daily_counts_of_resource_use_w2-13.csv")
count.vars <- colnames(daily.res.counts)

## remove columns related to weeks 6 and 13
weeks_6_13 <- grep("^(W6|W13).+", count.vars)
count.vars <- count.vars[-weeks_6_13]
daily.res.counts <- daily.res.counts %>% select(one_of(count.vars))

## identify active days
daily.counts <- read.csv("Intermediate_results/regularity_of_study/weekly_counts_of_daily_logins_w2-13.csv")
colnames(daily.counts)
## columns from 86 onwards are irrelevant in this context
daily.counts <- daily.counts[,c(1:85)]
## remove columns related to weeks 6 and 13
weeks_6_13 <- grep("^(W6|W13).+", colnames(daily.counts))
daily.counts <- daily.counts[,-weeks_6_13]

## data frame with binary 1-0 values indicating if a student was active on a certain day 
active.days <- as.data.frame(apply(daily.counts[,-1], 2, 
                                   function(x) ifelse(test = x > 0, yes = 1, no = 0)))
active.days <- cbind(user_id=daily.counts$user_id, active.days)

## from daily.res.counts select only variables related to video counts
video.vars <- count.vars[grep(".+_VIDEO", count.vars)]
video.vars <- c('user_id', video.vars)
video.counts <- daily.res.counts %>% select(one_of(video.vars))

video.stats <- compute.count.stats(video.counts, active.days)
colnames(video.stats) <- c('user_id', 'tot_video_cnt', 'median_video_cnt', 'mad_video_cnt', 
                           'days_video_used', 'prop_video_used')
summary(video.stats)
length(which(video.stats$mad_video_cnt>0))
# only 18 values - useless indicator
length(which(video.stats$median_video_cnt>0))
# also, only 18


## from daily.res.counts select only variables related to exercise counts
exe.vars <- count.vars[grep(".+_EXE", count.vars)]
exe.vars <- c('user_id', exe.vars)
exe.counts <- daily.res.counts %>% select(one_of(exe.vars))

exe.stats <- compute.count.stats(exe.counts, active.days)
# Ratio of student with normally distrubuted counts: 0.039 -> use median and MAD
colnames(exe.stats) <- c('user_id', 'tot_exe_cnt', 'median_exe_cnt', 'mad_exe_cnt', 
                           'days_exe_used', 'prop_exe_used')
summary(exe.stats)

## start merging the resource use statistics
res.use.stats <- merge(x = video.stats, y = exe.stats, by = 'user_id', all = T)


## from daily.res.counts select only variables related to MCQ counts
mcq.vars <- count.vars[grep(".+_MCQ", count.vars)]
mcq.vars <- c('user_id', mcq.vars)
mcq.counts <- daily.res.counts %>% select(one_of(mcq.vars))

mcq.stats <- compute.count.stats(mcq.counts, active.days)
# Ration of student with normally distrubuted counts: 0.004 -> use median and MAD
colnames(mcq.stats) <- c('user_id', 'tot_mcq_cnt', 'median_mcq_cnt', 'mad_mcq_cnt', 
                         'days_mcq_used', 'prop_mcq_used')
summary(mcq.stats)
## too many zeros for median_mcq_cnt and mad_mcq_cnt, let's check
length(which(mcq.stats$median_mcq_cnt>0))
# only 22
length(which(mcq.stats$mad_mcq_cnt>0))
# 21

## add to resource use statistics
res.use.stats <- merge(x = res.use.stats, y = mcq.stats, by = 'user_id', all = TRUE)


## from daily.res.counts select only variables related to METACOG counts
mcog.vars <- count.vars[grep(".+_METACOG", count.vars)]
mcog.vars <- c('user_id', mcog.vars)
mcog.counts <- daily.res.counts %>% select(one_of(mcog.vars))

mcog.stats <- compute.count.stats(mcog.counts, active.days)
# Ratio of student with normally distrubuted counts: 0.002 -> use median and MAD
colnames(mcog.stats) <- c('user_id', 'tot_mcog_cnt', 'median_mcog_cnt', 'mad_mcog_cnt', 
                         'days_mcog_used', 'prop_mcog_used')
summary(mcog.stats)
## again, too many zeros for median and mad values; let's check
length(which(mcog.stats$median_mcog_cnt>0))
# only 22
length(which(mcog.stats$mad_mcog_cnt>0))
# 21

## add to resource use statistics
res.use.stats <- merge(x = res.use.stats, y = mcog.stats, by = 'user_id', all = TRUE)


## from daily.res.counts select only variables related to RES counts
res.vars <- count.vars[grep(".+_RES", count.vars)]
res.vars <- c('user_id', res.vars)
res.counts <- daily.res.counts %>% select(one_of(res.vars))

res.stats <- compute.count.stats(res.counts, active.days)
# Ratio of student with normally distrubuted counts: 0.064 -> use median and MAD
colnames(res.stats) <- c('user_id', 'tot_res_cnt', 'median_res_cnt', 'mad_res_cnt', 
                          'days_res_used', 'prop_res_used')
summary(res.stats)

## add to resource use statistics
res.use.stats <- merge(x = res.use.stats, y = res.stats, by = 'user_id', all = TRUE)

## save the resource use statistics
write_csv(res.use.stats, 
          "Intermediate_results/regularity_of_study/daily_resource_use_statistics_w2-5_7-12.csv")


#############################################################################
# For each student and each day of the course compute the number of 
# events / actions that were:
# - 'ontopic' - the topic associated with the action is the topic of the current week
# - 'revisiting' - the topic associated with the action is the topic of one of the previous weeks
# - 'metacognitive' - the topic associated with the action is one of the following:
#    'ORG', 'DBOARD', 'STRAT', 'STUDYKIT'
# - 'orientiring' - the topic associated with the action is one of the following:
#    'HOME', 'HOF', 'SEARCH', 'TEA', 'EXAM', 'W01'-'W13'
# - 'project' - the action is related to project work
#
# Events with unknown topic (topic attribute not set) are removed
#
# Weeks 6 and 13 are excluded from these computations, as during these weeks one 
# can expect  different behavioral patterns than usual
#############################################################################

session.data <- readRDS("Intermediate_results/filtered_sessions_w2to13.RData")

events.data <- readRDS("Intermediate_results/all_events_with_sessions_and_action_len_w2to13.RData") 
str(events.data)

## keep only relevant columns and sessions that are not overly short (i.e. at least 30sec long)
## also, remove events from weeks 6 and 13; also, events where the topic is unknown
events.data <- events.data %>%
  select(user_id, timestamp, week, topic, session) %>%
  filter( (session %in% session.data$session_id) & (week %in% c(2:5,7:12)) & (is.na(topic)==FALSE))

## examine topics
events.data %>% select(topic) %>% table()

## group some of the topics:
## - ORG, DBOARD, STRAT, STUDYKIT - metacognitive actions
## - HOME, W01 - W13, HOF, SEARCH - orientiring actions
events.data$topic[events.data$topic %in% c('ORG', 'DBOARD', 'STRAT', 'STUDYKIT')] <- 'METACOG'
events.data$topic[events.data$topic %in% c('HOME', 'HOF', 'SEARCH', 'TEA', 'EXAM',
                                           paste0('W0', 1:9), paste0('W', 10:13))] <- 'ORIENTIRING'
events.data %>% select(topic) %>% table()

events.data %>% filter(topic=="PRJ") %>% select(week) %>% table()
# students are work on the project from week 7

daily.topic.cnt <- daily.topic.counts(events.data, c(2:5,7:12))
colnames(daily.topic.cnt)

## store the computed counts
write_csv(daily.topic.cnt,
          "Intermediate_results/regularity_of_study/daily_topic_counts_w2-5_7-12.csv")


####################################################################################
# Compute some basic statistics about the counts of the students' topic focus 
# during their active days; possible topic foci (described above, lines 671-677): 
# - 'ontopic' 
# - 'revisiting' 
# - 'metacognitive' 
# - 'orientiring' 
# - 'project' 
#
# Computed statistics:
# - total number of learning actions (during active days) with a particular topic focus
# - median number of learning actions per active day with a particular topic focus 
# - MAD of learning actions per active day with a particular topic focus
# - number of days with at least one action with particular topic focus 
# - proportion of days with the given type of topic focus versus total number of
#   active days
# note: when computing these statistics, only the days when a student was active are considered
#
# Weeks 6 and 13 are excluded from these computations, as during these weeks one 
# can expect  different behavioral patterns than usual
#########################################################################################

daily.topic.cnt <- read.csv("Intermediate_results/regularity_of_study/daily_topic_counts_w2-5_7-12.csv")
cnt.vars <- colnames(daily.topic.cnt)

## identify students' active days
daily.counts <- read.csv("Intermediate_results/regularity_of_study/weekly_counts_of_daily_logins_w2-13.csv")
colnames(daily.counts)
## columns from 86 onwards are irrelevant in this context
daily.counts <- daily.counts[,c(1:85)]
## remove columns related to weeks 6 and 13
weeks_6_13 <- grep("^(W6|W13).+", colnames(daily.counts))
daily.counts <- daily.counts[,-weeks_6_13]
## data frame with binary 1-0 values indicating if a student was active on a certain day 
active.days <- as.data.frame(apply(daily.counts[,-1], 2, function(x) ifelse(test = x > 0, yes = 1, no = 0)))
active.days <- cbind(user_id=daily.counts$user_id, active.days)

## from daily.topic.prop select only variables related to 'ontopic' events
ontopic.vars <- cnt.vars[grep(".+_ontopic", cnt.vars)]
ontopic.vars <- c('user_id', ontopic.vars)
ontopic.cnt <- daily.topic.cnt %>% select(one_of(ontopic.vars))

ontopic.stats <- compute.count.stats(ontopic.cnt, active.days)
# ratio of student with normally distrubuted proportions: 0.01 -> use median and MAD
colnames(ontopic.stats) <- c('user_id', 'tot_ontopic_cnt', 'median_ontopic_cnt', 'mad_ontopic_cnt', 
                             'ontopic_days', 'ontopic_prop')
summary(ontopic.stats)


## from daily.topic.prop select only variables related to 'revisiting' events
rev.vars <- cnt.vars[grep(".+_revisit", cnt.vars)]
rev.vars <- c('user_id', rev.vars)
rev.cnt <- daily.topic.cnt %>% select(one_of(rev.vars))

rev.stats <- compute.count.stats(rev.cnt, active.days)
# ratio of student with normally distrubuted proportions: 0.008 -> use median and MAD
colnames(rev.stats) <- c('user_id', 'tot_revisit_cnt', 'median_revisit_cnt', 'mad_revisit_cnt',
                         'revisit_days', 'revisit_prop')
summary(rev.stats)

## merge stats related to different topic foci into one data frame
topic.stats <- merge(x = ontopic.stats, y = rev.stats, by = 'user_id', all = TRUE)


## from daily.topic.prop select only variables related to 'metacognitive' events
mcog.vars <- cnt.vars[grep(".+_metacog", cnt.vars)]
mcog.vars <- c('user_id', mcog.vars)
mcog.cnt <- daily.topic.cnt %>% select(one_of(mcog.vars))

mcog.stats <- compute.count.stats(mcog.cnt, active.days)
# ratio of student with normally distrubuted proportions: 0.06 -> use median and MAD
colnames(mcog.stats) <- c('user_id', 'tot_metacog_cnt', 'median_metacog_cnt', 'mad_metacog_cnt',
                          'metacog_days', 'metacog_prop')
summary(mcog.stats)

topic.stats <- merge(x = topic.stats, y = mcog.stats, by = 'user_id', all = TRUE)


## from daily.topic.prop select only variables related to 'orientiring' events
ornt.vars <- cnt.vars[grep(".+_orient", cnt.vars)]
ornt.vars <- c('user_id', ornt.vars)
ornt.cnt <- daily.topic.cnt %>% select(one_of(ornt.vars))

ornt.stats <- compute.count.stats(ornt.cnt, active.days)
# ratio of student with normally distrubuted proportions: 0.06 -> use median and MAD
colnames(ornt.stats) <- c('user_id', 'tot_orient_cnt', 'median_orient_cnt', 'mad_orient_cnt',
                          'orinet_days', 'orient_prop')
summary(ornt.stats)

topic.stats <- merge(x = topic.stats, y = ornt.stats, by = 'user_id', all = TRUE)


## from daily.topic.prop select only variables related to 'project' events
prj.vars <- cnt.vars[grep(".+_prj", cnt.vars)]
prj.vars <- c('user_id', prj.vars)
prj.cnt <- daily.topic.cnt %>% select(one_of(prj.vars))

prj.stats <- compute.count.stats(prj.cnt, active.days)
# ratio of student with normally distrubuted proportions: 0 -> use median and MAD
colnames(prj.stats) <- c('user_id', 'tot_prj_cnt', 'median_prj_cnt', 'mad_prj_cnt',
                         'prj_days', 'prj_prop')
summary(prj.stats)

topic.stats <- merge(x = topic.stats, y = prj.stats, by = 'user_id', all = TRUE)
str(topic.stats)

## save topic proportions statistics
write_csv(topic.stats, 
          "Intermediate_results/regularity_of_study/topic_counts_statistics_w2-5_7-12.csv")


#######################################################################################
# Compute engagement indicators based on the different kinds of resources students used
# during their active days; the types of resources considered: 
# - video
# - exercises
# - MCQs
# - reading materials (RES)
# - metacognitive items (METACOG)
#
# Indicators are to be computed at the week level, based on the following principle:
# a score of one is given to a student (for a given week), if he/she used certain kind 
# of resurce (e.g. video) more than the average (median) use of the that resource type
# in the given week
##########################################################################################

daily.res.counts <- read.csv("Intermediate_results/regularity_of_study/daily_counts_of_resource_use_w2-13.csv")
count.vars <- colnames(daily.res.counts)

## compute resource use at the week level
res_types <- c('MCQ', 'EXE', 'VIDEO', 'RES', 'METACOG')
weekly.counts <- data.frame(user_id=daily.res.counts$user_id)
for(w in 2:13) {
  for(rt in res_types) {
    pattern <- paste0('W',w,"_\\w{3}_",rt)
    rt.vars <- count.vars[grep(pattern, count.vars)]
    rt.vars <- c('user_id', rt.vars)
    rt.counts <- daily.res.counts %>% select(one_of(rt.vars))
    rt.counts$sum <- rowSums(rt.counts[,-1])
    colnames(rt.counts)[ncol(rt.counts)] <- paste0('W',w,'_',rt,'_tot')
    weekly.counts <- merge(x = weekly.counts, y = rt.counts[,c(1,ncol(rt.counts))], 
                           by = "user_id", all = TRUE)
  }
}

## compute weekly average values for each week and each resource type
## these will be the mean / median values of each column of the weekly.counts df
## first check if variables are normally distributed
apply(weekly.counts %>% select(-user_id), 2, shapiro.test)
# no -> compute median
weekly.rt.median <- apply(weekly.counts %>% select(-user_id), 2, median)

## for each week and each resource type, give student score 1 if his/her counts
## are above the median value for that week and the resource type 
m <- matrix(nrow = nrow(weekly.counts), ncol = ncol(weekly.counts)-1)
cnt <- 1 
for(s in weekly.counts$user_id) {
  stud_data <- weekly.counts %>% filter(user_id==s) %>% select(-user_id) %>% as.numeric()
  m[cnt,] <- (stud_data > weekly.rt.median) * 1 # multiplying with 1 to transform into number
  cnt <- cnt + 1
}
rt.indicators <- data.frame(m)
rt.indicators <- as.data.frame(cbind(user_id=weekly.counts$user_id, rt.indicators))

# create column names
c.names <- 'user_id'
for(w in 2:13) {
  c.names <- c(c.names, paste0('W', w, '_', res_types, '_bin'))
}
colnames(rt.indicators) <- c.names

## save these indicators
write.csv(rt.indicators, "Intermediate_results/regularity_of_study/res_use_above_median_indicators_w2-13.csv",
          quote = FALSE, row.names = FALSE)

## for each type of resource, compute the number of weeks when it was used 
## above the median level of use 
rt.sum.indicators <- compute.engagement.indicator(rt.indicators, c(2:13), res_types)
head(rt.sum.indicators)

## save these indicators
write.csv(rt.sum.indicators, "Intermediate_results/regularity_of_study/res_use_indicators_w2-13.csv",
          quote = FALSE, row.names = FALSE)


#######################################################################################
# Compute engagement indicators based on the different kinds of 'topic focus' students 
# had during their active days; the considered kinds of topic focus: 
# - 'ontopic' 
# - 'revisiting' 
# - 'metacognitive' 
# - 'orientiring' 
# - 'project' 
#
# Indicators are to be computed at the week level, based on the following principle:
# a score of one is given to a student (for a given week), if his/her number of events 
# related to a particular topic type (e.g. revisiting) was above the average (median) 
# number of events with that topic type in the given week
#
# Weeks 6 and 13 are excluded from these computations, as during these weeks one 
# can expect  different behavioral patterns than usual
##########################################################################################

daily.topic.cnt <- read.csv("Intermediate_results/regularity_of_study/daily_topic_counts_w2-5_7-12.csv")
count.vars <- colnames(daily.topic.cnt)

## compute topic focus at the week level
topic_types <- c('ontopic', 'revisit', 'metacog', 'orient', 'prj')
weekly.counts <- data.frame(user_id=daily.topic.cnt$user_id)
for(w in c(2:5,7:12)) {
  for(tt in topic_types) {
    pattern <- paste0('W',w,"_\\w{3}_",tt)
    tt.vars <- count.vars[grep(pattern, count.vars)]
    tt.vars <- c('user_id', tt.vars)
    tt.counts <- daily.topic.cnt %>% select(one_of(tt.vars))
    tt.counts$sum <- rowSums(tt.counts[,-1])
    colnames(tt.counts)[ncol(tt.counts)] <- paste0('W',w,'_',tt,'_tot')
    weekly.counts <- merge(x = weekly.counts, y = tt.counts[,c(1,ncol(tt.counts))], 
                           by = "user_id", all = TRUE)
  }
}

## compute weekly average values for each week and each topic type
## these will be the mean / median values of each column of the weekly.counts df
## first check if variables are normally distributed
apply(weekly.counts %>% select(-c(user_id, ends_with("prj_tot"))), 2, shapiro.test)
# no -> compute median
weekly.tt.median <- apply(weekly.counts %>% select(-user_id), 2, median)

## for each week and each topic type, give student score 1 if his/her counts
## are above the median value for that week and the topic type 
m <- matrix(nrow = nrow(weekly.counts), ncol = ncol(weekly.counts)-1)
cnt <- 1 
for(s in weekly.counts$user_id) {
  stud_data <- weekly.counts %>% filter(user_id==s) %>% select(-user_id) %>% as.numeric()
  m[cnt,] <- (stud_data > weekly.tt.median) * 1 # multiplying with 1 to transform into number
  cnt <- cnt + 1
}
topic.indicators <- data.frame(m)
topic.indicators <- as.data.frame(cbind(user_id=weekly.counts$user_id, topic.indicators))

# create column names
c.names <- 'user_id'
for(w in c(2:5,7:12)) {
  c.names <- c(c.names, paste0('W', w, '_', topic_types, '_bin'))
}
colnames(topic.indicators) <- c.names

## save these indicators
write.csv(topic.indicators, 
          "Intermediate_results/regularity_of_study/topics_above_median_indicators_w2-5_7-12.csv",
          quote = FALSE, row.names = FALSE)

## for each topic type, compute the number of weeks when it was present 
## above the median level 
topic.sum.indicators <- compute.engagement.indicator(topic.indicators, c(2:5,7:12), topic_types)
head(topic.sum.indicators)

## save these indicators
write.csv(topic.sum.indicators, 
          "Intermediate_results/regularity_of_study/topic_based_indicators_w2-5_7-12.csv",
          quote = FALSE, row.names = FALSE)


#############################################################################
# For each course week compute counts and proportions of:
# - correctly solved MCQs
# - incorrectly solved MCQs
# - solution was requested for MCQ
# - correctly solved exercises
# - incorrectly solved exercises
#
# Then, compute 
# - SD (or MAD) of proportions as indicators of regularity 
# - entropy of each type of assessment action; this will be 
#   entropy of weekly counts 
#############################################################################

traces <- readRDS(file = "Intermediate_results/study_mode_weeks2-13.RData")
str(traces)

traces$ACTION <- as.character(traces$ACTION)
## rename EQT_CO, VEQ_CO, and EXE_F_CO to FA_CO (FA = Formative Assessment), also
## rename EQT_IN, VEQ_IN, and EXE_F_IN to FA_IN, 
## rename EQT_SR and VEQ_SR to FA_SR 
traces$ACTION[traces$ACTION %in% c("EQT_CO", "VEQ_CO", "EXE_F_CO")] <- "FA_CO"
traces$ACTION[traces$ACTION %in% c("EQT_IN", "VEQ_IN", "EXE_F_IN")] <- "FA_IN"
traces$ACTION[traces$ACTION %in% c("EQT_SR", "VEQ_SR")] <- "FA_SR"
## rename EXE_S_CO to SA_CO (SA = Summative Assessment), and EXE_S_IN to SA_IN
traces$ACTION[traces$ACTION=="EXE_S_CO"] <- "SA_CO"
traces$ACTION[traces$ACTION=="EXE_S_IN"] <- "SA_IN"
## remove columns and rows that are not needed
traces <- traces %>% 
  select(-c(STUDY_MODE, TIME_GAP, TOPIC)) 
traces <- subset(traces, ACTION %in% c("FA_CO", "FA_IN", "FA_SR", "SA_CO", "SA_IN"))
table(traces$ACTION)  
## sort the data based on the 1) student, 2) WEEK, 3) timestamp
sorted.traces <- traces[ with(traces, order(USER_ID, WEEK, TIMESTAMP)), ]
# remove timestamp - for some reason, its causing problems and is not needed any more
sorted.traces <- sorted.traces %>% select(-TIMESTAMP)
colnames(sorted.traces) <- c("user_id", "session_id", "action", "week")

## compute proportions of different types of assessment actions and based on that 
## regularity indicators (SD, MAD)
weekly.assess.prop <- weekly.prop.of.assessment.actions(sorted.traces, c(2:5,7:12))

## save these indicators
write.csv(weekly.assess.prop, 
          "Intermediate_results/regularity_of_study/weekly_assessement_action_prop_w2-5_7-12.csv",
          quote = FALSE, row.names = FALSE)

## compute entropy of different types of assessment action counts
assess.weekly.entropy <- weekly.entropy.of.assessment.actions(sorted.traces, c(2:5,7:12))

## save these indicators
write.csv(assess.weekly.entropy, 
          "Intermediate_results/regularity_of_study/weekly_assessement_action_entropy_w2-5_7-12.csv",
          quote = FALSE, row.names = FALSE)

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
