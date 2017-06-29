## the f. determines students' study sessions based on the time gap between 
## two subsequent learning events; it is assumed that the time gap greater than the 
## 'threshold' parameter is indicative of a termination of one session and the begining of
## another one; the threshold is expressed in minutes
## the f. returns the input dataframe with two new columns: 'time_gap' and 'session'
compute.study.sessions <- function(traces, threshold) {
  traces$time_gap <- vector(mode = "numeric", length = nrow(traces))
  traces$session <- vector(mode = "numeric", length = nrow(traces))
  traces$time_gap[1] <- NA
  
  s <- 1
  traces$session[1] <- s
  
  current.stud <- traces$user_id[1];
  for(i in 2:nrow(traces)) {
    if (traces$user_id[i]==current.stud) {
      traces$time_gap[i] <- difftime(time1 = traces$timestamp[i], 
                                     time2 = traces$timestamp[i-1], 
                                     units = "mins" )
      ## consider that a 'threshold' long gap between two consecutive events indicate the end
      ## of one and begining of a new session; to indicate that, set time_gap to NA 
      ## with meaning "not applicable" or "not meaningful"
      if (traces$time_gap[i] > threshold) {
        traces$time_gap[i] <- NA
        s <- s + 1
      }
    } else {
      traces$time_gap[i] <- NA  
      s <- s + 1
      current.stud <- traces$user_id[i]
    }  
    traces$session[i] <- s
  }
  return (traces)
}


## the f. identifies overly short study sessions - those that are so short 
## than no meaninful learning activity could have taken place; the 'threshold' parameter 
## sets the minimum lenght of a meaningful session, so any session with length less than 
## the given threshold will be consdered as overly short to be meaningful
## the threshold is expressed in minutes
detect.overly.short.sessions <- function(traces, threshold) {
  ## variable to store the length of the study sessions
  traces$action_len <- vector(mode = 'numeric', length = nrow(traces))
  ## variable (list) for storing the sessions with length below the threshold 
  short.sessions <- list()
  k <- 1
  ## compute action and session length and detect overly short sessions
  s_len <- 0 # length of the current session
  action_cnt <- 0 # number of actions in the current session
  for(i in 1:(nrow(traces)-1)) {
    if ( !is.na(traces$time_gap[i+1]) ) {
      traces$action_len[i] <- traces$time_gap[i+1]
      s_len <- s_len + traces$action_len[i]
      action_cnt <- action_cnt + 1 
    } else {
      if (s_len > 0) { ## s_len == 0 if there were two (or more) successive actions 
                       ## with the time gap above the threshold, so each action ended up in a separate
                       ## session with time_gap value equal to NA 
      
        ## approximate the length of the last action in the session 
        ## as the mean length of the other actions in the same session
        traces$action_len[i] <- s_len/action_cnt
        ## add the appriximated length of the last action in the session
        s_len <- s_len + traces$action_len[i]
      } 
      ## if the session length is less than the threshold, mark it for removal
      if (s_len <= threshold) {
        short.sessions[[k]] <- traces$session[i]
        print(paste(k, ". overly short session, lasted:", s_len, "minutes"))
        k <- k + 1
      }
      ## reset session length and action count, as the next iteration will move to the next session
      s_len <- 0
      action_cnt <- 0
    }
  }
  
  return (list(trace_data=traces, short_sessions=short.sessions))
}


## the f. creates a data frame focused on study sessions; it should include the following variables:
## - session_id
## - user_id
## - start_time - the timestamp of the first event in the session
## - week
## - session_len - length of the session
create.sessions.df <- function(events_data) {
  sessions <- unique(events_data$session)
  sessions.list <- list()
  s <- 1
  for(i in 1:length(sessions)) {
    print(paste0("session: ", sessions[i]))
    session.data <- subset(events_data, session==sessions[i])
    user.id <- unique(session.data$user_id)[1]
    start.time <- session.data$timestamp[1]
    week <- names(sort(table(session.data$week), decreasing = TRUE)[1])
    session.len <- sum(session.data$action_len, na.rm = T)
    sessions.list[[s]] <- list(session_id=sessions[i], user_id=user.id, start_time=start.time, 
                               week=week, session_len=session.len) #topic=topic
    s <- s + 1
  }
  sessions.df <- do.call(rbind.data.frame, sessions.list)
  colnames(sessions.df) <- c('session_id', 'user_id', 'start_time', 'week', 'session_len')
  sessions.df
}


## the f. identifies the primary topic for each study session;
## since within a session different learning actions can be related to one or more topics
## this function should identify the most dominant topic for each session; 
## it returns a data frame with session id and the primary/dominant topic
dominant.session.topic <- function(events_data) {
  
  sessions <- unique(events_data$session)
  m.sessions <- matrix(nrow = length(sessions), ncol = 2)
  s <- 1
  for(i in 1:length(sessions)) {
    session.data <- subset(events_data, session==sessions[i])
    topics <- unique(session.data$topic)
    if ( length(topics)==1 & is.na(topics[1]) ) {
      m.sessions[s,] <- c(as.character(sessions[i]), NA)
      print(paste0("session: ", sessions[i], "; NO topic"))
    } else {
      topic <- names(sort(table(session.data$topic), decreasing = TRUE)[1])
      m.sessions[s,] <- c(as.character(sessions[i]), topic)
    }
    s <- s + 1
  }
  
  sessions.df <- data.frame(m.sessions, stringsAsFactors = FALSE)
  colnames(sessions.df) <- c('session_id', 'main_topic')
  sessions.df

}


## the f. computes an indicator of regularity of study as the regularity of intervals between 
## consecutive study sessions; in fact, for each student, it computes median and MAD (median
## absolute deviation - an alternative to SD for not normally distributed data) of
## the time period between two consecutive logins (sessions); 
## the time is expressed in minutes
regularity.session.intervals <- function(sessions) {
  students <- unique(sessions$user_id)
  stud.session.data <- matrix(nrow = length(students), ncol = 3)
  ss <- 1
  shapiro.pvals <- vector(mode = 'numeric', length = length(students))
  for(i in 1:length(students)) {
    stud.sessions <- subset(sessions, user_id==students[i])
    ## the case when a student had only one 'meaningful' session (ie. session longer than 30sec)
    if (nrow(stud.sessions)==1) {
      stud.session.data[ss,] <- c(as.numeric(students[i]), NA, NA)
      ss <- ss + 1
      shapiro.pvals[i] <- NA
      next
    }
    ## regular case: if the number of sessions is >1
    session_gaps <- vector(mode = 'numeric', length = nrow(stud.sessions))
    sg <- 1
    for(j in 1:(nrow(stud.sessions)-1)) {
      time_gap <- difftime(time1 = stud.sessions$start_time[j+1], 
                           time2 = stud.sessions$start_time[j], 
                           units = "mins" )
     # print(paste0("student id: ", students[i], " time gap: ", time_gap))
      session_gaps[sg] <- time_gap
      sg <- sg + 1
    }
    ## check if session gaps are normally distributed
    if (length(session_gaps) >= 3) # sample size must be between 3 and 5000
      shapiro.pvals[i] <- shapiro.test(session_gaps)$p.value
    else shapiro.pvals[i] <- NA
    print(paste("shapiro p val:", shapiro.pvals[i]))
    stud.session.data[ss,] <- c(as.numeric(students[i]), 
                                median(session_gaps), 
                                mad(session_gaps))
    ss <- ss + 1
  }
  
  norm_dist_prop <- length(which(shapiro.pvals>0.05))/length(students)
  
  student.session.df <- data.frame(stud.session.data)
  colnames(student.session.df) <- c('user_id', 'median_s_gap', 'mad_s_gap')
  student.session.df
}



## the f. computes the following indicators, per student:
## - total number of study sessions
## - number of sessions per week, and based on that:
##  -- number of inactive weeks
##  -- SD and MAD of weekly sessions proportions
##  -- entropy of weekly session 
compute.weekly.counts <- function(sessions) {
  students <- unique(sessions$user_id)
  s.matrix <- matrix(nrow = length(students), ncol=16)
  s <- 1
  
  shapiro.pvals <- vector(mode = 'numeric', length = length(students))
  
  for(i in 1:length(students)) {
    s.sessions <- subset(sessions, user_id==students[i])
    s.tot <- nrow(s.sessions)
    weekly.sessions <- vector(mode = 'numeric', length = 10) ## weeks 6 and 13 are excluded
    weekly.proportions <- vector(mode = 'numeric', length = 10)
    wn <- 1
    for(w in c(2:5,7:12)) {
      weekly.sessions[wn] <- length(which(s.sessions$week==w))
      weekly.proportions[wn] <- weekly.sessions[wn]/s.tot
      wn <- wn + 1
    }
    
    shapiro.pvals[s] <- shapiro.test(weekly.proportions)$p.value
    
    weeks.inactive <- length(which(weekly.sessions==0))
    weekly.sd <- sd(weekly.proportions)
    weekly.mad <- mad(weekly.proportions)
    entropy <- -sum(sapply(weekly.proportions, function(p) {p * log1p(p)}))
    s.matrix[s,] <- c(students[i], s.tot, weekly.sessions, weeks.inactive, 
                      weekly.sd, weekly.mad, entropy)
    s <- s + 1
  }
  
  above_0.05 <- length(which(shapiro.pvals > 0.05))
  print(paste("Proportion of students with normal dist for weekly proportions:", above_0.05/length(students)))
  
  counts.df <- data.frame(s.matrix)
  colnames(counts.df) <- c('user_id', 's_total', paste0('count_w',c(2:5,7:12)), 
                           'inactive_weeks', 'weekly_prop_sd', 'weekly_prop_mad', 'weekly_entropy')
  counts.df
}



## the f. computes, for each student, the number of sessions per 
## each day of a week (Mon - Sun), and based on that indicators 
## of (ir)regularity:
## SD, MAD and entropy of session proportions per week day
make.weekdays.counts <- function(sessions) {
  require(lubridate)
  
  ## first, change the time to Sydney time zone
  sessions$start_time <- as.POSIXct(sessions$start_time, tz = 'Australia/Sydney')
  
  students <- unique(sessions$user_id)
  s.matrix <- matrix(nrow = length(students), ncol=11)
  s <- 1
  
  ## vector for keeping p value of the shapiro test of normality
  ## this is to examine if the weekly counts follow normal distribution
  shapiro.pvals <- vector(mode = 'numeric', length = length(students))
  
  for(i in 1:length(students)) {
    s.sessions <- subset(sessions, user_id==students[i])
    weekday.sessions <- vector(mode = 'numeric', length = 7)
    weekday.proportions <- vector(mode = 'numeric', length = 7)
    s.tot <- nrow(s.sessions)
    for(d in 1:7) {
      weekday.sessions[d] <- length(which(wday(s.sessions$start_time, label=F)==d))
      weekday.proportions[d] <- weekday.sessions[d]/s.tot
    }
    entropy <- -sum(sapply(weekday.proportions, function(p) {p * log1p(p)}))
    s.matrix[s,] <- c(students[i], weekday.sessions, sd(weekday.proportions), 
                      mad(weekday.proportions), entropy)
    s <- s + 1
    
    shapiro.pvals[i] <- shapiro.test(weekday.proportions)$p.value
  }
  
  above_0.05 <- length(which(shapiro.pvals > 0.05))
  print(paste("Proportion of students with normal dist for weekday props:", above_0.05/length(students)))
  
  counts.df <- data.frame(s.matrix)
  weekdays <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat') 
  colnames(counts.df) <- c('user_id', paste0(weekdays,'_count'), 
                           'weekday_prop_sd', 'weekday_prop_mad', 'weekday_entropy')
  counts.df
}


## for each student, the f. computes the proportion of learning events being 
## of 'revisiting', 'catching-up', 'preparing', or 'ahead' study modes
compute.smode.proportions <- function(study_mode_data) {
  students <- unique(study_mode_data$USER_ID)
  s.matrix <- matrix(nrow = length(students), ncol=5)
  s <- 1
  for(i in 1:length(students)) {
    stud.data <- subset(study_mode_data, USER_ID==students[i])
    stud.tot <- nrow(stud.data)
    preparing <- length(which(stud.data$STUDY_MODE=="preparing"))/stud.tot
    revisits <- length(which(stud.data$STUDY_MODE=="revisiting"))/stud.tot
    catchups <- length(which(stud.data$STUDY_MODE=="catching-up"))/stud.tot
    aheads <- length(which(stud.data$STUDY_MODE=="ahead"))/stud.tot
    s.matrix[s,] <- c(students[i], preparing, revisits, catchups, aheads) 
    s <- s + 1  
  }
  study.mode.df <- data.frame(s.matrix)
  colnames(study.mode.df) <- c("user_id", "preparing_prop", "revisits_prop", 
                               "catch_ups_prop", "ahead_prop")
  study.mode.df
}


## for each student and each course week (except weeks 6 and 13), 
## the f. computes the proportion of each study mode; 
## then, for each study mode, it computes SD and MAD of the weekly proportions
weekly.studymode.regularity <- function(study_mode_data) {
  smodes <- c('preparing','revisiting','catching-up','ahead')
  students <- unique(study_mode_data$USER_ID)
  s.matrix <- matrix(nrow = length(students), ncol=9)
  s <- 1
  for(user in students) {
    stud.data <- subset(study_mode_data, USER_ID==user)
    ## make weekly counts
    weekly.prepare.counts <- vector(mode = 'numeric', length = 10) ## 10 (not 12) since weeks 6 and 13 are excluded
    weekly.revisit.counts <- vector(mode = 'numeric', length = 10)
    weekly.catchup.counts <- vector(mode = 'numeric', length = 10)
    weekly.ahead.counts <- vector(mode = 'numeric', length = 10)
    weekly.tot.counts <- vector(mode = 'numeric', length = 10)
    c <- 1
    for(w in c(2:5,7:12)) {
      week.data <- subset(stud.data, WEEK == w)
      weekly.prepare.counts[c] <- length(which(week.data$STUDY_MODE==smodes[1]))
      weekly.revisit.counts[c] <- length(which(week.data$STUDY_MODE==smodes[2]))
      weekly.catchup.counts[c] <- length(which(week.data$STUDY_MODE==smodes[3]))
      weekly.ahead.counts[c] <- length(which(week.data$STUDY_MODE==smodes[4]))
      weekly.tot.counts[c] <- nrow(week.data)
      c <- c + 1
    }
    ## compute weekly proportions
    weekly.prepare.prop <- weekly.prepare.counts / weekly.tot.counts
    weekly.revisits.prop <- weekly.revisit.counts / weekly.tot.counts
    weekly.catchup.prop <- weekly.catchup.counts / weekly.tot.counts
    weekly.ahead.prop <- weekly.ahead.counts / weekly.tot.counts
    
    s.matrix[s,] <- c(user, sd(weekly.prepare.prop, na.rm = T), 
                      sd(weekly.revisits.prop, na.rm = T),
                      sd(weekly.catchup.prop, na.rm = T), 
                      sd(weekly.ahead.prop, na.rm = T),
                      mad(weekly.prepare.prop, na.rm = T),
                      mad(weekly.revisits.prop, na.rm = T),
                      mad(weekly.catchup.prop, na.rm = T),
                      mad(weekly.ahead.prop, na.rm = T))
    
    s <- s + 1
  }
  
  results.df <- data.frame(s.matrix)
  colnames(results.df) <- c('user_id', paste0('weekly_',smodes,'_prop_sd'), 
                            paste0('weekly_', smodes,'_prop_mad')) 
  results.df
}



## for each student, the f. computes median and MAD of the number of different topics 
## considered during study sessions 
topic.diversity <- function(events_data) {
  
  students <- unique(events_data$user_id)
  stud.matrix <- matrix(nrow = length(students), ncol = 3)
  s <- 1
  
  ## for examining normality of dist. of topic counts
  shapiro.pvals <- vector(mode = 'numeric', length = length(students))
  
  for (stud in students) {
    stud.data <- subset(events_data, user_id==stud)
    stud.sessions <- unique(stud.data$session)
    ses.topic.cnt <- vector(mode = 'numeric', length = length(stud.sessions))
    c <- 1
    for(ses in stud.sessions) {
      session.data <- subset(stud.data, session==ses)
      topics <- unique(session.data$topic)
      if ( length(topics)==1 & is.na(topics[1]) ) {
        ses.topic.cnt[c] <- 0
        print(paste0("session: ", ses, "; NO topic"))
      } else {
        ses.topic.cnt[c] <- length(which(topics != 'ORIENTIRING')) 
      }
      c <- c + 1
    }
    if (length(ses.topic.cnt) >= 3) # sample size must be between 3 and 5000
      shapiro.pvals[s] <- shapiro.test(ses.topic.cnt)$p.value 
    else shapiro.pvals[s] <- NA
    
    stud.matrix[s,] <- c(stud, median(ses.topic.cnt, na.rm = T), mad(ses.topic.cnt, na.rm = T))
    s <- s + 1
  }
  
  above_0.05 <- length(which(shapiro.pvals > 0.05))
  print(paste("Proportion of students with normal dist for topic ses. counts", 
              above_0.05/length(students)))
  
  result.df <- data.frame(stud.matrix)
  colnames(result.df) <- c('user_id', 'topic_cnt_median', 'topic_cnt_mad')
  result.df
  
}



## for each student, the f. computes median and MAD of the number of different kinds of learning
## resources used during a session 
resource.diversity <- function(events_data) {
  
  students <- unique(events_data$user_id)
  stud.matrix <- matrix(nrow = length(students), ncol = 3)
  s <- 1
  for (stud in students) {
    stud.data <- subset(events_data, user_id==stud)
    stud.sessions <- unique(stud.data$session)
    ses.res.cnt <- vector(mode = 'numeric', length = length(stud.sessions))
    c <- 1
    for(ses in stud.sessions) {
      session.data <- subset(stud.data, session==ses)
      ses.res.cnt[c] <- length(unique(session.data$res_type)) 
      c <- c + 1
    }
    stud.matrix[s,] <- c(stud, median(ses.res.cnt, na.rm = T), mad(ses.res.cnt, na.rm = T))
    s <- s + 1
  }
  
  result.df <- data.frame(stud.matrix)
  colnames(result.df) <- c('user_id', 'res_type_median', 'res_type_mad')
  result.df
  
}


# the f. compute proportions of 
# - on-topic sessions, that is, sessions with the main_topic being the topic
#    of the week's lecture
# - 'last minute' on-topic sessions, ie, on-topic sessions done in 24h before a 
#   week's lecture; lectures took place on Friday at noon, so, last minute are 
#   on-topic sessions that happened between Thur noon and Fri noon
# Also, the f. computes SD of on-topic and last minute on-topic proportions at the weekly level
ontopic.and.last.minute.stats <- function(sessions) {
  require(lubridate)
  
  ## first, change the time to Sydney time zone
  sessions$start_time <- as.POSIXct(sessions$start_time, tz = 'Australia/Sydney')
  
  students <- unique(sessions$user_id)
  stud.matrix <- matrix(nrow = length(students), ncol=7)
  s <- 1
  for(stud in students) {
    stud.ses <- sessions %>% filter(user_id==stud) 
    
    ontopic.weekly.cnt <- vector(mode = 'numeric', length = 10) # weeks 6 and 13 are excluded
    ontopic.weekly.prop <- vector(mode = 'numeric', length = 10)
    last.min.weekly.cnt <- vector(mode = 'numeric', length = 10) 
    last.min.weekly.prop <- vector(mode = 'numeric', length = 10)
    
    wn <- 1 
    for(w in c(2:5,7:12)) {
      w.ses <- stud.ses %>% filter(week==w)
      w.ses.on.topic <- w.ses %>% filter(main_topic %in% get.week.topics(w))
      last.min.on.topic <- w.ses.on.topic %>% filter((wday(start_time)==5 & hour(start_time)>12) | # Sunday is 1
                                                      (wday(start_time)==6 & hour(start_time)<12))
      ontopic.weekly.cnt[wn] <- nrow(w.ses.on.topic)
      ontopic.weekly.prop[wn] <- ontopic.weekly.cnt[wn]/nrow(w.ses)
      last.min.weekly.cnt[wn] <- nrow(last.min.on.topic)
      last.min.weekly.prop[wn] <- last.min.weekly.cnt[wn]/ontopic.weekly.cnt[wn]
      wn <- wn + 1  
    }
    
    ontopic.prop <- sum(ontopic.weekly.cnt)/nrow(stud.ses) # overall proportion of ontopic sessions (for all weeks)
    last.min.prop <- sum(last.min.weekly.cnt)/nrow(stud.ses) #overall proportion of last minute on topic sessions
    ontopic.mad <- mad(ontopic.weekly.prop, na.rm = T) # MAD of ontopic weekly prop
    last.min.mad <- mad(last.min.weekly.prop, na.rm = T) # MAD of last minute ontopic weekly prop
    ontopic.sd <- sd(ontopic.weekly.prop, na.rm = T)
    last.min.sd <- sd(last.min.weekly.prop, na.rm = T)
    stud.matrix[s,] <- c(stud, ontopic.prop, last.min.prop, 
                         ontopic.mad, last.min.mad, 
                         ontopic.sd, last.min.sd)
    s <- s + 1
  }
  result.df <- data.frame(stud.matrix)
  colnames(result.df) <- c('user_id', 'on_topic_prop', 'last_min_prop', 
                           'on_topic_prop_mad', 'last_min_prop_mad', 
                           'on_topic_prop_sd', 'last_min_prop_sd')
  result.df
}


## the f. returns the course topic(s) for the given week
get.week.topics <- function(week) {
  if (week==2) return(c('W02','COD'))
  if (week==3) return(c('W03','DRM'))
  if (week==4) return(c('W04','CDL'))
  if (week==5) return(c('W05','SDL'))
  if (week==7) return(c('W07','ARC'))
  if (week==8) return(c('W08','ISA'))
  if (week==9) return(c('W09','ASP'))
  if (week==10) return(c('W10','ADM'))
  if (week==11) return(c('W11', 'HLP'))
  if (week==12) return(c('W12', 'HLP'))
  return(NA)
}


## for each student, the f. computes the proportion of preparation events 
## (actions/events having 'preparing' study mode) that took place at most 
## 24h before the lecture, that is, between Thu noon and Friday noon
## it also computes SD and MAD of the weekly proportions of last minute preparation events
# last.minute.proportion <- function(events) {
#   require(lubridate)
#   
#   ## first, change the time to Sydney time zone
#   events$TIMESTAMP <- as.POSIXct(events$TIMESTAMP, tz = 'Australia/Sydney')
#   
#   students <- unique(events$USER_ID)
#   stud.matrix <- matrix(nrow = length(students), ncol=5)
#   s <- 1
#   for(stud in students) {
#     stud.events <- events %>% filter((USER_ID==stud) & (STUDY_MODE=='preparing')) 
#     last.min.weekly.cnt <- vector(mode = 'numeric', length = 10) # weeks 6 and 13 are excluded
#     last.min.weekly.prop <- vector(mode = 'numeric', length = 10)
#     wn <- 1 
#     for(w in c(2:5,7:12)) {
#       w.prep.events <- stud.events %>% filter(WEEK==w)
#       last.min.prep <- w.prep.events %>% filter((wday(TIMESTAMP)==4 & hour(TIMESTAMP)>12) | 
#                                                 (wday(TIMESTAMP)==5 & hour(TIMESTAMP)<12))
#       if (nrow(last.min.prep)>0)
#         print(paste(last.min.prep$TIMESTAMP, collapse = ' '))
#       last.min.weekly.cnt[wn] <- nrow(last.min.prep)
#       last.min.weekly.prop[wn] <- last.min.weekly.cnt[wn]/nrow(w.prep.events)
#       wn <- wn + 1  
#     }
#     last.min.prop <- sum(last.min.weekly.cnt)/nrow(stud.events)
#     last.min.sd <- sd(last.min.weekly.prop, na.rm = T)
#     last.min.mad <- mad(last.min.weekly.prop, na.rm = T)
#     entropy <- -sum(sapply(last.min.weekly.prop, function(p) {p * log1p(p)}))
#     stud.matrix[s,] <- c(stud, last.min.prop, last.min.sd, last.min.mad, entropy)
#     s <- s + 1
#   }
#   result.df <- data.frame(stud.matrix)
#   colnames(result.df) <- c('user_id', 'last_min_prop', 'last_min_prop_sd', 
#                            'last_min_prop_mad', 'last_min_entropy')
#   result.df
# }


## the f. computes, for each student, the number of days the student
## was active in each week of the course; it also computes the number
## of active days per week, as well as SD and entropy of weekly counts
## the second argument - weeks - is a vector of weeks to be considered
## in computations
make.daily.counts <- function(sessions, weeks) {
  require(lubridate)
  
  ## first, change the time to Sydney time zone
  sessions$start_time <- as.POSIXct(sessions$start_time, tz = 'Australia/Sydney')
  
  students <- unique(sessions$user_id)
  
  n.weeks <- length(weeks)
  ## number of columns: 1 + n.weeks * 7 + n.weeks
  ## user_id
  ## for each week (n.weeks), for each day of the week, an indicator if a student was active (7*n.weeks) 
  ## plus, for each week, the number of days the student was active in that week 
  s.matrix <- matrix(nrow = length(students), ncol=(1+7*n.weeks+n.weeks)) 
  s <- 1
  
  ## vector for keeping p value of the shapiro test of normality
  ## this is to examine if the weekly counts follow normal distribution
  shapiro.pvals <- vector(mode = 'numeric', length = length(students))
  
  for(stud in students) {
    stud.sessions <- subset(sessions, user_id==stud)
    
    ## vector for keeping the computed counts data about the given student
    stud.all.counts <- c(stud) 
    ## vector for keeping weekly summaries = number of engaged days for each week
    stud.weekly.counts <- vector(mode = 'numeric', length = n.weeks)
    c <- 1
    
    ## consider each week individualy
    for(w in weeks) {
      stud.weekly.ses <- subset(stud.sessions, week==w)
      weekday.sessions <- vector(mode = 'numeric', length = 7)
      for(d in 1:7) 
        weekday.sessions[d] <- length(which(wday(stud.weekly.ses$start_time, label=F)==d))
      stud.all.counts <- c(stud.all.counts, weekday.sessions)  
      
      ## it is only important to have at least one session in a day; 
      ## it is irrelevant how many session there were in a day
      weekday.binary <- ifelse(test = weekday.sessions > 0, yes = 1, no = 0)
      stud.weekly.counts[c] <- sum(weekday.binary)
      c <- c + 1
    }
    stud.all.counts <- c(stud.all.counts, stud.weekly.counts)
    s.matrix[s,] <- stud.all.counts
    s <- s + 1
    
    shapiro.pvals[s] <- shapiro.test(stud.weekly.counts)$p.value
  }
  
  above_0.05 <- length(which(shapiro.pvals > 0.05))
  print(paste("Proportion of students with normal dist for weekly counts of engaged days:", above_0.05/length(students)))
  
  counts.df <- data.frame(s.matrix)
  ## create column names
  weekdays <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat') 
  cnames <- 'user_id'
  for(w in weeks)
    cnames <- c(cnames, paste0('W',w,'_', weekdays))
  cnames <- c(cnames, paste0('W',weeks,'_cnt'))
  colnames(counts.df) <- cnames
  
  counts.df
}


## the f. computes the number of days (as the time gap) between 
## two consecutive 'active' days (ie days when a student had at least
## one study session)
## the result is a data frame with mean, median, and SD of
## the time gap (expressed in days)
gaps.between.consecutive.active.days <- function(daily_counts) {
  
  students <- unique(daily_counts$user_id)
  result.matrix <- matrix(nrow = length(students), ncol = 4)
  c <- 1 
  
  ## vector for keeping p value of the shapiro test of normality
  ## this is to examine if the weekly counts follow normal distribution
  shapiro.pvals <- vector(mode = 'numeric', length = length(students))
  
  for(stud in students) {
    stud.data <- daily_counts %>% filter(user_id==stud)
    # vector of time gaps
    gaps <- 0
    # number of consecutive inactive days
    inactive_cnt <- 0
    for(j in 2:ncol(stud.data)) {
      if (stud.data[j] > 0) {
        gaps <- c(gaps, inactive_cnt)
        inactive_cnt <- 0
      } else {
        inactive_cnt <- inactive_cnt + 1
      }
    }
    # remove the initial 0 value
    gaps <- gaps[-1]
    
    #check if the gaps are normally distributed
    if ( length(gaps) > 3 ) # sample size must be between 3 and 5000
      shapiro.pvals[c] <- shapiro.test(gaps)$p.value
    
    # compute mean, median, sd, and add to the matrix
    result.matrix[c,] <- c(stud, mean(gaps), median(gaps), sd(gaps))
    c <- c + 1
  }
  
  above_0.05 <- length(which(shapiro.pvals > 0.05))
  print(paste("Proportion of students with normal dist for time gaps:", above_0.05/length(students)))
  
  result.df <- data.frame(result.matrix)
  colnames(result.df) <- c('user_id', 'mean_gap', 'median_gap', 'sd_gap')
  result.df
}


## the f. computes, for each student and each day of the course the number of 
## different kinds of resources the student used that day 
## the second argument - weeks - is a vector of weeks to be considered
## in computations
daily.counts.of.resource.use <- function(events, weeks) {
  require(lubridate)
  
  ## first, change the time to Sydney time zone
  events$timestamp <- as.POSIXct(events$timestamp, tz = 'Australia/Sydney')
  
  students <- unique(events$user_id)
  
  n.weeks <- length(weeks)
  ## number of columns: 1 + n.weeks * 7 (days) * 5 (resource types)
  ## user_id
  ## for each week (n.weeks), for each day of the week, we need 5 variables (counts),
  ## one for each type of resource (EXE, MCQ, METACOG, RES, VIDEO)
  s.matrix <- matrix(nrow = length(students), ncol=(1 + n.weeks*7*5)) 
  s <- 1
  
  for(stud in students) {
    stud.events <- subset(events, user_id==stud)
    
    ## vector for keeping the computed counts data about the given student
    stud.all.counts <- c(stud) 
    
    ## consider each week individualy
    for(w in weeks) {
      stud.weekly.events <- subset(stud.events, week==w)
      # vector keeping counts for each day of a week
      daily.res.counts <- vector(mode = 'numeric', length = 7*5) # 7 days * 5 resource types
      c <- 1 # counter for the daily.res.counts vector
      
      for(d in 1:7) {
        daily.events <- stud.weekly.events %>% filter( wday(timestamp, label=F)==d )
        if (nrow(daily.events) == 0) {
          for(j in 0:4)
            daily.res.counts[c+j] <- 0 # set zeros for all (5) types of resources 
        } else {
          daily.res.counts[c] <- nrow(daily.events %>% filter( res_type == "MCQ"))
          daily.res.counts[c+1] <- nrow(daily.events %>% filter( res_type == "EXE"))
          daily.res.counts[c+2] <- nrow(daily.events %>% filter( res_type == "VIDEO"))
          daily.res.counts[c+3] <- nrow(daily.events %>% filter( res_type == "RES"))
          daily.res.counts[c+4] <- nrow(daily.events %>% filter( res_type == "METACOG"))
        }
        c <- c + 5  
      }
        
      stud.all.counts <- c(stud.all.counts, daily.res.counts)  
      
    }
    
    s.matrix[s,] <- stud.all.counts
    s <- s + 1
    
  }
  
  counts.df <- data.frame(s.matrix)
  ## create column names
  res.types <- c('MCQ', 'EXE', 'VIDEO', 'RES', 'METACOG')
  weekday.res <- list()
  weekday.res[[1]] <- paste0('Sun','_', res.types)
  weekday.res[[2]] <- paste0('Mon','_', res.types)
  weekday.res[[3]] <- paste0('Tue','_', res.types)
  weekday.res[[4]] <- paste0('Wed','_', res.types)
  weekday.res[[5]] <- paste0('Thu','_', res.types)
  weekday.res[[6]] <- paste0('Fri','_', res.types)
  weekday.res[[7]] <- paste0('Sat','_', res.types)
  cnames <- 'user_id'
  for(w in weeks) {
    for(d in 1:7)
      cnames <- c(cnames, paste0('W',w,'_', weekday.res[[d]]))
  }
  colnames(counts.df) <- cnames
  
  counts.df
}


# the f. computes the following statistics for the given counts:
# - total count
# - median 
# - MAD 
# - number of days with positive counts
# - proportion of days with positive counts
# When computing these statistics, only the days when a student was active are considered,
# hence the 2nd argument of the function
compute.count.stats <- function(counts, active.days) {
  counts.matrix <- matrix(nrow = nrow(counts), ncol = 6)
  shapiro.p.vals <- vector(mode = 'numeric', length = nrow(counts))
  
  for(s in 1:nrow(counts)) {
    stud.data <- counts$user_id[s] 
    for(d in 2:ncol(counts)) {
      if (active.days[s,d] > 0) stud.data <- c(stud.data, counts[s,d])
    }
    counts.matrix[s,1] <- stud.data[1]
    stud.data <- stud.data[-1]
    counts.matrix[s,2] <- sum(stud.data)
    counts.matrix[s,3] <- median(stud.data) 
    counts.matrix[s,4] <- mad(stud.data) 
    counts.matrix[s,5] <- length(which(stud.data>0)) 
    counts.matrix[s,6] <- counts.matrix[s,5]/length(stud.data) 
    # was used and the number of active days
    if ( (length(unique(stud.data)) > 1) & (length(stud.data) >= 3) )
      shapiro.p.vals[s] <- shapiro.test(stud.data)$p.value
    else shapiro.p.vals[s] <- NA
  }
  
  above_0.05 <- length(which(shapiro.p.vals>0.05))
  print(paste("ratio of student with normally distrubuted counts:", above_0.05/nrow(counts)))
  
  stats <- data.frame(counts.matrix)
  stats
}



## the f. computes, for each student and each day of the course 
## the number of events / actions that were:
## - 'ontopic' - the topic associated with the action is the topic of the current week
## - 'revisiting' - the topic associated with the action is the topic of one of the previous weeks
## - 'metacognitive' - the topic associated with the action is one of the following:
##    'ORG', 'DBOARD', 'STRAT', 'STUDYKIT'
## - 'orientiring' - the topic associated with the action is one of the following:
##    'HOME', 'HOF', 'SEARCH', 'TEA', 'EXAM', 'W01'-'W13'
## - 'project' - the action is related to project work
##
## the second argument - weeks - is a vector of weeks to be considered in computations
daily.topic.counts <- function(events, weeks) {
  require(lubridate)
  
  ## first, change the time to Sydney time zone
  events$timestamp <- as.POSIXct(events$timestamp, tz = 'Australia/Sydney')

  students <- unique(events$user_id)
  
  n.weeks <- length(weeks)
  ## number of columns: 1 + n.weeks * 7 (days) * 5 (topic foci)
  ## user_id
  ## for each week (n.weeks), for each day of the week, we need 5 variables (proportions),
  ## one for each topic foci (ontopic, revisiting, metacog, orientiring)
  s.matrix <- matrix(nrow = length(students), ncol=(1 + n.weeks*7*5)) 
  s <- 1
  
  for(stud in students) {
    stud.events <- subset(events, user_id==stud)
    
    ## vector for keeping the computed counts data about the given student
    stud.all.counts <- c(stud) 
    
    ## consider each week individualy
    for(w in weeks) {
      stud.weekly.events <- subset(stud.events, week==w)
      # vector keeping counts for each day of a week
      daily.counts <- vector(mode = 'numeric', length = 7*5) # 7 days * 5 topic foci
      c <- 1 # counter for the daily.prop vector
      
      week.topic <- get.week.study.topic(w) # topic of the current week
      prev.topics <- get.prev.weeks.topics(w) # topic of the previous weeks
      
      for(d in 1:7) {
        daily.events <- stud.weekly.events %>% filter( wday(timestamp, label=F)==d )
        daily.tot <- nrow(daily.events)
        if (daily.tot == 0) {
          for(j in 0:3)
            daily.counts[c+j] <- 0 # set zeros for all (4) types of topic foci 
        } else {
          
          daily.counts[c] <- nrow(daily.events %>% filter(topic == week.topic)) # ontopic counts
          daily.counts[c+1] <- nrow(daily.events %>% filter(topic %in% prev.topics)) # revisiting counts
          daily.counts[c+2] <- nrow(daily.events %>% filter(topic == "METACOG")) # metacognitive counts
          daily.counts[c+3] <- nrow(daily.events %>% filter(topic == "ORIENTIRING")) # orientiring counts
          daily.counts[c+4] <- nrow(daily.events %>% filter(topic == "PRJ")) # project work counts
        
        }
        c <- c + 5  
      }
      
      stud.all.counts <- c(stud.all.counts, daily.counts)  
      
    }
    
    s.matrix[s,] <- stud.all.counts
    s <- s + 1
    
  }
  
  counts.df <- data.frame(s.matrix)
  ## create column names
  topic.foci <- c('ontopic', 'revisit', 'metacog', 'orient', 'prj')
  wd.topic <- list()
  wd.topic[[1]] <- paste0('Sun','_', topic.foci)
  wd.topic[[2]] <- paste0('Mon','_', topic.foci)
  wd.topic[[3]] <- paste0('Tue','_', topic.foci)
  wd.topic[[4]] <- paste0('Wed','_', topic.foci)
  wd.topic[[5]] <- paste0('Thu','_', topic.foci)
  wd.topic[[6]] <- paste0('Fri','_', topic.foci)
  wd.topic[[7]] <- paste0('Sat','_', topic.foci)
  cnames <- 'user_id'
  for(w in weeks) {
    for(d in 1:7)
      cnames <- c(cnames, paste0('W',w,'_', wd.topic[[d]]))
  }
  colnames(counts.df) <- cnames
  
  counts.df
}


get.week.study.topic <- function(week) {
  switch(week, NA, 'COD', 'DRM', 'CDL', 'SDL', NA, 'ARC', 'ISA', 'ASP', 'ADM', 'HLP', 'HLP')
}

get.prev.weeks.topics <- function(week) {
  topics <- c('CST', 'COD', 'DRM', 'CDL', 'SDL', 'ARC', 'ISA', 'ASP', 'ADM', 'HLP') 
  t <- switch(week, NA, topics[1], topics[1:2], topics[1:3], topics[1:4], topics[1:5],
              topics[1:5], topics[1:6], topics[1:7], topics[1:8], topics[1:9], topics[1:9])
  t
}


## f. for Winsorizing a variable  
## taken from: https://www.r-bloggers.com/winsorization/
winsor1 <- function (x, fraction=.05) {
  lim <- quantile(x, probs=c(fraction, 1-fraction), na.rm = T)
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  ## check now for outliers
  boxplot(x)
  x
}


## another f. for Winsorizing a variable  
## taken from: https://www.r-bloggers.com/winsorization/
winsor2 <- function (x, multiple=3) {
  med <- median(x, na.rm = T)
  y <- x - med
  sc <- mad(y, center=0, na.rm = T) * multiple
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  z <- y + med
  boxplot(z)
  z
}


normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}


## the f. scales the given feature set by standardizing them
## as features are assumed to have outliers, instead of using mean and SD, 
## median and Interquartile Range (IQR) are used, as suggested here:
## http://scikit-learn.org/stable/modules/preprocessing.html#scaling-data-with-outliers
scale.features <- function(features) {
  scaled.data <- data.frame(apply(features, 2, 
                                  function(x) {(x-median(x, na.rm = T))/IQR(x, na.rm = T)} ))
  scaled.data
}


do.hclustering <- function(features, hc.method) {
  # compute the distance between the observations
  distance <- dist(features)
  # use the computed distances to do the clustering
  hc <- hclust(distance, method = hc.method)
  # plot the clustering tree
  plot(hc, cex=0.69, main = "Student clustering")
  
  hc
}


do.kmedoids.clustering <- function(features, k.min, k.max) {
  
  require(fpc)
  require(cluster)
  
  # partitioning around medoids with estimation of the optimal number of clusters
  pam.res <- pamk(features, krange = c(k.min:k.max), metric="euclidean", diss = F)
  # number of clusters identified as the best
  best.k <- pam.res$nc
  print(best.k)
  
  # useful results are, in fact, stored in the pamobject element
  pam.res <- pam.res$pamobject
  
  # print the distribution of instances across the clusters
  print(table( pam.res$clustering ))
  
  # inspect the silhouette plot, to more objectively assess the quality of the clusters
  si <- silhouette(pam.res)
  colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
  pdf(file = "charts/k_medoids_regularity_of_study.pdf")
  plot(si, col=colors[1:best.k])
  dev.off()
  print(summary(si))
  
  pam.res
}


## function for visual comparison - using boxplots - of clusters w.r.t. each of the
## clustering features
plot.indicators <- function(indicators) {
  require(ggplot2)
  p1 <- ggplot(indicators, aes(x=group, y=ses_tot, fill=group)) + geom_boxplot()
  p2 <- ggplot(indicators, aes(x=group, y=on_topic_prop, fill=group)) + geom_boxplot()
  p3 <- ggplot(indicators, aes(x=group, y=last_min_prop, fill=group)) + geom_boxplot()
  p4 <- ggplot(indicators, aes(x=group, y=week_prop_sd, fill=group)) + geom_boxplot()
  p5 <- ggplot(indicators, aes(x=group, y=weekday_prop_sd, fill=group)) + geom_boxplot()
  p6 <- ggplot(indicators, aes(x=group, y=on_topic_prop_sd, fill=group)) + geom_boxplot()
  p7 <- ggplot(indicators, aes(x=group, y=last_min_prop_sd, fill=group)) + geom_boxplot()
  p8 <- ggplot(indicators, aes(x=group, y=res_type_mad, fill=group)) + geom_boxplot()
  p9 <- ggplot(indicators, aes(x=group, y=weekly_revisiting_prop_mad, fill=group)) + geom_boxplot()
  
  plots <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9)
  for(p in plots) {
    print(p)
    readline(prompt="Press [enter] to continue")
  }
  
}

