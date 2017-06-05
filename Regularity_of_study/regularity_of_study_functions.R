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
## sets the minimum lenght of a meaninful session, so any session with length less than 
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
## consecutive study sessions; in fact, for each student, it computes the standard deviation of 
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



## the f. computes the following counts (frequencies), per student:
## - total number of study sessions
## - number of sessions per week; and based on that:
##  -- number of inactive weeks
##  -- SD and MAD of weekly sessions counts
##  -- entropy of weekly session counts
compute.weekly.counts <- function(sessions) {
  students <- unique(sessions$user_id)
  s.matrix <- matrix(nrow = length(students), ncol=16)
  s <- 1
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
    weeks.inactive <- length(which(weekly.sessions==0))
    weekly.sd <- sd(weekly.sessions)
    weekly.mad <- mad(weekly.sessions)
    entropy <- -sum(sapply(weekly.proportions, function(p) {p * log1p(p)}))
    s.matrix[s,] <- c(students[i], s.tot, weekly.sessions, weeks.inactive, 
                      weekly.sd, weekly.mad, entropy)
    s <- s + 1
  }
  counts.df <- data.frame(s.matrix)
  colnames(counts.df) <- c('user_id', 's_total', paste0('count_w',c(2:5,7:12)), 
                           'inactive_weeks', 'weekly_count_sd', 'weekly_count_mad', 'weekly_entropy')
  counts.df
}



## the f. computes, for each student, the number of sessions per 
## each day of the week (Mon - Sun), and based on that indicators 
## of (ir)regularity:
## SD, MAD and entropy of session counts per week day
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
    s.matrix[s,] <- c(students[i], weekday.sessions, sd(weekday.sessions), 
                      mad(weekday.sessions), entropy)
    s <- s + 1
    
    shapiro.pvals[i] <- shapiro.test(weekday.sessions)$p.value
  }
  
  above_0.05 <- length(which(shapiro.pvals > 0.05))
  print(paste("Proportion of students with normal dist for weekday counts", above_0.05/length(students)))
  
  counts.df <- data.frame(s.matrix)
  weekdays <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
  colnames(counts.df) <- c('user_id', paste0(weekdays,'_count'), 
                           'weekday_count_sd', 'weekday_count_mad', 'weekday_entropy')
  counts.df
}


## for each student, the f. computes the proportion of 'revisiting', 'catching-up', 
## 'preparing', and 'ahead' study modes
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
## then, for each study mode, it computes SD of the weekly proportions
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


## f. for Winsorizing a variable  
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

## the f. scales the given feature set 
## due to the presence of outliers, standardization is used (instead of normalization); 
## also due to outliers, instead of using mean and SD, median and Interquartile Range (IQR) 
## are used, as suggested here:
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



