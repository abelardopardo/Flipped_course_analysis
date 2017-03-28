######################################################################################
## Create HMM for each week of the course (2-13), using the same set of features and 
## session-focus (each state corresponds to one learning session) as in the
## session_based_HMM.R script
## 
## In summary: 
## - student-session is the unit of analysis, ie. features (given below) are computed 
##   for each learning session of each student
## - features (computed at the session-level):
## -- FA_PERC - percentage (ratio) of formative assessment actions within the study session
## -- FA_CO_PERC - percentage (or ratio) of correct formative assessment actions 
## -- SA_PERC - percentage (ratio) of summative assessment actions within the study session 
## -- SA_CO_PERC - percentage (or ratio) of correct summative assessment actions  
## -- VID_PERC - percentage (ratio) of video play actions within the study session 
## -- READ_PERC - percentage (ratio) of reading (content access) actions within the study session 
## -- METACOG_PERC - percentage (ratio) of metacognitive (dashboard + orientation) actions 
##                   within the study session
##
############################################################################################

traces <- readRDS(file = "Intermediate_results/study_mode_weeks2-13.RData")
str(traces)

table(traces$ACTION)
traces$ACTION <- as.character(traces$ACTION)
## substitute DBOARD_ACCESS with MC_EVAL, and ORIENT with MC_ORIENT
traces$ACTION[traces$ACTION=="DBOARD_ACCESS"] <- "MC_EVAL"
traces$ACTION[traces$ACTION=="ORIENT"] <- "MC_ORIENT"

## rename EQT_CO, VEQ_CO, and EXE_F_CO to FA_CO (FA = Formative Assessment), also
## rename EQT_IN, VEQ_IN, and EXE_F_IN to FA_IN, 
## rename EQT_SR and VEQ_SR to FA_SR 
traces$ACTION[traces$ACTION %in% c("EQT_CO", "VEQ_CO", "EXE_F_CO")] <- "FA_CO"
traces$ACTION[traces$ACTION %in% c("EQT_IN", "VEQ_IN", "EXE_F_IN")] <- "FA_IN"
traces$ACTION[traces$ACTION %in% c("EQT_SR", "VEQ_SR")] <- "FA_SR"

## rename EXE_S_CO to SA_CO (SA = Summative Assessment), and EXE_S_IN to SA_IN
traces$ACTION[traces$ACTION=="EXE_S_CO"] <- "SA_CO"
traces$ACTION[traces$ACTION=="EXE_S_IN"] <- "SA_IN"

table(traces$ACTION)
action.stats <- as.data.frame(round(prop.table(table(traces$ACTION)), digits = 4))
colnames(action.stats) <- c("action", "proportion")
kable(action.stats, format = "pandoc")

## sort the data based on the 1) student, 2) WEEK, 3) timestamp
sorted.traces <- traces[ with(traces, order(STUDENT_ID, WEEK, TIMESTAMP)), ]
head(sorted.traces)

## turn ACTION to factor variable
sorted.traces$ACTION <- factor(sorted.traces$ACTION)
table(sorted.traces$ACTION)

## change the order of variables
sorted.traces <- sorted.traces[,c(7,1,3,2,4,5,9)]
str(sorted.traces)

length(unique(sorted.traces$SESSION_ID))
# 36314 

## get the session count for each student
session.count <- get.session.count(sorted.traces)
summary(session.count) 
sort(session.count)[1:20]
# only 2 students have less than 10 sessions 
## get ids of students with < 10 sessions
low.session.stud <- unique(sorted.traces$STUDENT_ID)[which(session.count < 10)]
## remove the students with < 10 sesssion 
sorted.traces <- subset(sorted.traces, !(STUDENT_ID %in% low.session.stud))

## compute session count again
session.count <- get.session.count(sorted.traces)
summary(session.count) 
#  Min. 1st Qu.  Median   Mean  3rd Qu.   Max. 
# 12.0    59.0    89.0    98.4   126.0   354.0 
## check for outliers
boxplot(session.count)
length(boxplot.stats(session.count)$out)
# 13 outliers, but for now, I'll leave them  

## check how many students are left
length(unique(sorted.traces$STUDENT_ID))
# 369 (the initial number of students = 371)
## and how many sessions
length(unique(sorted.traces$SESSION_ID))
# 36311 (initially, 36314)

###################################################
## COMPUTE THE FEATURES FOR EACH WEEK OF THE COURSE
###################################################

for(w in 2:13) {
  w.features <- compute.weekly.features(sorted.traces, w)
  discret.features <- discretize.weekly.features(w.features)
  ## add the ntimes attribute to the feature set (requied for the depmixS4)
  discret.features <- add.ntimes.feature(discret.features)
  str(discret.features)
  factorize.and.save.weekly.features(discret.features, w)
}


####################################
## FIT THE HMMs FOR EACH COURSE WEEK
####################################

#install.packages("depmixS4")
library(depmixS4)

## WEEK 2
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w2.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, week = 2, max.ns = 7)
## fit model with 6 states
result <- fit.and.save.hmm(w.features, ns = 6, week = 2)
summary(result)

## WEEK 3
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w3.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 3, max.ns = 7)
## fit model with 4 states
result <- fit.and.save.hmm(w.features, ns = 4, week = 3)
summary(result)


## WEEK 4
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w4.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 4, max.ns = 7)
## fit model with 4 states
result <- fit.and.save.hmm(w.features, ns = 4, week = 4)
summary(result)

## WEEK 5
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w5.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 5, max.ns = 7)
## fit model with 5 states
result <- fit.and.save.hmm(w.features, ns = 5, week = 5)
summary(result)


## WEEK 6
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w6.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 6, max.ns = 7)
## fit model with 4 states
result <- fit.and.save.hmm(w.features, ns = 4, week = 6)
summary(result)


## WEEK 7
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w7.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 7, max.ns = 7)
## fit model with 4 states 
result <- fit.and.save.hmm(w.features, ns = 4, week = 7)
summary(result)


## WEEK 8
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w8.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 8, max.ns = 7)
## fit model with 5 states
result <- fit.and.save.hmm(w.features, ns = 5, week = 8)
summary(result)


## WEEK 9
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w9.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 9, max.ns = 7)
## fit model with 6 states
result <- fit.and.save.hmm(w.features, ns = 6, week = 9)
summary(result)


## WEEK 10
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w10.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 10, max.ns = 7)
## fit model with 5 states
result <- fit.and.save.hmm(w.features, ns = 5, week = 10)
summary(result)


## WEEK 11
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w11.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 11, max.ns = 7)
## fit model with 4 states
result <- fit.and.save.hmm(w.features, ns = 4, week = 11)
summary(result)


## WEEK 12
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w12.RData")
get.stud.and.session.num(w.features)
compare.models(w.features, 12, max.ns = 7)
## fit a model with 6 states 
result <- fit.and.save.hmm(w.features, ns = 6, week = 12)
summary(result)


## WEEK 13
w.features <- readRDS(file = "Intermediate_results/weekly_HMM_features/w13.RData")
str(w.features)
get.stud.and.session.num(w.features)
compare.models(w.features, 13, max.ns = 7)
## fit a model with 4 states 
result <- fit.and.save.hmm(w.features, ns = 4, week = 13)
summary(result)


##################################################
## COMPUTE STATE PROPORTIONS FOR EACH COURSE WEEK
##################################################

w.files <- list.files(path = "results/session_based_weekly_HMM/", 
                      pattern = ".csv", full.names = T)
for(i in 1:length(w.files)) {
  w.states <- read.csv(file = w.files[i])
  print(paste("WEEK:", unique(w.states$WEEK)))
  print(paste("student count:", length(unique(w.states$STUDENT_ID))))
  print(paste("session count:", length(unique(w.states$SESSION_ID))))
  print(round(prop.table(table(w.states$state)), digits = 2))
}


####################
## UTILITY FUNCTIONS
####################


## the f. computes the number of sessions per student
get.session.count <- function(trace.data) {
  stud.ids <- unique(trace.data$STUDENT_ID)
  stud.sessions <- list()
  for(s in 1:length(stud.ids)) {
    stud.sessions[[s]] <- unique(trace.data$SESSION_ID[trace.data$STUDENT_ID==stud.ids[s]])
  }
  
  session.count <- sapply(X = stud.sessions, FUN = length)
  session.count
}

## the f. computes features for all the study sessions made by a student
## the input is a vector of the session identifiers
## it returs a dataframe with the feature values; the first column is the session id
compute.stud.session.features <- function(stud.session.data) {
  feature.data <- list()
  for(s in 1:length(stud.session.data)) {
    stud.session <- stud.session.data[s]
    session.data <- subset(sorted.traces, SESSION_ID==stud.session)
    n.actions <- nrow(session.data)
    n.fa <- nrow(session.data[session.data$ACTION %in% c("FA_CO", "FA_IN", "FA_SR"),])
    n.sa <- nrow(session.data[session.data$ACTION %in% c("SA_CO", "SA_IN"),])
    FA_PERC <- n.fa/n.actions
    FA_CO_PERC <- 0
    if (n.fa > 0) {
      FA_CO_PERC <- nrow(session.data[session.data$ACTION=="FA_CO",])/n.fa
    } 
    SA_PERC <- n.sa/n.actions
    SA_CO_PERC <- 0
    if (n.sa > 0) {
      SA_CO_PERC <- nrow(session.data[session.data$ACTION=="SA_CO",])/n.sa  
    }
    VID_PERC <- nrow(session.data[session.data$ACTION=="VIDEO_PL",])/n.actions
    READ_PERC <- nrow(session.data[session.data$ACTION=="CONTENT_ACCESS",])/n.actions
    METACOG_PERC <- nrow(session.data[session.data$ACTION %in% c("MC_ORIENT", "MC_EVAL"),])/n.actions
    
    feature.data[[s]] <- c(stud.session, FA_PERC, FA_CO_PERC, SA_PERC, SA_CO_PERC, 
                           VID_PERC, READ_PERC, METACOG_PERC)
  }
  
  feature.matrix <- matrix(data = unlist(feature.data), 
                           nrow = length(feature.data), ncol = 8, byrow = T) 
  feature.df <- as.data.frame(feature.matrix)
  colnames(feature.df) <- c('SESSION_ID', 'FA_PERC', 'FA_CO_PERC', 'SA_PERC', 
                            'SA_CO_PERC', 'VID_PERC', 'READ_PERC', 'METACOG_PERC')
  feature.df
}

## the f. computes 'ntimes' attribute required for the depmix function
## the f. assumes that the input df (feature.data) is sorted based on STUDENT.ID
compute.ntimes <- function(feature.data) {
  ntimes <- vector()
  k <- 1
  i <- 1
  while(i <= nrow(feature.data)) {
    current.student <- feature.data$STUDENT_ID[i]
    count <- 1
    if ( i == nrow(feature.data)) {
      ntimes[k] <- count
      break
    }
    same.student <- T
    j <- i + 1
    while( same.student == T & j <= nrow(feature.data)) {
      if ( feature.data$STUDENT_ID[j] == current.student ) { 
        count <- count + 1
        j <- j + 1 
      } else {
        same.student <- F
      }
    }
    i <- j
    ntimes[k] <- count
    k <- k + 1
  }
  ntimes
}

## the f. adds the ntimes vector to the feature data, by associating each 
## observation related to a particular student with the number of observations
## available for that student (ntimes)
add.ntimes.feature <- function(features.data) {
 
  ntimes.vector <- compute.ntimes(features.data)
  
  seq.lengths <- as.data.frame( cbind(stud.id = unique(features.data$STUDENT_ID),
                                      length = ntimes.vector))
  features.data$ntimes <- vector(mode = "integer", length = nrow(features.data))
  for (i in 1:nrow(features.data)) {
    features.data$ntimes[i] <- 
      seq.lengths[ seq.lengths$stud.id == features.data$STUDENT_ID[i], 2]  
  }
  features.data
  
}


## the f. creates a data frame with features for the given week
## using the sorted trace data; features are computed for each student 
## and each study session (in the given week)
compute.weekly.features <- function(sorted.trace.data, week) {
  
  weekly.traces <- subset(sorted.trace.data, WEEK==week)
  stud.ids <- unique(weekly.traces$STUDENT_ID)
  
  stud.sessions <- list()
  for(s in 1:length(stud.ids)) {
    stud.traces <- subset(weekly.traces, STUDENT_ID==stud.ids[s])
    if ( nrow(stud.traces) > 0 )
      stud.sessions[[s]] <- c(stud.ids[s], list(unique(stud.traces$SESSION_ID)))
  }
  
  weekly.features <- data.frame()
  for(s in 1:length(stud.sessions)) {
    stud.session.features <- compute.stud.session.features(as.vector(unlist(stud.sessions[[s]][2])))
    stud.session.features$STUDENT_ID <- unlist(stud.sessions[[s]][1])
    stud.session.features$WEEK <- week
    weekly.features <- as.data.frame(rbind(weekly.features, stud.session.features))
  }
  
  # place student id in the first column
  weekly.features <- weekly.features[,c(9,1:8,10)] 
  
  weekly.features
}


## the f. discretizes the features, as depmix cannot work with 
## the feature values in their 'original' form
discretize.weekly.features <- function(weekly.features) {
  require(infotheo)
  
  discret.features <- as.data.frame(apply(X = weekly.features[,c(3:9)], MARGIN = 2, 
                                          FUN = discretize, disc = "equalwidth", nbins = 10))
  colnames(discret.features) <- c('FA_PERC', 'FA_CO_PERC', 'SA_PERC', 'SA_CO_PERC', 
                                  'VID_PERC', 'READ_PERC', 'METACOG_PERC')
  discret.features <- as.data.frame(cbind(weekly.features[,c(1,2,10)], discret.features))
  discret.features <- discret.features[,c(1,2,4:10,3)]
  
  ## those feature values that are originaly zero should be set to zero also in the discretized dataset
  ## to indicate the complete absence of certain type of action
  for(i in 1:nrow(discret.features)) {
    for(j in 3:9) { # features are in the columns 3:9
      if (weekly.features[i,j]==0) discret.features[i,j] <- 0
    }
  }
  discret.features
  
}

## the f. turns all the features into factor variables, and saves them
## in the 
factorize.and.save.weekly.features <- function(weekly.features, week) {
  
  final.features <- as.data.frame(apply(X = weekly.features[,c(3:9)], MARGIN = 2, FUN = factor))
  final.features <- as.data.frame(cbind(weekly.features[,c(1,2,10,11)], final.features))
  final.features <- final.features[,c(1,2,5:11,3,4)]
  
  ## save the final feature set
  saveRDS(object = final.features, 
          file = paste0("Intermediate_results/weekly_HMM_features/w",week,".RData"))
  
}


fit.hmm <- function(feature.data, ns, week) {
  
  response.vars <- list(FA_PERC ~ 1, FA_CO_PERC ~ 1, 
                        SA_PERC ~ 1, SA_CO_PERC ~ 1, VID_PERC ~ 1, 
                        READ_PERC ~ 1, METACOG_PERC ~ 1)
  family.dist <- list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), multinomial("identity"), multinomial("identity"),
                      multinomial("identity"), multinomial("identity"))
  
  set.seed(24022017)
  mod <- depmix(response = response.vars,
                data = feature.data, 
                nstates = ns, 
                family = family.dist)
  mod.fit <- fit(mod, verbose = FALSE)
  
  return (mod.fit)
  
}

## the f. computes several HMM models, with ns=>min.ns & ns<=max.ns
## and produces a table with evaluation metrics
compare.models <- function(feature.data, week, min.ns=2, max.ns=6) {

  eval.metrics <- data.frame()
  for (ns in min.ns:max.ns) {
    mod.fit <- fit.hmm(feature.data, ns = ns, week = week)
    metrics <- c(ns, AIC(mod.fit), BIC(mod.fit), logLik(mod.fit))
    eval.metrics <- as.data.frame( rbind(eval.metrics, metrics) )
  }
  colnames(eval.metrics) <- c('n.states', 'AIC', 'BIC', 'logLik')
  require(knitr)
  kable(x = eval.metrics, format = "rst")

}


fit.and.save.hmm <- function(feature.data, ns, week) {
  
  mod.fit <- fit.hmm(feature.data, ns, week)
  ## get the estimated state for each observation 
  estimates <- posterior(mod.fit)
  # add the estimated states to the features set and save the resulting df
  f.and.s <- as.data.frame(cbind(feature.data, estimates))
  write.csv(x = f.and.s[,c(1:10,12:ncol(f.and.s))],  
            file = paste0("results/session_based_weekly_HMM/w", week, "_", ns, "_states.csv"), 
            quote = F, row.names = F)
  
  return(mod.fit)
}

get.stud.and.session.num <- function(weekly.features) {
  stud.num <- length(unique(weekly.features$STUDENT_ID))
  session.num <- length(unique(weekly.features$SESSION_ID))
  c(STUD_CNT=stud.num, SESSION_CNT=session.num)
}