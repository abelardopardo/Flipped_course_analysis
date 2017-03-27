#####################################################################
## PREPARE THE TRACE LOG DATA (ORIGINALLY CREATED FOR PROCESS MINING) 
## FOR SEQUENCE ANALYSIS 
#####################################################################

traces <- read.csv(file = "Intermediate_files/trace_data_with_sessions_(no-1-event)_w0-16.csv",
                   stringsAsFactors = F)
str(traces)

## restrict sessions to those that took place during the course,
## that is, during the weeks 2-13 (the same time period that was 
## considered for cluster analysis) 
traces <- subset(traces, traces$WEEK %in% c(2:13))
table(traces$WEEK)

## substitute DBOARD_ACCESS with MC_EVAL
## also ORIENT with MC_ORIENT
activity.rev <- as.vector(sapply(traces$ACTIVITY, function(x) {
  if (x == "DBOARD_ACCESS") x <- "MC_EVAL"
  else if (x == "ORIENT") x <- "MC_ORIENT"
  x
}))
traces$ACTIVITY <- activity.rev
table(traces$ACTIVITY)

## CREATE SEQUENCES IN SUCH A WAY THAT EACH SEQUENCE CORRESPONDS TO ONE SESSION,  
## AND EACH STATE OF THE SEQUENCE CORRESPONDS TO AN ACTIVITY IN THE SESSION
## SEQUENCES SHOULD BE OF THE FORM:
## - MC_ORIENT/3 CONTENT_ACCESS/5
## - MCQ_CO/4 MCQ_IN/1  MCQ_CO/1  MCQ_IN/1  MCQ_CO/1  CONTENT_ACCESS/4


## f. that computes the length of the student's sequences and returns a
## vector of the computed lengths
## the input is the data frame with the student's trace data
seq.lengths <- function(stud.trace.data) {
  stud.sessions <- unique(stud.trace.data$CASE_ID)
  ## vector storing the number of activities in each sesssion
  ses.activity.cnt <- vector(mode = "numeric", length = length(stud.sessions))
  ## count the number of activities (states) in each session
  for(i in 1:length(stud.sessions)){
    ses.traces <- subset(stud.trace.data, stud.trace.data$CASE_ID == stud.sessions[i])
    ses.activity.cnt[i] <- nrow(ses.traces)  
  }
  ses.activity.cnt
}

## f. creates sequences for the student with the given stud.id
## each sequence corresponds to one learning sesssion
## the output is a list of sequences, where each sequence (i.e., list element)
## is a vector of activities the sequence consists of 
## (activities of the same kind are aggregated, so the vector has the form:
## MC_ORIENT/3, CONTENT_ACCESS/5, MCQ_CO/1,  MCQ_IN/1)
create.stud.sequences <- function(trace.data, stud.id) {
    stud.traces <- subset(trace.data, trace.data$RESOURCE_ID == stud.id)
    
    if (nrow(stud.traces) == 0) return(list())
    
    stud.sessions <- unique(stud.traces$CASE_ID)
    ## vector storing the number of activities in each sesssion
    ses.activity.cnt <- seq.lengths(stud.traces)
    
    sequences <- list()
    seq.count <- 1
    ## examine sessions individually; create a sequence for each session 
    for(i in 1:length(stud.sessions)) {
      ses.traces <- subset(stud.traces, stud.traces$CASE_ID == stud.sessions[i])
      ## check if the session comprises just one activity; if so, disregard it
      if (nrow(ses.traces) == 1) next
      ## vector to store sequence activities
      sequence <- vector()
      ## go through activities of this session
      a.current <- ses.traces$ACTIVITY[1]
      n.occurrence <- 1
      k <- 1
      for(j in 2:ses.activity.cnt[i]) {
        if ( ses.traces$ACTIVITY[j] == a.current ) {
          n.occurrence <- n.occurrence + 1
          ## if this is the last activity
          if ( j == ses.activity.cnt[i] )
            sequence[k] <- paste(a.current,"/",n.occurrence, sep = "")
        }
        else {
          sequence[k] <- paste(a.current,"/",n.occurrence, sep = "")
          k <- k + 1
          a.current <- ses.traces$ACTIVITY[j]
          n.occurrence <- 1
        }
      }
      sequences[[seq.count]] <- sequence
      seq.count <- seq.count + 1
    }
    sequences
}

## for the given student id and the given trace data,
## the f. creates a list of sequences that corresond to  
## the learning sesssions of the given student
## each element of the (output) list stores data about one sequence
## in the form of list of the following elements 
## - uid - user.id (trace.data$RESOURCE_ID)
## - sid - session.id (trace.data$SESSION_ID)
## - w - week (trace.data$WEEK)
## - seq - a vector of activities the sequence consists of
## (hence, the output is a list of lists)
create.stud.sequences.v2 <- function(trace.data, stud.id) {
  
  stud.traces <- subset(trace.data, trace.data$RESOURCE_ID == stud.id)
  ## learning sessions of the given student
  stud.sessions <- unique(stud.traces$CASE_ID)
  ## vector storing the number of activities in each sesssion
  ses.activity.cnt <- seq.lengths(stud.traces)
  
  seq.data <- list()
  seq.count <- 1
  ## examine sessions individually; create a sequence for each session 
  for(i in 1:length(stud.sessions)) {
    ## list to store all the data about this particular session
    session.data <- list()
    session.data$uid <- stud.id
    session.data$sid <- stud.sessions[i]
    session.data$w <- unique(stud.traces$WEEK[stud.traces$CASE_ID == stud.sessions[i]])
    ## get all activities performed within the current session
    ses.traces <- subset(stud.traces, stud.traces$CASE_ID == stud.sessions[i])
    ## if the sesssion contains just one activity, ignore it
    if (nrow(ses.traces) == 1) next
    ## vector to store sequence activities
    sequence <- vector()
    ## go through activities of this session
    a.current <- ses.traces$ACTIVITY[1]
    n.occurrence <- 1
    k <- 1
    for(j in 2:ses.activity.cnt[i]) {
      if ( ses.traces$ACTIVITY[j] == a.current ) {
        n.occurrence <- n.occurrence + 1
        ## if this is the last activity
        if ( j == ses.activity.cnt[i] )
          sequence[k] <- paste(a.current,"/",n.occurrence, sep = "")
      }
      else {
        sequence[k] <- paste(a.current,"/",n.occurrence, sep = "")
        k <- k + 1
        a.current <- ses.traces$ACTIVITY[j]
        n.occurrence <- 1
      }
    }
    session.data$seq <- sequence
    ## add the list with the data about the current session to the 
    ## list with the data about all the sessions
    seq.data[[seq.count]] <- session.data
    seq.count <- seq.count + 1
  }
  seq.data
}

## f. creates sequences for the given group of students, that is
## for the students whose ids are passed to the f. in the stud.ids vector
## the f. creates sequences of the same length - equal to the length of the 
## longest sequence - by extending shorter sequences with NAs
create.group.sequences <- function(trace.data, stud.ids) {
  seq.list <- list()
  for(i in 1:length(stud.ids)) {
    seq.list <- append(seq.list, create.stud.sequences(trace.data, stud.ids[i]))
  }
  seq.list
}

## f. first creates a new sequence list where all sequences will be
## of the same length - max.length - by extending shorter sequences with NAs
## then, this list is transformed into a dataframe, as required for the 
## creation of sequences in TraMineR
create.equal.length.seq <- function(sequences, max.length) { 
  eq.len.sequences <- list()
  for(i in 1:length(sequences)) {
    if ( length(sequences[[i]]) < max.length ) {
      d <- max.length - length(sequences[[i]])
      eq.len.sequences[[i]] <- c(sequences[[i]], rep(NA, times=d))
    } else {
      eq.len.sequences[[i]] <- sequences[[i]]
    }
  }
  ## transform the eq.len.sequences list into a dataframe
  ## from: http://stackoverflow.com/questions/4227223/r-list-to-data-frame?lq=1
  seq.df <- data.frame(matrix(unlist(eq.len.sequences), nrow=length(eq.len.sequences), byrow=T),
                       stringsAsFactors=FALSE)
  seq.df
}

## f. takes as its input 
## 1) a list with the data about one learning sequence: user.id, session.id,
## week, vector of <ACTIVITY/OCCURRENCE> elements (within the sequence)
## 2) the length of the longest learning sequence; all the sequences should
## be of this length
## the output is data frame with the following columns: user.id, session.id, 
## and one column for each <ACTIVITY/OCCURRENCE> element
create.seq.df <- function(sequence.data, max.length) {
  df <- data.frame(user.id=sequence.data$uid,
                   session.id=sequence.data$sid)
  s <- sequence.data$seq
  d <- max.length - length(s)
  if ( d > 0 ) ## if the length of the current seq is shorter than the maximum
    s <- c(s, rep(NA, times=d)) ## fill it with NAs to make all the seq of the same length
  j <- 4 ## start from the 4th column (the first 3 are already populated)
  for(i in 1:length(s)){
    df[1,j] <- s[i]
    j <- j+1
  }
  df
}
  
create.seq.data.vect <- function(sequence.data, max.length) {
  v <- c(sequence.data$uid, sequence.data$sid, sequence.data$seq)
  if ( length(sequence.data$seq) < max.length ) {
    d <- max.length - length(sequence.data$seq)
    v <- c(v, rep(NA, times=d))
  }  
  v
}


###############################################
## CREATE SEQUENCES FOR THE TOP 10% STUDENTS ##
###############################################

## load the f. for identifying top performing students
source("util_functions.R")
counts.data <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv") 
## extract exam scores
scores <- counts.data[, c('user_id', 'SC_MT_TOT', 'SC_FE_TOT')]
best.and.worst <- best.and.worst.performers(scores)
top10.ids <- best.and.worst$top10

## create sequences for the top 10% students
top10.seq <- create.group.sequences(trace.data = traces, stud.ids = top10.ids)

## inspect the length of the sequences, in terms of the number of activities
## they consist of 
s.length <- vector()
## compute the length of sequences - in terms of number of activities - for 
## all considered students
for(i in 1:length(top10.ids)) {
  stud.traces <- subset(traces, traces$RESOURCE_ID == top10.ids[i])
  s.length <- c(s.length, seq.lengths(stud.traces))
}
summary(s.length)
hist(s.length)
plot(s.length)

## there are outliers, that should be removed
quantile(x = s.length, probs = c(0.9, 0.95, 0.99, 1))
## 99th percentile is 143.9; still too granular
## 95th percentile is 88; remove sequences with length >=88
outliers <- which(s.length >= 88)
## there are 43 such sequences (out of 829)
## remove the outliers
top10.seq[outliers] <- NULL

## find the sequence with the max length, but length is here 
## considered in terms of the <ACTIVITY/OCCURRENCE> elements
## (not in terms of activites as in the above lines)
## this is required for making all the sequences of the same 
## length (in terms of <ACTIVITY/OCCURRENCE> elements)
seq.elem.counts <- vector(mode = "integer", length = length(top10.seq))
for(i in 1:length(top10.seq)) {
  seq.elem.counts[i] <- length(top10.seq[[i]])
}
max.length <- max(seq.elem.counts)
summary(seq.elem.counts)
quantile(x = seq.elem.counts, probs = c(0.9, 0.95, 0.99, 1))

top10.seq.df <- create.equal.length.seq(top10.seq, max.length)
str(top10.seq.df)

## create sequence out of the data frame
require(TraMineR)
top10.seq <- seqdef(data = top10.seq.df, var = 1:max.length, informat = "SPS", 
                   SPS.in = list(xfix = "", sdsep = "/"))
print(top10.seq[1:10, 1:30], format = 'SPS')

## store the created sequences
save(top10.seq, file = "Intermediate_files/top10perc_sequences_SPS_format.RData")

#################################################
## CREATE SEQUENCES FOR THE WORST 25% STUDENTS ##
#################################################

worst10.ids <- best.and.worst$worst10
worst25.ids <- best.and.worst$worst25

## create sequences for the worst 10% students
worst10.seq <- create.group.sequences(trace.data = traces, stud.ids = worst10.ids)
## too few sequences (128) in comparison with the number of seq for the top 10
## so, compute sequences for the worst 25% students
worst25.seq <- create.group.sequences(trace.data = traces, stud.ids = worst25.ids)
## n=721 - still below the number of seq for the top 10, but close to it (829) 

## inspect the length of the sequences, in terms of the number of activities
## they consist of 
s.length <- vector()
## compute the length of sequences - in terms of number of activities - for 
## all considered students
for(i in 1:length(worst25.ids)) {
  stud.traces <- subset(traces, traces$RESOURCE_ID == worst25.ids[i])
  s.length <- c(s.length, seq.lengths(stud.traces))
}
summary(s.length)
plot(s.length)

## check for outliers that should be removed
quantile(x = s.length, probs = c(0.9, 0.95, 0.99, 1))
## 99th percentile is 160.5; still too granular
## 95th percentile is 101; remove sequences with length >101
outliers <- which(s.length > 101)
## there are 36 such sequences (out of 721)
## remove the outliers
worst25.seq[outliers] <- NULL

## find the sequence with the max length, but length is here 
## considered in terms of the <ACTIVITY/OCCURRENCE> elements
## (not in terms of activites as in the above lines)
## this is required for making all the sequences of the same 
## length (in terms of <ACTIVITY/OCCURRENCE> elements)
seq.elem.counts <- vector(mode = "integer", length = length(worst25.seq))
for(i in 1:length(worst25.seq)) {
  seq.elem.counts[i] <- length(worst25.seq[[i]])
}
max.length <- max(seq.elem.counts)
sort(seq.elem.counts, decreasing = T)
## [1] 70 54 52 51 51 49 49 49 48 48 45
## remove the outlier with the length of 70
outlier <- which( seq.elem.counts == max.length )
worst25.seq[outlier] <- NULL
## recompute the max.length now that the outlier has been removed
seq.elem.counts <- vector(mode = "integer", length = length(worst25.seq))
for(i in 1:length(worst25.seq)) {
  seq.elem.counts[i] <- length(worst25.seq[[i]])
}
max.length <- max(seq.elem.counts)

## create dataframe to be used for generating sequences
worst25.seq.df <- create.equal.length.seq(worst25.seq, max.length)
head(worst25.seq.df)

## create sequence out of the data frame
require(TraMineR)
worst25.seq <- seqdef(data = worst25.seq.df, var = 1:max.length, informat = "SPS", 
                    SPS.in = list(xfix = "", sdsep = "/"))
print(worst25.seq[1:10, 1:30], format = 'SPS')

## store the created sequences
save(worst25.seq, file = "Intermediate_files/worst25perc_sequences_SPS_format.RData")


############################################################################
## CREATE SEQUENCES FOR ALL THE STUDENTS, BASED ON THE LEARNING TRACES DATA;
## EACH SEQUENCE CORRESPONDS TO ONE LEARNING SESSION;
## FOR EACH SEQUENCE, THE FOLLOWING DATA IS EXTRACTED/CREATED:
## - uid - user.id 
## - sid - session.id
## - w - week
## - seq - a vector of activities the sequence consists of
############################################################################
set.seed(1804)

stud.ids <- unique(traces$RESOURCE_ID)
## list of learning sequences of all the students
seq.list <- list()
for(i in 1:length(stud.ids)) {
  seq.list <- append(seq.list, create.stud.sequences.v2(traces, stud.ids[i]))
}
seq.list[100:110]
length(seq.list)
## n=11914 new: 12163

## inspect the length of the sequences, in terms of the number of activities they consist of 
s.length <- vector()
unique.sessions <- unique(traces$CASE_ID)
## compute the length of sequences (in terms of number of activities)
for(i in 1:length(unique.sessions)) {
  session.traces <- subset(traces, traces$CASE_ID == unique.sessions[i]) 
  s.length <- c(s.length, nrow(session.traces))
}
summary(s.length)
plot(s.length)
## check for outliers that should be removed
quantile(x = s.length, probs = c(0.9, 0.95, 0.99, 1))
## 99th percentile is 151; still too granular
## 95th percentile is 86; remove sequences with length >= 86
outliers <- which(s.length > 86)
outliers <- unique.sessions[outliers]
## there are 608 such sequences (out of 12,163)

## remove the outliers
to.remove <- vector()
j <- 1
for(i in 1:length(seq.list)) {
  if (as.integer(seq.list[[i]][2]) %in% outliers ) {
    to.remove[j] <- i 
    j <- j + 1
  }
}
seq.list[to.remove] <- NULL

## find the sequence with the max length, but length is here 
## considered in terms of the <ACTIVITY/OCCURRENCE> elements
seq.elem.counts <- vector(mode = "integer", length = length(seq.list))
for(i in 1:length(seq.list)) {
  seq.elem.counts[i] <- length(seq.list[[i]]$seq)
}
max.length <- max(seq.elem.counts)

## transform the list of sequences into data frames of the same structure 
## (same number and type of columns)
## combine these individual data frames into an 'agregate' dataframe 
## that will be used for generating TraMineR sequences
seq.df <- as.data.frame(matrix(nrow = length(seq.list), ncol = (max.length+2), byrow = T))
for(i in 1:length(seq.list)) {
  v <- create.seq.data.vect(sequence.data = seq.list[[i]], max.length)  
  seq.df[i,] <- v
}

## create (TraMineR) sequences out of the data frame
require(TraMineR)
seq.final <- seqdef(data = seq.df, var = 3:(max.length+2), informat = "SPS", 
                   SPS.in = list(xfix = "", sdsep = "/"))
print(seq.final[1:10, 1:30], format = 'SPS')

## store the data frame with sequence (session) data
saveRDS(object = seq.df, file = "Intermediate_files/all_students_sequences_dataframe.RData")
write.csv(seq.df, file = "Intermediate_files/all_students_sequences_dataframe.csv", 
          quote = F, row.names = F)

## store the created sequences
saveRDS(object = seq.final, file = "Intermediate_files/all_students_sequences_SPS_format.RData")


############################################################################
## CREATE LEARNING SEQUENCES AS IN THE PREVIOUS SECTION, BUT NOT FOR ALL 
## THE STUDENTS, BUT ONLY THOSE STUDENTS WHO FILLED IN THE SPQ QUESTIONNAIRE
############################################################################

## extract ids of the students who filled in the SPQ questionnaire
## load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
spq.scores <- all.scores[, c('user_id', 'SPQ_DA', 'SPQ_SA', 'SPQ_DM', 'SPQ_SM', 'SPQ_DS', 'SPQ_SS')]
str(spq.scores)
## find the students who filled in the questionnaire
not.NA.scores <-  which(complete.cases(spq.scores))
length(not.NA.scores)

## ids of students for whom sequences should be extracted
stud.ids <- spq.scores$user_id[not.NA.scores]
## take the subset of events data that are associated with these students  
traces <- subset(traces, RESOURCE_ID %in% stud.ids)
## 176572

set.seed(2106)

## list of the students' learning sequences
seq.list <- list()
for(i in 1:length(stud.ids)) {
  seq.list <- append(seq.list, create.stud.sequences.v2(traces, stud.ids[i]))
}
seq.list[100:110]
length(seq.list)
## n=6513

## inspect the length of the sequences, in terms of the number of activities they consist of 
s.length <- vector()
unique.sessions <- unique(traces$CASE_ID)
## compute the length of sequences (in terms of number of activities)
for(i in 1:length(unique.sessions)) {
  session.traces <- subset(traces, traces$CASE_ID == unique.sessions[i]) 
  s.length <- c(s.length, nrow(session.traces))
}
summary(s.length)
plot(s.length)
## check for outliers that should be removed
quantile(x = s.length, probs = c(0.9, 0.95, 0.99, 1))
## 99th percentile is 179; still too granular
## 95th percentile is 96; remove sequences with length > 96
outliers <- which(s.length > 96)
outliers <- unique.sessions[outliers]
## there are 317 such sequences (out of 6,513)

## remove the outliers
to.remove <- vector()
j <- 1
for(i in 1:length(seq.list)) {
  if (as.integer(seq.list[[i]][2]) %in% outliers ) {
    to.remove[j] <- i 
    j <- j + 1
  }
}
seq.list[to.remove] <- NULL

## find the sequence with the max length, but length is here 
## considered in terms of the <ACTIVITY/OCCURRENCE> elements
seq.elem.counts <- vector(mode = "integer", length = length(seq.list))
for(i in 1:length(seq.list)) {
  seq.elem.counts[i] <- length(seq.list[[i]]$seq)
}
max.length <- max(seq.elem.counts)

## transform the list of sequences into data frames of the same structure 
## (same number and type of columns)
## combine these individual data frames into an 'agregate' dataframe 
## that will be used for generating TraMineR sequences
seq.df <- as.data.frame(matrix(nrow = length(seq.list), ncol = (max.length+2), byrow = T))
for(i in 1:length(seq.list)) {
  v <- create.seq.data.vect(sequence.data = seq.list[[i]], max.length)  
  seq.df[i,] <- v
}

## create (TraMineR) sequences out of the data frame
require(TraMineR)
seq.final <- seqdef(data = seq.df, var = 3:(max.length+2), informat = "SPS", 
                    SPS.in = list(xfix = "", sdsep = "/"))
print(seq.final[1:10, 1:30], format = 'SPS')

## store the data frame with sequence (session) data
saveRDS(object = seq.df, file = "Intermediate_files/SPQ_students_sequences_dataframe.RData")
write.csv(seq.df, file = "Intermediate_files/SPQ_students_sequences_dataframe.csv", 
          quote = F, row.names = F)

## store the created sequences
saveRDS(object = seq.final, file = "Intermediate_files/SPQ_students_sequences_SPS_format.RData")


####################################################################
## CREATE SEQUENCES FOR STUDENTS FROM THE HIGH PERFORMING CLUSTERS:
## INTENSIVE, STRATEGIC AND HIGHLY STRATEGIC; AND ONLY FOR WEEKS 2-5
## THAT IS TILL MIDTERM, THE PERIOD WHEN THESE STUDENTS USED DIFFERENT 
## STRATEGIES
####################################################################

## load the f. student cluster data
stud.clusters.data <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv") 
## select students from the first 3 clusters
students <- subset(stud.clusters.data, cluster %in% c(1:3))
stud.ids <- students$stud.id

## select only traces for the weeks 2-5
traces.w2to5 <- subset(traces, WEEK %in% c(2:5))

## create sequences for weeks 2-6 for students from high performing clusters
high.perf.w2to5.seq <- create.group.sequences(trace.data = traces.w2to5, 
                                              stud.ids = stud.ids)

## inspect the length of the sequences, in terms of the number of activities
## they consist of 
s.length <- vector()
## compute the length of sequences - in terms of number of activities - for 
## all considered students
for(i in 1:length(stud.ids)) {
  stud.traces <- subset(traces.w2to5, traces.w2to5$RESOURCE_ID == stud.ids[i])
  s.length <- c(s.length, seq.lengths(stud.traces))
}
summary(s.length)
hist(s.length)
plot(s.length)

## there are outliers, that should be removed
quantile(x = s.length, probs = c(0.9, 0.95, 0.99, 1))
## 99th percentile is 173.05; still too granular
## 95th percentile is 92.05; remove sequences with length > 92
outliers <- which(s.length > 92)
## there are 129 such sequences (out of 2580)
## remove the outliers
high.perf.w2to5.seq[outliers] <- NULL

## find the sequence with the max length, but length is here 
## considered in terms of the <ACTIVITY/OCCURRENCE> elements
## (not in terms of activites as in the above lines)
## this is required for making all the sequences of the same 
## length (in terms of <ACTIVITY/OCCURRENCE> elements)
seq.elem.counts <- vector(mode = "integer", length = length(high.perf.w2to5.seq))
for(i in 1:length(high.perf.w2to5.seq)) {
  seq.elem.counts[i] <- length(high.perf.w2to5.seq[[i]])
}
max.length <- max(seq.elem.counts)
summary(seq.elem.counts)
quantile(x = seq.elem.counts, probs = c(0.9, 0.95, 0.99, 1))

high.perf.w2to5.seq.df <- create.equal.length.seq(high.perf.w2to5.seq, max.length)
str(high.perf.w2to5.seq.df)

## create sequence out of the data frame
require(TraMineR)
high.perf.w2to5.seq <- seqdef(data = high.perf.w2to5.seq.df, 
                              var = 1:max.length, informat = "SPS", 
                              SPS.in = list(xfix = "", sdsep = "/"))
print(high.perf.w2to5.seq[1:10, 1:30], format = 'SPS')

## store the created sequences
save(high.perf.w2to5.seq, 
     file = "Intermediate_files/high_perf_clusters_weeks_2-5_sequences_SPS_format.RData")


########################################################################
## CREATE SEQUENCES FOR STUDENTS FROM THE LOW PERFORMING CLUSTERS:
## SELECTIVE AND HIGHLY SELECTIVE; AND ONLY FOR WEEKS 2-5 (TILL MIDTERM)
########################################################################

## load the f. student cluster data
stud.clusters.data <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv") 
## select students from the first 3 clusters
students <- subset(stud.clusters.data, cluster %in% c(4,5))
stud.ids <- students$stud.id

## select only traces for the weeks 2-6
traces.w2to5 <- subset(traces, WEEK %in% c(2:5))

## create sequences for weeks 2-5 for students from low performing clusters
low.perf.w2to5.seq <- create.group.sequences(trace.data = traces.w2to5, 
                                              stud.ids = stud.ids)

## inspect the length of the sequences, in terms of the number of activities
## they consist of 
s.length <- vector()
## compute the length of sequences - in terms of number of activities - for 
## all considered students
for(i in 1:length(stud.ids)) {
  stud.traces <- subset(traces.w2to5, traces.w2to5$RESOURCE_ID == stud.ids[i])
  s.length <- c(s.length, seq.lengths(stud.traces))
}
summary(s.length)
plot(s.length)

## there are outliers, that should be removed
quantile(x = s.length, probs = c(0.9, 0.95, 0.99, 1))
## 99th percentile is 188.76; still too granular
## 95th percentile is 115; remove sequences with length > 114
outliers <- which(s.length > 114)
## there are 107 such sequences (out of 2124)
## remove the outliers
low.perf.w2to5.seq[outliers] <- NULL

## find the sequence with the max length, but length is here 
## considered in terms of the <ACTIVITY/OCCURRENCE> elements
## (not in terms of activites as in the above lines)
## this is required for making all the sequences of the same 
## length (in terms of <ACTIVITY/OCCURRENCE> elements)
seq.elem.counts <- vector(mode = "integer", length = length(low.perf.w2to5.seq))
for(i in 1:length(low.perf.w2to5.seq)) {
  seq.elem.counts[i] <- length(low.perf.w2to5.seq[[i]])
}
max.length <- max(seq.elem.counts)
# too high - 216
summary(seq.elem.counts)
quantile(x = seq.elem.counts, probs = c(0.9, 0.95, 0.99, 1))
#   90%    95%    99%   100% 
#  34.00  48.00  88.52 216.00 
outliers <- which(seq.elem.counts > 48)
## remove and compute max.length again
low.perf.w2to5.seq[outliers] <- NULL
## now, max is 48

low.perf.w2to5.seq.df <- create.equal.length.seq(low.perf.w2to5.seq, max.length)
str(low.perf.w2to5.seq.df)


## create sequence out of the data frame
require(TraMineR)
low.perf.w2to5.seq <- seqdef(data = low.perf.w2to5.seq.df, 
                              var = 1:max.length, informat = "SPS", 
                              SPS.in = list(xfix = "", sdsep = "/"))
print(low.perf.w2to5.seq[1:10, 1:30], format = 'SPS')

## store the created sequences
saveRDS(low.perf.w2to5.seq, 
        file = "Intermediate_files/low_perf_clusters_weeks_2-5_sequences_SPS_format.RData")
