## start from the dataset with trace data (for weeks 1-16) organized into sessions
traces <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv",
                   stringsAsFactors = F)
str(traces)
## rename some of the columns to make their meaning more intuitive
colnames(traces)[c(1,2)] <- c("SESSION_ID", "STUDENT_ID")

## restrict sessions to those that took place during the course,
## that is, during the weeks 2-13 (the same time period that was 
## considered for cluster analysis) 
traces <- subset(traces, traces$WEEK %in% c(2:13))

table(traces$ACTIVITY)
## in the ACTIVITY column, substitute DBOARD_ACCESS with MC_EVAL
## also ORIENT with MC_ORIENT
traces$ACTIVITY[traces$ACTIVITY=="DBOARD_ACCESS"] <- "MC_EVAL"
traces$ACTIVITY[traces$ACTIVITY=="ORIENT"] <- "MC_ORIENT"

table(traces$TOPIC)

##############################################################################
## ADD CLUSTER ASSIGNMENTS including:
## - 'original' clusters, numerical, as obtained from the clustering algorithm
## - 'interpreted' clusters, labels A-F
##############################################################################

## read cluster assignments
clust.data <- data.frame()
for(w in 2:13) {
  weekly.clust <- read.csv(file = paste0("results/weekly_clusters/week", w, ".csv"))
  weekly.clust <- weekly.clust[,c(1,16)]
  colnames(weekly.clust) <- c("STUDENT_ID", "W_CLUST_NUM")
  weekly.clust$WEEK <- w
  weekly.clust <- weekly.clust[,c(1,3,2)]
  clust.data <- rbind(clust.data, weekly.clust)
}
str(clust.data)

## map weekly clusters to cluster labels (interpretations) A-F
clust.data$W_CLUST_AF <- clust.data$W_CLUST_NUM
# week 2
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==2] <- 'A'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==2] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==2] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==2] <- 'B1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==5 & clust.data$WEEK==2] <- 'C1'
# week 3
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==3] <- 'A'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==3] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==3] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==3] <- 'B2'
# week 4
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==4] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==4] <- 'C2'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==4] <- 'B1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==4] <- 'A'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==5 & clust.data$WEEK==4] <- 'B2'
# week 5
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==5] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==5] <- 'B1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==5] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==5] <- 'A'
# week 6
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==6] <- 'C2'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==6] <- 'C1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==6] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==6] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==5 & clust.data$WEEK==6] <- 'A'
# week 7
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==7] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==7] <- 'B1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==7] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==7] <- 'C1'
# week 8
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==8] <- 'B1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==8] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==8] <- 'B2'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==8] <- 'A'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==5 & clust.data$WEEK==8] <- 'D'
# week 9
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==9] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==9] <- 'A'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==9] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==9] <- 'B1'
# week 10
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==10] <- 'B1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==10] <- 'A'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==10] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==10] <- 'B2'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==5 & clust.data$WEEK==10] <- 'E'
# week 11
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==11] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==11] <- 'B1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==11] <- 'F'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==11] <- 'E'
# week 12
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==12] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==12] <- 'F'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==12] <- 'B1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==12] <- 'B2'
# week 13
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==1 & clust.data$WEEK==13] <- 'A'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==2 & clust.data$WEEK==13] <- 'E'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==3 & clust.data$WEEK==13] <- 'D'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==4 & clust.data$WEEK==13] <- 'C1'
clust.data$W_CLUST_AF[clust.data$W_CLUST_AF==5 & clust.data$WEEK==13] <- 'C2'

## add cluster assignments to the trace data
traces$WEEKLY_CLUST_NUM <- vector(length = nrow(traces), mode = "numeric")
traces$WEEKLY_CLUST_AF <- vector(mode = "character", length = nrow(traces))
stud.ids <- unique(traces$STUDENT_ID)
for(i in 1:length(stud.ids)) {
  for(j in 2:13) {
    traces$WEEKLY_CLUST_NUM[traces$STUDENT_ID==stud.ids[i] & traces$WEEK==j] <- 
        clust.data$W_CLUST_NUM[clust.data$STUDENT_ID==stud.ids[i] & clust.data$WEEK==j]
    traces$WEEKLY_CLUST_AF[traces$STUDENT_ID==stud.ids[i] & traces$WEEK==j] <- 
      clust.data$W_CLUST_AF[clust.data$STUDENT_ID==stud.ids[i] & clust.data$WEEK==j]
  }
}


#################
# ADD HMM STATES
#################

hmm.states <- read.csv(file = "results/HMM_5_states_9_features_(March2016).csv")
str(hmm.states)
hmm.states <- hmm.states[,c(1,2,13)]

traces$HMM_STATE <- rep(x = -1, times=nrow(traces))
for(i in 1:length(stud.ids)) {
  for(j in 2:13) {
    if (length(hmm.states$state[hmm.states$user_id==stud.ids[i] & hmm.states$week==j]) > 0) {
      traces$HMM_STATE[traces$STUDENT_ID==stud.ids[i] & traces$WEEK==j] <- 
        hmm.states$state[hmm.states$user_id==stud.ids[i] & hmm.states$week==j]
    }
  }
}


########################################
# ADD STRATEGIES
# (identified through sequence analysis)
########################################

strat.data <- read.csv(file = "results/all_stud_sequences_4_seq_clusters.csv")
str(strat.data)

## associate clusters with their labels as defined in our paper 
strat.data$cluster <- factor(strat.data$cluster, levels = c(1:4),
                             labels = c("formative_assess", "summative_assess",
                                        "readings", "videos_and_form_assess"))
table(strat.data$cluster)

traces <- merge(x = traces, y = strat.data[,c(2,3)], 
                by.x = "SESSION_ID", by.y = "session.id",
                all.x = T, all.y = T)

colnames(traces)[10] <- "SEQ_STRATEGY"
  
str(traces)
table(traces$SEQ_STRATEGY)
summary(traces$SEQ_STRATEGY)
length(which(is.na(traces$SEQ_STRATEGY)==T))/nrow(traces)
# 0.257

##################
# ADD EXAM SCORES
##################

## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the relevant score
scores <- all.scores[, c('user_id', 'SC_FE_TOT', 'SC_MT_TOT')]
remove(all.scores)

traces$MT_SCORE <- vector(mode = "numeric", length = nrow(traces))
traces$FE_SCORE <- vector(mode = "numeric", length = nrow(traces))
for(i in 1:length(stud.ids)) {
  traces$MT_SCORE[traces$STUDENT_ID==stud.ids[i]] <- scores$SC_MT_TOT[scores$user_id==stud.ids[i]]
  traces$FE_SCORE[traces$STUDENT_ID==stud.ids[i]] <- scores$SC_FE_TOT[scores$user_id==stud.ids[i]]
}
str(traces)


#############################################################################
# ADD TIME MANAGEMENT INFO:
# a student's learning action can be qualified as:
# 'preparing' - student is working on the topic of the given week's lecture, 
#               meaning that he/she is preparing for the week's lecture
# 'revisiting' - student is working on a topic from one of the previous weeks
#               that he/she has previously worked on
# 'catching-up' - student is working on a topic from one of the previous weeks
#               for the first time (didn't work on the topic when it was scheduled)
# 'ahead'- student is working on a topic from one of the following weeks
#############################################################################

## associate each event with the week the event's topic refers to
## this initial value is to help identify events where no topic info
## is available and therefore study mode cannot be determined
traces$topic.week <- -20
traces$topic.week[traces$TOPIC=='CST'] <- 1
traces$topic.week[traces$TOPIC=='COD'] <- 2
traces$topic.week[traces$TOPIC=='DRM'] <- 3
traces$topic.week[traces$TOPIC=='CDL'] <- 4
traces$topic.week[traces$TOPIC=='SDL'] <- 5
traces$topic.week[traces$TOPIC=='ARC'] <- 7
traces$topic.week[traces$TOPIC=='ISA'] <- 8
traces$topic.week[traces$TOPIC=='ASP'] <- 9
traces$topic.week[traces$TOPIC=='ADM'] <- 10
## HLP is the topic of weeks 11 and 12
traces$topic.week[traces$TOPIC=='HLP'] <- 11.5 

table(traces$topic.week)

## the difference between the week for which the topic is scheduled
## and the week when topic-related event took place
traces$week.diff <- traces$topic.week - traces$WEEK
traces$week.diff[1:30]
# all values <11 should be ignored 

## add the study mode attribute
traces$STUDY_MODE <- vector(mode = "character", length = nrow(traces))
## student's events related to the week's topic - >student is preparing for the week's lecture
traces$STUDY_MODE[abs(traces$week.diff) <= 0.5] <- 'preparing'
## student is 'revisiting' the topic, that is doing a topic-related activity in some week
## after the week for which the topic was scheduled 
traces$STUDY_MODE[traces$week.diff <= -1 & traces$week.diff >= -11] <- 'revisiting'
## student is working ahead (on a topic from one of the following weeks)
traces$STUDY_MODE[traces$week.diff >= 1] <- 'ahead'

table(traces$STUDY_MODE)
round(prop.table(table(traces$STUDY_MODE)), digits = 4)

## examine 'revisiting' in more detail and differentiate between 'revisiting' and 
## 'catching up with the course' 
revisits <- subset(traces, STUDY_MODE == "revisiting")
catching.up.indices <- vector()
k <- 1
for(i in 1:nrow(revisits)) {
  stud.id <- revisits$STUDENT_ID[i]
  week <- revisits$WEEK[i]
  topic <- revisits$TOPIC[i]
  # check if the student had activities related to the give topic in some of the previous weeks
  earlier.events <- traces[traces$STUDENT_ID==stud.id & traces$TOPIC==topic & traces$WEEK < week, ]
  if ( nrow(earlier.events) == 0 ) {
    catching.up.indices[k] <- row.names(revisits[i,])
    k <- k + 1
  }
}

## substitute the study.mode attribute of the identified catching up events 
## (those with the indices in catching.up.indices) with "catching-up"
traces$STUDY_MODE[ row.names(traces) %in% catching.up.indices ] <- "catching-up"
table(traces$STUDY_MODE)
traces$STUDY_MODE[traces$STUDY_MODE==""] <- "NA"
traces$STUDY_MODE <- factor(traces$STUDY_MODE)


#####################################################
# do the final 'polishing' of the dataset and save it
#####################################################

str(traces)
## remove columns that are no longer needed
traces <- traces[,-c(13,14)]
## reorder columns slightly
traces <- traces[,c(2,1,3:10,13,11,12)]

## turn nominal variables into factors  
traces$ACTIVITY <- factor(traces$ACTIVITY)
traces$TOPIC[traces$TOPIC=="-"] <- NA
traces$TOPIC <- factor(traces$TOPIC)
traces$WEEKLY_CLUST_AF <- factor(traces$WEEKLY_CLUST_AF)
traces$STUDY_MODE[traces$STUDY_MODE=="NA"] <- NA
traces$STUDY_MODE <- factor(traces$STUDY_MODE)

colnames(traces)[4] <- "LACTION_TYPE"

head(traces)
str(traces)

saveRDS(traces, file = "Intermediate_files/ENA_dataset.RData")
write.csv(x = traces, file = "results/dataset_for_ENA.csv", row.names = F, quote = F)


############################
# add learning trajectories
############################

traces <- read.csv(file="results/dataset_for_ENA_Include_STRATG.csv")
str(traces)

lca.results <- read.csv(file = "results/lca_w2_to_w13_6classes(April2016).csv")
str(lca.results)
traces$TIMESTAMP <- as.POSIXct(traces$TIMESTAMP, format = "%d/%m/%Y %H:%M")

traces.extended <- merge(x = traces, y = lca.results[,c(1,14)], 
                         by.x = 'STUDENT_ID', by.y = 'user_id',
                         all.x = T, all.y = F)

sum(table(traces.extended$lca6))

colnames(traces.extended)[38] <- 'TRAJECT_GROUP'

write.csv(x = traces.extended,
          file = "results/dataset_for_ENA_Include_STRATG_TRAJECT.csv", 
          quote = F, row.names = F)
