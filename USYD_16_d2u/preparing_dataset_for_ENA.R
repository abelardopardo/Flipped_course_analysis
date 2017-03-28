#############################################################################
## start from the dataset with trace data organized into sessions and with 
## associate time management data (ie study modes derived from student time
## management practices)
#############################################################################
traces <- readRDS(file = "Intermediate_results/study_mode_weeks2-13.RData")
str(traces)

## remove columns that are not needed and re-arrange the rest
traces <- traces[,-8]
traces <- traces[,c(2,1,4,3,5:7)]

table(traces$ACTION)
## in the ACTION column, substitute DBOARD_ACCESS with MC_EVAL
## also ORIENT with MC_ORIENT
traces$ACTION <- as.character(traces$ACTION)
traces$ACTION[traces$ACTION=="DBOARD_ACCESS"] <- "MC_EVAL"
traces$ACTION[traces$ACTION=="ORIENT"] <- "MC_ORIENT"

table(traces$TOPIC)

################################################################################
## ADD STUDY STRATEGIES
## - these are identified by building a HMM using the students’ study sessions 
##   in the entire course; the strategies are the distinct states of the built HMM
## - each learning session (all events within a session) is associate with one 
##   study strategy 
## - strategies are identified for weeks 2-5 and 7-12, that is, weeks 6 and 13 
##   are skipped as those are the weeks when students prepared for the exams and 
##   their behaviour deviates from the regular study behaviour; so, sessions from
##   weeks 6 and 13 do not have associated study strategies 
################################################################################

strat.data <- read.csv(file = "results/session_based_HMM_5_states.csv")
str(strat.data)

## keep only the required data columns
strat.data <- strat.data[,c(1,2,10)]
colnames(strat.data)[3] <- "STUDY_STRATEGY"

## associate clusters with their labels as defined in our paper 
strat.data$STUDY_STRATEGY <- factor(strat.data$STUDY_STRATEGY, levels = c(1:5),
                             labels = paste0("S",1:5))
table(strat.data$STUDY_STRATEGY)

traces <- merge(x = traces, y = strat.data[,c(2,3)], 
                by = "SESSION_ID",
                all.x = T, all.y = F)

table(traces$STUDY_STRATEGY)
summary(traces$STUDY_STRATEGY)
round(prop.table(summary(traces$STUDY_STRATEGY)), digits = 3)
#   S1    S2    S3    S4    S5   NA's 
# 0.044 0.221 0.088 0.037 0.331 0.279 
# the large percentage of NAs primarily originates from the fact that 
# strategies were not identified for weeks 6 and 13


################################################################################
## ADD STRATEGY GROUPS
## these are the clusters that each student is assigned to based on the sequence 
## of learning strategies within the course (where strategies are identified 
## with HMM (the STUDY_STRATEGY variable), and student groups through sequence 
## clustering); so, all events originating from one student will be associtated 
## with the strategy group the student belongs to.
## 
## Strategy groups are identified for 443 (out of 486 students); others were 
## removed as outliers - having either too many or two few study sessions 
################################################################################

strat.groups <- read.csv(file = "results/seq_of_5HMM_states_student_clusters_indel=1.75.csv")

## the solution with 4 clusters proved to be the best one, so, keep only 
## the cl4 variable
strat.groups <- strat.groups[,c(1,3)]
colnames(strat.groups)[2] <- "STRATEGY_GROUP"

traces <- merge(x = traces, y = strat.groups, 
                by.x = "USER_ID", by.y = "STUDENT_ID", all = T)

traces$STRATEGY_GROUP <- factor(traces$STRATEGY_GROUP, levels = c(1:4),
                                labels = c("SG1", "SG2", "SG3", "SG4"))

table(traces$STRATEGY_GROUP)
summary(traces$STRATEGY_GROUP)
round(prop.table(summary(traces$STRATEGY_GROUP)), digits = 3)
#   SG1   SG2   SG3   SG4  NA's 
# 0.279 0.268 0.254 0.092 0.106
# NAs are associated with students who:
# - were active only during weeks 6 and/or 13; or
# - were removed from the analysis as outliers (having either too few or too many 
#   learning sessions); study groups are identified for 443 students (out of 370) 


##########################################
## ADD MIDTERM AND FINAL EXAM SCORES
##
## scores are not available for 9 students
##########################################

## Load the scores data
scores <- read.csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
str(scores)

traces <- merge(x = traces, y = scores,
                by = "USER_ID", all.x = T, all.y = F)
## remove email
traces <- traces[,-10]

summary(traces[,c(10,11)])
## check for how many students scores are unavailable
no.scores <- subset(traces, is.na(SC_FE_TOT)==T)
length(unique(no.scores$USER_ID))
# for 9 students

#####################################################
# do the final 'polishing' of the dataset and save it
#####################################################

str(traces)

## turn nominal variables into factors  
traces$ACTION <- factor(traces$ACTION)

saveRDS(traces, file = "Intermediate_results/ENA_dataset.RData")
#write.csv(x = traces, file = "results/dataset_for_ENA.csv", row.names = F, quote = F)
