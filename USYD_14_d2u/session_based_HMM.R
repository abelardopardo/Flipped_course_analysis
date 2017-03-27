######################################################################################
## Create HMM for the entire course, using student-session as the unit of analysis,
## that is, by examining students learning actions at the session level (instead of 
## the week level as done previously)
## Identify sessions based on the time gap between consecutive learning actions
## For each session, compute the following features:
## - FA_PERC - percentage (ratio) of formative assessment actions within the study session
## - FA_CO_PERC - percentage (or ratio) of correct formative assessment actions 
## - FA_SR_PERC - percentage (or ratio) of formative assessment actions that were requests 
##                for the display of the solution (answers) 
## - SA_PERC - percentage (ratio) of summative assessment actions within the study session 
## - SA_CO_PERC - percentage (or ratio) of correct summative assessment actions  
## - VID_PERC - percentage (ratio) of video play actions within the study session 
## - READ_PERC - percentage (ratio) of reading (content access) actions within the study session 
## - METACOG_PERC - percentage (ratio) of metacognitive (dashboard + orientation) actions 
##                  within the study session
##
## Consider that summative assessment (SA) actions that were done in either 'revisiting' 
## or 'catching-up' study mode served, in fact, as formative assessment (FA) and
## count them as FA actions
##
## Do not consider the data from weeks 6 and 13 as those are the weeks when students prepare
## for the exams and their behaviour deviates from the regular study behaviour
#######################################################################################

traces <- read.csv(file = "Intermediate_files/study_mode_weeks2-13_v01-02-2017.csv",
                   stringsAsFactors = F)
str(traces)

## remove data from weeks 6 and 13
traces <- subset(traces, WEEK %in% c(2:5,7:12))
table(traces$WEEK)
require(knitr)
kable(t(table(traces$WEEK)), format = 'pandoc')

## transform the timestamp into format suitable for date-based comparison and sorting
traces$TIMESTAMP <- as.POSIXct(traces$TIMESTAMP)

table(traces$ACTIVITY)
## substitute DBOARD_ACCESS with MC_EVAL, and ORIENT with MC_ORIENT
traces$ACTIVITY[traces$ACTIVITY=="DBOARD_ACCESS"] <- "MC_EVAL"
traces$ACTIVITY[traces$ACTIVITY=="ORIENT"] <- "MC_ORIENT"

## rename MCQ_CO to FA_CO (FA = Formative Assessment), also
## rename MCQ_IN to FA_IN, and MCQ_SR to FA_SR 
traces$ACTIVITY[traces$ACTIVITY=="MCQ_CO"] <- "FA_CO"
traces$ACTIVITY[traces$ACTIVITY=="MCQ_IN"] <- "FA_IN"
traces$ACTIVITY[traces$ACTIVITY=="MCQ_SR"] <- "FA_SR"

## rename EXE_CO to SA_CO (SA = Summative Assessment), and EXE_IN to SA_IN
traces$ACTIVITY[traces$ACTIVITY=="EXE_CO"] <- "SA_CO"
traces$ACTIVITY[traces$ACTIVITY=="EXE_IN"] <- "SA_IN"

table(traces$ACTIVITY)

## now, those summative assessment (SA) items that were done in either 'revisiting' or 'catching-up' 
## study mode served, in fact, as formative assessment (FA); so, turn them into FA items
length(traces$ACTIVITY[traces$ACTIVITY=="SA_CO" & 
                  traces$STUDY_MODE %in% c("revisiting", "catching-up")])
# 16675 - 30.86% of all SA_CO items
traces$ACTIVITY[traces$ACTIVITY=="SA_CO" & 
                  traces$STUDY_MODE %in% c("revisiting", "catching-up")] <- "FA_CO"

length(traces$ACTIVITY[traces$ACTIVITY=="SA_IN" & 
                         traces$STUDY_MODE %in% c("revisiting", "catching-up")])
# 22790 such items - 28.97% of all SA_IN items
traces$ACTIVITY[traces$ACTIVITY=="SA_IN" & 
                  traces$STUDY_MODE %in% c("revisiting", "catching-up")] <- "FA_IN"

table(traces$ACTIVITY)
round(prop.table(table(traces$ACTIVITY)), digits = 4)

## sort the data based on the 1) student, 2) timestamp
sorted.traces <- traces[ with(traces, order(RESOURCE_ID, TIMESTAMP)), ]
head(sorted.traces)

## turn activity to factor variable
sorted.traces$ACTIVITY <- factor(sorted.traces$ACTIVITY)

## rename the variables
colnames(sorted.traces)[1:4] <- c("SESSION.ID", "STUDENT.ID", "TIMESTAMP", "ACTION")

## compute the timegap between consecutive actions of the same student
sorted.traces <- compute.action.timegap(sorted.traces)
summary(sorted.traces$TIME_GAP)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0       4       8    4802      34  604700     990 
quantile(sorted.traces$TIME_GAP, probs = seq(0.9, 1, 0.025), na.rm = T)
#   90%     92.5%       95%     97.5%      
# 188.00    335.00    768.00  16672.45 

## use the time gap of 12.8 min (95th percentile) as the 'session delimiter'
## split events into sessions
session.id <- 0
for(i in 1:nrow(sorted.traces)) {
  if ( is.na(sorted.traces$TIME_GAP[i]) || sorted.traces$TIME_GAP[i] >= 768 )
    session.id <- session.id + 1
  sorted.traces$SESSION.ID[i] <- session.id
}
length(unique(sorted.traces$SESSION.ID))
# 13403 (with week 6 and 13 sessions and 900s as the threshold for the session length, 
# there were 16089 sessions)  

## get the session count for each student
session.count <- get.session.count(sorted.traces)
summary(session.count) 
less.than.5 <- which(session.count < 5)
## get id of students with < 5 sessions
low.session.stud <- unique(sorted.traces$STUDENT.ID)[less.than.5]
## remove students with < 5 sesssion (2 students)
sorted.traces <- subset(sorted.traces, !(STUDENT.ID %in% low.session.stud))

## compute session count again
session.count <- get.session.count(sorted.traces)
summary(session.count) 
## check for outliers
boxplot(session.count)
out.values <- boxplot.stats(session.count)$out
# 7 outliers; will remove them
out.indices <- which(session.count %in% out.values)
stud.ids <- unique(sorted.traces$STUDENT.ID)[-out.indices]

## remove the traces of the student outliers
sorted.traces <- subset(sorted.traces, STUDENT.ID %in% stud.ids )
## re-compute the session count (after the removal of outliers)
session.count <- get.session.count(sorted.traces)
summary(session.count)  
boxplot(session.count)
# again one outlier
out.value <- boxplot.stats(session.count)$out
out.index <- which(session.count==out.value)
# remove the outlier (student)
stud.ids <- stud.ids[-out.index]
sorted.traces <- subset(sorted.traces, STUDENT.ID %in% stud.ids )
session.count <- get.session.count(sorted.traces)
summary(session.count)  
boxplot(session.count)
# now it's fine

########################
## COMPUTE THE FEATURES
########################

## features are computed for each student and each session
## create a data frame with the feature values
stud.sessions <- list()
for(s in 1:length(stud.ids)) {
  stud.sessions[[s]] <- unique(sorted.traces$SESSION.ID[sorted.traces$STUDENT.ID==stud.ids[s]])
}

features.data <- data.frame()
for(s in 1:length(stud.sessions)) {
  session.features.df <- compute.stud.session.features(stud.sessions[[s]])
  session.features.df$STUDENT_ID <- stud.ids[s]
  features.data <- as.data.frame(rbind(features.data, session.features.df))
}
str(features.data)
features.data <- features.data[,c(10,1:9)]
head(features.data)

## add the ntimes attribute to the feature set
## (requied for the depmixS4)
ntimes.vector <- compute.ntimes(features.data)
features.data <- add.ntimes.feature(features.data, ntimes.vector)
str(features.data)

## discretize the features, as depmix cannot work with the feature values in the present form
#install.packages("infotheo")
library(infotheo)

discret.features <- as.data.frame(apply(X = features.data[,c(3:10)], MARGIN = 2, 
                                        FUN = discretize, disc = "equalwidth", nbins = 10))
colnames(discret.features) <- c('FA_PERC', 'FA_CO_PERC', 'FA_SR_PERC', 'SA_PERC', 
                                'SA_CO_PERC', 'VID_PERC', 'READ_PERC', 'METACOG_PERC')
discret.features <- as.data.frame(cbind(features.data[,c(1,2,11)], discret.features))
str(discret.features)
discret.features <- discret.features[,c(1,2,4:11,3)]

## those feature values that are originaly zero should be set to zero also in the discretized dataset
for(i in 1:nrow(discret.features)) {
  for(j in 3:10) { # features are in the columns 3:10
    if (features.data[i,j]==0) discret.features[i,j] <- 0
  }
}
head(discret.features)

## turn all the features into factor variables
final.features <- as.data.frame(apply(X = discret.features[,c(3:10)], MARGIN = 2, FUN = factor))
final.features <- as.data.frame(cbind(discret.features[,c(1,2,11)], final.features))
final.features <- final.features[,c(1,2,4:11,3)]
str(final.features)
## the feature FA_SR_PERC has 10 instead of 11 levels; should be expanded
final.features$FA_SR_PERC <- factor(final.features$FA_SR_PERC, 
                                    levels = c(levels(final.features$FA_SR_PERC), "9"))

## save the feature set
saveRDS(object = final.features, file = "Intermediate_files/multinom_features_for_session_based_HMM.RData")

########################
## FIT THE HMM MODEL
########################

#install.packages("depmixS4")
library(depmixS4)

compare.models(final.features, max.ns = 7)

# ========  ========  ========  =========
# n.states       AIC       BIC     logLik
# ========  ========  ========  =========
#        2  198139.5  199334.6  -98908.75
#        3  186920.3  188738.9  -93215.15
#        4  177905.8  180362.8  -88621.90 *** lowest AIC, BIC, LogLike
#        5  170936.0  174046.3  -85049.02
#        6  168605.1  172383.4  -83793.54
#        7  162777.3  167238.5  -80787.63
# ========  ========  ========  =========

mod.4s <- depmix(response = list(FA_PERC ~ 1, FA_CO_PERC ~ 1, FA_SR_PERC ~ 1,
                              SA_PERC ~ 1, SA_CO_PERC ~ 1, VID_PERC ~ 1, 
                              READ_PERC ~ 1, METACOG_PERC ~ 1),
              data = final.features, 
              nstates = 4, 
              family = list(multinomial("identity"), multinomial("identity"), multinomial("identity"), 
                            multinomial("identity"), multinomial("identity"), multinomial("identity"),
                            multinomial("identity"), multinomial("identity")))

mod.fit.4s <- fit(mod.4s, verbose = T)
summary(mod.fit.4s)

## get the estimated state for each observation 
estimates <- posterior(mod.fit.4s)
str(estimates)
# add the estimated states to the features set and save it
features.and.states <- as.data.frame(cbind(final.features, estimates))
str(features.and.states)
write.csv(x = features.and.states[,c(1:10,12:16)], 
          file = "results/session_based_HMM_4_states(Feb2017).csv", 
          quote = F, row.names = F)

#############################
## COMPUTE STATES PER STUDENT
#############################

features.and.states <- read.csv(file = "results/session_based_HMM_4_states(Feb2017).csv")

n.stud <- length(unique(features.and.states$STUDENT_ID))
stud.states <- data.frame(STUDENT_ID=unique(features.and.states$STUDENT_ID),
                          ST1_CNT=rep(x = 0, times=n.stud),
                          ST2_CNT=rep(0, n.stud),
                          ST3_CNT=rep(0, n.stud),
                          ST4_CNT=rep(0, n.stud))
for(i in 1:n.stud) {
  stud.data <- subset(features.and.states, STUDENT_ID==stud.states$STUDENT_ID[i])
  stud.states$ST1_CNT[i] <- nrow(stud.data[stud.data$state==1,])
  stud.states$ST2_CNT[i] <- nrow(stud.data[stud.data$state==2,])
  stud.states$ST3_CNT[i] <- nrow(stud.data[stud.data$state==3,])
  stud.states$ST4_CNT[i] <- nrow(stud.data[stud.data$state==4,])
}
str(stud.states)

colSums(stud.states[,c(2:5)])
# ST1_CNT ST2_CNT ST3_CNT ST4_CNT 
#  4937    3790    2119    1524 
round(prop.table(colSums(stud.states[,c(2:5)])), digits = 3)
# ST1_CNT ST2_CNT ST3_CNT ST4_CNT 
#  0.399   0.306   0.171   0.123 

## transform the stud.states df (from wide) to long format 
## so that a plot can be created
## based on: http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
require(tidyr)
stud.states$STUDENT_ID <- as.factor(stud.states$STUDENT_ID)
stud.states.long <- gather(data = stud.states, key = state, 
                           value = freq, ST1_CNT:ST4_CNT, factor_key=TRUE)
str(stud.states.long)

## write this df to a file, it will be needed later for visualization
saveRDS(stud.states.long, file = "Intermediate_files/student_4HMM_state_distribution_long.RData")

## make a plot
library(ggplot2)
ggplot(data=stud.states.long, aes(x=STUDENT_ID, y=freq, fill=state)) +
  geom_bar(stat="identity") +
  ylab("Number of sessions") + xlab("Students") +
  scale_fill_manual(values = c("#0072B2","#56B4E9", "#009E73", "#F0E442"), # "#e41a1c", "#E69F00"
                    breaks=c("ST1_CNT", "ST2_CNT", "ST3_CNT", "ST4_CNT"),
                    labels=c("S1", "S2", "S3", "S4")) +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limits = c(0,105)) +
  theme_bw() +
  theme(panel.border = element_blank(),
  		  legend.title = element_text(size=11, face="bold"),
        legend.text = element_text(size = 10),
        legend.key = element_rect(colour = NA))


## create a df with percentages of states for each student  
stud.states.perc <- stud.states
for(r in 1:n.stud) {
  stud.states.perc[r,c(2:5)] <- stud.states.perc[r,c(2:5)]/sum(stud.states.perc[r,c(2:5)])
}
head(stud.states.perc)
rowSums(stud.states.perc[,c(2:5)])
colnames(stud.states.perc) <- c("STUDENT_ID", "ST1_PERC", "ST2_PERC", "ST3_PERC", "ST4_PERC")

stud.states.perc.long <- gather(data = stud.states.perc, key = state,
                                value = percent, ... = ST1_PERC:ST4_PERC, factor_key = T)
head(stud.states.perc.long)
## write this df to a file, it will be needed later for visualization
saveRDS(stud.states.perc.long, file = "Intermediate_files/student_4HMM_state_perc_dist_long.RData")

## make a plot
ggplot(data=stud.states.perc.long, aes(x=STUDENT_ID, y=percent, fill=state)) +
  geom_bar(stat="identity") +
  ylab("Percent of student sessions") + 
  xlab("Students") +
  scale_fill_manual(values = c("#0072B2","#56B4E9", "#009E73", "#F0E442"), # "#e41a1c", "#E69F00"
                    breaks=c("ST1_PERC", "ST2_PERC", "ST3_PERC", "ST4_PERC"),
                    labels=c("S1", "S2", "S3", "S4")) +
  scale_x_discrete(breaks=NULL) +
  theme_bw() +
  theme(panel.border = element_blank(),
  		  legend.title = element_text(size=11, face="bold"),
        legend.text = element_text(size = 10),
        legend.key = element_rect(colour = NA))


#########################################################
# SEQUENCE ANALYSIS: 
# CLUSTERING OF STUDENTS BASED ON THEIR STATE SEQUENCES
#
# each student will be represented with the sequence of
# states that correspond to the sequence of their learning 
# sessions (states are identified using HMM, and each state 
# corresponds to one learning session)
##########################################################

######################################################
## prepare the data in the format required by TraMineR
######################################################

## create sequences in such a way that each sequence corresponds to one student;
## each sequence is a sequence of (HMM) states, where each state corresponds to 
## a learning session within the course; so, the whole sequence for one student
## represents the (HMM) states he/she "has been through" during the course 
## sequences should be of the form:  
## ST1/3 ST1/2 ST4/1 ...
## if the student's sequence was ST1-ST1-ST1-ST2-ST2-ST4-...
## in general, a sequence (student) should be represented as a vector of elements 
## <STATE/OCCURRENCE_CNT>

## get the data:
src.data <- read.csv(file = "results/session_based_HMM_4_states(Feb2017).csv")
str(src.data)

## before formatting the data for sequence analysis, it would be good to examine
## the sequences (of students' study sessions) to identify the presence of outliers
## (ie. overly long or short sequences)

stud.ids <- unique(src.data$STUDENT_ID)
n.stud <- length(stud.ids)
session.count <- list()
for(s in 1:n.stud) {
  s.cnt <- nrow(src.data[src.data$STUDENT_ID==stud.ids[s],])
  session.count[[s]] <- c(stud.ids[s],s.cnt)
}
session.count.df <- as.data.frame(matrix(unlist(session.count), 
                                         nrow = n.stud, ncol = 2, byrow = T))
colnames(session.count.df) <- c("STUDENT_ID", "SESSION_CNT")
remove(session.count)

summary(session.count.df$SESSION_CNT)
boxplot(session.count.df$SESSION_CNT)
# no outliers
quantile(session.count.df$SESSION_CNT, probs = seq(0, 0.1, 0.01))
# check the number of students with less than 10 sessions (3rd perc = 9.37) 
which(session.count.df$SESSION_CNT < 10)
# 9 of them
quantile(session.count.df$SESSION_CNT, probs = seq(0.9, 1, 0.01))
# check the number of students with more than than 89 sessions (97th percentile) 
which(session.count.df$SESSION_CNT > 89)
# 8 of them
## remove those with less than 10 (3rd perc.) or more than 89 (97th perc)
## so, in total, 6% of students are removed
stud.to.remove <- session.count.df$STUDENT_ID[(session.count.df$SESSION_CNT < 10) | 
                                                (session.count.df$SESSION_CNT > 89)]
# 17 students to remove
session.count.df <- subset(session.count.df, !(STUDENT_ID %in% stud.to.remove))
## remove session data related to the removed students
session.data <- subset(src.data, STUDENT_ID %in% session.count.df$STUDENT_ID)
length(setdiff(stud.ids, unique(session.data$STUDENT_ID)))
# 17 students removed, which is correct
## re-compute stud.ids and n.stud as might be needed later
stud.ids <- unique(session.data$STUDENT_ID)
n.stud <- length(stud.ids)
# working with 263 students (90.7% of the initial number (290))

## keep only the data required for creating sequences
session.data <- session.data[,c(1,2,11)]

## for each student, create state sequences of the form:  
## ST1/3 ST1/2 ST4/1 ... 
sequence.list <- list()
for(i in 1:n.stud) {
  stud.states <- subset(session.data, STUDENT_ID==stud.ids[i], select = 3)
  stud.states <- as.vector(stud.states$state)
  stud.states.new <- vector()
  current.state <- stud.states[1]
  st.cnt <- 1
  for(j in 2:length(stud.states)) {
    if (current.state==stud.states[j]) {
      st.cnt <- st.cnt + 1
    } else {
      stud.states.new <- c(stud.states.new, paste0(current.state,"/",st.cnt))
      current.state <- stud.states[j]
      st.cnt <- 1
    }
    if (j == length(stud.states)) 
      stud.states.new <- c(stud.states.new, paste0(current.state,"/",st.cnt))
  }
  sequence.list[[i]] <- stud.states.new
}

## find the sequence with the max length, where length is  
## considered in terms of the <STATE/OCCURRENCE_CNT> elements
## this is required for making all the sequences of the samelength 
seq.length <- sapply(X = sequence.list, FUN = function(x) length(x))
summary(seq.length)
quantile(seq.length, probs=seq(from = 0.9, to = 1, by = 0.025))

## make a list where all the sequences of the same length 
## (required for the TraMineR specific format)
equal.len.seq.df <- create.equal.length.seq(sequence.list, max(seq.length))
## add student ids
equal.len.seq.df <- as.data.frame(cbind(STUDENT_ID=stud.ids, equal.len.seq.df))

## create sequence out of the data frame
require(TraMineR)
require(RColorBrewer)
traminer.seq <- seqdef(data = equal.len.seq.df, var = 2:ncol(equal.len.seq.df), 
                       informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"),
                       cpal = c("#0072B2","#56B4E9", "#009E73", "#F0E442"))
print(traminer.seq[1:10, 1:30], format = 'SPS')

## store the sequence data
saveRDS(object = traminer.seq, 
        file = "Intermediate_files/state_sequence_data_SPS_format.RData")
## store the created sequences
saveRDS(object = equal.len.seq.df, 
        file = "Intermediate_files/state_sequence_data_df.RData")


########################################
## do sequence analysis (clustering)
########################################

traminer.seq <- readRDS(file = "Intermediate_files/state_sequence_data_SPS_format.RData")

## first, examine the sequences using different diagrams

## index plot of all the sequences
png(file = "graphics/seqiplot_all_student_sequences.png", width = 1800, height = 1500, pointsize = 50)
seqiplot(traminer.seq, title = "Entire student population (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(traminer.seq, title="Sequences of student states throughout the course", 
         withlegend=F, axes=F)
seqlegend(traminer.seq, fontsize = 0.65)


## first, compute dissimilarities among clusters using the Optimal Matching method
## to compute the dissimilarities, we need the substitution-cost matrix
## since we have transition probabilities from one state to the next - obtained 
## from the HMM method - we can use these to compute the substitution costs;
## using the following formula (from TraMineR documentation):
## SC(i,j) = cval - p(i,j) - p(j,i)
## where cval is, by default, equal to 2 

state.trans.matrix <- matrix(data = c(0.487, 0.264, 0.144, 0.105,
                                      0.331, 0.352, 0.161, 0.156,
                                      0.352, 0.314, 0.233, 0.101,
                                      0.350, 0.317, 0.198, 0.135),
                             nrow = 4, ncol = 4, byrow = T,
                             dimnames = list(seq(1,4,1), seq(1,4,1))) 
state.trans.matrix
cost.matrix <- matrix(data = rep(2,16), nrow = 4, ncol = 4, byrow = T)
for(i in 1:nrow(cost.matrix)) {
  for(j in 1:ncol(cost.matrix))
    if (i==j) 
      cost.matrix[i,j] <- 0  ## all elements on the diagonal must be zero (which makes sense)
    else
      cost.matrix[i,j] <- cost.matrix[i,j] - state.trans.matrix[i,j] - state.trans.matrix[j,i]
}
cost.matrix
#submat <- seqsubm(seqdata = traminer.seq, method = "TRATE")
## normalize the computed distances to account for differences in sequence lengths
#dist.om1 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T)
dist.om1.5 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 1.5)
#dist.om2 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 2)

require(cluster)
set.seed(1222017)
## IMPORTANT: clustering is done using OM-based distances with indel_cost = 1.5, and substitution matrix
## derived from the HMM state transition matrix
seq.ward <- agnes(dist.om1.5, diss = T, method = "ward")
png(file = "graphics/seq.ward_dist.om1.5.png", width = 1800, height = 1500, pointsize = 50)
plot(seq.ward)
dev.off()

## check the solution with 5 clusters
cl5 <- cutree(seq.ward, k = 5)
table(cl5)
## get the cluster assignment for each sequence
cl5.fac <- factor(cl5, labels = paste("cl:",1:5))
## plot the state distribution at each time point for each cluster
seqdplot(seqdata = traminer.seq, group = cl5.fac, 
         title="State distribution", withlegend=F)

## check the solution with 4 clusters
cl4 <- cutree(seq.ward, k = 4)
table(cl4)
## get the cluster assignment for each sequence
cl4.fac <- factor(cl4, labels = paste("cl:",1:4))
## plot the state distribution for each cluster
seqdplot(seqdata = traminer.seq, group = cl4.fac, #axes = "bottom", cex.plot = 0.5, 
         title="State distribution", withlegend=F)
seqlegend(traminer.seq, fontsize = 0.65)

## create a new data frame with one row for each student and the following columns: 
## 1) student id
## 2) the cluster the student is assigned to in 5 cluster model 
## 3) the cluster the student is assigned to in 4 cluster model 
student.clusters <- data.frame(STUDENT_ID=equal.len.seq.df$STUDENT_ID)
student.clusters$cl5 <- as.factor(cl5)
student.clusters$cl4 <- as.factor(cl4)
str(student.clusters)
## save the student cluster assignments 
write.csv(student.clusters, file = "results/seq_of_HMM_states_student_clusters_dist_1.5.csv", 
          quote = F, row.names = F)


##########################################################
## compute the level of discrepancy for each seq. cluster
## 
## Discrepancy is defined as an extension of the concept 
## of variance to any kind of objects for which  
## pairwise dissimilarities can be computed 
##########################################################

## compute for the 4 cluster solution
clust4.dissvar <- vector()
for(c in 1:4) {
  cl.diss <- seqdist(seqdata = traminer.seq[cl4==c,],
                     method = "OM", sm = cost.matrix, indel = 1.5)
  clust4.dissvar[c] <- dissvar(diss = cl.diss)
}
round(clust4.dissvar, digits = 2)
# compare the discrepancy with the size of the clusters
table(cl4)

## now, compute for the 5 cluster solution
clust5.dissvar <- vector()
for(c in 1:5) {
  cl.diss <- seqdist(seqdata = traminer.seq[cl5==c,],
                     method = "OM", sm = cost.matrix, indel = 1.5)
  clust5.dissvar[c] <- dissvar(diss = cl.diss)
}
round(clust5.dissvar, digits = 2)
table(cl5)


###############################################################
## identify and plot representative sequences for each cluster
###############################################################

## SKIPPING THIS FOR NOW

## notes regarding the representative sequence (rseq) plot:
# - width of a rseq is proportional to the number of sequences assigned to that rseq
# - sequences are plotted bottom-up according to their representativeness score
# - coverage is the percentage of sequences assigned to one of the rseq (ie. sequences that are 
#   in the neighbourhood of the rseq, where the border of the neighbourhood is determined by the
#   specified threshold tsim); min coverage is specified via the trep parameter  
# - the axis A of the horizontal line above the plot represents for each rseq the (pseudo) 
#   variance within the subset of sequences assigned to that rseq 
# - the axis B represents the mean distance between each rseq and sequences assigned to that rseq
# - the horizontal line (related to the A and B axes) ranges from 0 to the 
#   max. theoretical distance (the formula for its computation is in the paper)

par(mfrow=c(1,1))
for(c in 1:4) {
  cl.diss <- seqdist(seqdata = traminer.seq[cl4==c,],
                     method = "OM", sm = cost.matrix, indel = 1)
  seqrplot(seqdata = traminer.seq[cl4==c,], dist.matrix=cl.diss, 
          criterion="density", tsim=0.2, trep=0.25,
          #criterion="dist", nrep=1, tsim=0.25,
          title = paste("Representative seq. for cluster", c), 
          withlegend=F)
}


#####################################################
## examine the identified clusters by comparing them
## based on students' exam (midterm and final) scores
#####################################################

source(file = "util_functions.R")

## get the students' scores on the final exam
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the relevant score
scores <- all.scores[, c('user_id', 'SC_FE_TOT', 'SC_MT_TOT')]
remove(all.scores)

setdiff(student.clusters$STUDENT_ID, scores$user_id)
## scores are available for all the students

student.clusters <- merge(x = student.clusters, y = scores,
                          by.x = "STUDENT_ID", by.y = "user_id",
                          all.x = T, all.y = F)
str(student.clusters)


#####################################
## examine first the 4 clusters model
#####################################

## compute the summary statistics for the students' exam scores
stud.cl4.stats <- summary.stats(student.clusters[,c(4,5)], student.clusters$cl4, 4)
kable(x = stud.cl4.stats, format = "rst")

## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl4)
# chi-squared = 37.631, df = 3, p-value = 3.383e-08
## there is a sign. difference bewtween the clusters; check where the difference is

## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
pairwise.fexam.compare <- function(nclust, ncomparison, hcl.plus.scores) {
  comparison <- matrix(nrow = ncomparison, ncol = 5, byrow = T)
  k <- 1
  for(i in 1:(nclust-1)) {
    for(j in (i+1):nclust) {
      comparison[k,] <- c(i, j, compare.fexam.scores.Mann.Whitney.test(hcl.plus.scores, i, j))
      k <- k+1
    }
  }
  comparison.df <- as.data.frame(comparison)
  colnames(comparison.df) <- c('c1', 'c2', 'Z', 'p', 'r')
  comparison.df <- apply.FDR.correction(comparison.df)
  comparison.df
}
stud.4cl.df <- student.clusters[,c(1,3:5)]
colnames(stud.4cl.df)[2] <- "class"
kable(pairwise.fexam.compare(4, 6, stud.4cl.df), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl4)
# chi-squared = 33.942, df = 3, p-value = 2.038e-07

## apply Mann-Whitney U Test to do pair-wise comparisons
pairwise.mtxam.compare <- function(nclust, ncomparison, hcl.plus.scores) {
  comparison <- matrix(nrow = ncomparison, ncol = 5, byrow = T)
  k <- 1
  for(i in 1:(nclust-1)) {
    for(j in (i+1):nclust) {
      comparison[k,] <- c(i, j, compare.midterm.scores.Mann.Whitney.test(hcl.plus.scores, i, j))
      k <- k+1
    }
  }
  comparison.df <- as.data.frame(comparison)
  colnames(comparison.df) <- c('c1', 'c2', 'Z', 'p', 'r')
  comparison.df <- apply.FDR.correction(comparison.df)
  comparison.df
}
kable(pairwise.mtxam.compare(4, 6, stud.4cl.df), format = "rst")


#########################################
## now, examine the model with 5 clusters
#########################################

## compute the summary statistics for the students' exam scores
stud.cl5.stats <- summary.stats(student.clusters[,c(4,5)], student.clusters$cl5, 5)
kable(x = stud.cl5.stats, format = "rst")

## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl5)
# chi-squared =  37.638, df = 4, p-value = 1.331e-07
## there is a sign. difference bewtween the clusters; check where the difference is

## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
stud.5cl.df <- student.clusters[,c(1,2,4,5)]
colnames(stud.5cl.df)[2] <- "class"
kable(pairwise.fexam.compare(5, 10, stud.5cl.df), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl5)
# chi-squared = 34.016, df = 4, p-value = 7.395e-07

## apply Mann-Whitney U Test to do pair-wise comparisons
kable(pairwise.mtxam.compare(5, 10, stud.5cl.df), format = "rst")


##################################
## examine the clusters visually
##################################

## SKIPPING THIS PART AS IT DID NOT PROVE VERY USEFUL

stud.state.cnt.long <- readRDS(file = "Intermediate_files/student_4HMM_state_distribution_long.RData")
## merge it with the cluster data
stud.state.cnt.long <- merge(x = stud.state.cnt.long, y = student.clusters[,c(1,3)],
                             by = "STUDENT_ID", all.x = F, all.y = T)
head(stud.state.cnt.long)
stud.state.cnt.long$cl4 <- factor(stud.state.cnt.long$cl4)

## make a plot
library(ggplot2)
ggplot(data=stud.state.cnt.long, aes(x=STUDENT_ID, y=freq, fill=state)) +
  geom_bar(stat="identity") +
  facet_wrap(~cl4) +
  ylab("Number of sessions") + xlab("Students") +
  scale_fill_manual(values = c("#0072B2","#56B4E9", "#009E73", "#F0E442"), # "#e41a1c", "#E69F00"
                    breaks=c("ST1_CNT", "ST2_CNT", "ST3_CNT", "ST4_CNT"),
                    labels=c("S1", "S2", "S3", "S4")) +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limits = c(0,105)) +
  theme_bw() +
  theme(legend.title = element_text(size=11, face="bold"),
        legend.text = element_text(size = 10),
        legend.key = element_rect(colour = NA))


##################################################################
## examine the identified student clusters by comparing them  
## based on proportion of the 4 different types of states 
## (study tactics) in the students' state sequences 
##################################################################
source(file = "util_functions.R")

stud.clusters <- read.csv(file = "results/seq_of_HMM_states_student_clusters.csv")
str(stud.clusters)

state.seq.data <- read.csv(file = "results/session_based_HMM_4_states(Feb2017).csv")
str(state.seq.data)

n.stud <- length(unique(state.seq.data$STUDENT_ID))
stud.states <- data.frame(STUDENT_ID=unique(state.seq.data$STUDENT_ID),
                          ST1_CNT=rep(x = 0, times=n.stud),
                          ST2_CNT=rep(0, n.stud),
                          ST3_CNT=rep(0, n.stud),
                          ST4_CNT=rep(0, n.stud))
for(i in 1:n.stud) {
  stud.data <- subset(state.seq.data, STUDENT_ID==stud.states$STUDENT_ID[i])
  stud.states$ST1_CNT[i] <- nrow(stud.data[stud.data$state==1,])
  stud.states$ST2_CNT[i] <- nrow(stud.data[stud.data$state==2,])
  stud.states$ST3_CNT[i] <- nrow(stud.data[stud.data$state==3,])
  stud.states$ST4_CNT[i] <- nrow(stud.data[stud.data$state==4,])
}
str(stud.states)

stud.states$ST_TOTAL <- apply(X = stud.states[,c(2:5)], MARGIN = 1, FUN = sum) 

stud.states.clust <- merge(x = stud.states, y = stud.clusters, 
                           by = "STUDENT_ID", all.x = F, all.y = T)
str(stud.states.clust)
n.stud <- length(unique(stud.states.clust$STUDENT_ID))

## check if the number of states is normally distributed
apply(X = stud.states.clust[,c(2:5)], MARGIN = 2, FUN = shapiro.test)
# none is normaly distributed

## compare the states in the 4 clusters solution:
clust4.stats <- summary.stats(stud.states.clust[,c(2:6)], stud.states.clust$cl4, 4)
require(knitr)
kable(clust4.stats, format = "rst")

## compare the states in the 5 clusters solution:
clust5.stats <- summary.stats(stud.states.clust[,c(2:6)], stud.states.clust$cl5, 5)
kable(clust5.stats, format = "rst")


## create a df with percentages of states for each student  
stud.states.perc <- stud.states.clust
for(r in 1:n.stud) {
  stud.states.perc[r,c(2:5)] <- stud.states.perc[r,c(2:5)]/sum(stud.states.perc[r,c(2:5)])
}
head(stud.states.perc)
colnames(stud.states.perc)[2:5] <- c("ST1_PERC", "ST2_PERC", "ST3_PERC", "ST4_PERC")

## compare the proportion of states in the 4 clusters solution:
clust4.perc.stats <- summary.stats(stud.states.perc[,c(2:5)], stud.states.perc$cl4, 4)
kable(clust4.perc.stats, format = "rst")

## compare the proportion of states in the 5 clusters solution:
clust5.perc.stats <- summary.stats(stud.states.perc[,c(2:5)], stud.states.perc$cl5, 5)
kable(clust5.perc.stats, format = "rst")

## use statitical tests to compare the clusters with respect to each of the STx_PERC, x=1:4 variables
## check if the variables representing proportions of states are normally distributed
apply(X = stud.states.perc[,c(2:5)], MARGIN = 2, FUN = shapiro.test)
# 2 (ST1_PERC, ST2_PERC) are normally distributed, but for consistency, use Kruskal-Wallis test for all the variables
kruskal.test(stud.states.perc$ST1_PERC ~ stud.states.perc$cl4)
# chi-squared = 165.663, df = 3, p-value = 3.618e-14
summary(aov(stud.states.perc$ST1_PERC ~ stud.states.perc$cl4))
# anova is also significant: p=1.88e-05
kruskal.test(stud.states.perc$ST2_PERC ~ stud.states.perc$cl4)
# chi-squared = 12.283, df = 3, p-value = 0.006475
kruskal.test(stud.states.perc$ST3_PERC ~ stud.states.perc$cl4)
# chi-squared = 88.703, df = 3, p-value < 2.2e-16
kruskal.test(stud.states.perc$ST4_PERC ~ stud.states.perc$cl4)
# chi-squared = 25.01, df = 3, p-value = 1.537e-05


####################
## UTILITY FUNCTIONS
####################

## the f. computes time gap between two subsequent learning actions of each student
## it returns the input dataframe with the new TIME_GAP column added  
compute.action.timegap <- function(weekly.traces) {
  weekly.traces$TIME_GAP <- vector(mode = "numeric", length = nrow(weekly.traces))
  weekly.traces$TIME_GAP[1] <- NA
  current.stud <- weekly.traces$STUDENT.ID[1];
  for(i in 2:nrow(weekly.traces)) {
    if (weekly.traces$STUDENT.ID[i]==current.stud) {
      weekly.traces$TIME_GAP[i] <- difftime(time1 = weekly.traces$TIMESTAMP[i], 
                                            time2 = weekly.traces$TIMESTAMP[i-1], 
                                            units = "secs" )
      ## since there is a week long gap between weeks 5 and 7, if the computed time gap is
      ## longer than 7 days (3600*24*7 = 604800), set it to NA
      if (weekly.traces$TIME_GAP[i] > 604800) 
          weekly.traces$TIME_GAP[i] <- NA
    } else {
      weekly.traces$TIME_GAP[i] <- NA  
      current.stud <- weekly.traces$STUDENT.ID[i]
    }  
  }
  weekly.traces
}

## the f. computes the number of sessions per student
get.session.count <- function(trace.data) {
  stud.ids <- unique(trace.data$STUDENT.ID)
  stud.sessions <- list()
  for(s in 1:length(stud.ids)) {
    stud.sessions[[s]] <- unique(trace.data$SESSION.ID[trace.data$STUDENT.ID==stud.ids[s]])
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
    session.data <- subset(sorted.traces, SESSION.ID==stud.session)
    n.actions <- nrow(session.data)
    n.fa <- nrow(session.data[session.data$ACTION %in% c("FA_CO", "FA_IN", "FA_SR"),])
    n.sa <- nrow(session.data[session.data$ACTION %in% c("SA_CO", "SA_IN"),])
    FA_PERC <- n.fa/n.actions
    FA_CO_PERC <- 0
    FA_SR_PERC <- 0
    if (n.fa > 0) {
      FA_CO_PERC <- nrow(session.data[session.data$ACTION=="FA_CO",])/n.fa
      FA_SR_PERC <- nrow(session.data[session.data$ACTION=="FA_SR",])/n.fa
    } 
    SA_PERC <- n.sa/n.actions
    SA_CO_PERC <- 0
    if (n.sa > 0) {
      SA_CO_PERC <- nrow(session.data[session.data$ACTION=="SA_CO",])/n.sa  
    }
    VID_PERC <- nrow(session.data[session.data$ACTION=="VIDEO_PLAY",])/n.actions
    READ_PERC <- nrow(session.data[session.data$ACTION=="CONTENT_ACCESS",])/n.actions
    METACOG_PERC <- nrow(session.data[session.data$ACTION %in% c("MC_ORIENT", "MC_EVAL"),])/n.actions
    
    feature.data[[s]] <- c(stud.session, FA_PERC, FA_CO_PERC, FA_SR_PERC, SA_PERC, SA_CO_PERC, 
                           VID_PERC, READ_PERC, METACOG_PERC)
  }
  
  feature.matrix <- matrix(data = unlist(feature.data), 
                           nrow = length(feature.data), ncol = 9, byrow = T) 
  feature.df <- as.data.frame(feature.matrix)
  colnames(feature.df) <- c('SESSION_ID', 'FA_PERC', 'FA_CO_PERC', 'FA_SR_PERC', 'SA_PERC', 
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
add.ntimes.feature <- function(feature.data, ntimes.vector) {
 
  seq.lengths <- as.data.frame( cbind(stud.id = unique(feature.data$STUDENT_ID),
                                      length = ntimes.vector))
  feature.data$ntimes <- vector(mode = "integer", length = nrow(feature.data))
  for (i in 1:nrow(feature.data)) {
    feature.data$ntimes[i] <- 
      seq.lengths[ seq.lengths$stud.id == feature.data$STUDENT_ID[i], 2]  
  }
  feature.data
  
}

## the f. computes several HMM models, with ns=>min.ns & ns<=max.ns
## and produces a table with evaluation metrics
compare.models <- function(feature.data, min.ns=2, max.ns=6) {
  set.seed(122017)
  eval.metrics <- data.frame()
  for (ns in min.ns:max.ns) {
    mod <- depmix(response = list(FA_PERC ~ 1, FA_CO_PERC ~ 1, FA_SR_PERC ~ 1,
                                  SA_PERC ~ 1, SA_CO_PERC ~ 1, VID_PERC ~ 1, 
                                  READ_PERC ~ 1, METACOG_PERC ~ 1),
                  data = feature.data, 
                  nstates = ns, 
                  family = list(multinomial("identity"), multinomial("identity"), multinomial("identity"), 
                                multinomial("identity"), multinomial("identity"), multinomial("identity"),
                                multinomial("identity"), multinomial("identity")))
    
    mod.fit <- fit(mod, verbose = FALSE)
    metrics <- c(ns, AIC(mod.fit), BIC(mod.fit), logLik(mod.fit))
    eval.metrics <- as.data.frame( rbind(eval.metrics, metrics) )
  }
  colnames(eval.metrics) <- c('n.states', 'AIC', 'BIC', 'logLik')
  require(knitr)
  kable(x = eval.metrics, format = "rst")
}

## the f. first creates a new sequence list where all sequences will be
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
  seq.df <- data.frame(matrix(unlist(eq.len.sequences), 
                              nrow=length(eq.len.sequences), byrow=T),
                       stringsAsFactors=FALSE)
  seq.df
}