######################################################################################
## Create HMM for the entire course, as it was done for the 2015 dataset
## 
## This means using student-session as the unit of analysis, ie. examining students 
## learning actions at the session level
## For each session, compute the following features:
## - FA_PERC - percentage (ratio) of formative assessment actions within the study session
## - FA_CO_PERC - percentage (or ratio) of correct formative assessment actions 
## - SA_PERC - percentage (ratio) of summative assessment actions within the study session 
## - SA_CO_PERC - percentage (or ratio) of correct summative assessment actions  
## - VID_PERC - percentage (ratio) of video play actions within the study session 
## - READ_PERC - percentage (ratio) of reading (content access) actions within the study session 
## - METACOG_PERC - percentage (ratio) of metacognitive (dashboard + orientation) actions 
##                  within the study session
##
## Do not consider the data from weeks 6 and 13 as those are the weeks when students prepare
## for the exams and their behaviour deviates from the regular study behaviour
############################################################################################

traces <- readRDS(file = "Intermediate_results/study_mode_weeks2-13.RData")
str(traces)

## remove data from weeks 6 and 13
traces <- subset(traces, WEEK %in% c(2:5,7:12))
# 23.13% of traces were removed 
table(traces$WEEK)
# significantly higher level of activity in the weeks 2, 3, 4 and 5 than in the subsequent weeks
## print this
require(knitr)
kable(t(table(traces$WEEK)), format = "pandoc")

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
round(prop.table(table(traces$ACTION)), digits = 4)

colnames(traces)[1] <- "STUDENT_ID"
## sort the data based on the 1) student, 2) week, 3) timestamp
sorted.traces <- traces[ with(traces, order(STUDENT_ID, WEEK, TIMESTAMP)), ]
head(sorted.traces)

## turn ACTION to factor variable
sorted.traces$ACTION <- factor(sorted.traces$ACTION)

## examine the presence of sessions with only one event
one.event.sessions <- vector()
event.count <- 1
j <- 1
for(i in 1:(nrow(sorted.traces)-1)) {
  if ( sorted.traces$SESSION_ID[i] == sorted.traces$SESSION_ID[i+1] )
    event.count <- event.count + 1
  else {
    if ( event.count == 1 ) {
      one.event.sessions[j] <- sorted.traces$SESSION_ID[i]
      j <- j + 1
    }
    event.count <- 1
  }
}
length(one.event.sessions)
# 5924
## check the proportion
length(one.event.sessions) / length(unique(sorted.traces$SESSION_ID))
# 19.02% 
## too much to be removed; still should be noted down

## get the session count for each student
session.count <- get.session.count(sorted.traces)
summary(session.count) 
sort(session.count)[1:10]
less.than.5 <- which(session.count < 5)
# 5 students with less than 5 sessions
## get ids of students with < 5 sessions
low.session.stud <- unique(sorted.traces$STUDENT_ID)[less.than.5]
## remove student with < 5 sesssion 
sorted.traces <- subset(sorted.traces, !(STUDENT_ID %in% low.session.stud))

## compute session count again
session.count <- get.session.count(sorted.traces)
summary(session.count) 
## check for outliers
boxplot(session.count)
out.values <- boxplot.stats(session.count)$out
# 11 outliers; will remove them
out.indices <- which(session.count %in% out.values)
stud.ids <- unique(sorted.traces$STUDENT_ID)[-out.indices]

## remove the traces of the student outliers
sorted.traces <- subset(sorted.traces, STUDENT_ID %in% stud.ids )
## re-compute the session count (after the removal of outliers)
session.count <- get.session.count(sorted.traces)
summary(session.count)  
boxplot(session.count)
# again a few outliers
out.value <- boxplot.stats(session.count)$out
# 3 now
out.index <- which(session.count %in% out.value)
# remove the outlier (student)
stud.ids <- stud.ids[-out.index]
sorted.traces <- subset(sorted.traces, STUDENT_ID %in% stud.ids )
session.count <- get.session.count(sorted.traces)
summary(session.count)  
boxplot(session.count)
# now it's fine 

## check how many students are left
length(stud.ids)
# 467 (93.4% of the initial number of students (500))
## and how many sessions
length(unique(sorted.traces$SESSION_ID))
# 28250

########################
## COMPUTE THE FEATURES
########################

## features are computed for each student and each session
## create a data frame with the feature values
stud.sessions <- list()
for(s in 1:length(stud.ids)) {
  stud.sessions[[s]] <- unique(sorted.traces$SESSION_ID[sorted.traces$STUDENT_ID==stud.ids[s]])
}

features.data <- data.frame()
for(s in 1:length(stud.sessions)) {
  session.features.df <- compute.stud.session.features(stud.sessions[[s]])
  session.features.df$STUDENT_ID <- stud.ids[s]
  features.data <- as.data.frame(rbind(features.data, session.features.df))
}
str(features.data)
features.data <- features.data[,c(9,1:8)]
head(features.data)

## add the ntimes attribute to the feature set
## (requied for the depmixS4)
ntimes.vector <- compute.ntimes(features.data)
features.data <- add.ntimes.feature(features.data, ntimes.vector)
str(features.data)

## discretize the features, as depmix cannot work with the feature values in the present form
#install.packages("infotheo")
library(infotheo)

discret.features <- as.data.frame(apply(X = features.data[,c(3:9)], MARGIN = 2, 
                                        FUN = discretize, disc = "equalwidth", nbins = 10))
colnames(discret.features) <- c('FA_PERC', 'FA_CO_PERC', 'SA_PERC', 'SA_CO_PERC', 
                                'VID_PERC', 'READ_PERC', 'METACOG_PERC')
discret.features <- as.data.frame(cbind(features.data[,c(1,2,10)], discret.features))
str(discret.features)
discret.features <- discret.features[,c(1,2,4:10,3)]

## those feature values that are originaly zero should be set to zero also in the discretized dataset
for(i in 1:nrow(discret.features)) {
  for(j in 3:9) { # features are in the columns 3:9
    if (features.data[i,j]==0) discret.features[i,j] <- 0
  }
}
head(discret.features)

## turn all the features into factor variables
final.features <- as.data.frame(apply(X = discret.features[,c(3:9)], MARGIN = 2, FUN = factor))
final.features <- as.data.frame(cbind(discret.features[,c(1,2,10)], final.features))
final.features <- final.features[,c(1,2,4:10,3)]
str(final.features)

## save the feature set
saveRDS(object = final.features, file = "Intermediate_results/multinom_features_for_session_based_HMM.RData")

########################
## FIT THE HMM MODEL
########################

#install.packages("depmixS4")
library(depmixS4)

final.features <- readRDS(file = "Intermediate_results/multinom_features_for_session_based_HMM.RData")

compare.models(final.features, max.ns = 7)

## model with 5 states should be the best option
mod.fit.5s <- fit.hmm(final.features, ns = 5)
summary(mod.fit.5s)

## get the estimated state for each observation 
estimates <- posterior(mod.fit.5s)
# add the estimated states to the features set and save it
features.and.states <- as.data.frame(cbind(final.features, estimates))
str(features.and.states)
write.csv(x = features.and.states[,c(1:9,11:16)], 
          file = "results/session_based_HMM_5_states.csv", 
          quote = F, row.names = F)

table(features.and.states$state)
round(prop.table(table(features.and.states$state)), digits = 3)


#############################
## COMPUTE STATES PER STUDENT
#############################

features.and.states <- read.csv(file = "results/session_based_HMM_5_states.csv")

n.stud <- length(unique(features.and.states$STUDENT_ID))
stud.states <- data.frame(STUDENT_ID=unique(features.and.states$STUDENT_ID),
                          ST1_CNT=rep(x = 0, times=n.stud),
                          ST2_CNT=rep(0, n.stud),
                          ST3_CNT=rep(0, n.stud),
                          ST4_CNT=rep(0, n.stud),
                          ST5_CNT=rep(0, n.stud))
for(i in 1:n.stud) {
  stud.data <- subset(features.and.states, STUDENT_ID==stud.states$STUDENT_ID[i])
  stud.states$ST1_CNT[i] <- nrow(stud.data[stud.data$state==1,])
  stud.states$ST2_CNT[i] <- nrow(stud.data[stud.data$state==2,])
  stud.states$ST3_CNT[i] <- nrow(stud.data[stud.data$state==3,])
  stud.states$ST4_CNT[i] <- nrow(stud.data[stud.data$state==4,])
  stud.states$ST5_CNT[i] <- nrow(stud.data[stud.data$state==5,])
}
str(stud.states)

## create a df with percentages of states for each student  
stud.states.perc <- stud.states
for(r in 1:n.stud) {
  stud.states.perc[r,c(2:6)] <- stud.states.perc[r,c(2:6)]/sum(stud.states.perc[r,c(2:6)])
}
head(stud.states.perc)
rowSums(stud.states.perc[,c(2:6)])
colnames(stud.states.perc) <- c("STUDENT_ID", paste0("ST",1:5,"_PERC"))

## transform the stud.states df (from wide) to long format 
## so that a plot can be created
## based on: http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
require(tidyr)
stud.states.perc$STUDENT_ID <- as.factor(stud.states.perc$STUDENT_ID)
stud.states.perc.long <- gather(data = stud.states.perc, key = state,
                                value = percent, ... = ST1_PERC:ST5_PERC, factor_key = T)
head(stud.states.perc.long)

## make a plot
library(ggplot2)
ggplot(data=stud.states.perc.long, aes(x=as.integer(STUDENT_ID), y=percent, fill=state)) +
  geom_bar(stat="identity") +
  ylab("Percent of student sessions") + 
  xlab("Students") +
  scale_fill_manual(values = c("#0072B2","#56B4E9", "#009E73", "#F0E442", "#ca0020"), # "#e41a1c" 
                    breaks = paste0("ST",1:5,"_PERC"),
                    labels = paste0("S",1:5)) +
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
src.data <- read.csv(file = "results/session_based_HMM_5_states.csv")
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

## examine session count in more detail 
quantile(session.count.df$SESSION_CNT, probs = seq(0, 0.1, 0.025))
# 2.5th percentile is 13
sort(session.count.df$SESSION_CNT)[1:15]
# check the number of students with up to 13 sessions
length(which(session.count.df$SESSION_CNT <= 13))
# 13 of them
quantile(session.count.df$SESSION_CNT, probs = seq(0.9, 1, 0.025))
# 97.5th percentile is 127
sort(session.count.df$SESSION_CNT, decreasing = T)[1:15]
length(which(session.count.df$SESSION_CNT > 127))
# 11
## remove those with up to 13 sessions (2.5th perc.) or more than 127 (97.5th perc)
## so, in total, 5% of students will be removed
stud.to.remove <- session.count.df$STUDENT_ID[(session.count.df$SESSION_CNT <= 13) | 
                                                (session.count.df$SESSION_CNT > 127)]
# 24 in total
session.count.df <- subset(session.count.df, !(STUDENT_ID %in% stud.to.remove))
## remove session data related to the removed students
session.data <- subset(src.data, STUDENT_ID %in% session.count.df$STUDENT_ID)
# 5.86% (1655) sessions were removed
length(setdiff(stud.ids, unique(session.data$STUDENT_ID)))
# 24 students removed, which is correct 
## re-compute stud.ids and n.stud as might be needed later
stud.ids <- unique(session.data$STUDENT_ID) 
n.stud <- length(stud.ids)
# working with 443 students (89% of the initial number (497))

## keep only the data required for creating sequences
session.data <- session.data[,c(1,2,10)]

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
traminer.seq <- seqdef(data = equal.len.seq.df, var = 2:ncol(equal.len.seq.df), 
                       informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"),
                       cpal = c("#0072B2","#56B4E9", "#009E73", "#F0E442", "#ca0020"))
print(traminer.seq[1:10, 1:30], format = 'SPS')

## store the sequence data
saveRDS(object = traminer.seq, 
        file = "Intermediate_results/5state_sequence_data_SPS_format.RData")
## store the created sequences
saveRDS(object = equal.len.seq.df, 
        file = "Intermediate_results/5state_sequence_data_df.RData")


########################################
## do sequence analysis (clustering)
########################################

traminer.seq <- readRDS(file = "Intermediate_results/5state_sequence_data_SPS_format.RData")

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

state.trans.matrix <- matrix(data = c(0.285, 0.139, 0.120, 0.199, 0.257,
                                      0.025, 0.359, 0.163, 0.358, 0.095,
                                      0.041, 0.186, 0.312, 0.204, 0.256,
                                      0.049, 0.160, 0.144, 0.515, 0.132,
                                      0.104, 0.174, 0.177, 0.241, 0.303),
                             nrow = 5, ncol = 5, byrow = T,
                             dimnames = list(seq(1,5,1), seq(1,5,1))) 
state.trans.matrix
cost.matrix <- matrix(data = rep(2,25), nrow = 5, ncol = 5, byrow = T)
for(i in 1:nrow(cost.matrix)) {
  for(j in 1:ncol(cost.matrix))
    if (i==j) 
      cost.matrix[i,j] <- 0  ## all elements on the diagonal must be zero (which makes sense)
    else
      cost.matrix[i,j] <- cost.matrix[i,j] - state.trans.matrix[i,j] - state.trans.matrix[j,i]
}
cost.matrix
## save cost matrix
saveRDS(object = cost.matrix, file = "Intermediate_results/cost_matrix_basedon_5_state_HMM_trans_matrix.RData")

#submat <- seqsubm(seqdata = traminer.seq, method = "TRATE")
## normalize the computed distances to account for differences in sequence lengths
#dist.om1 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T)
dist.om1.75 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 1.75)
#dist.om1.5 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 1.5)
#dist.om2 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 2)

require(cluster)
set.seed(232017)
## NOTE: using OM distance with indel cost set to 1.75 (proved to be the best), 
## and sustitution costs derived from the HMM transition matrix
seq.ward <- agnes(dist.om1.75, diss = T, method = "ward")
plot(seq.ward)

## check the solution with 3 clusters
cl3 <- cutree(seq.ward, k = 3)
table(cl3)
## get the cluster assignment for each sequence
cl3.fac <- factor(cl3, labels = paste("cl:",1:3))
## plot the state distribution at each time point for each cluster
seqdplot(seqdata = traminer.seq, group = cl3.fac,
         title="State dist.", withlegend=F)

## check the solution with 4 clusters
cl4 <- cutree(seq.ward, k = 4)
table(cl4)
## get the cluster assignment for each sequence
cl4.fac <- factor(cl4, labels = paste("cl:",1:4))
## plot the state distribution for each cluster
seqdplot(seqdata = traminer.seq, group = cl4.fac, #axes = "bottom", cex.plot = 0.5, 
         title="State dist.", withlegend=F)
seqlegend(traminer.seq, fontsize = 0.65)

## curious to see the solution with 5 clusters
cl5 <- cutree(seq.ward, k = 5)
table(cl5)
## get the cluster assignment for each sequence
cl5.fac <- factor(cl5, labels = paste("cl:",1:5))
seqdplot(seqdata = traminer.seq, group = cl5.fac,
         title="State dist.", withlegend=F)

## create a new data frame with one row for each student and the following columns: 
## 1) student id
## 2) the cluster the student is assigned to in the 3 cluster model 
## 3) the cluster the student is assigned to in the 4 cluster model
## 4) the cluster the student is assigned to in the 5 cluster model

state.seq.df <- readRDS(file = "Intermediate_results/5state_sequence_data_df.RData")
str(state.seq.df)
student.clusters <- data.frame(STUDENT_ID=state.seq.df$STUDENT_ID)
student.clusters$cl3 <- as.factor(cl3)
student.clusters$cl4 <- as.factor(cl4)
student.clusters$cl5 <- as.factor(cl5)
str(student.clusters)
## save the student cluster assignments 
write.csv(student.clusters, file = "results/seq_of_5HMM_states_student_clusters_indel=1.75.csv", 
          quote = F, row.names = F)
remove(state.seq.df)


#####################################################
## examine the identified clusters by comparing them
## based on students' exam (midterm and final) scores
#####################################################

source(file = "util_functions.R")

student.clusters <- read.csv(file = "results/seq_of_5HMM_states_student_clusters_indel=1.75.csv")

## get the students' exam scores
scores <- read.csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
summary(scores)
# 3 students with no ids (NAs); remove those
scores <- subset(scores, is.na(USER_ID)==F)

setdiff(student.clusters$STUDENT_ID, scores$USER_ID)
# 7 students are lacking scores

student.clusters <- merge(x = student.clusters, y = scores,
                          by.x = "STUDENT_ID", by.y = "USER_ID",
                          all.x = F, all.y = F)
str(student.clusters)
# 436 - number of students for whom both the scores and traces are available 

## remove email address
student.clusters <- student.clusters[,-5]


#####################################
## examine first the 4 clusters model
#####################################

## compute the summary statistics for the students' exam scores
stud.cl4.stats <- summary.stats(student.clusters[,c(5,6)], student.clusters$cl4, 4)
require(knitr)
kable(x = stud.cl4.stats, format = "rst")

## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl4)
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
stud.4cl.df <- student.clusters[,c(1,3,5,6)]
colnames(stud.4cl.df)[2] <- "class"
kable(pairwise.fexam.compare(4, 6, stud.4cl.df), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl4)

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
stud.cl5.stats <- summary.stats(student.clusters[,c(5,6)], student.clusters$cl5, 5)
kable(x = stud.cl5.stats, format = "rst")

## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl5)
## there is a sign. difference bewtween the clusters; check where the difference is

## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
stud.5cl.df <- student.clusters[,c(1,4:6)]
colnames(stud.5cl.df)[2] <- "class"
kable(pairwise.fexam.compare(5, 10, stud.5cl.df), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl5)

## apply Mann-Whitney U Test to do pair-wise comparisons
kable(pairwise.mtxam.compare(5, 10, stud.5cl.df), format = "rst")


##############################################
## finally, examine the model with 3 clusters
##############################################

## compute the summary statistics for the students' exam scores
stud.cl3.stats <- summary.stats(student.clusters[,c(5,6)], student.clusters$cl3, 3)
kable(x = stud.cl3.stats, format = "rst")

## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl3)
## there is a sign. difference bewtween the clusters; check where the difference is

## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
stud.3cl.df <- student.clusters[,c(1,2,5,6)]
colnames(stud.3cl.df)[2] <- "class"
kable(pairwise.fexam.compare(3, 3, stud.3cl.df), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl3)
## a sign. difference bewtween the clusters;

## apply Mann-Whitney U Test to do pair-wise comparisons
kable(pairwise.mtxam.compare(3, 3, stud.3cl.df), format = "rst")


##############################################################
## examine the identified student clusters by comparing them
## based on proportion of the 5 different types of states 
## (study tactics) in the students' state sequences 
##############################################################

source(file = "util_functions.R")

stud.clusters <- read.csv(file = "results/seq_of_5HMM_states_student_clusters_indel=1.75.csv")
str(stud.clusters)

state.seq.data <- read.csv(file = "results/session_based_HMM_5_states.csv")
str(state.seq.data)

n.stud <- length(unique(state.seq.data$STUDENT_ID))
stud.states <- data.frame(STUDENT_ID=unique(state.seq.data$STUDENT_ID),
                          ST1_CNT=rep(x = 0, times=n.stud),
                          ST2_CNT=rep(0, n.stud),
                          ST3_CNT=rep(0, n.stud),
                          ST4_CNT=rep(0, n.stud),
                          ST5_CNT=rep(0, n.stud))
for(i in 1:n.stud) {
  stud.data <- subset(state.seq.data, STUDENT_ID==stud.states$STUDENT_ID[i])
  stud.states$ST1_CNT[i] <- nrow(stud.data[stud.data$state==1,])
  stud.states$ST2_CNT[i] <- nrow(stud.data[stud.data$state==2,])
  stud.states$ST3_CNT[i] <- nrow(stud.data[stud.data$state==3,])
  stud.states$ST4_CNT[i] <- nrow(stud.data[stud.data$state==4,])
  stud.states$ST5_CNT[i] <- nrow(stud.data[stud.data$state==5,])
}
str(stud.states)

stud.states$ST_TOTAL <- apply(X = stud.states[,c(2:6)], MARGIN = 1, FUN = sum) 

stud.states.clust <- merge(x = stud.states, y = stud.clusters[,c(1,3,4)], 
                           by = "STUDENT_ID", all.x = F, all.y = T)
str(stud.states.clust)
n.stud <- length(unique(stud.states.clust$STUDENT_ID))

## check if the number of states is normally distributed
apply(X = stud.states.clust[,c(2:6)], MARGIN = 2, FUN = shapiro.test)
# none is normaly distributed

clust.stats <- summary.stats(stud.states.clust[,c(2:7)], stud.states.clust$cl4, 4)
kable(clust.stats, format = "rst")

clust5.stats <- summary.stats(stud.states.clust[,c(2:7)], stud.states.clust$cl5, 5)
kable(clust5.stats, format = "rst")

## create a df with percentages of states for each student  
stud.states.perc <- stud.states.clust
for(r in 1:n.stud) {
  stud.states.perc[r,c(2:6)] <- stud.states.perc[r,c(2:6)]/sum(stud.states.perc[r,c(2:6)])
}
head(stud.states.perc)
colnames(stud.states.perc)[2:6] <- paste0("ST",1:5,"_PERC")

clust4.perc.stats <- summary.stats(stud.states.perc[,c(2:7)], stud.states.perc$cl4, 4)
kable(clust4.perc.stats, format = "rst")

clust5.perc.stats <- summary.stats(stud.states.perc[,c(2:7)], stud.states.perc$cl5, 5)
kable(clust5.perc.stats, format = "rst")

## use statitical tests to compare the clusters with respect to each of the STx_PERC, x=1:4 variables
## check if the variables representing proportions of states are normally distributed
apply(X = stud.states.perc[,c(2:6)], MARGIN = 2, FUN = shapiro.test)
# none is normally distributed; use Kruskal-Wallis test for all the variables
apply(stud.states.perc[,c(2:6)], MARGIN = 2, 
      FUN = function(x) kruskal.test(x~stud.states.perc$cl4))


##########################################################
## compute the level of discrepancy for each seq. cluster
## 
## Discrepancy is defined as an extension of the concept 
## of variance to any kind of objects for which  
## pairwise dissimilarities can be computed 
##########################################################

traminer.seq <- readRDS(file = "Intermediate_results/5state_sequence_data_SPS_format.RData")
stud.clust <- read.csv(file = "results/seq_of_5HMM_states_student_clusters_indel=1.75.csv")
cost.matrix <- readRDS(file = "Intermediate_results/cost_matrix_basedon_5_state_HMM_trans_matrix.RData")

## compute for the 4 cluster solution
clust4.dissvar <- vector()
for(c in 1:4) {
  cl.diss <- seqdist(seqdata = traminer.seq[stud.clust$cl4==c,],
                     method = "OM", sm = cost.matrix, indel = 1.75)
  clust4.dissvar[c] <- dissvar(diss = cl.diss)
}
round(clust4.dissvar, digits = 2)

###############################################################
## identify and plot representative sequences for each cluster
###############################################################

## SKIPPING THIS PART FOR NOW

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
                     method = "OM", sm = cost.matrix, indel = 1.75)
  seqrplot(seqdata = traminer.seq[cl4==c,], dist.matrix=cl.diss, 
           criterion="density", tsim=0.2, trep=0.25,
           #criterion="dist", nrep=1, tsim=0.25,
           title = paste("Representative seq. for cluster", c), 
           withlegend=F)
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


fit.hmm <- function(feature.data, ns) {
  
  response.vars <- list(FA_PERC ~ 1, FA_CO_PERC ~ 1, 
                        SA_PERC ~ 1, SA_CO_PERC ~ 1, VID_PERC ~ 1, 
                        READ_PERC ~ 1, METACOG_PERC ~ 1)
  family.dist <- list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), multinomial("identity"), multinomial("identity"),
                      multinomial("identity"), multinomial("identity"))
  
  set.seed(2032017)
  mod <- depmix(response = response.vars,
                data = feature.data, 
                nstates = ns, 
                family = family.dist)
  mod.fit <- fit(mod, verbose = FALSE)
  
  return (mod.fit)
  
}

## the f. computes several HMM models, with ns=>min.ns & ns<=max.ns
## and produces a table with evaluation metrics
compare.models <- function(feature.data, min.ns=2, max.ns=6) {
  
  eval.metrics <- data.frame()
  for (ns in min.ns:max.ns) {
    mod.fit <- fit.hmm(feature.data, ns = ns)
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