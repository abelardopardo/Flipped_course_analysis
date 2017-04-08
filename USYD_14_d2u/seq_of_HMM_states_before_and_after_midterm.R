#######################################################################################
## Create 2 HMMs, one for the first part of the course, and one for the 2nd part, where
## midterm exam is used as the delimiter. 
## For both models use student-session as the unit of analysis, that is, by examine 
## student learning actions at the session level (instead of the week level as done previously)
## Identify sessions based on the time gap between consecutive learning actions
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
## Consider that summative assessment (SA) actions that were done in either 'revisiting' 
## or 'catching-up' study mode served, in fact, as formative assessment (FA) and
## count them as FA actions
##
## Do not consider the data from weeks 6 and 13 as those are the weeks when students prepare
## for the exams and their behaviour deviates from the regular study behaviour
#######################################################################################

## load the data
traces <- read.csv(file = "Intermediate_files/study_mode_weeks2-13_v01-02-2017.csv",
                   stringsAsFactors = F)
str(traces)

## load the required functions
source(file = "functions_for_analyzing_seq_of_HMM_states.R")

require(knitr)
kable(t(table(traces$WEEK)), format = 'pandoc')

## remove data from weeks 6 and 13
traces <- subset(traces, WEEK %in% c(2:5,7:12))
table(traces$WEEK)

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
quantile(sorted.traces$TIME_GAP, probs = seq(0.9, 1, 0.01), na.rm = T)
# 90%       91%       92%       93%       94%       95%       96%       97%       98%       99% 
#188.00    231.00    294.00    387.00    533.00    768.00   1253.00   3575.00  49739.56 198178.86 

## use the time gap of  21min (~ 1253sec, which is 96th percentile) as the 'session delimiter'
## why 96th? 95th of 12.8min seems to be insufficiently long considering the length of videos
## 97th of 1h seems to be overly long 

## split events into sessions
session.id <- 0
for(i in 1:nrow(sorted.traces)) {
  if ( is.na(sorted.traces$TIME_GAP[i]) || sorted.traces$TIME_GAP[i] >= 1260 )
    session.id <- session.id + 1
  sorted.traces$SESSION.ID[i] <- session.id
}
length(unique(sorted.traces$SESSION.ID))
# 10894 - the initial number of sessions  
length(unique(sorted.traces$STUDENT.ID))
# 290 - the initial number of students 

## store the data
saveRDS(sorted.traces, file = "Intermediate_files/sorted_traces_w_sessions_weeks2-5_7-12.csv")

## remove students who do not have at least one session 
## in each of the 2 time periods (weeks 2-5 and 7-12)
traces.w2to5 <- subset(sorted.traces, WEEK %in% c(2:5))
s.count.2to5 <- get.session.count(traces.w2to5)
summary(s.count.2to5)
sort(s.count.2to5)[1:20]
# 5 students with 1 session only

traces.w7to12 <- subset(sorted.traces, WEEK %in% c(7:12))
s.count.7to12 <- get.session.count(traces.w7to12)
summary(s.count.7to12)
sort(s.count.7to12)[1:20]
# no students with 1 session only

## remove those 5 with 1 session only in weeks 2-5 
## get ids of those students
less.than.2 <- which(s.count.2to5 < 2)
one.session.stud <- unique(traces.w2to5$STUDENT.ID)[less.than.2]
## remove traces of students with < 2 sesssion (5 students)
traces.w2to5 <- subset(traces.w2to5, !(STUDENT.ID %in% one.session.stud))
## compute session count again
s.count.2to5 <- get.session.count(traces.w2to5)
summary(s.count.2to5)
# no more students with 1 session only

## check for outliers
boxplot(s.count.2to5)
w2to5.outliers <- boxplot.stats(s.count.2to5)$out
# 8 outliers; check them
sort(s.count.2to5, decreasing = T)[1:20]
# 72 52 52 51 49 47 47 45 43 42 42 42 41 41 41 41 40 39 39 38
# might be better to remove just the one with 72 sessions

boxplot(s.count.7to12)
w7to12.outliers <- boxplot.stats(s.count.7to12)$out
# 11 outliers; check them as well
sort(s.count.7to12, decreasing = T)[1:20]
# 60 59 53 53 51 51 49 49 47 47 46 43 41 41 40 40 39 38 38 38
# might be better just to remove the top two (60 and 59)

## going with "lighter" version of outliers removal - removing "extreme" outliers
## first do the removal for weeks 2-5
stud.to.remove <- unique(traces.w2to5$STUDENT.ID)[which(s.count.2to5 == max(s.count.2to5))]
traces.w2to5 <- subset(traces.w2to5, STUDENT.ID != stud.to.remove)
## now, for the weeks 7-12
stud.to.remove <- unique(traces.w7to12$STUDENT.ID)[which(s.count.7to12 %in% c(60,59))]
traces.w7to12 <- subset(traces.w7to12, !(STUDENT.ID %in% stud.to.remove))

## re-compute the session count (after the removal of extreme outliers)
s.count.2to5 <- get.session.count(traces.w2to5)
boxplot.stats(s.count.2to5)$out
# 51 52 52 45 47 49 47
s.count.7to12 <- get.session.count(traces.w7to12)
boxplot.stats(s.count.7to12)$out
# 47 51 47 53 53 49 51 49 46
## still a number of outliers, but let's keep them for now

## keep only students who had sessions in both periods (2-5 and 7-12)
stud.w2to5 <- unique(traces.w2to5$STUDENT.ID)
stud.w7to12 <- unique(traces.w7to12$STUDENT.ID)
stud.ids <- intersect(stud.w2to5, stud.w7to12)
traces.w2to5 <- subset(traces.w2to5, STUDENT.ID %in% stud.ids)
traces.w7to12 <- subset(traces.w7to12, STUDENT.ID %in% stud.ids)

## check the number of students and sessions after the cleaning
length(stud.ids)
# 281 - 9 removed
length(unique(traces.w2to5$SESSION.ID))
# 5395
length(unique(traces.w7to12$SESSION.ID))
# 5210
# in total: 10605 sessions; 289 (2.65%) sessions removed


########################
## COMPUTE THE FEATURES
########################

## compute separate feature sets for weeks 2-5 and weeks 7-12 
## features are computed for each student and each session

## first, compute features for weeks 2-5

## create a list of study sessions for each student
stud.sessions <- list()
for(s in 1:length(stud.ids)) {
  stud.sessions[[s]] <- unique(traces.w2to5$SESSION.ID[traces.w2to5$STUDENT.ID==stud.ids[s]])
}
## create a data frame with the feature values for weeks 2-5
features.w2to5 <- data.frame()
for(s in 1:length(stud.sessions)) {
  session.features.df <- compute.stud.session.features(stud.sessions[[s]])
  session.features.df$STUDENT_ID <- stud.ids[s]
  features.w2to5 <- as.data.frame(rbind(features.w2to5, session.features.df))
}
str(features.w2to5)
features.w2to5 <- features.w2to5[,c(9,1:8)]
head(features.w2to5)

## add the ntimes attribute to the feature set (requied for the depmixS4)
ntimes.vector <- compute.ntimes(features.w2to5)
features.w2to5 <- add.ntimes.feature(features.w2to5, ntimes.vector)
str(features.w2to5)

## discretize the feature set
discrete.w2to5 <- discretize.features(features.w2to5)

## turn all the features into factor variables and save the feature set
f.path <- "Intermediate_files/multinom_features_for_session_based_HMM_weeks2to5"
ff.w2to5 <- factorize.and.store(discrete.w2to5, f.path)

## now, do the same for weeks 7-12

## create a list of study sessions for each student
stud.sessions <- list()
for(s in 1:length(stud.ids)) {
  stud.sessions[[s]] <- unique(traces.w7to12$SESSION.ID[traces.w7to12$STUDENT.ID==stud.ids[s]])
}
## create a data frame with the feature values for weeks 7-12
features.w7to12 <- data.frame()
for(s in 1:length(stud.sessions)) {
  session.features.df <- compute.stud.session.features(stud.sessions[[s]])
  session.features.df$STUDENT_ID <- stud.ids[s]
  features.w7to12 <- as.data.frame(rbind(features.w7to12, session.features.df))
}
str(features.w7to12)
features.w7to12 <- features.w7to12[,c(9,1:8)]
head(features.w7to12)

## add the ntimes attribute to the feature set (requied for the depmixS4)
ntimes.vector <- compute.ntimes(features.w7to12)
features.w7to12 <- add.ntimes.feature(features.w7to12, ntimes.vector)
str(features.w7to12)

## discretize the feature set
discrete.w7to12 <- discretize.features(features.w7to12)

## turn all the features into factor variables and save the feature set
f.path <- "Intermediate_files/multinom_features_for_session_based_HMM_weeks7to12"
ff.w7to12 <- factorize.and.store(discrete.w7to12, f.path)


##################
## FIT HMM MODELS
##################

#install.packages("depmixS4")
library(depmixS4)

## fit a model for weeks 2-5

seed <- 442017
compare.models(ff.w2to5, max.ns = 7, seed)
## choose model with 4 states - the largest drop in AIC and BIC
mod.fit.4s <- fit.HMM(ff.w2to5, ns = 4, seed)
summary(mod.fit.4s)
## get the estimated state for each observation 
estimates <- posterior(mod.fit.4s)
# add the estimated states to the features set and save it
ff.w2to5 <- as.data.frame(cbind(ff.w2to5, estimates))
str(ff.w2to5)
write.csv(x = ff.w2to5[,c(1:9,11:15)], 
          file = "results/session_based_HMM_4_states_weeks2to5.csv", 
          quote = F, row.names = F)

## fit a model for weeks 7-12

compare.models(ff.w7to12, max.ns = 7, seed)
## choose model with 6 states - both AIC and BIC have minimum there
mod.fit.6s <- fit.HMM(ff.w7to12, ns = 6, seed)
summary(mod.fit.6s)
# add the estimated states to the features set and save it
ff.w7to12 <- as.data.frame(cbind(ff.w7to12, posterior(mod.fit.6s)))
str(ff.w7to12)
write.csv(x = ff.w7to12[,c(1:9,11:17)], 
          file = "results/session_based_HMM_6_states_weeks7to12.csv", 
          quote = F, row.names = F)


#############################
## COMPUTE STATES PER STUDENT
#############################

## first for weeks 2-5
w2to5.dat <- read.csv(file = "results/session_based_HMM_4_states_weeks2to5.csv")

states.df <- compute.state.dist.per.student(w2to5.dat, n.states = 4)

## create a df with percentages of states for each student  
states.perc <- states.df
for(r in 1:n.stud) {
  states.perc[r,c(2:5)] <- states.perc[r,c(2:5)]/sum(states.perc[r,c(2:5)])
}
colnames(states.perc)[2:5] <- paste0("ST",1:4,"_PERC")

## create long format suitable for plotting
library(tidyr)
states.perc.long <- gather(data = states.perc, key = state,
                           value = percent, ... = ST1_PERC:ST4_PERC, factor_key = T)

custom.pallet <- c("#0072B2","#56B4E9", "#009E73", "#F0E442")
plot.session.percents(states.perc.long, 4, custom.pallet)

##############################################################################
# SEQUENCE ANALYSIS: 
# CLUSTERING OF STUDENTS BASED ON THEIR STATE SEQUENCES
#
# each student will be represented with the sequence of states that correspond 
# to the sequence of their learning sessions (states are identified using HMM, 
# and each state corresponds to one learning session)
#
# sequence clustering will be done separately for the two parts of the course,
# first, for weeks 2-5, and then for weeks 7-12
#
# For each student, there will be 2 sequences: one for the weeks 2-5, and the 
# other for the weeks 7-12.
# The set of sequences for all the students for each of the 2 time periods will
# be clustered separately.
# To be analyzed with TraMineR, sequences should be represented in the form:  
# ST1/3 ST1/2 ST4/1 ...
# if the student's sequence was ST1-ST1-ST1-ST2-ST2-ST4-...
# in general, a sequence (student) should be represented as a vector of elements 
# <STATE/OCCURRENCE_CNT>
##############################################################################

######################################
#
# DO SEQUENCE CLUSTERING FOR WEEKS 2-5
#
######################################

## load the data
src.data <- read.csv(file = "results/session_based_HMM_4_states_weeks2to5.csv")
str(src.data)

## before formatting the data for sequence analysis, it would be good to examine
## the sequences (of students' study sessions) to identify the presence of outliers
## (ie. overly long or short sequences)
seq.len <- compute.seqence.length(src.data)
summary(seq.len$SESSION_CNT)
boxplot.stats(seq.len$SESSION_CNT)$out
# 7 outliers; will keep them for now

# ## examine quantiles
# quantile(x = seq.len$SESSION_CNT, probs = seq(0, 0.1, 0.025))
# quantile(x = seq.len$SESSION_CNT, probs = seq(0.9, 1, 0.025))
# ## remove those students who have up to 3 sessions (2.5th perc) or more than 43 sessions (97.5% perc)
# ## this way, 5% of outliers will be removed
# to.remove <- seq.len$STUDENT_ID[seq.len$SESSION_CNT <= 3]
# to.remove <- c(to.remove, seq.len$STUDENT_ID[seq.len$SESSION_CNT > 43])
# # 15 students in total (5.3%)
# seq.data <- subset(src.data, !(STUDENT_ID %in% to.remove))
# # 5030  (93.23% of the initial 5395 sessions)

## keep only the data required for creating sequences
seq.data <- src.data[,c(1,2,10)]

## for each student, create state sequences of the form: ST1/3 ST1/2 ST4/1 ... 
traminer.seq <- create.state.seq(seq.data, "Intermediate_files/state_sequences_w2to5")

## do sequence clustering

## first, compute dissimilarities among sequences using the Optimal Matching method;
## to compute the dissimilarities, we need the substitution-cost matrix
## since we have transition probabilities from one state to the next - obtained 
## from the HMM method - we can use these to compute the substitution costs;
## using the following formula (from TraMineR documentation):
## SC(i,j) = cval - p(i,j) - p(j,i)
## where cval is, by default, equal to 2 

state.trans.matrix <- matrix(data = c(0.313, 0.217, 0.123, 0.346,
                                      0.222, 0.151, 0.147, 0.480,
                                      0.241, 0.207, 0.113, 0.439,
                                      0.245, 0.180, 0.097, 0.477),
                             nrow = 4, ncol = 4, byrow = T,
                             dimnames = list(seq(1,4,1), seq(1,4,1))) 
cost.matrix <- compute.cost.matrix(state.trans.matrix)
cost.matrix
## normalize the computed distances to account for differences in sequence lengths
#dist.om1.25 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 1.25)
dist.om1.5 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 1.5)
#dist.om1.75 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 1.75)

require(cluster)
set.seed(542017)
## IMPORTANT: clustering is done using OM-based distances with indel_cost = 1.5, and substitution matrix
## derived from the HMM state transition matrix
seq.ward <- agnes(dist.om1.5, diss = T, method = "ward")
#png(file = "graphics/seq.ward_dist.om1.5.png", width = 1800, height = 1500, pointsize = 50)
plot(seq.ward)
#dev.off()

custom.pallet <- c("#0072B2","#56B4E9", "#009E73", "#F0E442")
## check the solution with 5 clusters
cl5 <- examine.clusters(cl.mod = seq.ward, n.clust = 5, 
                        custom.pallet = custom.pallet)
## check the solution with 4 clusters
cl4 <- examine.clusters(cl.mod = seq.ward, n.clust = 4, custom.pallet)

## create a new data frame with one row for each student and the following columns: 
## 1) student id
## 2) the cluster the student is assigned to in 5 cluster model 
## 3) the cluster the student is assigned to in 4 cluster model 
student.clusters <- data.frame(STUDENT_ID=unique(seq.data$STUDENT_ID))
student.clusters$cl5 <- as.factor(cl5)
student.clusters$cl4 <- as.factor(cl4)
str(student.clusters)
## save the student cluster assignments 
write.csv(student.clusters, file = "results/seq_of_HMM_states_stud_clusts_w2to5_dist_1.5.csv", 
          quote = F, row.names = F)


#####################################################
## examine the identified clusters by comparing them
## based on students' exam (midterm and final) scores
#####################################################

source(file = "util_functions.R")

student.clusters <- add.scores.to.clusters(student.clusters)
str(student.clusters)

## examine first the 4 clusters model

## compute the summary statistics for the students' exam scores
stud.cl4.stats <- summary.stats(student.clusters[,c(4,5)], student.clusters$cl4, 4)
kable(x = stud.cl4.stats, format = "rst")

## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl4)
## there is a sign. difference bewtween the clusters; check where the difference is

## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
stud.4cl.df <- student.clusters[,c(1,3:5)]
colnames(stud.4cl.df)[2] <- "class"
kable(pairwise.exam.compare(4, 6, stud.4cl.df, "FE"), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl4)

## apply Mann-Whitney U Test to do pair-wise comparisons
kable(pairwise.exam.compare(4, 6, stud.4cl.df, "MT"), format = "rst")

## now, examine the model with 5 clusters

## compute the summary statistics for the students' exam scores
stud.cl5.stats <- summary.stats(student.clusters[,c(4,5)], student.clusters$cl5, 5)
kable(x = stud.cl5.stats, format = "rst")

## examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl5)
## there is a sign. difference bewtween the clusters; check where the difference is

## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
stud.5cl.df <- student.clusters[,c(1,2,4,5)]
colnames(stud.5cl.df)[2] <- "class"
kable(pairwise.exam.compare(5, 10, stud.5cl.df, "FE"), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl5)

## apply Mann-Whitney U Test to do pair-wise comparisons
kable(pairwise.exam.compare(5, 10, stud.5cl.df, "MT"), format = "rst")

##################################################################
## examine the identified student clusters by comparing them  
## based on proportion of the 4 different types of states 
## (study tactics) in the students' state sequences 
##################################################################

stud.states.clust <- merge.state.dist.and.clusters(src.data, student.clusters, 4)

## check if the number of states is normally distributed
apply(X = stud.states.clust[,c(2:5)], MARGIN = 2, FUN = shapiro.test)
# none is normaly distributed

## compare the states in the 4 clusters solution:
clust4.stats <- summary.stats(stud.states.clust[,c(2:6)], stud.states.clust$cl4, 4)
kable(clust4.stats, format = "rst")

## create a df with percentages of states for each student  
stud.states.perc <- stud.states.clust
for(r in 1:n.stud) {
  stud.states.perc[r,c(2:5)] <- stud.states.perc[r,c(2:5)]/sum(stud.states.perc[r,c(2:5)])
}
head(stud.states.perc)
colnames(stud.states.perc)[2:5] <- c("ST1_PERC", "ST2_PERC", "ST3_PERC", "ST4_PERC")

## compare the proportion of states in the 4 clusters solution:
clust4.perc.stats <- summary.stats(stud.states.perc[,c(2:6)], stud.states.perc$cl4, 4)
kable(clust4.perc.stats, format = "rst")

## use statitical tests to compare the clusters with respect to each of the STx_PERC, x=1:4 variables
## check if the variables representing proportions of states are normally distributed
apply(X = stud.states.perc[,c(2:5)], MARGIN = 2, FUN = shapiro.test)
# none is normally distributed, use Kruskal-Wallis test for all the variables
for(v in 2:5)
  print(kruskal.test(as.vector(stud.states.perc[,v]) ~ stud.states.perc$cl4))


#######################################
#
# DO SEQUENCE CLUSTERING FOR WEEKS 7-12
#
#######################################

## load the data
src.data <- read.csv(file = "results/session_based_HMM_6_states_weeks7to12.csv")
str(src.data)

## before formatting the data for sequence analysis, it would be good to examine
## the sequences (of students' study sessions) to identify the presence of outliers
## (ie. overly long or short sequences)
seq.len <- compute.seqence.length(src.data)
summary(seq.len$SESSION_CNT)
boxplot.stats(seq.len$SESSION_CNT)$out
# 9 outliers; will keep them for now

## keep only the data required for creating sequences
seq.data <- src.data[,c(1,2,10)]

## for each student, create state sequences of the form: ST1/3 ST1/2 ST4/1 ... 
traminer.seq <- create.state.seq(seq.data, "Intermediate_files/state_sequences_w7to12")

## do sequence clustering

## first, compute dissimilarities among clusters using the Optimal Matching method
## to compute the dissimilarities, we need the substitution-cost matrix
## since we have transition probabilities from one state to the next - obtained 
## from the HMM method - we can use these to compute the substitution costs;
## using the following formula (from TraMineR documentation):
## SC(i,j) = cval - p(i,j) - p(j,i)
## where cval is, by default, equal to 2 

state.trans.matrix <- matrix(data = c(0.213, 0.164, 0.242, 0.104, 0.191, 0.086,
                                      0.125, 0.221, 0.213, 0.261, 0.080, 0.100,
                                      0.128, 0.188, 0.389, 0.084, 0.119, 0.092,
                                      0.087, 0.344, 0.289, 0.084, 0.098, 0.098,
                                      0.144, 0.183, 0.278, 0.141, 0.124, 0.130,
                                      0.192, 0.196, 0.230, 0.108, 0.118, 0.155),
                             nrow = 6, ncol = 6, byrow = T,
                             dimnames = list(seq(1,6,1), seq(1,6,1))) 
cost.matrix <- compute.cost.matrix(state.trans.matrix, n.state = 6)
cost.matrix
## normalize the computed distances to account for differences in sequence lengths
#dist.om2 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 2)
#dist.om1.5 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 1.5)
dist.om1.75 <- seqdist(seqdata = traminer.seq, method = "OM", sm = cost.matrix, norm = T, indel = 1.75)

require(cluster)
set.seed(542017)
## IMPORTANT: clustering is done using OM-based distances with indel_cost = 1.75, and substitution matrix
## derived from the HMM state transition matrix
seq.ward <- agnes(dist.om1.75, diss = T, method = "ward")
#png(file = "graphics/seq.ward_dist.om1.5.png", width = 1800, height = 1500, pointsize = 50)
plot(seq.ward)
#dev.off()

custom.pallet <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
## check the solution with 5 clusters
cl5 <- examine.clusters(cl.mod = seq.ward, n.clust = 5, 
                        custom.pallet = custom.pallet)
## check the solution with 4 clusters
cl4 <- examine.clusters(cl.mod = seq.ward, n.clust = 4, custom.pallet)

## create a new data frame with one row for each student and the following columns: 
## 1) student id
## 2) the cluster the student is assigned to in 5 cluster model 
## 3) the cluster the student is assigned to in 4 cluster model 
student.clusters <- data.frame(STUDENT_ID=unique(seq.data$STUDENT_ID))
student.clusters$cl5 <- as.factor(cl5)
student.clusters$cl4 <- as.factor(cl4)
str(student.clusters)
## save the student cluster assignments 
write.csv(student.clusters, file = "results/seq_of_HMM_states_stud_clusts_w7to12_dist_1.75.csv", 
          quote = F, row.names = F)

#####################################################
## examine the identified clusters by comparing them
## based on students' exam (midterm and final) scores
#####################################################

source(file = "util_functions.R")

student.clusters <- add.scores.to.clusters(student.clusters)
str(student.clusters)

## examine first the 4 clusters model

## compute the summary statistics for the students' exam scores
stud.cl4.stats <- summary.stats(student.clusters[,c(4,5)], student.clusters$cl4, 4)
kable(x = stud.cl4.stats, format = "rst")

## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl4)
## there is a sign. difference bewtween the clusters; check where the difference is

## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
stud.4cl.df <- student.clusters[,c(1,3:5)]
colnames(stud.4cl.df)[2] <- "class"
kable(pairwise.exam.compare(4, 6, stud.4cl.df, "FE"), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl4)

## apply Mann-Whitney U Test to do pair-wise comparisons
kable(pairwise.exam.compare(4, 6, stud.4cl.df, "MT"), format = "rst")

## now, examine the model with 5 clusters

## compute the summary statistics for the students' exam scores
stud.cl5.stats <- summary.stats(student.clusters[,c(4,5)], student.clusters$cl5, 5)
kable(x = stud.cl5.stats, format = "rst")

## examine differences across the clusters
kruskal.test(student.clusters$SC_FE_TOT ~ student.clusters$cl5)
## there is a sign. difference bewtween the clusters; check where the difference is

## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
stud.5cl.df <- student.clusters[,c(1,2,4,5)]
colnames(stud.5cl.df)[2] <- "class"
kable(pairwise.exam.compare(5, 10, stud.5cl.df, "FE"), format = "rst")

## check the difference based on the midterm score
kruskal.test(student.clusters$SC_MT_TOT ~ student.clusters$cl5)

## apply Mann-Whitney U Test to do pair-wise comparisons
kable(pairwise.exam.compare(5, 10, stud.5cl.df, "MT"), format = "rst")

##################################################################
## examine the identified student clusters by comparing them  
## based on proportion of the 6 different types of states 
## (study tactics) in the students' state sequences 
##################################################################

stud.states.clust <- merge.state.dist.and.clusters(src.data, student.clusters, 6)

## check if the number of states is normally distributed
apply(X = stud.states.clust[,c(2:7)], MARGIN = 2, FUN = shapiro.test)
# none is normaly distributed

## compare the states in the 4 clusters solution:
clust4.stats <- summary.stats(stud.states.clust[,c(2:8)], stud.states.clust$cl4, 4)
kable(clust4.stats, format = "rst")

## create a df with percentages of states for each student  
stud.states.perc <- stud.states.clust
for(r in 1:n.stud) {
  stud.states.perc[r,c(2:7)] <- stud.states.perc[r,c(2:7)]/sum(stud.states.perc[r,c(2:7)])
}
head(stud.states.perc)
colnames(stud.states.perc)[2:7] <- paste0("ST",1:6,"_PERC")

## plot the distribution of states for each student
## first, create long format suitable for plotting
## create long format suitable for plotting
library(tidyr)
states.perc.long <- gather(data = stud.states.perc, key = state,
                           value = percent, ... = ST1_PERC:ST6_PERC, factor_key = T)
## use the same pallet as the one used for plotting clusters
custom.pallet <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
plot.session.percents(states.perc.long, n.states = 6, c.pallet = custom.pallet)

## compare the proportion of states in the 4 clusters solution:
clust4.perc.stats <- summary.stats(stud.states.perc[,c(2:8)], stud.states.perc$cl4, 4)
kable(clust4.perc.stats, format = "rst")

## use statitical tests to compare the clusters with respect to each of the STx_PERC, x=1:6 variables
## check if the variables representing proportions of states are normally distributed
apply(X = stud.states.perc[,c(2:7)], MARGIN = 2, FUN = shapiro.test)
# none is normally distributed, use Kruskal-Wallis test for all the variables
for(v in 2:7)
  print(kruskal.test(as.vector(stud.states.perc[,v]) ~ stud.states.perc$cl4))

## compare the proportion of states in the 5 clusters solution:
clust5.perc.stats <- summary.stats(stud.states.perc[,c(2:8)], stud.states.perc$cl5, 5)
kable(clust5.perc.stats, format = "rst")
## use statitical tests to compare the clusters with respect to each of the STx_PERC, x=1:6 variables
for(v in 2:7)
  print(kruskal.test(as.vector(stud.states.perc[,v]) ~ stud.states.perc$cl5))


#############################
## DATASET SPECIFIC FUNCTIONS
#############################

## the f. adds the students' scores on the midterm and final exam
## to the data frame with cluster data
add.scores.to.clusters <- function(clust.data) {
  ## load all the scores
  all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
  ## keep only the relevant score
  scores <- all.scores[, c('user_id', 'SC_FE_TOT', 'SC_MT_TOT')]
  
  s.diff <- setdiff(clust.data$STUDENT_ID, scores$user_id)
  if (length(s.diff) > 0)
    print(paste("socres are not available for students with IDs:", s.diff))
  
  clust.data <- merge(x = clust.data, y = scores,
                      by.x = "STUDENT_ID", by.y = "user_id",
                      all.x = T, all.y = F)
  clust.data
}

