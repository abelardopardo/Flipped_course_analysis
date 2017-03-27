library(TraMineR)
library(seqHMM)

seed <- 2408

## build a HMM with 4 hidden states, each state corresponding to one of the 
## identified learning strategies:
##  1) focus on formative assessment
##  2) focus on summative assessment
##  3) focus on course readings
##  4) focus on course videos
nstates <- 4

## define initial probabilities for hidden states
## due to the lack of prior knowledge about these states
## make each state almost equally probable - 
## strategies based on reading and videos might be receive more attention
## in the initial learning phase, so, they are assigned a bit higher probabilities 
hidden.stats.init <- c(0.20,0.20,0.30,0.30)

## define initial values for the transition matrix
## again, I'll make all transitions equally likely
## only, assign somewhat higher probability to keeping 
## the same state / stategy
trans.init <- matrix(data = 0.20, nrow = 4, ncol = 4)
diag(trans.init) <- 0.40

## define initial values for emission probabilities
## the order of observed states:
# [1] "CONTENT_ACCESS" "EXE_CO"         "EXE_IN"         "MCQ_CO"         "MCQ_IN"        
# [6] "MCQ_SR"         "MC_EVAL"        "MC_ORIENT"      "VIDEO_PLAY"   
emiss.init <- matrix(NA, nrow = 4, ncol = 9)
## stategy 1: focus on formative assessment
emiss.init[1,] <- c(0.2, 0.01, 0.01, 0.3, 0.225, 0.125, 0.02, 0.01, 0.1)
## strategy 2: focus on summative assessment
emiss.init[2,] <- c(0.05, 0.35, 0.45, 0.05, 0.025, 0.01, 0.02, 0.01, 0.035)
## stategy 3: focus on course readings
emiss.init[3,] <- c(0.80, 0.01, 0.01, 0.025, 0.025, 0.01, 0.01, 0.01, 0.1)
## stategy 3: focus on course videos
emiss.init[4,] <- c(0.175, 0.02, 0.02, 0.175, 0.09, 0.05, 0.01, 0.01, 0.45) 
rowSums(emiss.init)

####################################################################
## CREATE HMM FOR STUDENTS FROM THE HIGH PERFORMING CLUSTERS:
## INTENSIVE, STRATEGIC AND HIGHLY STRATEGIC; AND ONLY FOR WEEKS 2-6
## (TILL MIDTERM) - THE PERIOD WHEN THESE STUDENTS USED DIFFERENT 
## STRATEGIES
####################################################################

## load learning sequences from weeks 2-6 of students from the 3 high performing clusters
## Intensive, Strategic and Highly strategic
load(file = "Intermediate_files/high_perf_clusters_weeks_2-5_sequences_SPS_format.RData") 
str(high.perf.w2to5.seq)

## plot the distribution of learning actions across the learning sequences
ssplot(high.perf.w2to5.seq, 
       type = "d", 
       ylab = "Proportion", yaxis = TRUE, 
       title = "Distribution of learning actions for high performing students in weeks 2-5",
       legend.prop = 0.4)

## create a model
hmm.high.w2to5 <- build_hmm(observations = high.perf.w2to5.seq, 
                            initial_probs = hidden.stats.init,
                            transition_probs = trans.init, 
                            emission_probs = emiss.init)

## fit the model; by default, it is done using EM only
## in this case, we fit a model using 10 random restarts of the EM algorithm
## followed by the local optimization step
set.seed(seed)
hmm.high.w2to5_fit <- fit_model(hmm.high.w2to5,
                                local_step = TRUE, 
                                control_em = list(restart = list(times = 10))) 
## get the resulting model
hmm.high.w2to5_fit$model
## loglikelihood of the model
hmm.high.w2to5_fit$logLik
## BIC
BIC(hmm.high.w2to5_fit$model)
## plot the model
plot(hmm.high.w2to5_fit$model,
     vertex.size = 45, vertex.label.dist = 1.5,
 #    edge.curved = c(0, 0.6, -0.8, 0.6, 0, 0.6, 0),
     legend.prop = 0.3, combined.slice.label = "States with prob. < 0.05")

########################################################################
## CREATE HMM FOR STUDENTS FROM THE LOW PERFORMING CLUSTERS:
## SELECTIVE AND HIGHLY SELECTIVE; AND ONLY FOR WEEKS 2-5 (TILL MIDTERM)
########################################################################

## load learning sequences from weeks 2-5 of students from the 2 low performing clusters
low.w2to5.seq <- readRDS(file = "Intermediate_files/low_perf_clusters_weeks_2-5_sequences_SPS_format.RData") 
str(low.w2to5.seq)

## plot the distribution of learning actions across the learning sequences
ssplot(low.w2to5.seq, 
       type = "d", 
       ylab = "Proportion", yaxis = TRUE, 
       title = "Distribution of learning actions for low performing students in weeks 2-5",
       legend.prop = 0.4)

## create a model
hmm.low.w2to5 <- build_hmm(observations = low.w2to5.seq, 
                            initial_probs = hidden.stats.init,
                            transition_probs = trans.init, 
                            emission_probs = emiss.init)
set.seed(seed)
hmm.low.w2to5_fit <- fit_model(hmm.low.w2to5,
                                local_step = TRUE, 
                                control_em = list(restart = list(times = 10))) 

## get the resulting model
hmm.low.w2to5_fit$model
## loglikelihood of the model
hmm.low.w2to5_fit$logLik
## BIC
BIC(hmm.low.w2to5_fit$model)
## plot the model
plot(hmm.low.w2to5_fit$model,
     vertex.size = 45, vertex.label.dist = 1.5,
     #    edge.curved = c(0, 0.6, -0.8, 0.6, 0, 0.6, 0),
     legend.prop = 0.25, combined.slice.label = "States with prob. < 0.05")
