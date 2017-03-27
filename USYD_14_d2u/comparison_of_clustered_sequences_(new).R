###########################################################################################
## COMPARE CLUSTERS/GROUPS OBTAINED FOR THE LEARNING PATHS (SEQUENCES) THROUGH THE WHOLE 
## COURSE (WEEKS 2-13) BASED ON THE STUDENTS MIDTERM AND FINAL EXAM SCORES 
############################################################################################

## load util functions required for comparison of student clusters
source(file = "util_functions.R")

# ## classes that students were assigned to using the LCA method 
clusters.w213 <- read.csv(file = "results/lca_w2_to_w13_6classes(April2016).csv")
#clusters.w213 <- read.csv(file = "results/lca_w2_to_w13_(March2016).csv")
#clusters.w213 <- read.csv(file = "results/lca_w2_to_w13_4classes(March2016).csv")

## clusters/groups that students were assigned to when clustering the sequences of
## their weekly learning 'states' (clusters) 
## reminder: this is the output of sequence analysis doen with TraMineR
# clusters.w213 <- read.csv(file = "results/coarse_clusters_w2-13_plus_sequence_clusters(feb2016).csv")
str(clusters.w213)
seq.classes <- clusters.w213[,c(1,14)] 

## get the students' scores on the final exam
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the relevant score
scores <- all.scores[, c('user_id', 'SC_MT_TOT', 'SC_FE_TOT')]

## check if there is difference between the users for whom scores are available and
## those for whom class info is available
setdiff(seq.classes$user_id, scores$user_id)
## no difference, excellent

classes.plus.scores <- merge(x = seq.classes, y = scores, by = 'user_id', all = T)
str(classes.plus.scores)

## first, compute descriptive statistics
# install.packages('plyr')
library(plyr)

## compute summary statistics for the final exam score for each class
colnames(classes.plus.scores)[2] <- 'class'
fe.summary <- final.score.per.class.stats(classes.plus.scores)                  
# this is for generating nice table layout
require(knitr)
kable(x = fe.summary, format = "rst")
## do the same for midterm exam scores
mt.summary <- midterm.score.per.class.stats(classes.plus.scores)                  
kable(x = mt.summary, format = "rst")

## check if the exam scores are normally distributed
qqnorm(classes.plus.scores$SC_FE_TOT)
qqline(classes.plus.scores$SC_FE_TOT)
shapiro.test(classes.plus.scores$SC_FE_TOT)
## W = 0.95035, p-value = 2.418e-08 - far from normally distributed
shapiro.test(classes.plus.scores$SC_MT_TOT)
## W = 0.95137, p-value = 3.169e-08
## use non-parametric tests

kruskal.test(classes.plus.scores$SC_FE_TOT ~ classes.plus.scores$class)
## chi-squared = 46.871, df = 5, p-value = 6.035e-09
## there is a sign. difference between the classes w.r.t. the final exam score
kruskal.test(classes.plus.scores$SC_MT_TOT ~ classes.plus.scores$class)
## chi-squared = 36.259, df = 5, p-value = 8.429e-07
## the same applies to midterm exam score

## apply Mann-Whitney U Test to do pair-wise comparisons for all class pairs
## first for final exam score
# fe.comparison <- matrix(nrow = 15, ncol = 5, byrow = T)
# k <- 1
# for(i in 1:5) {
#   for(j in (i+1):6) {
#     fe.comparison[k,] <- c(i, j, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, i, j))
#     k <- k+1
#   }
# }

## do the comparison only for the following classes: 1,2,4,6
fe.comparison <- matrix(nrow = 6, ncol = 5, byrow = T)
fe.comparison[1,] <- c(1, 2, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 1, 2))
fe.comparison[2,] <- c(1, 4, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 1, 4))
fe.comparison[3,] <- c(1, 6, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 1, 6))
fe.comparison[4,] <- c(2, 4, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 2, 4))
fe.comparison[5,] <- c(2, 6, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 2, 6))
fe.comparison[6,] <- c(4, 6, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 4, 6))
fe.comparison.df <- as.data.frame(fe.comparison)
colnames(fe.comparison.df) <- c('c1', 'c2', 'Z', 'p', 'effect.size')
## apply the FDR correction to the comparisons
fe.comparison.df <- apply.FDR.correction(fe.comparison.df)
kable(x = fe.comparison.df, format = 'rst')

## now for midterm exam score
# mt.comparison <- matrix(nrow = 15, ncol = 5, byrow = T)
# k <- 1
# for(i in 1:5) {
#   for(j in (i+1):6) {
#     mt.comparison[k,] <- c(i, j, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, i, j))
#     k <- k+1
#   }
# }

## do the comparison only for the following classes: 1,2,4,6
mt.comparison <- matrix(nrow = 6, ncol = 5, byrow = T)
mt.comparison[1,] <- c(1, 2, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 1, 2))
mt.comparison[2,] <- c(1, 4, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 1, 4))
mt.comparison[3,] <- c(1, 6, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 1, 6))
mt.comparison[4,] <- c(2, 4, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 2, 4))
mt.comparison[5,] <- c(2, 6, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 2, 6))
mt.comparison[6,] <- c(4, 6, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 4, 6))
mt.comparison.df <- as.data.frame(mt.comparison)
colnames(mt.comparison.df) <- c('c1', 'c2', 'Z', 'p', 'effect.size')
## apply the FDR correction to the comparisons
mt.comparison.df <- apply.FDR.correction(mt.comparison.df)
kable(x = mt.comparison.df, format = 'rst')


################################################################################################
## COMPARE CLUSTERS/GROUPS OBTAINED FOR THE LEARNING PATHS THROUGH THE WHOLE COURSE (WEEKS 2-13) 
## BASED ON THE STUDENTS' PROFILE DERIVED THROUGH THE MSLQ QUESTIONNAIRE
################################################################################################
##
## MSLQ MEASURES ACADEMIC MOTIVATION AND TYPES OF LEARNING STRATEGIES; IN PARTICULAR, THE
## FOLLOWING VARIABLES ARE EXAMINED:
## - IVAL: Intrinsic Value
## - SEFF: Self-efficacy
## - TANX: Test anxiety
## - CSUS: Cognitive Strategy Use
## - SREL: Self regulation
##
############################################################################################

## classes that students were assigned to using the LCA method 
## clusters.w213 <- read.csv(file = "results/lca_w2_to_w13_(feb2016).csv")

## clusters that students were assigned to based on the detected learning strategies
## (i.e., detected clusters of learning sequences)
clusters.w213 <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv")

str(clusters.w213)
stud.clusters <- clusters.w213[,c(1,7)] 

## get the students' scores on the above given MSLQ variables
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the MSLQ-related scores 
mslq.scores <- all.scores[, c('user_id', 'MSLQ_IVAL', 'MSLQ_SEFF', 'MSLQ_TANX', 'MSLQ_CSUS', 'MSLQ_SREL')]
str(mslq.scores)

## compute for how many students the MSQL scores are available
length(which(complete.cases(mslq.scores)))
## 144

## merge the users' classes and their mslq scores
user.data <- merge(x = stud.clusters, y = mslq.scores, by.x = "stud.id", by.y = "user_id", all = T)
str(user.data)
colnames(user.data)[2] <- 'class'
user.data$class <- factor(user.data$class)

## first, compute descriptive statistics
#install.packages('plyr')
library(plyr)

## compute summary statistics for each of the MSLQ variables and each LCA class

#############
# MSLQ_IVAL #
#############
mslq.ival.stats <- ddply(user.data, 
                    c('class'), 
                    summarise,
                    N    = length(MSLQ_IVAL),
                    N_NotNA = count(is.na(MSLQ_IVAL))[1,2],
                    median = round(median(MSLQ_IVAL, na.rm = T), digits = 2),
                    Q1  = round(quantile( MSLQ_IVAL, probs = 0.25, names = F, na.rm = T), digits = 2),
                    Q3  = round(quantile( MSLQ_IVAL, probs = 0.75, names = F, na.rm = T),digits = 2))

# this is for generating nice table layout
require(knitr)
kable(x = mslq.ival.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
library(car)
qqnorm(user.data$MSLQ_IVAL)
qqline(user.data$MSLQ_IVAL)
shapiro.test(user.data$MSLQ_IVAL)
# W = 0.98325, p-value = 0.07632
# normal distribution can be assumed
# check for the equality in variation of means
leveneTest(user.data$MSLQ_IVAL ~ user.data$class)
# p=0.9996 so, homogeneity of variance can be assumed 
# and anova can be applied
aov.ival <- aov(MSLQ_IVAL ~ class, data = user.data)
summary(aov.ival)
## F(4,139)=0.772, p=0.545 
## NO SIG. DIFFERENCE BETWEEN CLASSES

#############
# MSLQ_SEFF #
#############
mslq.seff.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(MSLQ_SEFF),
                         N_NotNA = count(is.na(MSLQ_SEFF))[1,2],
                         median = round(median(MSLQ_SEFF, na.rm = T), digits = 2),
                         Q1  = round(quantile( MSLQ_SEFF, probs = 0.25, names = F, na.rm = T), digits = 2),
                         Q3  = round(quantile( MSLQ_SEFF, probs = 0.75, names = F, na.rm = T),digits = 2))

#require(knitr)
kable(x = mslq.seff.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
# library(car)
qqnorm(user.data$MSLQ_SEFF)
qqline(user.data$MSLQ_SEFF)
shapiro.test(user.data$MSLQ_SEFF)
# W = 0.99335, p-value = 0.7454
# normal distribution can be assumed
# check for the equality in variation of means
leveneTest(user.data$MSLQ_SEFF ~ user.data$class)
# p=0.705 so, homogeneity of variance can be assumed 
# and anova can be applied
aov.seff <- aov(MSLQ_SEFF ~ class, data = user.data)
summary(aov.seff)
## F(4,139)=0.258, p=0.904
## NO SIG. DIFFERENCE BETWEEN CLASSES

#############
# MSLQ_TANX #
#############
mslq.tanx.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(MSLQ_TANX),
                         N_NotNA = count(is.na(MSLQ_TANX))[1,2],
                         median = round(median(MSLQ_TANX, na.rm = T), digits = 2),
                         Q1  = round(quantile( MSLQ_TANX, probs = 0.25, names = F, na.rm = T), digits = 2),
                         Q3  = round(quantile( MSLQ_TANX, probs = 0.75, names = F, na.rm = T),digits = 2))

#require(knitr)
kable(x = mslq.tanx.stats, format = "rst")

## check if the MSLQ_TANX is normally distributed
# library(car)
qqnorm(user.data$MSLQ_TANX)
qqline(user.data$MSLQ_TANX)
shapiro.test(user.data$MSLQ_TANX)
# W = 0.98138, p-value = 0.04751
# the p-value is very close to be acceptable
# check for the equality in variation of means
leveneTest(user.data$MSLQ_TANX ~ user.data$class)
# p=0.453 so, homogeneity of variance can be assumed 
# all in all anova can be used

aov.tanx <- aov(MSLQ_TANX ~ class, data = user.data)
summary(aov.tanx)
## F(4,139)=0.1071, p=0.373
# NO SIG. DIFFERENCE BETWEEN CLASSES

#############
# MSLQ_CSUS #
#############
mslq.csus.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(MSLQ_CSUS),
                         N_NotNA = count(is.na(MSLQ_CSUS))[1,2],
                         median = round(median(MSLQ_CSUS, na.rm = T), digits = 2),
                         Q1  = round(quantile( MSLQ_CSUS, probs = 0.25, names = F, na.rm = T), digits = 2),
                         Q3  = round(quantile( MSLQ_CSUS, probs = 0.75, names = F, na.rm = T),digits = 2))

#require(knitr)
kable(x = mslq.csus.stats, format = "rst")

## check if the MSLQ_CSUS is normally distributed
# library(car)
qqnorm(user.data$MSLQ_CSUS)
qqline(user.data$MSLQ_CSUS)
shapiro.test(user.data$MSLQ_CSUS)
# W = 0.99012, p-value = 0.4069
# normal distribution can be assumed
# check for the equality in variation of means
leveneTest(user.data$MSLQ_CSUS ~ user.data$class)
# p=0.2751 so, homogeneity of variance can be assumed 
aov.csus <- aov(MSLQ_CSUS ~ class, data = user.data)
summary(aov.csus)
## F(4,139)=0.405, p=0.805 
## NO SIG. DIFFERENCE BETWEEN CLASSES

#############
# MSLQ_SREL #
#############
mslq.srel.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(MSLQ_SREL),
                         N_NotNA = count(is.na(MSLQ_SREL))[1,2],
                         median = round(median(MSLQ_SREL, na.rm = T), digits = 2),
                         Q1  = round(quantile( MSLQ_SREL, probs = 0.25, names = F, na.rm = T), digits = 2),
                         Q3  = round(quantile( MSLQ_SREL, probs = 0.75, names = F, na.rm = T),digits = 2))

#require(knitr)
kable(x = mslq.srel.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
#library(car)
qqnorm(user.data$MSLQ_SREL)
qqline(user.data$MSLQ_SREL)
shapiro.test(user.data$MSLQ_SREL)
# W = 0.99229, p-value = 0.6274
# normal distribution can be assumed
# check for the equality in variation of means
leveneTest(user.data$MSLQ_SREL ~ user.data$class)
# p=0.8034 so, homogeneity of variance can be assumed 
aov.srel <- aov(MSLQ_SREL ~ class, data = user.data)
summary(aov.srel)
## F(4,139)=0.443, p=0.778 
## NO SIG. DIFFERENCE BETWEEN CLASSES


############################################################################################
## COMPARE STUDENT CLASSES/GROUPS OBTAINED FOR THE LEARNING PATHS THROUGH THE WHOLE COURSE
##  (WEEKS 2-13) BASED ON THE STUDENTS' PROFILE DERIVED THROUGH THE STUDY PROCESS QUESTIONNAIRE
############################################################################################
##
## IN PARTICULAR, THE FOLLOWING VARIABLES ARE EXAMINED:
## - DA: Deep approach
## - SA: Surface approach
## - DM: Deep motive
## - SM: Surface motive
## - DS: Deep strategy
## - SS: Surface strategy
##
############################################################################################
source('util_functions.R')

clusters.w213 <- read.csv(file = "results/sequence_based_SPQ_student_clusters_4cl.csv")
#clusters.w213 <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv")
str(clusters.w213)
stud.clusters <- clusters.w213[,c(1,7)] 

## get the students' scores on the above given SPQ variables
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## extract the SPQ-related scores 
spq.scores <- all.scores[, c('user_id', 'SPQ_DA', 'SPQ_SA', 'SPQ_DM', 'SPQ_SM', 'SPQ_DS', 'SPQ_SS')]
str(spq.scores)

## check the number of students who filled in the questionnaire
not.NA.scores <-  which(complete.cases(spq.scores))
length(not.NA.scores)
## 144
## remove rows with NA for scores
spq.scores <- spq.scores[not.NA.scores,]

## merge the users' classes and their mslq scores
user.data <- merge(x = stud.clusters, y = spq.scores, 
                   by.x = 'stud.id', by.y = 'user_id', all.x = F, all.y = T)
str(user.data)
colnames(user.data)[2] <- 'class'
user.data$class <- factor(user.data$class)

## Now, group students from clusters 1 and 2 as students following
## deep approach (based on the analysis of the trace data);
## likewise, group clusters 4 and 5 as a group of students following 
## surface approach; cluster 3 will stay as a group of stragegic learners
## (the rationale: comparison of clusters did not reveale any significant 
## difference with respect to the SPQ variables)
str(user.data)
user.data$la.group <- user.data$class
# for (i in 1:nrow(user.data)) {
#   if (user.data$la.group[i] %in% c(1,2))
#     user.data$la.group[i] <- 1
#   else if (user.data$la.group[i] == 3)
#     user.data$la.group[i] <- 2
#   else user.data$la.group[i] <- 3
# }
for (i in 1:nrow(user.data)) {
   user.data$la.group[i] <- ifelse(user.data$la.group[i] %in% c(1,2), yes = 1, no = 2)
}
user.data$la.group <- factor(user.data$la.group)


##########
# SPQ_DA #
##########

## check if the SPQ_DA is normally distributed
library(car)
qqnorm(user.data$SPQ_DA)
qqline(user.data$SPQ_DA)
shapiro.test(user.data$SPQ_DA)
# W = 0.98796, p-value = 0.2472
# normal distribution can be assumed

spq.da.stats <- ddply(user.data, 
                      c('la.group'), #c('class'), 
                        summarise,
                        N    = length(SPQ_DA),
                        #N_NotNA = count(is.na(SPQ_DA))[1,2],
                        median = round(median(SPQ_DA, na.rm = T), digits = 2),
                        #mean = round(mean(SPQ_DA, na.rm = T), digits = 2),
                        #SD  = round(sd( SPQ_DA, na.rm = T), digits = 2))
                        Q1  = round(quantile( SPQ_DA, probs = 0.25, names = F, na.rm = T), digits = 2),
                        Q3  = round(quantile( SPQ_DA, probs = 0.75, names = F, na.rm = T),digits = 2))

require(knitr)
kable(x = spq.da.stats, format = "rst")

# check for the equality in variation of means
leveneTest(user.data$SPQ_DA ~ user.data$class)
# p=0.443 so, homogeneity of variance can be assumed 
aov.da <- aov(SPQ_DA ~ class, data = user.data)
summary(aov.da)
## NO SIG. DIFFERENCE between clusters

## now, do the comparison based on the learning approaches
leveneTest(user.data$SPQ_DA ~ user.data$la.group)
# p=0.1104 so, homogeneity of variance can be assumed 
summary(aov(SPQ_DA ~ la.group, data = user.data))
## F(2,141)=3.501, p=0.033
## use t-test to do pairwise comparison
compare.SPQ_DA <- function(df, c1, c2) {
  tt.res <- t.test(df$SPQ_DA[df$la.group==c1], df$SPQ_DA[df$la.group==c2], var.equal = T)
  cD <- cohens_d(df$SPQ_DA[df$la.group == c1], df$SPQ_DA[df$la.group == c2]) 
  c(c1=c1, c2=c2, round(tt.res$statistic,4), p=round(tt.res$p.value, digits = 4), cohen_d=round(cD, digits = 4))
}
comparisons <- matrix(nrow=3, ncol = 5, byrow = T)
comparisons[1,] <- compare.SPQ_DA(user.data, 1, 2)
comparisons[2,] <- compare.SPQ_DA(user.data, 1, 3)
comparisons[3,] <- compare.SPQ_DA(user.data, 2, 3)
comparisons.df <- as.data.frame(comparisons)
colnames(comparisons.df) <- c('G1', 'G2', 't', 'p', 'cohen_d')
comparisons.df <- apply.FDR.correction(comparisons.df)
kable(comparisons.df, format = 'rst')


## apply Mann-Whitney U Test to do pair-wise comparisons
## in case when considering only 2 distinct learning approach groups
do.Mann.Whitney.test(user.data$SPQ_DA[user.data$la.group==1], 
                     user.data$SPQ_DA[user.data$la.group==2], 
                     1, 2)

##########
# SPQ_SA #
##########

## check if the SPQ_SA is normally distributed
# library(car)
qqnorm(user.data$SPQ_SA)
qqline(user.data$SPQ_SA)
shapiro.test(user.data$SPQ_SA)
# W = 0.989, p-value = 0.3162
# normal distribution can be assumed

spq.sa.stats <- ddply(user.data, 
                      c('la.group'), # c('class'), 
                      summarise,
                      N    = length(SPQ_SA),
                      #N_NotNA = count(is.na(SPQ_SA))[1,2],
                      median = round(median(SPQ_SA, na.rm = T), digits = 2),
                      #mean = round(mean(SPQ_SA, na.rm = T), digits = 2),
                      #SD  = round(sd( SPQ_SA, na.rm = T), digits = 2))
                      Q1  = round(quantile( SPQ_SA, probs = 0.25, names = F, na.rm = T), digits = 2),
                      Q3  = round(quantile( SPQ_SA, probs = 0.75, names = F, na.rm = T),digits = 2))

# require(knitr)
kable(x = spq.sa.stats, format = "rst")

# check for the equality in variation of means
leveneTest(user.data$SPQ_SA ~ user.data$class)
# # p=0.6615 so, homogeneity of variance can be assumed 
aov.sa <- aov(SPQ_SA ~ class, data = user.data)
summary(aov.sa)
## NO SIG. DIFFERENCE BETWEEN CLASSES

leveneTest(user.data$SPQ_SA ~ user.data$la.group)
# p=0.7781 so, homogeneity of variance can be assumed 
summary(aov(SPQ_SA ~ la.group, data = user.data))
## F(2,141)=3.01, p=0.0525
## use t-test to do pairwise comparison
compare.SPQ_SA <- function(df, c1, c2) {
  tt.res <- t.test(df$SPQ_SA[df$la.group==c1], df$SPQ_SA[df$la.group==c2], var.equal = T)
  cD <- cohens_d(df$SPQ_SA[df$la.group == c1], df$SPQ_SA[df$la.group == c2]) 
  c(c1=c1, c2=c2, round(tt.res$statistic,4), p=round(tt.res$p.value, digits=4), cohen_d=round(cD, digits = 4))
}
comparisons <- matrix(nrow=3, ncol = 5, byrow = T)
comparisons[1,] <- compare.SPQ_SA(user.data, 1, 2)
comparisons[2,] <- compare.SPQ_SA(user.data, 1, 3)
comparisons[3,] <- compare.SPQ_SA(user.data, 2, 3)
comparisons.df <- as.data.frame(comparisons)
colnames(comparisons.df) <- c('G1', 'G2', 't', 'p', 'cohen_d')
comparisons.df <- apply.FDR.correction(comparisons.df)
kable(comparisons.df, format = 'rst')

##########
# SPQ_DM #
##########

## check if the SPQ_DM is normally distributed
#library(car)
qqnorm(user.data$SPQ_DM)
qqline(user.data$SPQ_DM)
shapiro.test(user.data$SPQ_DM)
# W = 0.98755, p-value = 0.2237
# normal distribution can be assumed

spq.dm.stats <- ddply(user.data, 
                      c('la.group'), #c('class'), 
                      summarise,
                      N    = length(SPQ_DM),
                      #N_NotNA = count(is.na(SPQ_DM))[1,2],
                      median = round(median(SPQ_DM, na.rm = T), digits = 2),
                      #mean = round(mean(SPQ_DM, na.rm = T), digits = 2),
                      #SD  = round(sd(SPQ_DM, na.rm = T), digits = 2))
                      Q1  = round(quantile( SPQ_DM, probs = 0.25, names = F, na.rm = T), digits = 2),
                      Q3  = round(quantile( SPQ_DM, probs = 0.75, names = F, na.rm = T),digits = 2))


#require(knitr)
kable(x = spq.dm.stats, format = "rst")

# check for the equality in variation of means
leveneTest(user.data$SPQ_DM ~ user.data$class)
# # p=0.985 so, homogeneity of variance can be assumed 
aov.dm <- aov(SPQ_DM ~ class, data = user.data)
summary(aov.dm)
# ## F(4,139)=0.917, p=0.456
# ## NO SIG. DIFFERENCE BETWEEN CLASSES

## Now, do the comparison based on the approaches to learning
## observed in the trace data
leveneTest(user.data$SPQ_DM ~ user.data$la.group)
# p=0.8062 so, homogeneity of variance can be assumed 
summary(aov(SPQ_DM ~ la.group, data = user.data))
## F(2,141)=1.603, p=0.205
## NO SIG. DIFFERENCE BETWEEN GROUPS


##########
# SPQ_SM #
##########

## check if the SPQ_SM is normally distributed
# library(car)
qqnorm(user.data$SPQ_SM)
qqline(user.data$SPQ_SM)
shapiro.test(user.data$SPQ_SM)
# W = 0.97643, p-value = 0.01379
# normal distribution cannot be assumed

spq.sm.stats <- ddply(user.data, 
                      c('la.group'), #c('class'), 
                      summarise,
                      N    = length(SPQ_SM),
                     # N_NotNA = count(is.na(SPQ_SM))[1,2],
                      median = round(median(SPQ_SM, na.rm = T), digits = 2),
                      Q1  = round(quantile( SPQ_SM, probs = 0.25, names = F, na.rm = T), digits = 2),
                      Q3  = round(quantile( SPQ_SM, probs = 0.75, names = F, na.rm = T),digits = 2))

# require(knitr)
kable(x = spq.sm.stats, format = "rst")

# still, check for the equality in variation of means
leveneTest(user.data$SPQ_SM ~ user.data$class)
# p=0.629 so, homogeneity of variance can be assumed 
## do both anova and kruskal-wallis
aov.sm <- aov(SPQ_SM ~ class, data = user.data)
summary(aov.sm)
## F(4,139)=1.626, p=0.171
kruskal.test(SPQ_SM ~ class, data = user.data)
## chi-squared = 5.5085, df = 4, p-value = 0.239
## NO SIG. DIFFERENCE BETWEEN CLASSES


## Now, do the comparison based on the approaches to learning
## observed in the trace data
leveneTest(user.data$SPQ_SM ~ user.data$la.group)
# p=0.8887 so, homogeneity of variance can be assumed 
## do both anova and kruskal-wallis (distribution is not normal)
summary(aov(SPQ_SM ~ la.group, data = user.data))
## F(2,141)=2.064, p=0.131
kruskal.test(SPQ_SM ~ la.group, data = user.data)
## chi-squared = 3.903, df = 2, p-value = 0.1421
## NO SIG. DIFFERENCE BETWEEN GROUPS



##########
# SPQ_DS #
##########

## check if the SPQ_DS is normally distributed
#library(car)
qqnorm(user.data$SPQ_DS)
qqline(user.data$SPQ_DS)
shapiro.test(user.data$SPQ_DS)
# W = 0.98296, p-value = 0.07079
# normal distribution can be assumed (though the QQ-plot suggests otherwise)

spq.ds.stats <- ddply(user.data, 
                      c('la.group'), #c('class'), 
                      summarise,
                      N    = length(SPQ_DS),
                      # N_NotNA = count(is.na(SPQ_DS))[1,2],
                      median = round(median(SPQ_DS, na.rm = T), digits = 2),
                      #m = round(mean(SPQ_DS, na.rm = T), digits = 2),
                      #sd = round(sd(SPQ_DS, na.rm = T), digits = 2))
                      Q1  = round(quantile( SPQ_DS, probs = 0.25, names = F, na.rm = T), digits = 2),
                      Q3  = round(quantile( SPQ_DS, probs = 0.75, names = F, na.rm = T),digits = 2))

#require(knitr)
kable(x = spq.ds.stats, format = "rst")

# check for the equality in variation of means
leveneTest(user.data$SPQ_DS ~ user.data$class)
# # p=0.1368 so, homogeneity of variance can be assumed 
aov.ds <- aov(SPQ_DS ~ class, data = user.data)
summary(aov.ds)
# ## F(4,139)=2.818, p=0.0414
# ## THERE IS A SIG. DIFFERENCE BETWEEN CLASSES, BUT IT WILL PROBABLY 
# ## FADE AWAY IN WHEN APPLYING CORRECTION FOR PAIR-WISE TESTS

# ## use t-test to do pairwise comparison
# compare.SPQ_DS <- function(df, c1, c2) {
#   tt.res <- t.test(df$SPQ_DS[df$class==c1], df$SPQ_DS[df$class==c2], var.equal = T)
#   c(c1=c1, c2=c2, round(tt.res$statistic,4), p=round(tt.res$p.value, digits = 6))
# }
# comparisons <- matrix(nrow=10, ncol = 4, byrow = T)
# k <- 1
# for(i in 1:4) {
#   for(j in (i+1):5) {
#     comparisons[k,] <- compare.SPQ_DS(user.data, i, j)
#     k <- k+1
#   }
# }
# comparisons.df <- as.data.frame(comparisons)
# colnames(comparisons.df) <- c('C1', 'C2', 't', 'p')
# comparisons.df[order(comparisons.df$p),]
# 
# ## apply FDR correction
# source(file = "util_functions.R")
# comparisons.df <- apply.FDR.correction(comparisons.df)
# comparisons.df
# ## NONE OF THE PAIRWISE COMPARISONS IS SIGNIFICANT

## do the comparison based on the approaches to learning
## observed in the trace data
leveneTest(user.data$SPQ_DS ~ user.data$la.group)
# p=0.0345 so, homogeneity of variance cannot be assumed 
## apply non-parametric test 
kruskal.test(SPQ_DS ~ la.group, data = user.data)
## chi-squared = 8.72, df = 2, p-value = 0.01278
## apply Mann-Whitney U Test to do pair-wise comparisons
compare.SPQ_DS.Mann.Whitney <- function(df, c1, c2) {
  score.c1 <- df$SPQ_DS[df$la.group == c1]
  score.c2 <- df$SPQ_DS[df$la.group == c2]
  do.Mann.Whitney.test(score.c1, score.c2, c1, c2)
}
comparison <- matrix(nrow = 3, ncol = 5, byrow = T)
comparison[1,] <- c(1, 2, compare.SPQ_DS.Mann.Whitney(user.data, 1, 2))
comparison[2,] <- c(1, 3, compare.SPQ_DS.Mann.Whitney(user.data, 1, 3))
comparison[3,] <- c(2, 3, compare.SPQ_DS.Mann.Whitney(user.data, 2, 3))
comparison.df <- as.data.frame(comparison)
colnames(comparison.df) <- c('c1', 'c2', 'Z', 'p', 'cohen_d')
comparison.df <- apply.FDR.correction(comparison.df)
kable(x = comparison.df, format = 'rst')



##########
# SPQ_SS #
##########

## check if the SPQ_SS is normally distributed
#library(car)
qqnorm(user.data$SPQ_SS)
qqline(user.data$SPQ_SS)
shapiro.test(user.data$SPQ_SS)
# W = 0.98723, p-value = 0.2069
# normal distribution can be assumed

spq.ss.stats <- ddply(user.data, 
                       c('la.group'), #c('class'), 
                       summarise,
                       N    = length(SPQ_SS),
                       # N_NotNA = count(is.na(SPQ_SS))[1,2],
                       median = round(median(SPQ_SS, na.rm = T), digits = 2),
                       #m = round(mean(SPQ_SS, na.rm = T), digits = 2),
                       #sd = round(sd(SPQ_SS, na.rm = T), digits = 2))
                       Q1  = round(quantile( SPQ_SS, probs = 0.25, names = F, na.rm = T), digits = 2),
                       Q3  = round(quantile( SPQ_SS, probs = 0.75, names = F, na.rm = T),digits = 2))

#require(knitr)
kable(x = spq.ss.stats, format = "rst")

# check for the equality in variation of means
leveneTest(user.data$SPQ_SS ~ user.data$class)
# # p=0.96 so, homogeneity of variance can be assumed 
aov.ss <- aov(SPQ_SS ~ class, data = user.data)
summary(aov.ss)
# ## F(4,139)=2.534, p=0.043
# ## THERE IS A SIG. DIFFERENCE BETWEEN CLASSES 

# ## use t-test to do pairwise comparison
# compare.SPQ_SS <- function(df, c1, c2) {
#   tt.res <- t.test(df$SPQ_SS[df$class==c1], df$SPQ_SS[df$class==c2], var.equal = T)
#   c(c1=c1, c2=c2, round(tt.res$statistic,4), p=round(tt.res$p.value, digits = 6))
# }
# comparisons <- matrix(nrow=10, ncol = 4, byrow = T)
# k <- 1
# for(i in 1:4) {
#   for(j in (i+1):5) {
#     comparisons[k,] <- compare.SPQ_SS(user.data, i, j)
#     k <- k+1
#   }
# }
# comparisons.df <- as.data.frame(comparisons)
# colnames(comparisons.df) <- c('C1', 'C2', 't', 'p')
# comparisons.df[order(comparisons.df$p),]
# ## apply FDR correction
# source(file = "util_functions.R")
# comparisons.df <- apply.FDR.correction(comparisons.df)
# comparisons.df

## do the comparison based on the approaches to learning
## observed in the trace data
leveneTest(user.data$SPQ_SS ~ user.data$la.group)
# homogeneity of variance can be assumed 
summary(aov(SPQ_SS ~ la.group, data = user.data))
# F(2,141)=3.568, p=0.0308
## use t-test to do pairwise comparison
compare.SPQ_SS <- function(df, c1, c2) {
  tt.res <- t.test(df$SPQ_SS[df$la.group==c1], df$SPQ_SS[df$la.group==c2], var.equal = T)
  cD <- cohens_d(df$SPQ_SS[df$la.group == c1], df$SPQ_SS[df$la.group == c2]) 
  c(c1=c1, c2=c2, round(tt.res$statistic,4), p=round(tt.res$p.value, digits=4), cohen_d=round(cD, digits = 4))
}
comparisons <- matrix(nrow=3, ncol = 5, byrow = T)
comparisons[1,] <- compare.SPQ_SS(user.data, 1, 2)
comparisons[2,] <- compare.SPQ_SS(user.data, 1, 3)
comparisons[3,] <- compare.SPQ_SS(user.data, 2, 3)
comparisons.df <- as.data.frame(comparisons)
colnames(comparisons.df) <- c('C1', 'C2', 't', 'p', 'cohen_d')
comparisons.df <- apply.FDR.correction(comparisons.df)
kable(comparisons.df, format = 'rst')


###############################################################################
## COMPARISON OF STUDENT GROUPS DERIVED FROM OBSERVED APPROACHES TO LEARNING  
## COMPARISON IS BASED ON THE STUDENTS' MIDTERM AND FINAL EXAM SCORES
## COMPARISON IS LIMITED TO ONLY THOSE STUDENTS WHO FILLED IN SPQ QUESTIOONAIRE
###############################################################################

## user.data frame is the data frame built in the previous section of analysis
str(user.data)

exam.scores <- all.scores[, c('user_id', 'SC_MT_TOT', 'SC_FE_TOT')]

stud.exam.scores <- merge(x = user.data[,c(1,9)], y = exam.scores,
                          by.x = 'stud.id', by.y = 'user_id',
                          all.x = T, all.y = F)
str(stud.exam.scores)
# stud.exam.scores$la.group <- factor(stud.exam.scores$la.group, levels = c(1:3),
#                                          labels = c('deep', 'strategic', 'surface'))
colnames(stud.exam.scores)[2] <- 'class'

## compute summary statistics for the final exam score for each class
f.exam.summary <- final.score.per.class.stats(stud.exam.scores)                  
## require(knitr)
kable(x = f.exam.summary, format = "rst")
## check if final exam scores are normally distributed
library(car)
qqnorm(stud.exam.scores$SC_FE_TOT)
qqline(stud.exam.scores$SC_FE_TOT)
shapiro.test(stud.exam.scores$SC_FE_TOT)
# W = 0.9534, p-value = 9.006e-05
compare.fexam.scores.Mann.Whitney.test(stud.exam.scores, 1, 2)


## compute summary statistics for the midterm exam score for each class
mt.exam.summary <- midterm.score.per.class.stats(stud.exam.scores)                  
kable(x = mt.exam.summary, format = "rst")
shapiro.test(stud.exam.scores$SC_MT_TOT)
# W = 0.94647, p-value = 2.467e-05
compare.midterm.scores.Mann.Whitney.test(stud.exam.scores, 1, 2)



############################################################################################
## COMPARE STUDENT GROUPS OBTAINED BY CLUSTERING THEIR LEARNING SEQUENCES (FOR WEEKS 2-13)
## COMPARISON IS BASED ON THE STUDENTS' MIDTERM AND FINAL EXAM SCORES 
############################################################################################

## groups that students were assigned to when clustering their learning sequences
## reminder: this is the output of sequence analysis done with TraMineR
stud.groups <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv")
str(stud.groups)
stud.groups <- stud.groups[,c(1,6)] 

## get the students' scores on the final exam
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the relevant scores
scores <- all.scores[, c('user_id', 'SC_MT_TOT', 'SC_FE_TOT')]

## check if there is difference between the users for whom scores are available and
## those for whom class info is available
setdiff(stud.groups$stud.id, scores$user_id)

groups.plus.scores <- merge(x = stud.groups, y = scores, by.x = 'stud.id', by.y = 'user_id', all = T)
str(groups.plus.scores)

## first, compute descriptive statistics
# install.packages('plyr')
library(plyr)

## compute summary statistics for the final exam score for each class
colnames(groups.plus.scores)[2] <- 'class'
summary.data <- final.score.per.class.stats(groups.plus.scores)                  
# this is for generating nice table layout
require(knitr)
kable(x = summary.data, format = "rst")

## final exam scores are not normally distributed (checked previously)
## clusters 1 and 4 are too tiny (4 and 13 students); comparison would not make sense
## so, focus only on the comparison of clusters 2 and 3
compare.fexam.scores.Mann.Whitney.test(groups.plus.scores, 2, 3)
## Z=-2.888800    p=0.003703    effect.size=0.169600

## compute summary statistics for the midterm exam score for each class
mt.summary.data <- midterm.score.per.class.stats(groups.plus.scores)                  
kable(x = mt.summary.data, format = "rst")

## midterm exam scores are not normally distributed (checked previously)
## clusters 1 and 4 are too tiny (4 and 13 students); comparison would not make sense
## again, focus only on the comparison of clusters 2 and 3
score.c2 <- groups.plus.scores$SC_MT_TOT[groups.plus.scores$class == 2]
score.c3 <- groups.plus.scores$SC_MT_TOT[groups.plus.scores$class == 3]
g <- factor(c(rep(2, length(score.c2)), rep(3, length(score.c3))))
v <- c(score.c2, score.c3)
w <- wilcox_test(v ~ g, distribution="exact")
pvalue(w)
## 0.011559
z.value <- round(statistic(w)[[1]], digits = 4)
## effect size:
round(abs(z.value)/sqrt(nrow(groups.plus.scores)), digits = 4)
## 0.1479



