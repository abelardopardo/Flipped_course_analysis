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
fe.comparison <- matrix(nrow = 15, ncol = 5, byrow = T)
k <- 1
for(i in 1:5) {
  for(j in (i+1):6) {
    fe.comparison[k,] <- c(i, j, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, i, j))
    k <- k+1
  }
}

## do the comparison only for the following classes: 1,2,4,6
# fe.comparison <- matrix(nrow = 6, ncol = 5, byrow = T)
# fe.comparison[1,] <- c(1, 2, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 1, 2))
# fe.comparison[2,] <- c(1, 4, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 1, 4))
# fe.comparison[3,] <- c(1, 6, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 1, 6))
# fe.comparison[4,] <- c(2, 4, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 2, 4))
# fe.comparison[5,] <- c(2, 6, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 2, 6))
# fe.comparison[6,] <- c(4, 6, compare.fexam.scores.Mann.Whitney.test(classes.plus.scores, 4, 6))
# fe.comparison.df <- as.data.frame(fe.comparison)
colnames(fe.comparison.df) <- c('c1', 'c2', 'Z', 'p', 'effect.size')
## apply the FDR correction to the comparisons
fe.comparison.df <- apply.FDR.correction(fe.comparison.df)
kable(x = fe.comparison.df, format = 'rst')

## now for midterm exam score
mt.comparison <- matrix(nrow = 15, ncol = 5, byrow = T)
k <- 1
for(i in 1:5) {
  for(j in (i+1):6) {
    mt.comparison[k,] <- c(i, j, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, i, j))
    k <- k+1
  }
}

## do the comparison only for the following classes: 1,2,4,6
# mt.comparison <- matrix(nrow = 6, ncol = 5, byrow = T)
# mt.comparison[1,] <- c(1, 2, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 1, 2))
# mt.comparison[2,] <- c(1, 4, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 1, 4))
# mt.comparison[3,] <- c(1, 6, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 1, 6))
# mt.comparison[4,] <- c(2, 4, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 2, 4))
# mt.comparison[5,] <- c(2, 6, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 2, 6))
# mt.comparison[6,] <- c(4, 6, compare.midterm.scores.Mann.Whitney.test(classes.plus.scores, 4, 6))
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
clusters.w213 <- read.csv(file = "results/coarse_clusters_w2-13_plus_sequence_clusters(feb2016).csv")
str(clusters.w213)
seq.classes <- clusters.w213[,c(1,14)] 

## get the students' scores on the above given MSLQ variables
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the MSLQ-related scores 
mslq.scores <- all.scores[, c('user_id', 'MSLQ_IVAL', 'MSLQ_SEFF', 'MSLQ_TANX', 'MSLQ_CSUS', 'MSLQ_SREL')]
str(mslq.scores)

## merge the users' classes and their mslq scores
user.data <- merge(x = seq.classes, y = mslq.scores, by = 'user_id', all = T)
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
                    median = median(MSLQ_IVAL, na.rm = T),
                    Q1  = quantile( MSLQ_IVAL, probs = 0.25, names = F, na.rm = T),
                    Q3  = quantile( MSLQ_IVAL, probs = 0.75, names = F, na.rm = T))

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
# exclude the 5th cluster as it has only 3 observations with MSLQ_IVAL != NA
aov.ival <- aov(MSLQ_IVAL ~ class, data = user.data)
summary(aov.ival)
## F(3,140)=2.935, p=0.0348 
## THERE IS A SIG. DIFFERENCE BETWEEN CLASSES, BUT IT WILL  
## FADE AWAY IN WHEN APPLYING CORRECTION FOR PAIR-WISE TESTS


#############
# MSLQ_SEFF #
#############
mslq.seff.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(MSLQ_SEFF),
                         N_NotNA = count(is.na(MSLQ_SEFF))[1,2],
                         median = median(MSLQ_SEFF, na.rm = T),
                         Q1  = quantile( MSLQ_SEFF, probs = 0.25, names = F, na.rm = T),
                         Q3  = quantile( MSLQ_SEFF, probs = 0.75, names = F, na.rm = T))

# this is for generating nice table layout
#require(knitr)
kable(x = mslq.seff.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
library(car)
qqnorm(user.data$MSLQ_SEFF)
qqline(user.data$MSLQ_SEFF)
shapiro.test(user.data$MSLQ_SEFF)
# W = 0.99335, p-value = 0.7454
# normal distribution can be assumed
# check for the equality in variation of means
leveneTest(user.data$MSLQ_SEFF ~ user.data$class)
# p=0.6428 so, homogeneity of variance can be assumed 
# and anova can be applied
aov.seff <- aov(MSLQ_SEFF ~ class, data = user.data)
summary(aov.seff)
## F(4,140)=1.578, p=0.197
## NO SIG. DIFFERENCE BETWEEN CLASSES

#############
# MSLQ_TANX #
#############
mslq.tanx.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(MSLQ_TANX),
                         N_NotNA = count(is.na(MSLQ_TANX))[1,2],
                         median = median(MSLQ_TANX, na.rm = T),
                         Q1  = quantile( MSLQ_TANX, probs = 0.25, names = F, na.rm = T),
                         Q3  = quantile( MSLQ_TANX, probs = 0.75, names = F, na.rm = T))

# this is for generating nice table layout
#require(knitr)
kable(x = mslq.tanx.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
library(car)
qqnorm(user.data$MSLQ_TANX)
qqline(user.data$MSLQ_TANX)
shapiro.test(user.data$MSLQ_TANX)
# W = 0.98138, p-value = 0.04751
# normal distribution cannot be assumed, though the p-value is close to be acceptable
# check for the equality in variation of means
leveneTest(user.data$MSLQ_TANX ~ user.data$class)
# p=0.047 so, homogeneity of variance cannot be assumed 
# so, no-parametric tests have to be used

kruskal.test(MSLQ_TANX ~ class, data = user.data)
# chi-squared = 2.7499, df = 3, p-value = 0.4318
# NO SIG. DIFFERENCE BETWEEN CLASSES

#############
# MSLQ_CSUS #
#############
mslq.csus.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(MSLQ_CSUS),
                         N_NotNA = count(is.na(MSLQ_CSUS))[1,2],
                         median = median(MSLQ_CSUS, na.rm = T),
                         Q1  = quantile( MSLQ_CSUS, probs = 0.25, names = F, na.rm = T),
                         Q3  = quantile( MSLQ_CSUS, probs = 0.75, names = F, na.rm = T))

# this is for generating nice table layout
#require(knitr)
kable(x = mslq.csus.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
library(car)
qqnorm(user.data$MSLQ_CSUS)
qqline(user.data$MSLQ_CSUS)
shapiro.test(user.data$MSLQ_CSUS)
# W = 0.99012, p-value = 0.4069
# normal distribution can be assumed
# check for the equality in variation of means
leveneTest(user.data$MSLQ_CSUS ~ user.data$class)
# p=0.7999 so, homogeneity of variance can be assumed 
aov.csus <- aov(MSLQ_CSUS ~ class, data = user.data)
summary(aov.csus)
## F(4,140)=1.825, p=0.145 
## NO SIG. DIFFERENCE BETWEEN CLASSES

#############
# MSLQ_SREL #
#############
mslq.srel.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(MSLQ_SREL),
                         N_NotNA = count(is.na(MSLQ_SREL))[1,2],
                         median = median(MSLQ_SREL, na.rm = T),
                         Q1  = quantile( MSLQ_SREL, probs = 0.25, names = F, na.rm = T),
                         Q3  = quantile( MSLQ_SREL, probs = 0.75, names = F, na.rm = T))

# this is for generating nice table layout
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
# p=0.7081 so, homogeneity of variance can be assumed 
aov.srel <- aov(MSLQ_SREL ~ class, data = user.data)
summary(aov.srel)
## F(4,140)=2.94, p=0.0354 
## THERE IS A SIG. DIFFERENCE BETWEEN CLASSES, BUT IT WILL PROBABLY 
## FADE AWAY IN WHEN APPLYING CORRECTION FOR PAIR-WISE TESTS


############################################################################################
## COMPARE LCA CLASSES OBTAINED FOR THE LEARNING PATHS THROUGH THE WHOLE COURSE (WEEKS 2-13) 
## BASED ON THE STUDENTS' PROFILE DERIVED THROUGH THE STUDY PROCESS QUESTIONNAIRE
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

## extract the SPQ-related scores 
spq.scores <- all.scores[, c('user_id', 'SPQ_DA', 'SPQ_SA', 'SPQ_DM', 'SPQ_SM', 'SPQ_DS', 'SPQ_SS')]
str(spq.scores)

## merge the users' classes and their mslq scores
user.data <- merge(x = seq.classes, y = spq.scores, by = 'user_id', all = T)
str(user.data)
colnames(user.data)[2] <- 'class'
user.data$class <- factor(user.data$class)

##########
# SPQ_DA #
##########
spq.da.stats <- ddply(user.data, 
                         c('class'), 
                         summarise,
                         N    = length(SPQ_DA),
                         N_NotNA = count(is.na(SPQ_DA))[1,2],
                         median = median(SPQ_DA, na.rm = T),
                         Q1  = quantile( SPQ_DA, probs = 0.25, names = F, na.rm = T),
                         Q3  = quantile( SPQ_DA, probs = 0.75, names = F, na.rm = T))

# this is for generating nice table layout
require(knitr)
kable(x = spq.da.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
library(car)
qqnorm(user.data$SPQ_DA)
qqline(user.data$SPQ_DA)
shapiro.test(user.data$SPQ_DA)
# W = 0.98796, p-value = 0.2472
# normal distribution can be assumed
# check for the equality in variation of means
leveneTest(user.data$SPQ_DA ~ user.data$class)
# p=0.758 so, homogeneity of variance can be assumed 
aov.da <- aov(SPQ_DA ~ class, data = user.data)
summary(aov.da)
## F(3,140)=1.857, p=0.14
## NO SIG. DIFFERENCE BETWEEN CLASSES

##########
# SPQ_DM #
##########
spq.dm.stats <- ddply(user.data, 
                      c('class'), 
                      summarise,
                      N    = length(SPQ_DM),
                      N_NotNA = count(is.na(SPQ_DM))[1,2],
                      median = median(SPQ_DM, na.rm = T),
                      Q1  = quantile( SPQ_DM, probs = 0.25, names = F, na.rm = T),
                      Q3  = quantile( SPQ_DM, probs = 0.75, names = F, na.rm = T))

# this is for generating nice table layout
#require(knitr)
kable(x = spq.dm.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
#library(car)
qqnorm(user.data$SPQ_DM)
qqline(user.data$SPQ_DM)
shapiro.test(user.data$SPQ_DM)
# W = 0.98755, p-value = 0.2237
# normal distribution can be assumed
# check for the equality in variation of means
leveneTest(user.data$SPQ_DM ~ user.data$class)
# p=0.5261 so, homogeneity of variance can be assumed 
aov.dm <- aov(SPQ_DM ~ class, data = user.data)
summary(aov.dm)
## F(3,140)=0.786, p=0.504
## NO SIG. DIFFERENCE BETWEEN CLASSES

##########
# SPQ_DS #
##########
spq.ds.stats <- ddply(user.data, 
                      c('class'), 
                      summarise,
                      N    = length(SPQ_DS),
                      N_NotNA = count(is.na(SPQ_DS))[1,2],
                      median = median(SPQ_DS, na.rm = T),
                      Q1  = quantile( SPQ_DS, probs = 0.25, names = F, na.rm = T),
                      Q3  = quantile( SPQ_DS, probs = 0.75, names = F, na.rm = T))

# this is for generating nice table layout
#require(knitr)
kable(x = spq.ds.stats, format = "rst")

## check if the MSLQ_IVAL is normally distributed
#library(car)
qqnorm(user.data$SPQ_DS)
qqline(user.data$SPQ_DS)
shapiro.test(user.data$SPQ_DS)
# W = 0.98296, p-value = 0.07079
# normal distribution can be assumed (though the QQ-plot suggests otherwise)
# check for the equality in variation of means
leveneTest(user.data$SPQ_DS ~ user.data$class)
# p=0.5243 so, homogeneity of variance can be assumed 
aov.ds <- aov(SPQ_DS ~ class, data = user.data)
summary(aov.ds)
## F(3,140)=2.818, p=0.0414
## THERE IS A SIG. DIFFERENCE BETWEEN CLASSES, BUT IT WILL PROBABLY 
## FADE AWAY IN WHEN APPLYING CORRECTION FOR PAIR-WISE TESTS

## use t-test to do pairwise comparison
compare.SPQ_DS <- function(df, c1, c2) {
  tt.res <- t.test(df$SPQ_DS[df$class==c1], df$SPQ_DS[df$class==c2], var.equal = T)
  c(c1=c1, c2=c2, round(tt.res$statistic,4), p=round(tt.res$p.value, digits = 6))
}
comparisons <- matrix(nrow=6, ncol = 4, byrow = T)
comparisons[1,] <- compare.SPQ_DS(user.data, 1, 2)
comparisons[2,] <- compare.SPQ_DS(user.data, 1, 3)
comparisons[3,] <- compare.SPQ_DS(user.data, 1, 4)
comparisons[4,] <- compare.SPQ_DS(user.data, 2, 3)
comparisons[5,] <- compare.SPQ_DS(user.data, 2, 4)
comparisons[6,] <- compare.SPQ_DS(user.data, 3, 4)

comparisons.df <- as.data.frame(comparisons)
colnames(comparisons.df) <- c('C1', 'C2', 't', 'p')
comparisons.df <- apply.FDR.correction(comparisons.df)
comparisons.df
## NONE OF THE PAIRWISE COMPARISONS IS SIGNIFICANT


## examine basic demographic data
demography <- all.scores[,c('GENDER..Female..0..Male..1.', 'EMPLOYED', 'NOH')]
str(demography)
length(which(is.na(demography$GENDER..Female..0..Male..1.)))
summary(demography$EMPLOYED)
## again, data for 146 students are missing


############################################################################################
## COMPARE STUDENT GROUPS OBTAINED BY CLUSTERING THEIR LEARNING SEQUENCES (FOR WEEKS 2-13)
## COMPARISON IS BASED ON THE STUDENTS' MIDTERM AND FINAL EXAM SCORES 
############################################################################################

## groups that students were assigned to when clustering their learning sequences
## reminder: this is the output of sequence analysis done with TraMineR
stud.groups <- read.csv(file = "results/sequence_based_student_clusters.csv")
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