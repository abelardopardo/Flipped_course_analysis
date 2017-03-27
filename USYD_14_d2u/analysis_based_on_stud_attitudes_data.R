##########################################################################
## THE IDEA IS TO DO MULTINOMIAL LOGISTIC REGRESSION ANALYSIS
## - OUTCOME VARIABLE: STUDENT CLUSTERS (BASED ON THE LEARNING STRATEGIES)
## - PREDICTOR VARIABLES: 5 MSLQ VARIABLES + 2 SPQ VARIABLES (DA and SA)
##########################################################################

## Load the students' data 
stud.data <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the variables required for the model  
mslq.spq.data <- stud.data[, c('user_id', 'MSLQ_IVAL', 'MSLQ_SEFF', 'MSLQ_TANX', 
                               'MSLQ_CSUS', 'MSLQ_SREL', 'SPQ_DA', 'SPQ_SA')]
str(mslq.spq.data)

## remove students for whom the questionnaire data (mslq and spq) are not available
length(which(complete.cases(mslq.spq.data) == F))
mslq.spq.data <- mslq.spq.data[which(complete.cases(mslq.spq.data) == T),]

summary(mslq.spq.data)

## load the data about the student clusters
stud.clust <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv")
str(stud.clust)

## merge the questionnaire data with the data about student clusters
stud.data <- merge(x = mslq.spq.data, y = stud.clust[,c(1,7)], 
                   by.x = 'user_id', by.y = 'stud.id',
                   all.x = T, all.y = F)
str(stud.data)
stud.data$cluster <- factor(stud.data$cluster)

## examine the predictor variables across the levels of the outcome variable
with(stud.data, do.call(rbind, tapply(MSLQ_IVAL, cluster, function(x) c(M = mean(x), SD = sd(x)))))
with(stud.data, do.call(rbind, tapply(MSLQ_SEFF, cluster, function(x) c(M = mean(x), SD = sd(x)))))
with(stud.data, do.call(rbind, tapply(MSLQ_TANX, cluster, function(x) c(M = mean(x), SD = sd(x)))))
with(stud.data, do.call(rbind, tapply(MSLQ_CSUS, cluster, function(x) c(M = mean(x), SD = sd(x)))))
with(stud.data, do.call(rbind, tapply(MSLQ_SREL, cluster, function(x) c(M = mean(x), SD = sd(x)))))
with(stud.data, do.call(rbind, tapply(SPQ_DA, cluster, function(x) c(M = mean(x), SD = sd(x)))))
with(stud.data, do.call(rbind, tapply(SPQ_SA, cluster, function(x) c(M = mean(x), SD = sd(x)))))
## there seems to be no difference among the clusters w.r.t. any of the predictor variables
## let's check that
apply(stud.data[,c(2:8)], 2, shapiro.test)
## all predictor variables are normally distributed, so anova can be used
summary(aov(MSLQ_IVAL ~ cluster, data = stud.data))
summary(aov(MSLQ_SEFF ~ cluster, data = stud.data))
summary(aov(MSLQ_TANX ~ cluster, data = stud.data))
summary(aov(MSLQ_CSUS ~ cluster, data = stud.data))
summary(aov(MSLQ_SREL ~ cluster, data = stud.data))
summary(aov(SPQ_DA ~ cluster, data = stud.data))
summary(aov(SPQ_SA ~ cluster, data = stud.data))
## there is really no sig. difference among the clusters


#######################################################
## the assumptions for multinomial log. regression
#######################################################

#1) there should be no multicollinearity
cor(stud.data[,c(2:8)])
## no particularly high correlation detected (the highest is 0.63)

#2) there needs to be a linear relationship between any continuous 
## independent variables and the logit transformation of the dependent variable
## how to test this ???

#3) there should be no outliers, high leverage values or highly influential points
summary(stud.data[,c(2:8)])
## looks fine

#4) The Independence of Irrelevant Alternatives (IIA) assumption: adding or deleting 
# alternative outcome categories does not affect the odds among the remaining outcomes
## how to test this ???

#5) the number of observations in the smallest frequency category of Y is large, 
# for example 10 times the number of parameters from the right hand side of the model
table(stud.data$cluster)
## not the case here - the smallest category (15) is less than 3 times the number of predictors  


require(nnet)

#############################################################
## setting cluster = 1 as the referrent (baseline) category #
#############################################################
m1 <- multinom(cluster ~ MSLQ_IVAL + MSLQ_SEFF + MSLQ_TANX + 
                         MSLQ_CSUS + MSLQ_SREL + SPQ_SA + SPQ_DA, data = stud.data)
summary(m1)

## compute Z and p values
z <- summary(m1)$coefficients/summary(m1)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
apply(p, 2, sum)


## remove the MSLQ_CSUS variable as the one of the least relevance
m1.6var <- multinom(cluster ~ MSLQ_IVAL + MSLQ_SEFF + MSLQ_TANX + 
                  MSLQ_SREL + SPQ_SA + SPQ_DA, data = stud.data)
summary(m1.6var)

z.6vars <- summary(m1.6var)$coefficients/summary(m1.6var)$standard.errors
p.6vars <- (1 - pnorm(abs(z.6vars), 0, 1))*2
p.6vars
apply(p.6vars, 2, sum)

## remove the SPQ_SA variable as the one of the least relevance
m1.5var <- multinom(cluster ~ MSLQ_IVAL + MSLQ_SEFF + MSLQ_TANX + 
                      MSLQ_SREL + SPQ_DA, data = stud.data)
summary(m1.5var)

z.5vars <- summary(m1.5var)$coefficients/summary(m1.5var)$standard.errors
p.5vars <- (1 - pnorm(abs(z.5vars), 0, 1))*2
p.5vars
apply(p.5vars, 2, sum)

## remove the MSLQ_SEFF variable as the one of the least relevance
m1.4var <- multinom(cluster ~ MSLQ_IVAL + MSLQ_TANX + MSLQ_SREL + SPQ_DA, data = stud.data)
summary(m1.4var)

z.4vars <- summary(m1.4var)$coefficients/summary(m1.4var)$standard.errors
p.4vars <- (1 - pnorm(abs(z.4vars), 0, 1))*2
p.4vars
apply(p.4vars, 2, sum)

## remove the MSLQ_SREL variable as the one of the least relevance
m1.3var <- multinom(cluster ~ MSLQ_IVAL + MSLQ_TANX + SPQ_DA, data = stud.data)
summary(m1.3var)

z.3vars <- summary(m1.3var)$coefficients/summary(m1.3var)$standard.errors
p.3vars <- (1 - pnorm(abs(z.3vars), 0, 1))*2
p.3vars
apply(p.3vars, 2, sum)

## remove the MSLQ_TANX variable as the one of the least relevance
m1.2var <- multinom(cluster ~ MSLQ_IVAL + SPQ_DA, data = stud.data)
summary(m1.2var)

z.2vars <- summary(m1.2var)$coefficients/summary(m1.2var)$standard.errors
p.2vars <- (1 - pnorm(abs(z.2vars), 0, 1))*2
p.2vars

#############################################################
## setting cluster = 2 as the referrent (baseline) category #
#############################################################
stud.data$clust2 <- relevel(stud.data$cluster, ref = "2")
m2 <- multinom(clust2 ~ MSLQ_IVAL + MSLQ_SEFF + MSLQ_TANX + 
                 MSLQ_CSUS + MSLQ_SREL + SPQ_SA + SPQ_DA, data = stud.data)
summary(m2)

## compute Z and p values
z <- summary(m2)$coefficients/summary(m2)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
apply(p, 2, sum)

## remove the MSLQ_SEFF variable as the one of the least relevance
m2.6vars <- multinom(clust2 ~ MSLQ_IVAL + MSLQ_TANX + MSLQ_CSUS + MSLQ_SREL +
                              SPQ_SA + SPQ_DA, data = stud.data)
summary(m2.6vars)

z.6vars <- summary(m2.6vars)$coefficients/summary(m2.6vars)$standard.errors
p.6vars <- (1 - pnorm(abs(z.6vars), 0, 1))*2
p.6vars
apply(p.6vars, 2, sum)

## remove the SPQ_SA variable as the one of the least relevance
m2.5vars <- multinom(clust2 ~ MSLQ_IVAL + MSLQ_TANX + MSLQ_CSUS + MSLQ_SREL +
                              SPQ_DA, data = stud.data)
summary(m2.5vars)

z.5vars <- summary(m2.5vars)$coefficients/summary(m2.5vars)$standard.errors
p.5vars <- (1 - pnorm(abs(z.5vars), 0, 1))*2
p.5vars
apply(p.5vars, 2, sum)

## remove the MSLQ_CSUS variable as the one of the least relevance
m2.4vars <- multinom(clust2 ~ MSLQ_IVAL + MSLQ_TANX + MSLQ_SREL +
                       SPQ_DA, data = stud.data)
summary(m2.4vars)

z.4vars <- summary(m2.4vars)$coefficients/summary(m2.4vars)$standard.errors
p.4vars <- (1 - pnorm(abs(z.4vars), 0, 1))*2
p.4vars
apply(p.4vars, 2, sum)

## remove the MSLQ_SREL variable as the one of the least relevance
m2.3vars <- multinom(clust2 ~ MSLQ_IVAL + MSLQ_TANX + SPQ_DA, data = stud.data)
summary(m2.3vars)

z.3vars <- summary(m2.3vars)$coefficients/summary(m2.3vars)$standard.errors
p.3vars <- (1 - pnorm(abs(z.3vars), 0, 1))*2
p.3vars
apply(p.3vars, 2, sum)

## remove the MSLQ_IVAL variable as the one of the least relevance
m2.2vars <- multinom(clust2 ~ MSLQ_TANX + SPQ_DA, data = stud.data)
summary(m2.2vars)

z.2vars <- summary(m2.2vars)$coefficients/summary(m2.2vars)$standard.errors
p.2vars <- (1 - pnorm(abs(z.2vars), 0, 1))*2
p.2vars



#############################################################
## setting cluster = 3 as the referrent (baseline) category #
#############################################################
stud.data$clust3 <- relevel(stud.data$cluster, ref = "3")
m3 <- multinom(clust3 ~ MSLQ_IVAL + MSLQ_SEFF + MSLQ_TANX + 
                 MSLQ_CSUS + MSLQ_SREL + SPQ_SA + SPQ_DA, data = stud.data)
summary(m3)

## compute Z and p values
z <- summary(m3)$coefficients/summary(m3)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
apply(p[,c(2:8)], 2, sum)

## remove the MSLQ_SEFF variable as the one of the least relevance
m3.6vars <- multinom(clust3 ~ MSLQ_IVAL + MSLQ_TANX + MSLQ_CSUS + MSLQ_SREL +
                       SPQ_SA + SPQ_DA, data = stud.data)
summary(m3.6vars)

z.6vars <- summary(m3.6vars)$coefficients/summary(m3.6vars)$standard.errors
p.6vars <- (1 - pnorm(abs(z.6vars), 0, 1))*2
p.6vars
apply(p.6vars[,c(-1)], 2, sum)

## remove the MSLQ_SREL variable as the one of the least relevance
m3.5vars <- multinom(clust3 ~ MSLQ_IVAL + MSLQ_TANX + MSLQ_CSUS +
                       SPQ_SA + SPQ_DA, data = stud.data)
summary(m3.5vars)

z.5vars <- summary(m3.5vars)$coefficients/summary(m3.5vars)$standard.errors
p.5vars <- (1 - pnorm(abs(z.5vars), 0, 1))*2
p.5vars
apply(p.5vars[,c(-1)], 2, sum)

## remove the MSLQ_TANX variable as the one of the least relevance
m3.4vars <- multinom(clust3 ~ MSLQ_IVAL + MSLQ_CSUS + SPQ_SA + SPQ_DA, data = stud.data)
summary(m3.4vars)

z.4vars <- summary(m3.4vars)$coefficients/summary(m3.4vars)$standard.errors
p.4vars <- (1 - pnorm(abs(z.4vars), 0, 1))*2
p.4vars
apply(p.4vars[,c(-1)], 2, sum)

## remove the SPQ_DA variable as the one of the least relevance
m3.3vars <- multinom(clust3 ~ MSLQ_IVAL + MSLQ_CSUS + SPQ_SA, data = stud.data)
summary(m3.3vars)

z.3vars <- summary(m3.3vars)$coefficients/summary(m3.3vars)$standard.errors
p.3vars <- (1 - pnorm(abs(z.3vars), 0, 1))*2
p.3vars
apply(p.3vars[,c(-1)], 2, sum)

## remove the MSLQ_IVAL variable as the one of the least relevance
m3.2vars <- multinom(clust3 ~ MSLQ_CSUS + SPQ_SA, data = stud.data)
summary(m3.2vars)

z.2vars <- summary(m3.2vars)$coefficients/summary(m3.2vars)$standard.errors
p.2vars <- (1 - pnorm(abs(z.2vars), 0, 1))*2
p.2vars
apply(p.2vars[,c(-1)], 2, sum)

##################################################
## try building a model with the mlogit package ##
##################################################

install.packages('mlogit')
require(mlogit)

## prepare the data
stud.data.ml <- mlogit.data(stud.data[,c(1:9)], choice = 'cluster', shape = 'wide')
head(stud.data.ml)

## create the model, with cluster = 1 as the referent category
ml1 <- mlogit(cluster ~ 1 | MSLQ_IVAL + MSLQ_SEFF + MSLQ_TANX + 
                MSLQ_CSUS + MSLQ_SREL + SPQ_SA + SPQ_DA,
              data = stud.data.ml, reflevel = "1")
summary(ml1)

## remove the MSLQ_CSUS variable as the one of the least relevance
ml1.6vars <- mlogit(cluster ~ 1 | MSLQ_IVAL + MSLQ_SEFF + MSLQ_TANX + 
                                  MSLQ_SREL + SPQ_SA + SPQ_DA,
                    data = stud.data.ml, reflevel = "1")
summary(ml1.6vars)

## remove the SPQ_SA variable as the one of the least relevance
ml1.5vars <- mlogit(cluster ~ 1 | MSLQ_IVAL + MSLQ_SEFF + MSLQ_TANX + 
                                  MSLQ_SREL + SPQ_DA,
                    data = stud.data.ml, reflevel = "1")
summary(ml1.5vars)

## remove the MSLQ_SEFF variable as the one of the least relevance
ml1.4vars <- mlogit(cluster ~ 1 | MSLQ_IVAL + MSLQ_TANX + MSLQ_SREL + SPQ_DA,
                    data = stud.data.ml, reflevel = "1")
summary(ml1.4vars)

## remove the MSLQ_SREL variable as the one of the least relevance
ml1.3vars <- mlogit(cluster ~ 1 | MSLQ_IVAL + MSLQ_TANX + SPQ_DA,
                    data = stud.data.ml, reflevel = "1")
summary(ml1.3vars)


##########################################################################
## COMPUTE CORRELATIONS BETWEEN STUDENTS' LEARNING STRATEGIES AND THEIR ##
## LEARNING ATTITUDES (MSLQ AND SPQ DATA)                               ##
##########################################################################

## Load the students' data 
stud.data <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the variables required for the model  
mslq.spq.data <- stud.data[, c('user_id', 'MSLQ_IVAL', 'MSLQ_SEFF', 'MSLQ_TANX', 
                               'MSLQ_CSUS', 'MSLQ_SREL', 'SPQ_DA', 'SPQ_SA')]
str(mslq.spq.data)

## remove students for whom the questionnaire data (mslq and spq) are not available
length(which(complete.cases(mslq.spq.data) == F))
mslq.spq.data <- mslq.spq.data[which(complete.cases(mslq.spq.data) == T),]

## load the strategies data
strategies <- read.csv(file = "results/all_stud_sequences_4_seq_clusters.csv")
str(strategies)

## auxiliary f. that computes for the given student the number of sessions per learning strategy
## returns a vector with 4 elements, each one representing the number of session for the 
## corresponding l. strategies
count.sessions.per.strategy <- function(strategy.data, stud.id) {
  stud.strat.data <- strategy.data$cluster[strategy.data$stud.id == stud.id]
  s1 = s2 = s3 = s4 = 0
  for(i in 1:length(stud.strat.data)) {
    if(stud.strat.data[i] == 1) s1 <- s1 + 1
    else if (stud.strat.data[i] == 2) s2 <- s2 + 1
    else if (stud.strat.data[i] == 3) s3 <- s3 + 1
    else s4 <- s4 + 1
  }
  c(s1,s2,s3,s4)
}

## f. creates a summary table where for each student, there are 4 variables 
## representing the number of learning sessions in each of the 4 strategies

strat.summary <- matrix(nrow = nrow(mslq.spq.data), ncol = 5, byrow = T)
for(i in 1:nrow(mslq.spq.data)) {
  current.stud.id <- mslq.spq.data$user_id[i]
  strat.summary[i,] <- c(current.stud.id, count.sessions.per.strategy(strategies, current.stud.id))
}
strat.summary <- as.data.frame(strat.summary)
colnames(strat.summary) <- c('stud.id', 'strat1', 'strat2', 'strat3', 'strat4')
head(strat.summary)

## merge strategy data and attitude data
stud.data <- merge(x = strat.summary, y = mslq.spq.data, 
                   by.x = 'stud.id', by.y = 'user_id',
                   all.x = F, all.y = T)
str(stud.data)

## correlations between strategy 1 and the attitude variables
cor(stud.data[,c(2,6:12)]) ## very low
## correlations between strategy 2 and the attitude variables
cor(stud.data[,c(3,6:12)]) ## very low
## correlations between strategy 3 and the attitude variables
cor(stud.data[,c(4,6:12)]) ## very low
## correlations between strategy 4 and the attitude variables
cor(stud.data[,c(5,6:12)]) ## very low


#############################################################################
## COMPARE STUDENTS WHO FILLED IN THE SPQ QUESTIONNAIRE AND THOSE WHO DIDN'T
#############################################################################

## extract the SPQ variables and the exam scores
## load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## extract the SPQ-related scores 
spq.scores <- all.scores[, c('user_id', 'SPQ_DA', 'SPQ_SA', 'SPQ_DM', 'SPQ_SM', 
                             'SPQ_DS', 'SPQ_SS', 'SC_MT_TOT', 'SC_FE_TOT')]
str(spq.scores)

length(which(complete.cases(spq.scores)==F))
## 144 didn't fill in the SPQ

completed.spq <- which(complete.cases(spq.scores)==T)
spq.group <- spq.scores[completed.spq,]
non.spq.group <- spq.scores[-completed.spq,]

do.Mann.Whitney.test <- function(spq.group.score, non.spq.group.score) {
  require(coin)
  g <- factor(c(rep('SPQ.GROUP', length(spq.group.score)), rep('NON.SPQ.GROUP', length(non.spq.group.score))))
  v <- c(spq.group.score, non.spq.group.score)
  w <- wilcox_test(v ~ g, distribution="exact")
  z.value <- round(statistic(w)[[1]], digits = 4)
  n <- length(spq.group.score) + length(non.spq.group.score)
  r <- round(abs(z.value)/sqrt(n), digits = 4)
  c(Z=z.value, p=round(pvalue(w), digits = 6), effect.size=r)
}

## compare the groups on midterm exam
do.Mann.Whitney.test(spq.group$SC_MT_TOT, non.spq.group$SC_MT_TOT)
##      Z           p       effect.size 
## -1.743500    0.081302    0.102400   


## compare the groups on final exam
do.Mann.Whitney.test(spq.group$SC_FE_TOT, non.spq.group$SC_FE_TOT)
##    Z           p         effect.size 
##-2.882900    0.003844    0.169300

## load the data about the sequence-based student clusters
stud.clust <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv")
str(stud.clust)

spq.group <- merge(x = spq.group, y = stud.clust[,c(1,6)], by.x = 'user_id', by.y = 'stud.id', all.x = T, all.y = F)
str(spq.group)
colnames(spq.group)[10] <- 'seq.total'

non.spq.group <- merge(x = non.spq.group, y = stud.clust[,c(1,6)], 
                       by.x = 'user_id', by.y = 'stud.id', all.x = T, all.y = F)
colnames(non.spq.group)[10] <- 'seq.total'
str(non.spq.group)

## compare the groups on the total number of sequences
do.Mann.Whitney.test(spq.group$seq.total, non.spq.group$seq.total)
##     Z           p        effect.size 
## -3.250500    0.001102    0.190900 


compute.group.stats <- function(group.scores) {
   group.stats <- apply(X = group.scores, MARGIN = 2,
                         FUN = function(x) {
                           q25 = quantile(x, probs = 0.25, names = F)
                           q75 = quantile(x, probs = 0.75, names = F)
                           c(median(x), q25, q75)
                         })
    group.stats <- data.frame(t(group.stats))
    rownames(group.stats) <- c('MT_SCORE', 'FE_SCORE', 'SEQ.TOT')
    colnames(group.stats) <- c('Median', 'Q25', 'Q75')
    require(knitr)
    kable(group.stats, format = 'rst')
}

compute.group.stats(spq.group[,c(8:10)])
compute.group.stats(non.spq.group[,c(8:10)])


##################################################################################
## CHECK IF THERE IS A DEPENDENCY BETWEEN THE STRATEGY-BASED CLUSTER A STUDENT 
## BELONGS TO (CLUSTERS BASED ON STUDENTS' LEARNING STRATEGIES) AND WHETHER
## THE STUDENT FILLED IN THE (SPQ AND MLSQ) QUESTIONNAIRES
##################################################################################

## load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## extract the SPQ-related scores 
spq.scores <- all.scores[, c('user_id', 'SPQ_DA', 'SPQ_SA', 'SPQ_DM', 
                             'SPQ_SM','SPQ_DS', 'SPQ_SS')]
str(spq.scores)
remove(all.scores)

length(which(complete.cases(spq.scores)==F))
## 146 (out of 290) didn't fill in the SPQ
completed.spq <- which(complete.cases(spq.scores)==T)

student.data <- data.frame(stud.id=spq.scores$user_id)
## first set that no one responded to the questionnaire
student.data$responded <- rep(x = "No", times=length(student.data)) 
## now, change the status for those who have responded
student.data$responded[completed.spq] <- "Yes"
student.data$responded <- factor(student.data$responded)
str(student.data)
table(student.data$responded)

## load the data about student clusters, in particular, 4 clusters solution
stud.clusters <- read.csv(file = "results/sequence_based_student_clusters_4cl.csv")
str(stud.clusters)
table(stud.clusters$cluster)

## add cluster information to the student data
stud.data <- merge(x = student.data, y = stud.clusters[,c(1,7)],
                  by = 'stud.id', all = T)
str(stud.data)
which(complete.cases(stud.data)==F)
## no missing values
stud.data$cluster <- factor(stud.data$cluster)
remove(stud.clusters, student.data)

## use Chi-square test to examine the strength of association 
## between the responded and cluster variables
tbl <- xtabs(~responded + cluster, data = stud.data)
chi2 <- chisq.test(tbl)
c(chi2=chi2$statistic, p=chi2$p.value)
# chi2.X-squared      p 
# 13.828307969    0.003148419
## the null hypothesis that the two variables are independent should be dropped
## compute Crammer's V:
sqrt(chi2$statistic/(sum(tbl)*min(nrow(tbl)-1,ncol(tbl)-1)))
# 0.218
## small to medinum effect size - according to: 
## http://www.real-statistics.com/chi-square-and-f-distributions/effect-size-chi-square/

## plot the relationship of the two variables
require(ggplot2)
ggplot(as.data.frame(prop.table(tbl, margin = 2)), aes(x=cluster, y=Freq, fill=responded)) +
  geom_bar(stat='identity', width=.5, position = 'dodge') +
  xlab("Student clusters") +
  ylab("Percentage") +
  ylim(c(0,1)) +
  labs(fill = "Responded") +
  theme_bw()
## the dependency is obvious

## create logistic regression model
stud.logit <- glm(responded ~ cluster, data = stud.data, family = 'binomial')
summary(stud.logit)

# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   1.3218     0.5627   2.349  0.01883 * 
# cluster2     -1.1567     0.6034  -1.917  0.05523 . 
# cluster3     -1.3218     0.5898  -2.241  0.02504 * 
# cluster4     -2.0409     0.6284  -3.248  0.00116 **

## test the overall effect of cluster on the response variable
## using wald test from the aod package
require(aod)
wald.test(b = coef(stud.logit), Sigma = vcov(stud.logit), Terms = 2:4)
# Chi-squared test: X2 = 12.7, df = 3, P(> X2) = 0.0054
## the test is significant, suggesting that the overall effect of the cluster
## variable is significant

exp(coefficients(stud.logit))
# (Intercept)  cluster2    cluster3    cluster4 
# 3.7500000   0.3145299   0.2666667   0.1299145 
## interpretation: the odds of responding to the questionnaire for cluster 2
## students over the oddds of responding for cluster 1 students is 0.31; in terms of
## percentages, the odds of cluster 2 students (to respond) are 69% lower than 
## the odds of cluster 1 students; for cluster 3, odds are ~74% lower than odds of 
## cluster 1; for cluster 4, the odds are ~87% lower

## do wald test to compare other clusters
## compare 2 and 3
l <- cbind(0,1,-1,0)
wald.test(b = coef(stud.logit), Sigma = vcov(stud.logit), L = l)
# no difference
## compare 2 and 4
l <- cbind(0,1,0,-1)
wald.test(b = coef(stud.logit), Sigma = vcov(stud.logit), L = l)
# X2 = 6.2, df = 1, P(> X2) = 0.013
## compare 3 and 4
l <- cbind(0,0,1,-1)
wald.test(b = coef(stud.logit), Sigma = vcov(stud.logit), L = l)
# Chi-squared test: X2 = 4.7, df = 1, P(> X2) = 0.03
## clusters 2 and 4, and 3 and 4 stat. differ in terms of their propensity to 
## fill in the questionnaire


## create new dichotomous variables, one for each cluster, indicating if 
## a student belongs to the given cluster or not
stud.data$cl1 <- rep(0, nrow(stud.data))
stud.data$cl2 <- rep(0, nrow(stud.data))
stud.data$cl3 <- rep(0, nrow(stud.data))
stud.data$cl4 <- rep(0, nrow(stud.data))
for (i in 1:nrow(stud.data)) {
  if (stud.data$cluster[i]==1) stud.data$cl1[i] <- 1
  else if (stud.data$cluster[i]==2) stud.data$cl2[i] <- 1
  else if (stud.data$cluster[i]==3) stud.data$cl3[i] <- 1
  else stud.data$cl4[i] <- 1
}
stud.data$cl1 <- factor(stud.data$cl1)
stud.data$cl2 <- factor(stud.data$cl2)
stud.data$cl3 <- factor(stud.data$cl3)
stud.data$cl4 <- factor(stud.data$cl4)

stud.logit2 <- glm(responded ~ cl1 + cl2 + cl3 + cl4, 
                   data = stud.data, family = 'binomial')
summary(stud.logit2)

# Coefficients: (1 not defined because of singularities)
#               Estimate Std. Error z value Pr(>|z|)   
#   (Intercept)  -0.7191     0.2798  -2.570  0.01016 * 
#   cl11          2.0409     0.6284   3.248  0.00116 **
#   cl21          0.8842     0.3545   2.494  0.01262 * 
#   cl31          0.7191     0.3309   2.173  0.02978 * 
#   cl41              NA         NA      NA       NA   
