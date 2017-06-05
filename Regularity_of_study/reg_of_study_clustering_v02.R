#################################
# Feature selection and scaling
#################################
library(tidyverse)
library(caret)

source("regularity_of_study_functions.R")

regularity_data <- read_csv("Intermediate_results/regularity_of_study/regularity_indicators.csv")
str(regularity_data)


## first, choose features / indicators to use
## primarily, choose between SD and MAD (Absolute Deviation Around the Median) 
## as measures of deviation / irregularity

regularity_data <- regularity_data %>%
  select(1:4,6,7,9:12,15:20,23,24)

summary(regularity_data)

## check for zero or near-zero variance predictors
## using the nearZeroVar f. from caret:
## https://topepo.github.io/caret/pre-processing.html#zero--and-near-zero-variance-predictors
nzv <- nearZeroVar(regularity_data[,-1], saveMetrics= TRUE)
nzv
# topic_cnt_median - near zero variance, remove it
regularity_data <- regularity_data %>% select(-`topic_cnt_median`)


## check for outliers
outliers <- apply(regularity_data[,-1], 2, function(x) length(boxplot.stats(x)$out))
outliers <- outliers[outliers>0]
sort(outliers, decreasing = T)
# some have excedengly high number:
# inactive_week_cnt: 76
# topic_cnt_mad: 71
# week_cnt_entropy: 33
# ses_timegap_mad: 27
# ...

## check again the data, especially variables with large number of outliers
summary(regularity_data)
length(which(regularity_data$inactive_week_cnt>0))
# 76 - and all are outliers
length(which(regularity_data$topic_cnt_mad>0))
# 71 - and all are ouliers
sort(boxplot.stats(regularity_data$week_cnt_entropy)$out)

## nothing can be done for the inactive_week_cnt and topic_cnt_mad, remove them
regularity_data <- regularity_data %>%
  select(-c(topic_cnt_mad, inactive_week_cnt))
outliers <- outliers[-c(2,10)]


## use Winsorizing to replace the extreme values with less extreme ones
## good practical guidance on this topic:
## https://www.r-bloggers.com/winsorization/
vars_to_winsorize <- regularity_data %>%
  select(one_of(names(outliers)))

winsorized_data_v1 <- data.frame(apply(vars_to_winsorize, 2, winsor1, fraction=0.05))
apply(winsorized_data_v1, 2, function(x) length(boxplot.stats(x)$out))
summary(winsorized_data_v1)

# winsorized_data_v2 <- data.frame(apply(vars_to_winsorize, 2, winsor2))
# apply(winsorized_data_v2, 2, function(x) length(boxplot.stats(x)$out))
# summary(winsorized_data_v2)

## remove the variables that kept large number of outliers even after winsorizing
winsorized_data_v1 <- winsorized_data_v1 %>% 
  select(-c(ses_timegap_mad, week_cnt_entropy))

base::setdiff(colnames(regularity_data), colnames(winsorized_data_v1))

features <- data.frame(cbind(regularity_data %>%
                               select(user_id, res_type_median, res_type_mad, last_min_prop_sd), 
                             winsorized_data_v1))
str(features)
summary(features)

## check for correlations among the indicators

## check if the data are normally distributed
apply(features[,-1], 2, shapiro.test)
# none of the variables is normally distributed, so, Pearson cannot be used

## using findCorrelation f. from caret, with Spearman
indicators_cor <- cor(features[,-1], use = "complete.obs", method = "spearman")
findCorrelation(indicators_cor, cutoff = .7, names = T)
# "ses_tot"      "res_type_mad"

## examine correlations in more detail

## use the ggcorr f. for computing and plotting the correlations
## https://briatte.github.io/ggcorr/
library(ggplot2)
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")

ggcorr(features[,-1], method = c("complete","spearman"), 
       #      geom = "circle", min_size = 0, max_size = 15,
       label = TRUE, label_size = 3.5,
       hjust = 0.85, size = 4, layout.exp = 1)


## feature selection?

## manually: 
## based on the correlations plot, the exclusion of 
## - res_type_mad (3),  
## - ses_tot (5)
## - weekly_preparing_prop_mad (9)
## will largely solve the problem of correlated features; let's check it
ggcorr(features[,-c(1,3,5,9)], method = c("complete","spearman"), 
       label = TRUE, label_size = 3.5,
       hjust = 0.85, size = 4, layout.exp = 1)


f.man.filtered <- features %>%
  select(-c(res_type_mad, ses_tot, weekly_preparing_prop_mad))

summary(f.man.filtered[,-1])
apply(f.man.filtered, 2, function(x) which(is.na(x)))
apply(f.man.filtered, 2, function(x) length(boxplot.stats(x)$out))

## scale features: week_cnt_sd, weekday_cnt_sd
## since outliers are removed, normalization can be used
f.scaled <- data.frame(apply(f.man.filtered %>% select(week_cnt_sd, weekday_cnt_sd), 
                           2, function(x) {(x-median(x, na.rm = T))/IQR(x, na.rm = T)} ))
summary(f.scaled)

f.scaled <- data.frame(cbind(f.man.filtered %>% select(-c(week_cnt_sd, weekday_cnt_sd)), f.scaled))


#############################################################
# cluster students based on the regularity indicators
# try both hierarchical clustering and k-medoids
#############################################################
source(file = "util_functions.R")
library(knitr)

## hierarchical clustering
hc <- do.hclustering(f.scaled[,-1], 'ward.D2')
## 3 or 4 clusters seem to be the best?

## examine various cluster models
clusters <- sapply(3:8, function(ncl) table(cutree(hc, k = ncl)))
names(clusters) <- c(3:8)
clusters

## try also k-medoids
med.cl <- do.kmedoids.clustering(f.scaled, k.min = 3, k.max = 7)
# 3 cluster as the best solution 


######################################################
## check first the cluster models obtained through HC
######################################################

stud.clust <- data.frame(user_id=f.scaled$user_id,
                         cl4=cutree(hc, k=4),
                         cl3=cutree(hc, k = 3))

## examine the 4 cluster model
cl4.stats <- summary.stats(f.man.filtered[,-1], 
                           clusters = stud.clust$cl4, cl.number = 4)
kable(cl4.stats, format = 'rst')


## examine the 3 cluster model
cl3.stats <- summary.stats(f.man.filtered[,-1], 
                           clusters = stud.clust$cl3, cl.number = 3)
kable(cl3.stats, format = 'rst')

## use boxplots to better examine feature values across the clusters
library(ggplot2)
cl3.data <- f.man.filtered
cl3.data$cluster <- as.factor(cutree(hc, k = 3))
ggplot(cl3.data, aes(x=cluster, y=weekday_cnt_entropy, fill=cluster)) + geom_boxplot()
ggplot(cl3.data, aes(x=cluster, y=last_min_prop, fill=cluster)) + geom_boxplot()
ggplot(cl3.data, aes(x=cluster, y=on_topic_prop, fill=cluster)) + geom_boxplot()

####################################################
## compare clusters w.r.t. the students' exam scores
####################################################

## retrieve students' exam scores
exam.scores <- read_csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
str(exam.scores)

## merge cluster data with exam scores
stud.clust <- merge(x = stud.clust, y = exam.scores[,-2], 
                    by.x = 'user_id', by.y = 'USER_ID',
                    all.x = T, all.y = F)
str(stud.clust)
summary(stud.clust[,c(4,5)])
# missing scores for 9 students; remove them
stud.clust <- stud.clust %>%
  filter( !is.na(SC_FE_TOT) )

## examine the 3 clusters model

## compute the summary statistics for the students' exam scores
stud.cl3.stats <- summary.stats(stud.clust[,c(4,5)], stud.clust$cl3, 3)
kable(x = stud.cl3.stats, format = "rst")

kruskal.test(stud.clust$SC_FE_TOT ~ stud.clust$cl3)
# chi-squared = 47.026, df = 2, p-value = 6.144e-11

## apply Mann-Whitney U Test to do pair-wise comparisons
cl3.df <- stud.clust[,-2]
colnames(cl3.df)[2] <- "class"
kable(pairwise.fexam.compare(3, 3, cl3.df), format = "rst")
# all are significant

kruskal.test(stud.clust$SC_MT_TOT ~ stud.clust$cl3)
# chi-squared = 43.148, df = 2, p-value = 4.27e-10

## do pairwise comparison
kable(pairwise.mtxam.compare(3, 3, cl3.df), format = "rst")
#  2 (out of 3) are significant


## examine the 4 clusters model

## compute the summary statistics for the students' exam scores
stud.cl4.stats <- summary.stats(stud.clust[,c(4,5)], stud.clust$cl4, 4)
kable(x = stud.cl4.stats, format = "rst")

kruskal.test(stud.clust$SC_FE_TOT ~ stud.clust$cl4)
# chi-squared = 47.217, df = 3, p-value = 3.125e-10

## apply Mann-Whitney U Test to do pair-wise comparisons
cl4.df <- stud.clust[,-3]
colnames(cl4.df)[2] <- "class"
kable(pairwise.fexam.compare(4, 6, cl4.df), format = "rst")
# 3 (of 6) are not significant

kruskal.test(stud.clust$SC_MT_TOT ~ stud.clust$cl4)
# chi-squared = 43.15, df = 3, p-value = 2.287e-09

## do pairwise comparison
kable(pairwise.mtxam.compare(4, 6, cl4.df), format = "rst")
# 3 (of 6) are not significant

##########################################################
## check now the cluster models obtained through K-Medoids
##########################################################

kmed.clust <- data.frame(user_id=features$user_id,
                         clust=med.cl$clustering)

## examine the clusters
kmed.stats <- summary.stats(f.man.filtered[,-1], 
                            clusters = kmed.clust$clust, cl.number = 3)
kable(kmed.stats, format = 'rst')

kmed.clust <- merge(x = kmed.clust, y = exam.scores[,-2], 
                    by.x = 'user_id', by.y = 'USER_ID',
                    all.x = T, all.y = F)
summary(kmed.clust)
kmed.clust <- kmed.clust %>% filter( !is.na(SC_MT_TOT) )
# removed 9 students who didn't have exam scores

## compute summary statistics for the students' exam scores
kmed.clust.stats <- summary.stats(kmed.clust[,c(3,4)], kmed.clust$clust, 3)
kable(x = kmed.clust.stats, format = "rst")

kruskal.test(kmed.clust$SC_FE_TOT ~ kmed.clust$clust)
# not significant!

kruskal.test(kmed.clust$SC_MT_TOT ~ kmed.clust$clust)
# no significant difference!


#############################################################################
## Build a Random Forest regression model to examine the importance of the 
## features / regularity indicators - as a kind of feature selection step
## 
## Use the whole dataset for training as in this case, the predictive power
## of the model on the test set is not that important; the focus is on the 
## features
#############################################################################

library(caret)

regularity_data <- read_csv("Intermediate_results/regularity_of_study/regularity_indicators.csv")

## remove features that proved useless - high near-zero variance, high number of outliers
regularity_data <- regularity_data %>%
  select(1:4,6,7,9:12,15:20,23,24)

regularity_data <- regularity_data %>%
  select(-c(topic_cnt_median, topic_cnt_mad, inactive_week_cnt))

f.outliers <- apply(regularity_data[,-1], 2, function(x) length(boxplot.stats(x)$out))
f.outliers <- f.outliers[f.outliers>0]

vars_to_winsorize <- regularity_data %>%
  select(one_of(names(f.outliers)))

winsorized_data_v1 <- data.frame(apply(vars_to_winsorize, 2, winsor1, fraction=0.075))
apply(winsorized_data_v1, 2, function(x) length(boxplot.stats(x)$out))
summary(winsorized_data_v1)

base::setdiff(colnames(regularity_data), colnames(winsorized_data_v1))

features <- data.frame(cbind(regularity_data %>%
                               select(user_id, res_type_median, res_type_mad, last_min_prop_sd), 
                             winsorized_data_v1))
features <- features[,c(1,5:15,2:4)]

apply(features, 2, function(x) which(is.na(x)))
## last_min_prop_sd has 13 NAs, examine them
regularity_data[is.na(regularity_data$last_min_prop_sd)==T,]
## on_topic_prop_sd has 5 NAs
regularity_data[is.na(regularity_data$on_topic_prop_sd)==T,]
## these are highly inactive students - have between 1 and 6 sessions in total!
## remove them as RF cannot deal with NA values

features <- features[is.na(features$on_topic_prop_sd)==F,]

exam.scores <- read_csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
summary(exam.scores)

all.data <- merge(x = features, y = exam.scores[,-c(2,3)], by.x = 'user_id', by.y = 'USER_ID', 
                  all.x = T, all.y = F)
summary(all.data)

all.data <- all.data %>% filter( !is.na(SC_FE_TOT) )
## examine scores of students with NAs for last_min_prop_sd
all.data$SC_FE_TOT[is.na(all.data$last_min_prop_sd)]


## check for outliers
apply(all.data[,-1], 2, function(x) sort(boxplot.stats(x)$out))
# one, but will ignore it

apply(all.data[,-1], 2, shapiro.test)

## rescale: ses_tot, week_cnt_sd, weekday_cnt_sd, ses_timegap_mad
scaled <- data.frame(apply(all.data %>% select(ses_tot, week_cnt_sd, weekday_cnt_sd, ses_timegap_mad), 
                           2, function(x) {(x-median(x, na.rm = T))/IQR(x, na.rm = T)} ))
summary(scaled)

f.scaled <- all.data %>% 
  select(-c(ses_tot, ses_timegap_mad, week_cnt_sd, weekday_cnt_sd)) %>%
  cbind(scaled)
str(f.scaled)  

## use caret to build a RF model
tr.control <- trainControl(method ="repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           search = "grid")
## default for mtry is 1/3 of the number of predictors (11 in this case)
tune.grid <- expand.grid(.mtry=c(2:5))

set.seed(562017)
rf.no.ses.cnt <- train(SC_FE_TOT ~ ., 
            data=f.scaled[,-c(1,13)], 
            method="rf", 
            ntree=3000,
            importance = TRUE,
            tuneGrid=tune.grid, 
            trControl=tr.control)
print(rf.no.ses.cnt)
plot(rf.no.ses.cnt)
# best model when ses_tot feature is included 
# mtry=3, RMSE:8.318385,  Rsquared: 0.2533053

# best model without the ses_tot feature
# mtry=3, RMSE:8.456089,  Rsquared: 0.2289698


## examine the importance of features
varImp(rf, scale = TRUE)
# ses_tot                    100.000
# week_cnt_entropy            63.685
# weekday_cnt_entropy         43.981
# weekday_cnt_sd              37.489
# on_topic_prop_sd            34.907
# weekly_revisiting_prop_mad  31.412
# week_cnt_sd                 30.452
# on_topic_prop               28.744
# ses_timegap_mad             27.809
# last_min_prop_sd            24.444
# weekly_preparing_prop_mad   21.358
# last_min_prop               13.174
# res_type_median              3.346
# res_type_mad                 0.000

## examine the importance of features
v.imp <- varImp(rf.no.ses.cnt, scale = TRUE)
v.imp
# week_cnt_entropy           100.0000
# weekday_cnt_entropy         67.5020
# weekday_cnt_sd              61.1775
# on_topic_prop_sd            57.8560
# week_cnt_sd                 52.5607
# on_topic_prop               38.7206
# weekly_revisiting_prop_mad  36.2821
# last_min_prop_sd            32.4286
# weekly_preparing_prop_mad   30.6869
# ses_timegap_mad             26.8359
# last_min_prop               17.4164
# res_type_mad                 0.6038
# res_type_median              0.0000

#############################################################
# cluster students using the indicators / features that were
# identified as important by RF model
#############################################################

summary(f.scaled)

imp.vars <- rownames(v.imp$importance)[v.imp$importance$Overall>30]

## add the 'ses_tot' feature to see if clustering will give better results
## (ie. be better in distinguishing students based on their exam scores)
imp.vars <- c('ses_tot', imp.vars)

f.rf.selection <- f.scaled %>% 
  select(one_of(imp.vars)) %>%
  mutate(user_id=f.scaled$user_id)

f.rf.selection <- f.rf.selection[,c(11,1:10)]

## hierarchical clustering
hc <- do.hclustering(f.rf.selection[,-1], 'ward.D2')
## 3, 4 or 5 clusters?

## examine various cluster models
clusters <- sapply(3:8, function(ncl) table(cutree(hc, k = ncl)))
names(clusters) <- c(3:8)
clusters

stud.clust <- data.frame(user_id=f.rf.selection$user_id,
                         cl4=cutree(hc, k=4),
                         cl3=cutree(hc, k = 3))

## examine the 4 cluster model
## for examining clusters, use the variables before scaling
f.unscaled <- all.data %>%
  select(one_of(c('user_id', imp.vars)))
f.unscaled <- f.unscaled[,c(1,2,10,3,11,4:9)]

cl4.stats <- summary.stats(f.unscaled[,-1], 
                           clusters = stud.clust$cl4, cl.number = 4)
kable(cl4.stats, format = 'rst')

## examine the 3 cluster model
cl3.stats <- summary.stats(f.unscaled[,-1], 
                           clusters = stud.clust$cl3, cl.number = 3)
kable(cl3.stats, format = 'rst')

## use boxplots to better examine feature values across the clusters
library(ggplot2)
cl3.data <- f.unscaled
cl3.data$cluster <- as.factor(cutree(hc, k = 3))
ggplot(cl3.data, aes(x=cluster, y=weekday_cnt_entropy, fill=cluster)) + geom_boxplot()
ggplot(cl3.data, aes(x=cluster, y=week_cnt_entropy, fill=cluster)) + geom_boxplot()
ggplot(cl3.data, aes(x=cluster, y=on_topic_prop, fill=cluster)) + geom_boxplot()

####################################################
## compare clusters w.r.t. the students' exam scores
####################################################

## retrieve students' exam scores
exam.scores <- read_csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
str(exam.scores)

## merge cluster data with exam scores
stud.clust <- merge(x = stud.clust, y = exam.scores[,-2], 
                    by.x = 'user_id', by.y = 'USER_ID',
                    all.x = T, all.y = F)
str(stud.clust)
summary(stud.clust[,c(4,5)])

## examine the 3 clusters model

## compute the summary statistics for the students' exam scores
stud.cl3.stats <- summary.stats(stud.clust[,c(4,5)], stud.clust$cl3, 3)
kable(x = stud.cl3.stats, format = "rst")

kruskal.test(stud.clust$SC_FE_TOT ~ stud.clust$cl3)
# chi-squared = 53.174, df = 2, p-value = 2.841e-12
# chi-squared = 69.757, df = 2, p-value = 7.119e-16 (with ses_tot)

## apply Mann-Whitney U Test to do pair-wise comparisons
cl3.df <- stud.clust[,-2]
colnames(cl3.df)[2] <- "class"
kable(pairwise.fexam.compare(3, 3, cl3.df), format = "rst")
# 1-2 and 2-3 are significant (also when ses_tot is added)

kruskal.test(stud.clust$SC_MT_TOT ~ stud.clust$cl3)
# chi-squared = 36.709, df = 2, p-value = 1.069e-08
# chi-squared = 57.262, df = 2, p-value = 3.678e-13 (with ses_tot)

## do pairwise comparison
kable(pairwise.mtxam.compare(3, 3, cl3.df), format = "rst")
# 1-2 and 2-3 are significant (also when ses_tot is added)


## examine the 4 clusters model

## compute the summary statistics for the students' exam scores
stud.cl4.stats <- summary.stats(stud.clust[,c(4,5)], stud.clust$cl4, 4)
kable(x = stud.cl4.stats, format = "rst")

kruskal.test(stud.clust$SC_FE_TOT ~ stud.clust$cl4)
# chi-squared = 55.749, df = 3, p-value = 4.752e-12
# chi-squared = 70.636, df = 3, p-value = 3.119e-15 (with ses_tot added)

## apply Mann-Whitney U Test to do pair-wise comparisons
cl4.df <- stud.clust[,-3]
colnames(cl4.df)[2] <- "class"
kable(pairwise.fexam.compare(4, 6, cl4.df), format = "rst")
# 2-3, 3-4, and 1-3 are not significant
# 3-4, 1-3, and 1-4 are not significant when ses_tot is added

kruskal.test(stud.clust$SC_MT_TOT ~ stud.clust$cl4)
# chi-squared = 38.793, df = 3, p-value = 1.92e-08
# chi-squared = 57.78, df = 3, p-value = 1.752e-12

## do pairwise comparison
kable(pairwise.mtxam.compare(4, 6, cl4.df), format = "rst")
# 2-3, 3-4, and 1-3 are not significant
# 3-4, 1-3, and 1-4 are not significant when ses_tot is added

#########################################################################
## examine values of regularity indicators for top perfoming students
## and those with the weakest performance on both midterm and final exams
#########################################################################

exam.scores <- read_csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
exam.scores <- exam.scores[,-2]
colnames(exam.scores)[1] <- 'user_id'

## get ids of students in top 10 percentile and bottom 10 percentile
stud.groups <- high.and.low.achievers(exam.scores)
top.10p <- stud.groups[[1]]
bottom.10p <- stud.groups[[5]]

reg.indicators <- read_csv("Intermediate_results/regularity_of_study/regularity_indicators.csv")

## check if the user ids match
length(intersect(top.10p, reg.indicators$user_id))
# 15
length(intersect(bottom.10p, reg.indicators$user_id))
# 13 - so, we are missing data for 11 students

reg.selection <- reg.indicators %>%
  filter(user_id %in% c(top.10p, bottom.10p)) %>%
  mutate(group = if_else(user_id %in% top.10p, true = 'top10p', false = 'bottom10p')) %>%
  mutate(group = factor(group))

str(reg.selection)

top.10p.data <- reg.selection %>% filter(group=="top10p") 
bottom.10p.data <- reg.selection %>% filter(group=="bottom10p") 

## plot the indicators for the two groups
ggplot(reg.selection, aes(x=group, y=ses_tot, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=week_cnt_sd, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=week_cnt_entropy, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=weekday_cnt_sd, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=weekday_cnt_entropy, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=on_topic_prop, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=on_topic_prop_sd, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=last_min_prop, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=last_min_prop_sd, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=ses_timegap_mad, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=weekly_preparing_prop_mad, fill=group)) + geom_boxplot()
ggplot(reg.selection, aes(x=group, y=weekly_revisiting_prop_mad, fill=group)) + geom_boxplot()
