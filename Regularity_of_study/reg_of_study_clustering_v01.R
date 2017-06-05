#################################
# Feature selection and scaling
#################################

library(caret)

## check for zero or near-zero variance predictors
## using the nearZeroVar f. from caret:
## https://topepo.github.io/caret/pre-processing.html#zero--and-near-zero-variance-predictors
nzv <- nearZeroVar(regularity.data[,-1], saveMetrics= TRUE)
nzv
# no predictor has near-zero variance

## check for correlations among the indicators

## check if the data are normally distributed
apply(regularity.data[,-1], 2, shapiro.test)
# none of the variables is normally distributed, so, Pearson cannot be used

## using findCorrelation f. from caret, with Spearman
indicators_cor <- cor(regularity.data[,-1], use = "complete.obs", method = "spearman")
findCorrelation(indicators_cor, cutoff = .7, names = T)
# "ses_tot", "week_cnt_sd", "last_min_prop_sd"

## examine correlations in more detail

## use the ggcorr f. for computing and plotting the correlations
## https://briatte.github.io/ggcorr/
library(ggplot2)
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")

ggcorr(regularity.data[,-1], method = c("complete","spearman"), 
       #      geom = "circle", min_size = 0, max_size = 15,
       label = TRUE, label_size = 3.5,
       hjust = 0.85, size = 4, layout.exp = 1)


## feature selection?

## manually: 
## based on the correlations plot, the exclusion of 
## - ses_timegap_sd (7),  
## - inactive_week_cnt (2),
## - week_cnt_sd (3)
## - weekly_prepare_prop_sd (8) or weekly_revisits_prop_sd (9)
## will largely solve the problem of correlated features; let's check it
ggcorr(regularity.data[,-c(1,3,7,8)], method = c("complete","spearman"), 
       label = TRUE, label_size = 3.5,
       hjust = 0.85, size = 4, layout.exp = 1)


## algoritmically:
## 1) use Random Forest to build a prediction (regression) model 
##    and examine the feature relevancy
## 2) check these links:
# - https://stats.stackexchange.com/questions/108743/methods-in-r-or-python-to-perform-feature-selection-in-unsupervised-learning
# (in particular, the last answer mentioning nsprcomp R package)
# - https://cran.r-project.org/web/packages/sparcl/sparcl.pdf - sparcl R package

## Doing it manually (for now)
## removing user_id (1), inactive_week_cnt (2), 
## week_cnt_sd (3), ses_timegap_sd (7), weekly_prepare_prop_sd (8)
features <- regularity.data[,-c(1:3,7,8)]
summary(features)

## check for outliers
apply(features, 2, function(x) length(boxplot.stats(x)$out))
# the number of outliers ranges from 1 to 81!

## scale features: weekday_cnt_sd and last_min_sd
## due to outliers, use standardization with median and Interquartile Range (IQR)
f.scaled <- data.frame(apply(features[,c(2,10)], 2, 
                             function(x) {(x-median(x, na.rm = T))/IQR(x, na.rm = T)} ))
summary(f.scaled)

f.scaled <- data.frame(cbind(features[,-c(2,10)], f.scaled))
f.scaled <- f.scaled[,c(9,1:8,10)]


#############################################################
# cluster students based on the regularity indicators
# try both hierarchical clustering and k-medoids
#############################################################
source(file = "util_functions.R")
library(knitr)

## hierarchical clustering
hc <- do.hclustering(f.scaled, 'ward.D2')
## 4 clusters seem to be the best?

## examine various cluster models
clusters <- sapply(3:8, function(ncl) table(cutree(hc, k = ncl)))
names(clusters) <- c(3:8)
clusters

## try also k-medoids
med.cl <- do.kmedoids.clustering(f.scaled, k.min = 3, k.max = 7)
# also 4 cluster as the best solution 


######################################################
## check first the cluster models obtained through HC
######################################################

stud.clust <- data.frame(user_id=regularity.data$user_id,
                         cl4=cutree(hc, k=4),
                         cl5=cutree(hc, k = 5))

## examine the 4 cluster model
cl4.stats <- summary.stats(features, 
                           clusters = stud.clust$cl4, cl.number = 4)
kable(cl4.stats, format = 'rst')

## use boxplots to better examine feature values across the clusters
library(ggplot2)
cl4.data <- regularity.data
cl4.data$cluster <- as.factor(cutree(hc, k = 4))
ggplot(cl4.data, aes(x=cluster, y=weekly_ahead_prop_sd, fill=cluster)) + geom_boxplot()
ggplot(cl4.data, aes(x=cluster, y=week_cnt_entropy, fill=cluster)) + geom_boxplot()
ggplot(cl4.data, aes(x=cluster, y=topic_cnt_sd, fill=cluster)) + geom_boxplot()
ggplot(cl4.data, aes(x=cluster, y=weekly_catchup_prop_sd, fill=cluster)) + geom_boxplot()

## examine the 5 cluster model
cl5.stats <- summary.stats(features, 
                           clusters = stud.clust$cl5, cl.number = 5)
kable(cl5.stats, format = 'rst')

## better go with 4 clusters

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


## examine the 4 clusters model

## compute the summary statistics for the students' exam scores
stud.cl4.stats <- summary.stats(stud.clust[,c(4,5)], stud.clust$cl4, 4)
kable(x = stud.cl4.stats, format = "rst")

kruskal.test(stud.clust$SC_FE_TOT ~ stud.clust$cl4)
# chi-squared = 34.995, df = 3, p-value = 1.221e-07

## apply Mann-Whitney U Test to do pair-wise comparisons
cl4.df <- stud.clust[,-3]
colnames(cl4.df)[2] <- "class"
kable(pairwise.fexam.compare(4, 6, cl4.df), format = "rst")

kruskal.test(stud.clust$SC_MT_TOT ~ stud.clust$cl4)
# chi-squared = 34.146, df = 3, p-value = 1.846e-07

## do pairwise comparison
kable(pairwise.mtxam.compare(4, 6, cl4.df), format = "rst")

##########################################################
## check now the cluster models obtained through K-Medoids
##########################################################

kmed.clust <- data.frame(user_id=regularity.data$user_id,
                         clust=med.cl$clustering)

## examine the clusters
kmed.stats <- summary.stats(features, 
                            clusters = kmed.clust$clust, cl.number = 4)
kable(kmed.stats, format = 'rst')

kmed.clust <- merge(x = kmed.clust, y = exam.scores[,-2], 
                    by.x = 'user_id', by.y = 'USER_ID',
                    all.x = T, all.y = F)
summary(kmed.clust)
kmed.clust <- kmed.clust %>% filter( !is.na(SC_MT_TOT) )
# removed 9 students who didn't have exam scores

## compute summary statistics for the students' exam scores
kmed.clust.stats <- summary.stats(kmed.clust[,c(3,4)], kmed.clust$clust, 4)
kable(x = kmed.clust.stats, format = "rst")

kruskal.test(kmed.clust$SC_FE_TOT ~ kmed.clust$clust)
# chi-squared = 41.597, df = 3, p-value = 4.885e-09

## apply Mann-Whitney U Test to do pair-wise comparisons
kmed.clust.df <- kmed.clust
colnames(kmed.clust.df)[2] <- "class"
kable(pairwise.fexam.compare(4, 6, kmed.clust.df), format = "rst")

kruskal.test(kmed.clust$SC_MT_TOT ~ kmed.clust$clust)
# chi-squared = 45.435, df = 3, p-value = 7.477e-10

kable(pairwise.mtxam.compare(4, 6, kmed.clust.df), format = "rst")
# 3 (out of 6) comparisons are insignificant


#############################################################################
## Build a Random Forest regression model to examine the importance of the 
## features / regularity indicators - as a kind of feature selection step
## 
## Use the whole dataset for training as in this case, the predictive power
## of the model on the test set is not that important; the focus is on the 
## features
#############################################################################

library(caret)

regularity.data <- read_csv("Intermediate_results/regularity_of_study/regularity_indicators.csv")
summary(regularity.data[,-1])

apply(regularity.data, 2, function(x) which(is.na(x)))
## NA values are associated with the following 4 records: 163 241 336 480; examine them
regularity.data[c(163, 241, 336, 480),]
## these are highly inactive students - were active only in 1 or 2 weeks
## remove them as RF cannot deal with NA values

regularity.data <- regularity.data[-c(163, 241, 336, 480),]

exam.scores <- read_csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
summary(exam.scores)

all.data <- merge(x = regularity.data, y = exam.scores[,-c(2,3)], by.x = 'user_id', by.y = 'USER_ID', 
                  all.x = T, all.y = F)
summary(all.data)

all.data <- all.data %>% filter( !is.na(SC_FE_TOT) )

## check for outliers
apply(all.data[,-1], 2, function(x) sort(boxplot.stats(x)$out))
## inactive_week_cnt and weekly_catchup_prop_sd have really huge number of outliers, 104 and 81, respectively
## turn them into binary features
all.data <- all.data %>%
  mutate(inactive_week_binary = if_else(inactive_week_cnt==0, true = 0, false = 1)) %>%
  mutate(weekly_catchup_binary = if_else(weekly_catchup_prop_sd < median(weekly_catchup_prop_sd), 
                                         true = 0, false = 1))

all.data <- all.data %>% dplyr::select( -c(inactive_week_cnt, weekly_catchup_prop_sd) )
summary(all.data)

## rescale: ses_timegap_sd, week_cnt_sd, weekday_cnt_sd
## due to outliers, use standardization with median and Interquartile Range (IQR)
scaled <- data.frame(apply(all.data %>% dplyr::select(ses_timegap_sd, week_cnt_sd, weekday_cnt_sd), 
                           2, function(x) {(x-median(x, na.rm = T))/IQR(x, na.rm = T)} ))
summary(scaled)

features <- all.data %>% 
  dplyr::select(-c(ses_timegap_sd, week_cnt_sd, weekday_cnt_sd)) %>%
  cbind(scaled)
str(features)  

## use caret to build a RF model
tr.control <- trainControl(method ="repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           search = "grid")
## default for mtry is 1/3 of the number of predictors (14 in this case)
tune.grid <- expand.grid(.mtry=c(2:5))

set.seed(362017)
rf <- train(SC_FE_TOT ~ ., 
            data=features[,-1], 
            method="rf", 
            ntree=3000,
            importance = TRUE,
            tuneGrid=tune.grid, 
            trControl=tr.control)
print(rf)
plot(rf)
# mtry value used for the model: 3

## examine the importance of features
varImp(rf, scale = TRUE)

# ses_timegap_sd          100.000
# week_cnt_entropy         81.734
# weekday_cnt_entropy      46.191
# weekly_prepare_prop_sd   41.465
# weekly_revisits_prop_sd  39.408
# weekday_cnt_sd           27.517
# inactive_week_binary     26.631
# week_cnt_sd              26.257
# weekly_ahead_prop_sd     25.456
# ------------------------------------ maybe drow a line here?
# res_type_sd              17.983
# last_min_prop            15.594
# last_min_sd              11.255
# weekly_catchup_binary     2.264
# topic_cnt_sd              0.000


#############################################################################
## LCA-based clustering
##
## Since all the regularity indicators have outliers, some even high number 
## of outliers, those outliers could have affected the clustering results
## As an alternative, transform the features into nominal ones with 
## 3 possible values, as follows:
## - 1: outlier
## - 2: outside of inter quartile range (IQR), but not an outlier
## - 3: within IQR
## Then use these features to cluster students with LCA 
#############################################################################

summary(features)
apply(features, 2, function(x) length(boxplot.stats(x)$out))

nom.f.matrix <- matrix(nrow = nrow(features), ncol = ncol(features), 
                       data = rep(2, nrow(features)*ncol(features)))
for(i in 1:ncol(features)) {
  f.outliers <- boxplot.stats(features[,i])$out 
  f.outliers <- which(features[,i] %in% f.outliers)
  nom.f.matrix[f.outliers,i] <- 1
  f.quantiles <- quantile(features[,i], probs = c(0.25,0.75), na.rm = T, names = F)
  f.iqr <- which((features[,i] >= f.quantiles[1]) & (features[,i] <= f.quantiles[2]))
  nom.f.matrix[f.iqr,i] <- 3  
}

nom.features <- data.frame(nom.f.matrix)
colnames(nom.features) <- colnames(features)

## check if outliers are well recognized
apply(nom.features, 2, function(x) length(which(x==1)))

nom.f.factor <- apply(nom.features, 2, as.factor)
summary(nom.f.factor)

require(poLCA)

nom.f <- cbind(week_cnt_entropy, weekday_cnt_sd, weekday_cnt_entropy, weekly_revisits_prop_sd,
               weekly_catchup_prop_sd, weekly_ahead_prop_sd, topic_cnt_sd, res_type_sd,
               last_min_prop,last_min_sd) ~ 1

set.seed(362917)
eval.metrics <- data.frame()
for(nc in 2:7) {
  lc <- poLCA(nom.f, data = nom.features, nrep = 50, nclass = nc, verbose = F, na.rm = F)
  metrics <- c(nclass = nc, 
               AIC = lc$aic, 
               BIC = lc$bic, 
               ABIC = (-2*lc$llik) + ((log((lc$N + 2)/24)) * lc$npar),
               CAIC = (-2*lc$llik) + lc$npar * (1 + log(lc$N)),
               LogLike = lc$llik, #log likelihood 
               ChiSq = lc$Chisq) # Chi square
  eval.metrics <- as.data.frame( rbind(eval.metrics, metrics) )
}
colnames(eval.metrics) <- c('nclass', 'AIC', 'BIC', 'ABIC', 'CAIC', 'LogLike', 'ChiSquare')
require(knitr)
kable(x = eval.metrics, format = "rst")

## seams to be a deadend...
## TODO: consider redoing it with 5 possible values

