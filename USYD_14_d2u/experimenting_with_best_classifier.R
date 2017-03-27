############################################################################
## THIS FILE IS FOR FURTHER EXPERIMENTING WITH THE BEST CLASSIFICATION MODEL
##  - RANDOM FOREST MODEL 
##  - FEATURES: THE SET OF 12 FEATURES USED FOR WEEKLY CLUSTERING + WEEK
##  - TRAINED ON NORMALIZED DATA (NORMALIZATION ON WEEKLY-LEVEL) FROM WEEKS 
##    2-5 & 7-12 (i.e. WEEKS 6 AND 13 OMITTED)
##  - UP-SAMPLING STRATEGY USED FOR HANDLING CLASS IMBALANCE 
############################################################################

###############################################################
## PREPARE 2014 DATASET FOR BUILDING (TRAINING) THE CLASSIFIER  
###############################################################

## load the normalized feature set
all.weeks <- read.csv(file = "Intermediate_files/normalized_strategy_classification_dataset.csv")

# R's Random Forest algorithm cannot work with missing values
# so, before applying the algorithm, the missing values have to be handled
all.nas <- which(is.na(all.weeks$VEQ.TOT))
all.weeks <- all.weeks[-all.nas,]
summary(all.weeks)
## for ORG.VIEW and DBOARD.VIEW, replace NAs with 0 (zero)
## since NA means that there were no events of this type
org.view.nas <- which(is.na(all.weeks$ORG.VIEW))
all.weeks$ORG.VIEW[org.view.nas] <- 0
dboad.view.nas <- which(is.na(all.weeks$DBOARD.VIEW))
all.weeks$DBOARD.VIEW[dboad.view.nas] <- 0
## check again
which(complete.cases(all.weeks)==F)
## now, it's fine - no more observations with NA values

## MERGE CLASSES B1 & B2, AND C1 & C2
all.weeks$CL <- as.character(all.weeks$CL)
all.weeks$CL[all.weeks$CL %in% c('B1','B2')] <- 'B'
all.weeks$CL[all.weeks$CL %in% c('C1','C2')] <- 'C'
all.weeks$CL <- factor(all.weeks$CL)

## create a dataset without weeks 6 and 13
weeks.minus613 <- subset(all.weeks, WEEK %in% c(2:5,7:12))
table(weeks.minus613$CL)
#  A   B   C   D   E   F 
# 407 999 272 598 443  91 
round(prop.table(table(weeks.minus613$CL)), digits = 4)
#    A      B      C      D      E      F 
# 0.1448 0.3555 0.0968 0.2128 0.1577 0.0324 

###########################################################
## BUILD (TRAIN) THE CLASSIFIER ON THE WHOLE 2014 DATASET
## THEN, USE THE 2015 DATA FOR TESTING
###########################################################
library(caret)
seed <- 2409
tr.control <- trainControl(method ="repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           search = "grid",
                           sampling = "up") 
tune.grid <- expand.grid(.mtry=c(4:7))

set.seed(seed)
rf.up.min613 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                        VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                        data = weeks.minus613, # use the entire 2014 dataset
                        method = "rf", 
                        ntree = 2000,
                        tuneGrid = tune.grid, 
                        trControl = tr.control)

print(rf.up.min613)
plot(rf.up.min613)
# best value for mtry: 7 
## store the model
saveRDS(object = rf.up.min613, file = "results/best_classifier_(trained_on_entire_2014data).RData")

###################################################
## PREPARE 2015 DATASET FOR TESTING THE CLASSIFIER  
###################################################

## get the 2015 data for week 2
w2.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w2_features_and_4cl.RData")
w2.2015 <- normalize.weekly.features(w2.2015) 
summary(w2.2015)
## add the CL (class) label
w2.2015$CL <- vector(length = nrow(w2.2015))
w2.2015$CL[w2.2015$CLUSTER==1] <- 'D'
w2.2015$CL[w2.2015$CLUSTER==2] <- 'A'
w2.2015$CL[w2.2015$CLUSTER==3] <- 'E'
w2.2015$CL[w2.2015$CLUSTER==4] <- 'C'
w2.2015$CL <- factor(w2.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w2.2015$WEEK <- 2

## get the 2015 data for week 3
w3.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w3_features_and_5cl.RData")
w3.2015 <- normalize.weekly.features(w3.2015) 
summary(w3.2015)
## add the CL (class) label
w3.2015$CL <- vector(length = nrow(w3.2015))
w3.2015$CL[w3.2015$CLUSTER==1] <- 'B'
w3.2015$CL[w3.2015$CLUSTER==2] <- 'D'
w3.2015$CL[w3.2015$CLUSTER %in% c(3,4)] <- 'C'
w3.2015$CL[w3.2015$CLUSTER==5] <- 'E'
w3.2015$CL <- factor(w3.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w3.2015$WEEK <- 3

## get the 2015 data for week 4
w4.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w4_features_and_4cl.RData")
w4.2015 <- normalize.weekly.features(w4.2015) 
summary(w4.2015)
## add the CL (class) label
w4.2015$CL <- vector(length = nrow(w4.2015))
w4.2015$CL[w4.2015$CLUSTER==1] <- 'B'
w4.2015$CL[w4.2015$CLUSTER==2] <- 'D'
w4.2015$CL[w4.2015$CLUSTER==3] <- 'E'
w4.2015$CL[w4.2015$CLUSTER==4] <- 'F'
w4.2015$CL <- factor(w4.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w4.2015$WEEK <- 4

## get the 2015 data for week 5
w5.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w5_features_and_4cl.RData")
w5.2015 <- normalize.weekly.features(w5.2015) 
summary(w5.2015)
## add the CL (class) label
w5.2015$CL <- vector(length = nrow(w5.2015))
w5.2015$CL[w5.2015$CLUSTER==1] <- 'F'
w5.2015$CL[w5.2015$CLUSTER==2] <- 'D'
w5.2015$CL[w5.2015$CLUSTER==3] <- 'B'
w5.2015$CL[w5.2015$CLUSTER==4] <- 'E'
w5.2015$CL <- factor(w5.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w5.2015$WEEK <- 5

## get the 2015 data for week 7 (intentionally skipping week 6)
w7.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w7_features_and_4cl.RData")
w7.2015 <- normalize.weekly.features(w7.2015) 
summary(w7.2015)
## add the CL (class) label
w7.2015$CL <- vector(length = nrow(w7.2015))
w7.2015$CL[w7.2015$CLUSTER==1] <- 'F'
w7.2015$CL[w7.2015$CLUSTER==2] <- 'C'
w7.2015$CL[w7.2015$CLUSTER==3] <- 'B'
w7.2015$CL[w7.2015$CLUSTER==4] <- 'E'
w7.2015$CL <- factor(w7.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w7.2015$WEEK <- 7

## get the 2015 data for week 8
w8.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w8_features_and_5cl.RData")
w8.2015 <- normalize.weekly.features(w8.2015) 
## add the CL (class) label
w8.2015$CL <- vector(length = nrow(w8.2015))
w8.2015$CL[w8.2015$CLUSTER %in% c(1,4)] <- 'B'
w8.2015$CL[w8.2015$CLUSTER==2] <- 'D'
w8.2015$CL[w8.2015$CLUSTER==3] <- 'A'
w8.2015$CL[w8.2015$CLUSTER==5] <- 'E'
w8.2015$CL <- factor(w8.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w8.2015$WEEK <- 8

## get the 2015 data for week 9
w9.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w9_features_and_5cl.RData")
w9.2015 <- normalize.weekly.features(w9.2015) 
summary(w9.2015)
## add the CL (class) label
w9.2015$CL <- vector(length = nrow(w9.2015))
w9.2015$CL[w9.2015$CLUSTER==1] <- 'D'
w9.2015$CL[w9.2015$CLUSTER==2] <- 'A'
w9.2015$CL[w9.2015$CLUSTER %in% c(3,4)] <- 'B'
w9.2015$CL[w9.2015$CLUSTER==5] <- 'E'
w9.2015$CL <- factor(w9.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w9.2015$WEEK <- 9

## get the 2015 data for week 10
w10.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w10_features_and_4cl.RData")
w10.2015 <- normalize.weekly.features(w10.2015) 
summary(w10.2015)
## add the CL (class) label
w10.2015$CL <- vector(length = nrow(w10.2015))
w10.2015$CL[w10.2015$CLUSTER %in% c(1,3)] <- 'B'
w10.2015$CL[w10.2015$CLUSTER==2] <- 'D'
w10.2015$CL[w10.2015$CLUSTER==4] <- 'F'
w10.2015$CL <- factor(w10.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w10.2015$WEEK <- 10

## get the 2015 data for week 11
w11.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w11_features_and_4cl.RData")
w11.2015 <- normalize.weekly.features(w11.2015) 
summary(w11.2015)
## add the CL (class) label
w11.2015$CL <- vector(length = nrow(w11.2015))
w11.2015$CL[w11.2015$CLUSTER %in% c(1,4)] <- 'B'
w11.2015$CL[w11.2015$CLUSTER==2] <- 'F'
w11.2015$CL[w11.2015$CLUSTER==3] <- 'D'
w11.2015$CL <- factor(w11.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w11.2015$WEEK <- 11

## get the 2015 data for week 12
w12.2015 <- readRDS("Intermediate_files/2015_weekly_clusters/w12_features_and_4cl.RData")
w12.2015 <- normalize.weekly.features(w12.2015) 
summary(w12.2015)
## add the CL (class) label
w12.2015$CL <- vector(length = nrow(w12.2015))
w12.2015$CL[w12.2015$CLUSTER==1] <- 'F'
w12.2015$CL[w12.2015$CLUSTER==2] <- 'E'
w12.2015$CL[w12.2015$CLUSTER==3] <- 'D'
w12.2015$CL[w12.2015$CLUSTER==4] <- 'B'
w12.2015$CL <- factor(w12.2015$CL, levels = c("A", "B", "C", "D", "E", "F"))
## add the WEEK feature
w12.2015$WEEK <- 12

## combine the data for all the weeks into one large dataset
all.2015 <- rbind(w2.2015, w3.2015, w4.2015, w5.2015, w7.2015, w8.2015, w9.2015,
                  w10.2015, w11.2015, w12.2015)

## check the distribution of instances across classes
table(all.2015$CL)
#  A    B    C    D    E    F 
# 234 1293  319  923  256  527 
round(prop.table(table(all.2015$CL)),digits = 4)
#    A      B      C      D      E      F 
# 0.0659 0.3640 0.0898 0.2599 0.0721 0.1484

## store the data
write.csv(all.2015, file = "Intermediate_files/2015_weekly_clusters/all_weeks_test_set.csv",
          quote = F, row.names = F)


################################################
## TEST THE CLASSIFIER ON THE 2015 DATASET
################################################

all2015.pred <- predict(rf.up.min613, newdata=all.2015)
confusionMatrix(all2015.pred, all.2015$CL)

## do manual, step-by-step calculation of evaluation metrics
## following: http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
## first, confusion matrix
cm <- table(actual=all.2015$CL, predicted=all2015.pred)
cm
## number of observations
n <- sum(cm)
## number of classes
nc <- nrow(cm)
## number of correctly classified instances per class
TP.per.class <- diag(cm)
## number of observations per class
obs.per.class <- apply(cm, 1, sum)
## number of predictions per class
pred.per.class <- apply(cm, 2, sum)
## distribution of observations across actual classes
p <- obs.per.class/n
## distribution of predictions over classes
q <- pred.per.class/n

## compute accuracy
accuracy <- sum(TP.per.class)/n
# 0.5619

## compute precision, recall, F1 for each class
precision <- TP.per.class/pred.per.class
round(precision, digits = 4)
#    A      B      C      D      E      F 
# 0.2379 0.7151 0.2562 0.6796 0.5419 0.4370 

recall <- TP.per.class/obs.per.class
round(recall, digits = 4)
#    A      B      C      D      E      F 
# 0.2735 0.8152 0.5172 0.5125 0.7070 0.1120

F1 <- 2*precision*recall/(precision+recall)
round(F1, digits = 4)
#    A      B      C      D      E      F 
# 0.2545 0.7618 0.3427 0.5843 0.6136 0.1782 

## macro-averaged precision, recall and F1
macro.eval.metrics <- c(mean(precision), mean(recall), mean(F1))
round(macro.eval.metrics, digits = 4)
# 0.4780 0.4896 0.4559

## ONE vs ALL
## when instances are not uniformly distributed over the classes, 
## it is useful to look at the performance of the classifier with 
## respect to one class - one class is considered the positive class 
## while the combination of all the other classes make up the negative class
oneVsAll = lapply(1 : nc, function(i){
                            v = c(cm[i,i], ## TP
                                  obs.per.class[i] - cm[i,i], ## FN
                                  pred.per.class[i] - cm[i,i], ## FP
                                  n - obs.per.class[i] - pred.per.class[i] + cm[i,i]); ## TN
                            return(matrix(v, nrow = 2, byrow = T))})
oneVsAll
## one 2x2 confusion matrix for each of the 6 classes
## summing up the values of these (6) matrices results in one confusion matrix 
## and allows for computing weighted metrics such as average accuracy and micro-averaged metrics
cm.sum <- matrix(0, nrow = 2, ncol = 2)
for(i in 1 : nc){
  cm.sum = cm.sum + oneVsAll[[i]]
}
## sum of one-vs-all matrices
cm.sum
#      [,1]  [,2]
# [1,] 1996  1556
# [2,] 1556 16204

## average accuracy: 
## the fraction of correctly classified instances in the sum of one-vs-all matrices matrix
avg.accuracy <- sum(diag(cm.sum))/sum(cm.sum)
round(avg.accuracy, digits = 4)
## 0.854

## micro-averaged precision, recall, and F1
## since the sum of the one-vs-all matrices is a symmetric matrix, these 3 metrics have the same values
micro.avg <- cm.sum[1,1]/colSums(cm.sum)[1]
round(micro.avg, digits = 4)
# 0.5619

## Kappa statistics
expAccuracy <- sum((obs.per.class/n)*(pred.per.class/n))
kappa <- (accuracy - expAccuracy) / (1 - expAccuracy)
round(kappa, digits = 4)
# 0.4269

###############################################################################
## COMPARE THE OBTAINED RESULTS TO THE RESULTS OF (RANDOM) BASELINE CLASSIFIERS
###############################################################################

## consider RANDOM-GUESS classifier as a baseline classifier that predicts labels randomly 
## confusion matrix for such a classifier:
rguess.cm <- (n / nc) * matrix(data = rep(obs.per.class, nc), nrow = nc, ncol = nc, byrow=F)
rguess.cm
## compute accuracy, precision, recall, F1 for this baseline classifier
rguess.accuracy <- sum(diag(rguess.cm))/sum(rguess.cm)
round(rguess.accuracy, digits = 4)
# 0.1667 - the same as 1/nc
rguess.recall <- diag(rguess.cm)/rowSums(rguess.cm)
round(rguess.recall, digits = 4)
# 0.1667 for all the classes (= 1/nc)
rguess.precison <- diag(rguess.cm)/colSums(rguess.cm)
round(rguess.precison, digits = 4)
# 0.0659 0.3640 0.0898 0.2599 0.0721 0.1484
rguess.F1 <- 2*rguess.precison*rguess.recall/(rguess.precison+rguess.recall)
round(rguess.F1, digits = 4)
# 0.0944 0.2286 0.1167 0.2031 0.1006 0.1570

rguess.metrics <- rbind(rguess.accuracy, rguess.precison, rguess.recall, rguess.F1)
colnames(rguess.metrics) <- c('A','B','C','D','E','F')
row.names(rguess.metrics) <- c('accuracy', 'precision', 'recall', 'F1')
kable(rguess.metrics, format='rst')  
  
## consider RANDOM-WEIGHTED-GUESS classifier as a baseline classifier that predicts labels 
## based on some prior knowledge about the data distribution (i.e., distribution of observations
## across the classes); for instance, distribution of observations in the training set

## we'll use here, as prior distribution, the distribution of obs. in the training set
prior.dist <- as.vector(prop.table(table(weeks.minus613$CL)))
## confusion matrix for this baseline classifier:
rwguess.cm <- n * prior.dist %*% t(prior.dist)
rwguess.cm
## compute accuracy, precision, recall, F1 for this baseline classifier
rwguess.accuracy <- sum(diag(rwguess.cm))/sum(rwguess.cm)
round(rwguess.accuracy, digits = 4)
# 0.2279
rwguess.precision <- diag(rwguess.cm)/colSums(rwguess.cm)
round(rwguess.precision, digits = 4)
# 0.1448 0.3555 0.0968 0.2128 0.1577 0.0324
## since the rwguess.cm matrix is symmetric, precions = recall = F1

rwguess.metrics <- rbind(rwguess.accuracy, rwguess.precision)
colnames(rwguess.metrics) <- c('A','B','C','D','E','F')
row.names(rwguess.metrics) <- c('accuracy', 'prf')
kable(rwguess.metrics, format='rst')  




###############################################################################
## CHECK IF CLASSIFIERS CAN BE BUILT USING THE DATA FROM THE FIRST COUPLE OF 
## WEEKS TO PREDICT STUDENTS' STRATEGY (CLASS ASSIGNMENT) IN THE LATER WEEKS OF
## THE COURSE; EACH WEEK INCREASE THE TRAINING SET USING THAT WEEK'S DATA, AND 
## TEST THE MODEL USING THE DATA FROM THE FOLLOWING WEEK
## START BY BUILDING A MODEL USING THE DATA FROM WEEKS 2-5, AND TEST IT ON THE 
## DATA FROM WEEK 7; THEN BUILD ANOTHER CLASSIFIER USING DATA FROM WEEKS
## 2-5 & 7 AND TEST IT ON THE DATA FROM WEEK 8; AND SO ON
## - WEEK 6 IS SKIPPED AS IT IS DIFFERENT FROM OTHER 'REGULAR' WEEKS
## - WEEKS 11 AND 12 ARE SKIPPED SINCE THESE ARE THE ONLY WEEKS WHERE CLUSTER F
##   APPEARS; SO, IT IS NOT POSSIBLE TO CREATE A CLASSIFIER THAT WOULD BE ABLE 
##   TO RECOGNIZE CLASS F USING THE DATA FROM THE FIRST COUPLE OF WEEKS  
###############################################################################

##################################################
## BUILDING A MODEL USING THE DATA FROM WEEKS 2-5, 
## AND TEST IT ON THE DATA FROM WEEK 7
##################################################
weeks2to5 <- subset(weeks.minus613, WEEK %in% c(2:5))
weeks2to5$CL <- factor(weeks2to5$CL)
## check the distribution of classes
round(prop.table(table(weeks2to5$CL)), digits = 3)
week7 <- subset(weeks.minus613, WEEK == 7)
week7$CL <- factor(week7$CL, levels = c("A", "B", "C", "D", "E"))
round(prop.table(table(week7$CL)), digits = 3)

tr.control <- trainControl(method ="repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           search = "grid",
                           sampling = "up") 
tune.grid <- expand.grid(.mtry=c(4:8))
# tr.control$sampling <- 'smote' - gave poorer results than up-sampling
set.seed(seed)
mod.w25 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                        VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                      data = weeks2to5, 
                      method = "rf", 
                      ntree = 2000,
                      tuneGrid = tune.grid, 
                      trControl = tr.control)

print(mod.w25)
plot(mod.w25)
# best value for mtry: 4
## make predictions on the data for week 7 and compute the performance
mod25.pred <- predict(mod.w25, newdata=week7)
confusionMatrix(mod25.pred, week7$CL)

###########################################################
## BUILDING A MODEL USING THE DATA FROM WEEKS 2-5 & WEEK 7, 
## AND TEST IT ON THE DATA FROM WEEK 8
###########################################################
weeks2to7 <- subset(weeks.minus613, WEEK %in% c(2:5,7))
weeks2to7$CL <- factor(weeks2to7$CL)
## check the distribution of classes
round(prop.table(table(weeks2to7$CL)), digits = 3)
week8 <- subset(weeks.minus613, WEEK == 8)
week8$CL <- factor(week8$CL, levels = c("A", "B", "C", "D", "E"))
round(prop.table(table(week8$CL)), digits = 3)

set.seed(seed)
mod.w27 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                   VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                   data = weeks2to7, 
                   method = "rf", 
                   ntree = 2000,
                   tuneGrid = tune.grid, 
                   trControl = tr.control)

print(mod.w27)
plot(mod.w27)
# best value for mtry: 6
## make predictions on the data for week 8 and compute the performance
mod27.pred <- predict(mod.w27, newdata=week8)
confusionMatrix(mod27.pred, week8$CL)

###########################################################
## BUILDING A MODEL USING THE DATA FROM WEEKS 2-5 & 7-8, 
## AND TEST IT ON THE DATA FROM WEEK 9
###########################################################

weeks2to8 <- subset(weeks.minus613, WEEK %in% c(2:5,7:8))
weeks2to8$CL <- factor(weeks2to8$CL)
## check the distribution of classes
round(prop.table(table(weeks2to8$CL)), digits = 3)
week9 <- subset(weeks.minus613, WEEK == 9)
week9$CL <- factor(week9$CL, levels = c("A", "B", "C", "D", "E"))
round(prop.table(table(week9$CL)), digits = 3)

set.seed(seed)
mod.w28 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                   VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                   data = weeks2to8, 
                   method = "rf", 
                   ntree = 2000,
                   tuneGrid = tune.grid, 
                   trControl = tr.control)

print(mod.w28)
plot(mod.w28)
# best value for mtry: 6
## make predictions on the data for week 9 and compute the performance
mod28.pred <- predict(mod.w28, newdata=week9)
confusionMatrix(mod28.pred, week9$CL)


###########################################################
## BUILDING A MODEL USING THE DATA FROM WEEKS 2-5 & 7-9, 
## AND TEST IT ON THE DATA FROM WEEK 10
###########################################################

weeks2to9 <- subset(weeks.minus613, WEEK %in% c(2:5,7:9))
weeks2to9$CL <- factor(weeks2to9$CL)
## check the distribution of classes
round(prop.table(table(weeks2to9$CL)), digits = 3)
week10 <- subset(weeks.minus613, WEEK == 10)
week10$CL <- factor(week10$CL, levels = c("A", "B", "C", "D", "E"))
round(prop.table(table(week10$CL)), digits = 3)

tr.control <- trainControl(method ="repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           search = "grid")
                           #sampling = "up") # try without resolving class imbalance, 
                                            # since the test set is largely unbalanced
tune.grid <- expand.grid(.mtry=c(4:8))

set.seed(seed)
mod.w29 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                   VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                   data = weeks2to9, 
                   method = "rf", 
                   ntree = 2000,
                   tuneGrid = tune.grid, 
                   trControl = tr.control)

print(mod.w29)
plot(mod.w29)
# best value for mtry: 6
## make predictions on the data for week 10 and compute the performance
mod29.pred <- predict(mod.w29, newdata=week10)
confusionMatrix(mod29.pred, week10$CL)
# horible results: Accuracy : 0.1491 ; Kappa : 0.1098
# only minutely better without sampling for class imbalance:
# Accuracy : 0.1564 ; Kappa : 0.1149


###########################################################
## BUILDING A MODEL USING THE DATA FROM WEEKS 2-5 & 7-11, 
## AND TEST IT ON THE DATA FROM WEEK 12
###########################################################

weeks2to11 <- subset(weeks.minus613, WEEK %in% c(2:5,7:11))
weeks2to11$CL <- factor(weeks2to11$CL)
## check the distribution of classes
round(prop.table(table(weeks2to11$CL)), digits = 3)
week12 <- subset(weeks.minus613, WEEK == 12)
week12$CL <- factor(week12$CL, levels = c("A", "B", "C", "D", "E", "F"))
round(prop.table(table(week12$CL)), digits = 3)

tr.control$sampling <- 'up'
set.seed(seed)
mod.w211 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                   VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                   data = weeks2to11, 
                   method = "rf", 
                   ntree = 2000,
                   tuneGrid = tune.grid, 
                   trControl = tr.control)
print(mod.w211)
plot(mod.w211)
# best value for mtry: 5
## make predictions on the data for week 12 and compute the performance
mod211.pred <- predict(mod.w211, newdata=week12)
confusionMatrix(mod211.pred, week12$CL)





#######################
## UTILITY FUNCTIONS ##
#######################

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}

normalize.weekly.features <- function(features.frame) {
  norm.features <- as.data.frame(apply(features.frame[,c(2:13)], 2, normalize.feature))
  n <- ncol(features.frame)
  norm.features <- as.data.frame(cbind(norm.features, features.frame[,c(1,14)]))
  norm.features <- norm.features[,c(13,1:12,14)]
  colnames(norm.features) <- colnames(features.frame)
  norm.features
}
