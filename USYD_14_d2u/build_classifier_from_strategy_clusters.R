#######################################################################################
## BUILD THE DATASET FOR THE CLASSIFICATION TASK:
## - MERGE THE DATA (FEATURE VALUES AND CLUSTERS) FROM ALL THE COURSE WEEKS (2-13)
## - MAP WEEKLY CLUSTER LABELS (NUMBERS) TO THE CORRESPONDING LEARNING STRATEGIES,
##   THAT IS, CLUSTER LABEL IDENTIFIED ACROSS ALL THE COURSE WEEKS: A,B1,B2,C1,C2,D,E,F
########################################################################################

## read the weekly feature sets and clusters from the data files
w2 <- read.csv(file = "results/weekly_clusters/week2.csv")
str(w2)
## add week label
w2$WEEK <- 2
## add learning strategy (class) label 
w2$CL <- factor(w2$cluster, levels = c(1:5), labels = c('A','D','E','B1','C1'))
w2$CL <- as.character(w2$CL)

w3 <- read.csv(file = "results/weekly_clusters/week3.csv")
## add week label
w3$WEEK <- 3
## add learning strategy (class) label sap
w3$CL <- factor(w3$cluster, levels = c(1:4), labels = c('A','E','D','B2'))
w3$CL <- as.character(w3$CL)

w4 <- read.csv(file = "results/weekly_clusters/week4.csv")
## add week label
w4$WEEK <- 4
## add learning strategy (class) label 
w4$CL <- factor(w4$cluster, levels = c(1:5), labels = c('E','C2','B1','A','B2'))
w4$CL <- as.character(w4$CL)

w5 <- read.csv(file = "results/weekly_clusters/week5.csv")
## add week label
w5$WEEK <- 5
## add learning strategy (class) label 
w5$CL <- factor(w5$cluster, levels = c(1:4), labels = c('D','B1','E','A'))
w5$CL <- as.character(w5$CL)

w6 <- read.csv(file = "results/weekly_clusters/week6.csv")
## add week label
w6$WEEK <- 6
## add learning strategy (class) label 
w6$CL <- factor(w6$cluster, levels = c(1:5), labels = c('C2','C1','E','D','A'))
w6$CL <- as.character(w6$CL)

w7 <- read.csv(file = "results/weekly_clusters/week7.csv")
## add week label
w7$WEEK <- 7
## add learning strategy (class) label 
w7$CL <- factor(w7$cluster, levels = c(1:4), labels = c('E','B1','D','C1'))
w7$CL <- as.character(w7$CL)

w8 <- read.csv(file = "results/weekly_clusters/week8.csv")
## add week label
w8$WEEK <- 8
## add learning strategy (class) label 
w8$CL <- factor(w8$cluster, levels = c(1:5), labels = c('B1','E','B2','A','D'))
w8$CL <- as.character(w8$CL)

w9 <- read.csv(file = "results/weekly_clusters/week9.csv")
## add week label
w9$WEEK <- 9
## add learning strategy (class) label 
w9$CL <- factor(w9$cluster, levels = c(1:4), labels = c('D','A','E','B1'))
w9$CL <- as.character(w9$CL)

w10 <- read.csv(file = "results/weekly_clusters/week10.csv")
## add week label
w10$WEEK <- 10
## add learning strategy (class) label 
w10$CL <- factor(w10$cluster, levels = c(1:5), labels = c('B1','A','D','B2','E'))
w10$CL <- as.character(w10$CL)

w11 <- read.csv(file = "results/weekly_clusters/week11.csv")
## add week label
w11$WEEK <- 11
## add learning strategy (class) label 
w11$CL <- factor(w11$cluster, levels = c(1:4), labels = c('D','B1','F','E'))
w11$CL <- as.character(w11$CL)

w12 <- read.csv(file = "results/weekly_clusters/week12.csv")
## add week label
w12$WEEK <- 12
## add learning strategy (class) label 
w12$CL <- factor(w12$cluster, levels = c(1:4), labels = c('E','F','B1','B2'))
w12$CL <- as.character(w12$CL)

w13 <- read.csv(file = "results/weekly_clusters/week13.csv")
## add week label
w13$WEEK <- 13
## add learning strategy (class) label 
w13$CL <- factor(w13$cluster, levels = c(1:5), labels = c('A','E','D','C1','C2'))
w13$CL <- as.character(w13$CL)

## build a merged dataset
all.weeks <- rbind(w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13)
str(all.weeks)
## remove exam scores (these were not used as clustering features) and the weekly cluster label
all.weeks <- all.weeks[,-c(14:16)]
## turn the class (CL) into a factor variable
all.weeks$CL <- factor(all.weeks$CL)
## store the data set
write.csv(all.weeks, file = "Intermediate_files/strategy_classification_dataset.csv", 
          quote = F, row.names = F)


## create normalized feature set, by normalizing features at weekly level
weeks <- list(w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13)
norm.weeks.df <- data.frame()
for(i in 1:length(weeks)) {
  norm.week <- normalize.weekly.features(weeks[[i]])
  norm.weeks.df <- rbind(norm.weeks.df, norm.week)
}
str(norm.weeks.df)
## remove exam scores (these were not used as clustering features) and the weekly cluster label
norm.weeks.df <- norm.weeks.df[,-c(14:16)]
## turn the class (CL) into a factor variable
norm.weeks.df$CL <- factor(norm.weeks.df$CL)
## store the data set
write.csv(norm.weeks.df, file = "Intermediate_files/normalized_strategy_classification_dataset.csv", 
          quote = F, row.names = F)


###########################################################################
## ADDITIONAL DATA PREPARATION:
## - REMOVING INSTANCES WITH MANY MISSING VALUES
## - SUBSTITUTING NAs for ORG.VIEW and DBOARD.VIEW WITH APPROPRIATE VALUES 
## - MERGING CLASSES B1 & B2, AND C1 & C2; RESULTING IN 6 CLASSES
###########################################################################

## load the 'regular' (not-normalized) feature set
#all.weeks <- read.csv(file = "Intermediate_files/strategy_classification_dataset.csv")

## load the normalized feature set
all.weeks <- read.csv(file = "Intermediate_files/normalized_strategy_classification_dataset.csv")

# R's Random Forest algorithm cannot work with missing values
# so, before applying the algorithm, the missing values have to be handled
summary(all.weeks)
## remove observations where almost all features have the NA value
which(is.na(all.weeks$EQT.TOT))
which(is.na(all.weeks$EXC.TOT))
all.nas <- which(is.na(all.weeks$VEQ.TOT))
## there are 5 such observations
all.weeks <- all.weeks[-all.nas,]
## check again
summary(all.weeks)
## for ORG.VIEW and DBOARD.VIEW, replace NAs with 0 (zero)
## since NA means that there were no events of this type
org.view.nas <- which(is.na(all.weeks$ORG.VIEW))
all.weeks$ORG.VIEW[org.view.nas] <- 0
dboad.view.nas <- which(is.na(all.weeks$DBOARD.VIEW))
all.weeks$DBOARD.VIEW[dboad.view.nas] <- 0
## check again
summary(all.weeks)
which(complete.cases(all.weeks)==F)
## now, it's fine - no more observations with NA values

## MERGE CLASSES B1 & B2, AND C1 & C2
all.weeks$CL <- as.character(all.weeks$CL)
all.weeks$CL[all.weeks$CL %in% c('B1','B2')] <- 'B'
all.weeks$CL[all.weeks$CL %in% c('C1','C2')] <- 'C'
all.weeks$CL <- factor(all.weeks$CL)
levels(all.weeks$CL)

## check how classes are distributed across the dataset
table(all.weeks$CL)
#     A   B   C   D   E   F 
#    551 999 544 685 514  91
round(prop.table(table(all.weeks$CL)), digits = 3)
#     A     B     C     D     E     F 
#   0.163 0.295 0.161 0.202 0.152 0.027  

## create a dataset without weeks 6 and 13
weeks.minus613 <- subset(all.weeks, WEEK %in% c(2:5,7:12))
unique(weeks.minus613$WEEK)
table(weeks.minus613$CL)
#  A   B   C   D   E   F 
# 407 999 272 598 443  91

round(prop.table(table(weeks.minus613$CL)), digits = 3)
#   A     B     C     D     E     F 
# 0.145 0.356 0.097 0.213 0.158 0.032 

#############################################
## CREATE TRAINING AND VALIDATION DATASETS ##
#############################################
# install.packages('caret')
library(caret)

seed <- 2907

## this approach to splitting the dataset allows for
## preserving the same/similar distribution of classes
## in the two datasets
set.seed(seed)
train.index <- createDataPartition(all.weeks$CL, p = .75,
                                   list = FALSE)
train <- all.weeks[train.index,]
test <- all.weeks[-train.index,]

## make the same kind of partitions for the dataset without data for weeks 6 and 13
set.seed(seed)
train.min613.index <- createDataPartition(weeks.minus613$CL, p = 0.75, list=F)
train.min613 <- weeks.minus613[train.min613.index,]
test.min613 <- weeks.minus613[-train.min613.index,]

################################################
## DO THE CLASSIFICATION USING RANDOM FORESTS ##
################################################
library(randomForest)

# since the random forest algorithm relies on random processes (for bagging and 
# selection of variables to consider at each split), set the seed for those process
set.seed(seed)

?randomForest
rf.model <- randomForest(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                              VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN, 
                              data = train, importance = TRUE, ntree=5000, mtry=5)
rf.model
## class.error given in the confusion matrix (last column) is (1-class.recall)

## performance on the training set
class.names <- levels(all.weeks$CL)
compute.perf.metrics(rf.model$confusion[,1:6], class.names)

# compute the performance on the validation set
rf.predict <- predict(object = rf.model, newdata = test )
# create confusion matrix
rf.cm <- as.matrix( table( true = test$CL, predicted = rf.predict) )
rf.cm
compute.perf.metrics(rf.cm, class.names)
# $accuracy
# 0.6524823

# the importance parameter was set to true to be able to inspect what features were important
# for building the model; so, let's inspect them
importance( rf.model )
# the table is not that easy to follow, better plot the variables
varImpPlot( rf.model )


## build a new model with only those predictor variables that proved relevant
## in the initial model
rf.m2 <- randomForest(CL ~ EQT.TOT + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                           VID.TOT + DBOARD.VIEW + GDEN, 
                           data = train, importance = TRUE, ntree=5000, mtry=3)
rf.m2
## performance on the training set
compute.perf.metrics(rf.m2$confusion[,1:6], class.names)
# compute the performance on the validation set
rf2.predict <- predict(object = rf.m2, newdata = test )
# create confusion matrix
rf2.cm <- as.matrix( table( true = test$CL, predicted = rf2.predict) )
rf2.cm
compute.perf.metrics(rf2.cm, class.names)
# $accuracy
# 0.641844
## the performance is somewhat lower than with the original feature set


# by setting the mtry parameter to the number of predictor variables, 
# the random forest model turns into a BAGGING model
bg.model <- randomForest(CL ~ EQT.TOT + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                           VID.TOT + DBOARD.VIEW + GDEN, 
                         data = train, importance = TRUE, ntree=5000, mtry=7)
bg.model
## performance on the training set
compute.perf.metrics(bg.model$confusion[,1:6], class.names)
# compute the performance on the validation set
bg.predict <- predict(object = bg.model, newdata = test )
# create confusion matrix
bg.cm <- as.matrix( table( true = test$CL, predicted = bg.predict) )
bg.cm
compute.perf.metrics(bg.cm, class.names)
# $accuracy
# 0.6300236
## overall, this (bagging) model has somewhat worse performance than rf


############################################################################
## USE the CARET PACKAGE TO TUNE THE RF PARAMETERS AND SELECT THE BEST MODEL
############################################################################
library(caret)
tr.control <- trainControl(method ="repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           search = "grid",
                           sampling = "smote") # set the subsampling technique
                                              # to deal with the class imbalance
tune.grid <- expand.grid(.mtry=c(2:7))

set.seed(seed)
rf.smote <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                       VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN, 
                       data=train, 
                       method="rf", 
                       ntree=2000,
                       #preProc = c("center","scale"),  
                       tuneGrid=tune.grid, 
                       trControl=tr.control)
print(rf.smote)
plot(rf.smote)
# best value for mtry: 6 for 'regular' data, 5 for 'normalized' data
## make predictions on the validation set and compute the performance
rf.pred <- predict(rf.smote, newdata=test)
confusionMatrix(rf.pred, test$CL)

## change the subsampling technique to ROSE
# tr.control$sampling <- "rose"
## for some reason the ROSE method did not work
## keep the SMOTE technique, but add WEEK as a feature
set.seed(seed)
rf.smote2 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                        VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                        data=train, 
                        method="rf", 
                        ntree=2000,
                        tuneGrid=tune.grid, 
                        trControl=tr.control)

print(rf.smote2)
plot(rf.smote2)
# best value for mtry=7
## make predictions on the validation set and compute the performance
rf.pred2 <- predict(rf.smote2, newdata=test)
confusionMatrix(rf.pred2, test$CL)
## the inclusion of WEEK significantly improved the results

## use all 13 features on the dataset without data for weeks 6 and 13 
set.seed(seed)
rf.smote.min613 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                          VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                          data=train.min613, 
                          method="rf", 
                          ntree=2000,
                         # preProc = c("center","scale"),  
                          tuneGrid=tune.grid, 
                          trControl=tr.control)
print(rf.smote.min613)
plot(rf.smote.min613)
# best value for mtry=6
## make predictions on the validation set and compute the performance
rf.pred3 <- predict(rf.smote.min613, newdata=test.min613)
confusionMatrix(rf.pred3, test.min613$CL)

## change the subsampling strategy to up-sampling
tr.control$sampling <- "up"
set.seed(seed)
rf.up.min613 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                           VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                         data=train.min613, 
                         method="rf", 
                         ntree=2000,
                         # preProc = c("center","scale"),  
                         tuneGrid=tune.grid, 
                         trControl=tr.control)
print(rf.up.min613)
plot(rf.up.min613)
# best value for mtry: 7 for 'regular', 5 for 'normalized' data
## make predictions on the validation set and compute the performance
rf.pred4 <- predict(rf.up.min613, newdata=test.min613)
confusionMatrix(rf.pred4, test.min613$CL)

## store the model
saveRDS(object = rf.up.min613, file = "results/best_classifier_(trained_on_75perc_2014data).RData")

## compare the 4 RF models
rf.models <- resamples(list(model1=rf.smote, model2=rf.smote2, 
                            model3=rf.smote.min613, model4=rf.up.min613))
summary(rf.models)
dotplot(rf.models)
## use t-test to compare the models 
## (with the null hypothesis that there is no difference between models)
dif.rf.mods <- diff(rf.models)
dif.rf.mods
summary(dif.rf.mods)


## the Caret package allows only for tuning the mtry parameter (considered the most important), but not other parameters
## since the performance depends also on the number of trees (ntree), 
## in the following several models with different ntree values are built and compared
## based on: http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
## the models are based on 13 variables (12 clustering features + WEEK), and the dataset does not
## include data for weeks 6 and 13
tr.control$sampling <- "smote"
modellist <- list()
for (ntree in c(1000, 2000, 3000, 4000, 5000)) {
  set.seed(seed)
  fit <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                   VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK,  
                   data=train.min613, 
                   method="rf", 
                   tuneGrid=tune.grid, 
                   trControl=tr.control, 
                   ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare models
rf.mods.smote.min63 <- resamples(modellist)
summary(rf.mods.smote.min63)
dotplot(rf.mods.smote.min63)

## use t-test to compare the models 
## (with the null hypothesis that there is no difference between models)
dif.rf.smote.min613 <- diff(rf.mods.smote.min63)
dif.rf.smote.min613
summary(dif.rf.smote.min613)
dotplot(dif.rf.smote.min613)

## the models barely differ, though the one with ntree=2000 seems to be the best 
best.rf.smote.min613 <- modellist[[2]]
print(best.rf.smote.min613) # mtry = 

## make predictions on the validation set and compute the performance
best.rf.smote.min613.pred <- predict(best.rf.smote.min613, newdata=test.min613)
confusionMatrix(best.rf.smote.min613.pred, test.min613$CL)


##############################
## TRY WITH SVM CLASSIFIERS ##
##############################
#install.packages("e1071")
library(e1071)

##############################
## SVM with a RADIAL KERNEL ##
##############################

# the kernel function to be used is set via the 'kernel' argument
# the 'cost' argument is used for specifying the cost of a violation to the margin. 
# when cost is small, the margin is wide (lower variance, high bias)
# when cost is large, the margins is narrow (high variance, low bias)
svm.radial <- svm(CL ~ EQT.TOT + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                       VID.TOT + DBOARD.VIEW + GDEN, 
                       kernel = "radial", data = train, cost = 0.1) 

summary(svm.radial)

## the tune f. can be used to tune the parameters through cross-validation; 
## by default, it performs 10-fold cross-validation on the specified set of SVM models
## the 'ranges' argument is used to specify the range of values to be tried out for the given SVM parameters
set.seed(seed)
tune.radial <- tune(svm, 
                    CL ~ EQT.TOT + EXC.TOT + EXC.DIFF + VEQ.TOT + VID.TOT + DBOARD.VIEW + GDEN, 
                    kernel = "radial", 
                    data = train,
                    ranges = list( cost=c(0.001, 0.01, 0.1, 0.5, 1, 10, 100), 
                                   gamma=c(0.5, 1, 2, 3, 4)))

summary( tune.radial )
## the tune function stores the best model obtained:
summary( tune.radial$best.model )

## make predictions on the validation set using the best model
best.radial.pred <- predict(object = tune.radial$best.model, newdata = test)
## create the confusion matrix and compute performance metrics
cm.radial <- as.matrix(table( true = test$CL, predicted = best.radial.pred ))
cm.radial

compute.perf.metrics(cm.radial, class.names)
# $accuracy
# 0.6028369

##############################################################################
## USE CARET PACKAGE TO DO THE TUNING AND MODEL SELECTION (SVM RADIAL KERNEL)
##############################################################################
train.vars <- train[,c(2:14)] ## include the WEEK variable
train.class <- train[,15]
## setup for cross validation
svm.tr.ctrl <- trainControl(method = "repeatedcv",   
                           number = 10, # 10 fold cross validation
                           repeats = 5, # do 5 repititions of cv
                           sampling = 'smote') 

## compute weights for classes, to compensate for the fact that some classes are 
## far more represented than the others
## each observation is weighted according to the corresponding class size:
## observations of larger classes are assigned smaller weights and vice versa
## (based on: https://geekoverdose.wordpress.com/2014/07/25/svm-classification-example-with-performance-measures-using-r-caret/)
# classes.num <- as.numeric(train.class)
# weights <- classes.num
# for(val in unique(classes.num)) {
#   weights[classes.num==val] <- (1/sum(classes.num==val))*(length(classes.num)/6) # normalized to sum to length(train.class)
# } 
## the use of weights did not prove very useful; using instead the smote sampling technique 

## 1st pass
## train and tune the SVM
set.seed(seed)
svm.rad.smote <- train(x = train.vars,
                      y = train.class,
                      # weights = weights,
                      method = "svmRadial",   # Radial kernel
                      tuneLength = 10,				# pick 10 arbitrary values of the C (cost) parameter 
                      # preProc = c("center","scale"),  # Center and scale data
                      trControl=svm.tr.ctrl)

svm.rad.smote
# the final values used for the model were sigma = 0.2146917 (0.099 for the 'normalized' data) and C = 128

## In the second pass, having seen the parameter values selected in the first pass, 
## use the train()'s tuneGrid parameter to do some sensitivity analysis around the 
## values of C and sigma that produced the model with the best accuracy

## 2nd pass
## use the expand.grid to specify the search space; this function builds a dataframe 
## containing all the combinations of C and sigma we want to look at
grid <- expand.grid(sigma = c(0.05, 0.1, 0.15, 0.2),     
                    C = c(125, 128, 130, 135))  
##set seach to grid in trainControl
svm.tr.ctrl$search <- 'grid'
## train and tune the SVM
set.seed(seed)
svm.rad.smote <- train(x = train.vars,
                      y = train.class,
                      # weights = weights,
                      method = "svmRadial",
                      #preProc = c("center","scale"),
                      tuneGrid = grid,
                      trControl=svm.tr.ctrl)

svm.rad.smote
# the final values used for the model were sigma = 0.2 (0.1 for 'normalized' data) and C = 128
## use the final model to make predictions
svm.rad.pred <- predict.train(svm.rad.smote, newdata=test[,c(2:14)], type='raw')
confusionMatrix(svm.rad.pred, test$CL)

## now, do the same, but on the dataset strip of the weeks 6 nd 13 data
train.min613.vars <- train.min613[,c(2:14)]
train.min613.class <- train.min613[,15]

## 1st pass
svm.tr.ctrl$search <- 'random'
set.seed(seed)
svm.rad.smote.min613 <- train(x = train.min613.vars,
                               y = train.min613.class,
                               # weights = weights,
                               method = "svmRadial",   # Radial kernel
                               tuneLength = 10,				# pick 10 arbitrary values of the C (cost) parameter 
                               #preProc = c("center","scale"),  # Center and scale data
                               trControl=svm.tr.ctrl)

svm.rad.smote.min613

## 2nd pass
grid <- expand.grid(sigma = c(0.05, 0.075, 0.1, 0.15, 0.2),     
                    C = c(45, 47.5, 50, 52.5))  
svm.tr.ctrl$search <- 'grid'
## train and tune the SVM
set.seed(seed)
svm.rad.smote.min613 <- train(x = train.min613.vars,
                               y = train.min613.class,
                               # weights = weights,
                               method = "svmRadial",
                               #preProc = c("center","scale"),
                               tuneGrid = grid,
                               trControl=svm.tr.ctrl)

svm.rad.smote.min613
# the final values used for the model were sigma = 0.1 and C = 50.
svm.rad.pred2 <- predict.train(svm.rad.smote.min613, newdata=test.min613[,c(2:14)], type='raw')
confusionMatrix(svm.rad.pred2, test.min613$CL)


## now, try with the up-sampling strategy 
svm.tr.ctrl$sample <- 'up'
## 1st pass
svm.tr.ctrl$search <- 'random'
set.seed(seed)
svm.rad.up.min613 <- train(x = train.min613.vars,
                              y = train.min613.class,
                              # weights = weights,
                              method = "svmRadial",   # Radial kernel
                              tuneLength = 10,				# pick 10 arbitrary values of the C (cost) parameter 
                              #preProc = c("center","scale"),  # Center and scale data
                              trControl=svm.tr.ctrl)

svm.rad.up.min613
## 2nd pass
grid <- expand.grid(sigma = c(0.05, 0.065, 0.075, 0.085), 
                    C = c(45, 47.5, 50, 52.5))  
svm.tr.ctrl$search <- 'grid'
## train and tune the SVM
set.seed(seed)
svm.rad.up.min613 <- train(x = train.min613.vars,
                              y = train.min613.class,
                              # weights = weights,
                              method = "svmRadial",
                              #preProc = c("center","scale"),
                              tuneGrid = grid,
                              trControl=svm.tr.ctrl)

svm.rad.up.min613
# the final values used for the model were sigma = 0.085 and C = 50 
svm.rad.pred3 <- predict.train(svm.rad.up.min613, newdata=test.min613[,c(2:14)], type='raw')
confusionMatrix(svm.rad.pred3, test.min613$CL)

## compare the 3 RF radial kernel models
svm.rad.models <- resamples(list(svm.rad.smote, svm.rad.smote.min613, svm.rad.up.min613))
summary(svm.rad.models)
dotplot(svm.rad.models)
## compare the models with t-test
summary(diff(svm.rad.models))

##################################
## SVM with a POLYNOMIAL KERNEL ##
##################################

set.seed(seed)
tune.polynomial <- tune(svm, CL ~ EQT.TOT + EXC.TOT + EXC.DIFF + VEQ.TOT + VID.TOT + DBOARD.VIEW + GDEN,  
                        kernel = "polynomial", data = train, degree = 3, coef0=1,
                        ranges = list( cost=c(0.05, 0.1, 0.5, 1, 5, 10), 
                                       gamma=c(0.25, 0.5, 0.75, 1)))
summary( tune.polynomial )
# the best values for the parameters: cost = 0.5, gamma = 0.5 

# see the best model
summary( tune.polynomial$best.model )

# make predictions on the validation set  
polynomial.predict <- predict(tune.polynomial$best.model, test)
cm.poly <- as.matrix( table( true = test$CL, predicted = polynomial.predict ) )
cm.poly
compute.perf.metrics(cm.poly)
# $accuracy
# 0.5482283

### USE CARET PACKAGE TO DO THE TUNING AND MODEL SELECTION (SVM POLYNOMIAL KERNEL)

## method: 'svmPoly'
## Tuning Parameters: degree (Polynomial Degree), scale (Scale), C (Cost)

## train and tune the SVM
poly.grid <- expand.grid(C = c(0.25, 0.5, 0.75, 1, 2.5, 5, 7.5),
                         degree = c(2,3,4),
                         scale = c(1,2))
set.seed(seed)
svm.poly.tune <- train(x = train.vars,
                      y = train.class,
                      weights = weights,
                      method = "svmPoly",
                      preProc = c("center","scale"),
                      tuneGrid = poly.grid,
                      trControl = tr.control)

svm.poly.tune  
# the final values used for the model were degree = 2, scale = 2 and C = 0.25

## Do the 2nd pass, to do some sensitivity analysis around the parameter
## values that produced the model with the best accuracy

## 2nd pass
poly.grid2 <- expand.grid(C = c(0.05, 0.1, 0.25, 0.3),
                         degree = c(2),
                         scale = c(1,2,3))
set.seed(seed)
svm.poly.tune <- train(x = train.vars,
                       y = train.class,
                       weights = weights,
                       method = "svmPoly",
                       preProc = c("center","scale"),
                       tuneGrid = poly.grid2,
                       trControl = tr.control)

svm.poly.tune
# final values used for the model were degree = 2, scale = 1 and C = 0.1. 

## make prediction on the validation set
svm.poly.predict <- predict.train(svm.poly.tune, newdata=test[,c(2:13)], type='raw')
confusionMatrix(svm.poly.predict, test$CL)

##############################
## SVM with a LINEAR KERNEL ##
##############################

set.seed(seed)
tune.linear <- tune(svm, CL ~ EQT.TOT + EXC.TOT + EXC.DIFF + VEQ.TOT + VID.TOT + DBOARD.VIEW + GDEN,  
                        kernel = "linear", data = train, 
                        ranges = list( cost=c(0.05, 0.1, 0.5, 1, 5, 10)))
summary( tune.linear )

# make predictions on the validation set  
linear.predict <- predict(tune.linear$best.model, test)
cm.lin <- as.matrix( table( true = test$CL, predicted = linear.predict ) )
cm.lin
compute.perf.metrics(cm.lin)
# $accuracy
# 0.5226378

### USE CARET PACKAGE TO DO THE TUNING AND MODEL SELECTION (SVM LINEAR KERNEL)

## train and tune the SVM
grid <- expand.grid(C = c(0.05, 0.1, 0.25, 0.5, 0.75, 1, 2.5, 5))
set.seed(seed)
svm.lin.tune <- train(x = train.vars,
                      y = train.class,
                      weights = weights,
                      method = "svmLinear",
                      preProc = c("center","scale"),
                      tuneGrid = grid,
                      trControl=tr.control)
svm.lin.tune
# final value used for the model was C = 2.5
## makre prediction on the validation set
svm.lin.predict <- predict.train(svm.lin.tune, newdata=test[,c(2:13)], type='raw')
confusionMatrix(svm.lin.predict, test$CL)


#################################################
## COMPARE SVM MODELS (WITH DIFFERENT KERNELS) ##
#################################################

svm.models <- resamples(list(svm.lin.tune, svm.poly.tune, svm.radial.tune))
summary(svm.models)
## present the comparison on a dotplot
dotplot(svm.models)
## box-whisker plot
trellis.par.set(caretTheme())
bwplot(svm.models)

## since models are fit on the same versions of the training data, 
## the differences between them can be computed and then compared using a
## t-test (with the null hypothesis that there is no difference between models)
dif.svm.models <- diff(svm.models)
dif.svm.models
summary(dif.svm.models)
dotplot(dif.svm.models)


###################################
## TRY BUILDING A GBM CLASSIFIER ##
## GBM - Gradient Boosted Model  ##
###################################
require(caret)
gbm.tr.control <- trainControl(method = "repeatedcv", 
                               repeats = 5,
                               sampling = 'smote')
gbm.grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                        n.trees = seq(500, 1000, by = 100),
                        shrinkage = c(0.01, 0.1),
                        n.minobsinnode = 20)

set.seed(seed)
gbm.smote <- train(x = train.vars,
                  y = train.class,
                  method = "gbm",
                  trControl = gbm.tr.control,
                 # preProc = c("center","scale"),
                  tuneGrid = gbm.grid,
                  verbose = FALSE)

gbm.smote
# final values used for the model were:n.trees = 900 (1000 for 'normalized' data), interaction.depth = 5,
#  shrinkage = 0.01, and n.minobsinnode = 20
## examine the relationship between the estimates of performance and the tuning parameters
trellis.par.set(caretTheme())
plot(gbm.smote, metric = "Kappa")
## make prediction on the validation set
gbm.predict <- predict.train(gbm.smote, newdata=test[,c(2:14)], type='raw')
confusionMatrix(gbm.predict, test$CL)

## create a model with dataset stripped of the data for weeks 6 and 13
set.seed(seed)
gbm.smote.min613 <- train(x = train.min613.vars,
                           y = train.min613.class,
                           method = "gbm",
                           trControl = gbm.tr.control,
                           # preProc = c("center","scale"),
                           tuneGrid = gbm.grid,
                           verbose = FALSE)

gbm.smote.min613
# final values used for the model were: n.trees = 1000, interaction.depth = 5, shrinkage = 0.01
# and n.minobsinnode = 20
## examine the relationship between the estimates of performance and the tuning parameters
plot(gbm.smote.min613, metric = "Kappa")
## make prediction on the validation set
gbm.predict2 <- predict.train(gbm.smote.min613, newdata=test.min613[,c(2:14)], type='raw')
confusionMatrix(gbm.predict2, test.min613$CL)

## create a model with dataset stripped of the data for weeks 6 and 13
## use the UP-sampling method (instead of SMOTE)
gbm.tr.control$sampling <- 'up'
## since R crashed couple of times when trying to build this model,
## reduce the search space (for optimal parameter values)
gbm.grid <- expand.grid(interaction.depth = 5,
                        n.trees = c(900, 1000),
                        shrinkage = 0.01,
                        n.minobsinnode = c(15,20,25))
set.seed(seed)
gbm.up.min613 <- train(x = train.min613.vars,
                          y = train.min613.class,
                          method = "gbm",
                          trControl = gbm.tr.control,
                          # preProc = c("center","scale"),
                          tuneGrid = gbm.grid,
                          verbose = FALSE)
gbm.up.min613
# final values used for the model were: n.trees = 1000, interaction.depth = 5, shrinkage = 0.01
# and n.minobsinnode = 15

## examine the relationship between the estimates of performance and the tuning parameters
trellis.par.set(caretTheme())
plot(gbm.up.min613, metric = "Kappa")
## make prediction on the validation set
gbm.predict3 <- predict.train(gbm.up.min613, newdata=test.min613[,c(2:14)], type='raw')
confusionMatrix(gbm.predict3, test.min613$CL)

## COMPARE THE 3 GBM MODELS
gbm.models <- resamples(list(gbm.smote, gbm.smote.min613, gbm.up.min613))
summary(gbm.models)
## present the comparison on a dotplot
dotplot(gbm.models)
## box-whisker plot
trellis.par.set(caretTheme())
bwplot(gbm.models)
## run t-tests (with the null hypothesis that there is no difference between models)
dif.gbm.models <- diff(gbm.models)
summary(dif.gbm.models)
dotplot(dif.gbm.models)


####################################################
## COMPARE THE BEST GBM MODEL WITH THE BEST RF MODEL 
## (the best two so far)
####################################################

## the best RF model:
tr.control <- trainControl(method ="repeatedcv", 
                           number = 10, repeats = 5, 
                           sampling = "up")
tune.grid <- expand.grid(.mtry=5)
set.seed(seed)
rf.up.min613 <- train(CL ~ EQT.TOT + EQT.DIFF + EQT.SH + EXC.TOT + EXC.DIFF + VEQ.TOT +    
                        VEQ.DIFF + VEQ.SH + VID.TOT + ORG.VIEW + DBOARD.VIEW + GDEN + WEEK, 
                        data=train.min613, 
                        method="rf", 
                        ntree=2000,
                        tuneGrid=tune.grid, 
                        trControl=tr.control)
rf.up.min613
 
## compare the RF and GBM models
best.models <- resamples(list(GBM=gbm.up.min613, RF=rf.up.min613))
summary(best.models)
## present the comparison on a dotplot
dotplot(best.models)
## box-whisker plot
bwplot(best.models)
## comapre the models using a statistical test (t-test)
dif.best.models <- diff(best.models)
summary(dif.best.models)

###################################
## TRY BUILDING A KNN CLASSIFIER ##
###################################


## PROBLEM: how to choose candidate values for k to avoid ties?


#################################################################
## TRY BUILDING A CLASSIFIER USING MULTIPLE LOGISTIC REGRESSION #
#################################################################

## check for multicolinearity

## first check if the variables are normally distributed
apply(all.weeks[,c(2:13)], 2, shapiro.test)
## no, none is normaly distributed
cor(all.weeks[,c(2:13)], method = 'spearman')
## since there is some variables are highly correlated (e.g.,EQT.TOT and EQT.SH, 
## VEQ.TOT and VEQ.SH, VEQ.TOT and VID.TOT), reduce the variable set to those that RF
## algorithm recognized as the most important
var.set <- c('EQT.TOT', 'EXC.TOT', 'EXC.DIFF', 'VEQ.TOT', 'VID.TOT', 'DBOARD.VIEW', 'GDEN')
cor(all.weeks[,var.set], method = 'spearman')

## check for outliers

boxplot(all.weeks$EQT.TOT, boxwex=0.5)
## detect outliers more precisely 
eqt.tot.outliers <- boxplot.stats(all.weeks$EQT.TOT)$out
# EQT.TOT: 425 outliers!
boxplot(all.weeks$EXC.TOT, boxwex=0.5)
exc.tot.outliers <- boxplot.stats(all.weeks$EXC.TOT)$out
# EXC.TOT: 194 outliers
## check if these outliers come from the same observations
length(intersect(eqt.tot.outliers, exc.tot.outliers))
# only 26 in common (EQT.TOT and EXC.TOT)!
boxplot(all.weeks$GDEN, boxwex=0.5)
# GDEN: no outliers
boxplot(all.weeks$VEQ.TOT, boxwex=0.5)
length(boxplot.stats(all.weeks$VEQ.TOT)$out)
# VEQ.TOT: 216 outliers!
boxplot(all.weeks$VID.TOT, boxwex=0.5)
length(boxplot.stats(all.weeks$VID.TOT)$out)
# VID.TOT: 418 outliers!

## use the outliers package for outlier detection
install.packages('outliers')
library(outliers)

outlier(all.weeks[, var.set])
## not very useful as it gives the same values as the max variable
outlier(all.weeks[, var.set], opposite = T)
## again, not much useful, as now I'm getting min values of the variables

## the scores f. looks more useful
## "mad" gives differences between each value and median, divided by median absolute deviation
## observations beyond the 95th percentile based on their mad score might be considered outliers
eqt.outliers <- which(scores(all.weeks$EQT.TOT, type="mad", prob=0.95)==T)  
length(eqt.outliers) # 1476

## NOT POSSIBLE TO APPLY MULTINOMIAL LOG. REGRESSION AS ASSUMPTIONS ARE NOT MET

#######################
## UTILITY FUNCTIONS ##
#######################

## f. that computes the overall accuracy and precision and recall for each class
compute.perf.metrics <- function(conf.matrix, classes) {
  acc <- sum(diag(conf.matrix)) / sum( conf.matrix )
  results <- matrix(nrow = length(classes), ncol = 3, byrow = T,
                    dimnames = list(classes,
                                    c('precision', 'recall', 'error')))
  for(i in 1:length(classes)) {
    results[i,1] <- conf.matrix[i,i]/sum(conf.matrix[,i])
    results[i,2] <- conf.matrix[i,i]/sum(conf.matrix[i,])
    results[i,3] <- 1-results[i,2]
  }
  list(accuracy=acc, class.level.perf=results)
}

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}

normalize.weekly.features <- function(features.frame) {
  norm.features <- as.data.frame(apply(features.frame[,c(2:13)], 2, normalize.feature))
  n <- ncol(features.frame)
  norm.features <- as.data.frame(cbind(norm.features, features.frame[,c(1,14:n)]))
  norm.features <- norm.features[,c(13,1:12,14:n)]
  colnames(norm.features) <- colnames(features.frame)
  norm.features
}
