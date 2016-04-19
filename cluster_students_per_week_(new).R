
## import functions that are to be used for clustering
source("clustering_related_functions.R")

######################################################
## COMPUTE CLUSTERS FOR FIRST SIX WEEKS (TILL MIDTERM)
## USING THE NEW FEATURE SET
######################################################

## load the trace data required for computing transition graphs, i.e., GDEN feature
trace.data <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv")
trace.data <- trace.data[trace.data$WEEK %in% c(2:13),]

set.seed(1204)

## WEEK 2
f.w2 <- create.new.feature.set(course_week = 2,
                           event_counts_file = "up_to_w02.csv", 
                           scores_file = "data2u_sem2_14_student_all_variables.csv",
                           score_columns = c("SC_MT_TOT", "SC_FE_TOT"),
                           trace.data)
str(f.w2)
hc.w2 <- do.hclustering(feature.set = f.w2[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w2, k = 5)

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(3:8, function(ncl) table(cutree(hc.w2, k = ncl)))
names(clusters) <- c(3:8)
clusters

stats.w2 <- summary.stats(feature.set = f.w2[,c(2:15)], clusters = cutree(hc.w2, k = 5), 5)
# this is for generating nice table layout
require('knitr')
kable(x = stats.w2, format = "rst")

# add clusters to the feature set
f.w2 <- as.data.frame( cbind(f.w2, cluster = cutree(hc.w2, k=5)))
str(f.w2)

## WEEK 3
f.w3 <- create.new.feature.set(course_week = 3,
                        event_counts_file = "up_to_w03.csv", 
                        scores_file = "data2u_sem2_14_student_all_variables.csv",
                        score_column = c("SC_MT_TOT", "SC_FE_TOT"),
                        trace.data)
str(f.w3)

## there are more students in this week than in week 2
setdiff(f.w2$user_id, f.w3$user_id)
setdiff(f.w3$user_id, f.w2$user_id)

hc.w3 <- do.hclustering(feature.set = f.w3[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w3, k = 4)

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(3:8, function(ncl) table(cutree(hc.w3, k = ncl)))
names(clusters) <- c(3:8)
clusters

stats.w3 <- summary.stats(feature.set = f.w3[,c(2:15)], clusters = cutree(hc.w3, k = 4), 4)
kable(x = stats.w3, format = "rst")

# add clusters to the feature set
f.w3 <- as.data.frame( cbind(f.w3, cluster = cutree(hc.w3, k=4)))

## WEEK 4
f.w4 <- create.new.feature.set(course_week = 4,
                           event_counts_file = "up_to_w04.csv", 
                           scores_file = "data2u_sem2_14_student_all_variables.csv",
                           score_column = c("SC_MT_TOT", "SC_FE_TOT"),
                           trace.data)
str(f.w4)

## check if these are the same students as in week 3
setdiff(f.w3$user_id, f.w4$user_id)
## in fact, not; these user_ids (from week 3) are not present in week 4: 175 235 260 371 
setdiff(f.w4$user_id, f.w3$user_id)
## these user_ids (from week 4) are not present in week 3: 39 118 396 403
setdiff(f.w2$user_id, f.w4$user_id)
## these user_ids (from week 2) are not present in week 4: 169 235 260 371

hc.w4 <- do.hclustering(feature.set = f.w4[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w4, k = 5)

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(3:8, function(ncl) table(cutree(hc.w4, k = ncl)))
names(clusters) <- c(3:8)
clusters

stats.w4 <- summary.stats(feature.set = f.w4[,c(2:15)], clusters = cutree(hc.w4, k = 5), 5)
kable(x = stats.w4, format = "rst")

# add clusters to the feature set
f.w4 <- as.data.frame( cbind(f.w4, cluster = cutree(hc.w4, k=5)))

## WEEK 5
f.w5 <- create.new.feature.set(course_week = 5,
                           event_counts_file = "up_to_w05.csv", 
                           scores_file = "data2u_sem2_14_student_all_variables.csv",
                           score_column = c("SC_MT_TOT", "SC_FE_TOT"),
                           trace.data)
str(f.w5)

## check if these are the same students as in week 4
setdiff(f.w4$user_id, f.w5$user_id)
## not the same; these user_ids from week 4 are missing in week 5: 39 158 159 268
setdiff(f.w5$user_id, f.w4$user_id)
## these user_ids (from week 5) are not present in week 4: 169 235 260 371

hc.w5 <- do.hclustering(feature.set = f.w5[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w5, k = 4)

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(3:8, function(ncl) table(cutree(hc.w5, k = ncl)))
names(clusters) <- c(3:8)
clusters

stats.w5 <- summary.stats(feature.set = f.w5[,c(2:15)], clusters = cutree(hc.w5, k = 4), 4)
kable(x = stats.w5, format = "rst")

# add clusters to the feature set
f.w5 <- as.data.frame( cbind(f.w5, cluster = cutree(hc.w5, k=4)))

## WEEK 6 (the last week before midterm)
f.w6 <- create.new.feature.set(course_week = 6,
                           event_counts_file = "up_to_w06.csv", 
                           scores_file = "data2u_sem2_14_student_all_variables.csv",
                           score_column = c("SC_MT_TOT", "SC_FE_TOT"),
                           trace.data)
str(f.w6)
hc.w6 <- do.hclustering(feature.set = f.w6[, c(2:13)], hc.method = "ward.D")
plot(hc.w6, cex=0.69, main = "Student clustering")
rect.hclust(hc.w6, k = 5)

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(3:8, function(ncl) table(cutree(hc.w6, k = ncl)))
names(clusters) <- c(3:8)
clusters

stats.w6 <- summary.stats(feature.set = f.w6[,c(2:15)], clusters = cutree(hc.w6, k = 5), 5)
kable(x = stats.w6, format = "rst")

# add clusters to the feature set
f.w6 <- as.data.frame( cbind(f.w6, cluster = cutree(hc.w6, k=5)))


#################################################################
## COMPUTE CLUSTERS FOR THE WEEKS AFTER MIDTERM EXAM (WEEKS 7-13)
#################################################################

# WEEK 7
f.w7 <- create.new.feature.set(course_week = 7,
                           event_counts_file = "up_to_w07.csv", 
                           scores_file = "data2u_sem2_14_student_all_variables.csv",
                           score_columns = c("SC_FE_TOT", "SC_MT_TOT"),
                           trace.data)
str(f.w7)
hc.w7 <- do.hclustering(feature.set = f.w7[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w7, k = 4)

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(3:7, function(ncl) table(cutree(hc.w7, k = ncl)))
names(clusters) <- c(3:7)
clusters

stats.w7 <- summary.stats(feature.set = f.w7[,c(2:15)], clusters = cutree(hc.w7, k = 4), 4)
kable(x = stats.w7, format = "rst")

# add clusters to the feature set
f.w7 <- as.data.frame( cbind(f.w7, cluster = cutree(hc.w7, k=4)))
str(f.w7)

## WEEK 8 
f.w8 <- create.new.feature.set(course_week = 8,
                            event_counts_file = "up_to_w08.csv", 
                            scores_file = "data2u_sem2_14_student_all_variables.csv",
                            score_columns = c("SC_FE_TOT", "SC_MT_TOT"),
                            trace.data)
str(f.w8)
hc.w8 <- do.hclustering(feature.set = f.w8[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w8, k = 5)

# Let's see what happens when we vary the number of clusters  
clusters <- sapply(3:7, function(ncl) table(cutree(hc.w8, k = ncl)))
names(clusters) <- c(3:7)
clusters

stats.w8 <- summary.stats(feature.set = f.w8[,c(2:15)], clusters = cutree(hc.w8, k = 5), 5)
kable(x = stats.w8, format = "rst")

# add clusters to the feature set
f.w8 <- as.data.frame( cbind(f.w8, cluster = cutree(hc.w8, k=5)))
str(f.w8)

## WEEK 9
f.w9 <- create.new.feature.set(course_week = 9,
                            event_counts_file = "up_to_w09.csv", 
                            scores_file = "data2u_sem2_14_student_all_variables.csv",
                            score_columns = c("SC_MT_TOT", "SC_FE_TOT"),
                            trace.data)

hc.w9 <- do.hclustering(feature.set = f.w9[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w9, k = 4)

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(4:8, function(ncl) table(cutree(hc.w9, k = ncl)))
names(clusters) <- c(4:8)
clusters

stats.w9 <- summary.stats(feature.set = f.w9[,c(2:15)], clusters = cutree(hc.w9, k = 4), 4)
kable(x = stats.w9, format = "rst")

# add clusters to the feature set
f.w9 <- as.data.frame( cbind(f.w9, cluster = cutree(hc.w9, k=4)))
str(f.w9)

## WEEK 10
f.w10 <- create.new.feature.set(course_week = 10,
                           event_counts_file = "up_to_w10.csv", 
                           scores_file = "data2u_sem2_14_student_all_variables.csv",
                           score_columns = c("SC_FE_TOT", "SC_MT_TOT"),
                           trace.data)

hc.w10 <- do.hclustering(feature.set = f.w10[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w10, k = 5)

# Let's see what happens when we vary the number of clusters  
clusters <- sapply(4:8, function(ncl) table(cutree(hc.w10, k = ncl)))
names(clusters) <- c(4:8)
clusters

stats.w10 <- summary.stats(feature.set = f.w10[,c(2:15)], clusters = cutree(hc.w10, k = 5), 5)
kable(x = stats.w10, format = "rst")

# add clusters to the feature set
f.w10 <- as.data.frame( cbind(f.w10, cluster = cutree(hc.w10, k=5)))
str(f.w10)

## WEEK 11
f.w11 <- create.new.feature.set(course_week = 11,
                            event_counts_file = "up_to_w11.csv", 
                            scores_file = "data2u_sem2_14_student_all_variables.csv",
                            score_columns = c("SC_FE_TOT", "SC_MT_TOT"),
                            trace.data)

hc.w11 <- do.hclustering(feature.set = f.w11[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w11, k = 4)

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(4:8, function(ncl) table(cutree(hc.w11, k = ncl)))
names(clusters) <- c(4:8)
clusters

stats.w11 <- summary.stats(feature.set = f.w11[,c(2:15)], clusters = cutree(hc.w11, k = 4), 4)
kable(x = stats.w11, format = "rst")

# add clusters to the feature set
f.w11 <- as.data.frame( cbind(f.w11, cluster = cutree(hc.w11, k=4)))
str(f.w11)


## WEEK 12
f.w12 <- create.new.feature.set(course_week = 12,
                            event_counts_file = "up_to_w12.csv", 
                            scores_file = "data2u_sem2_14_student_all_variables.csv",
                            score_columns = c("SC_FE_TOT", "SC_MT_TOT"),
                            trace.data)

hc.w12 <- do.hclustering(feature.set = f.w12[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w12, k = 4)

# Let's see what happens when we vary the number of clusters  
clusters <- sapply(4:8, function(ncl) table(cutree(hc.w12, k = ncl)))
names(clusters) <- c(4:8)
clusters

stats.w12 <- summary.stats(feature.set = f.w12[,c(2:15)], clusters = cutree(hc.w12, k = 4), 4)
kable(x = stats.w12, format = "rst")

# add clusters to the feature set
f.w12 <- as.data.frame( cbind(f.w12, cluster = cutree(hc.w12, k=4)))
str(f.w12)

## WEEK 13
f.w13 <- create.new.feature.set(course_week = 13,
                                event_counts_file = "up_to_w13.csv", 
                                scores_file = "data2u_sem2_14_student_all_variables.csv",
                                score_columns = c("SC_FE_TOT", "SC_MT_TOT"),
                                trace.data)

hc.w13 <- do.hclustering(feature.set = f.w13[, c(2:13)], hc.method = "ward.D")
rect.hclust(hc.w13, k = 5)

# Let's see what happens when we vary the number of clusters  
clusters <- sapply(4:8, function(ncl) table(cutree(hc.w13, k = ncl)))
names(clusters) <- c(4:8)
clusters

stats.w13 <- summary.stats(feature.set = f.w13[,c(2:15)], clusters = cutree(hc.w13, k = 5), 5)
kable(x = stats.w13, format = "rst")

# add clusters to the feature set
f.w13 <- as.data.frame( cbind(f.w13, cluster = cutree(hc.w13, k=5)))
str(f.w13)


##################################################
## TRACE STUDENTS PATHS THROUGH WEEKLY CLUSTERS ##
##################################################

trace.w23 <- merge(x = f.w2[,c(1,16)], y = f.w3[,c(1,16)], by = "user_id", all = T)
str(trace.w23)
colnames(trace.w23) <- c('user_id', 'cl.w2', 'cl.w3')

trace.w24 <- merge(x = trace.w23, y = f.w4[,c(1,16)], by = "user_id", all = T)
str(trace.w24)
colnames(trace.w24)[4] <- 'cl.w4'

trace.w25 <- merge(x = trace.w24, y = f.w5[,c(1,16)], by = 'user_id', all = T)
colnames(trace.w25)[5] <- 'cl.w5'
str(trace.w25)

trace.w26 <- merge(x = trace.w25, y = f.w6[,c(1,16)], by = 'user_id', all = T)
colnames(trace.w26)[6] <- 'cl.w6'
str(trace.w26)

trace.w27 <- merge(x = trace.w26, y = f.w7[,c(1,16)], by = "user_id", all = T)
colnames(trace.w27)[7] <- 'cl.w7'
str(trace.w27)

trace.w28 <- merge(x = trace.w27, y = f.w8[,c(1,16)], by = "user_id", all = T)
colnames(trace.w28)[8] <- 'cl.w8'
str(trace.w28)

trace.w29 <- merge(x = trace.w28, y = f.w9[, c(1,16)], by = 'user_id', all = T)
colnames(trace.w29)[9] <- 'cl.w9'
str(trace.w29)

trace.w210 <- merge(x = trace.w29, y = f.w10[,c(1,16)], by = 'user_id', all = T)
colnames(trace.w210)[10] <- 'cl.w10'
str(trace.w210)

trace.w211 <- merge(x = trace.w210, y = f.w11[,c(1,16)], by = 'user_id', all = T)
colnames(trace.w211)[11] <- 'cl.w11'
str(trace.w211)

trace.w212 <- merge(x = trace.w211, y = f.w12[,c(1,16)], by = 'user_id', all = T)
colnames(trace.w212)[12] <- 'cl.w12'
str(trace.w212)

trace.w213 <- merge(x = trace.w212, y = f.w13[,c(1,16)], by = 'user_id', all = T)
colnames(trace.w213)[13] <- 'cl.w13'
str(trace.w213)

## write the computed clusters to a file
write.csv(x = trace.w213, file = "Intermediate_files/original_weekly_clusters_w2_to_w13_(feb2016).csv", 
          row.names = F, quote = F)


##############################################################
## USE LCA TO GROUP STUDENTS BASED ON THE PATHS THEY FOLLOWED 
## THROUGHOUT THE COURSE (WEEKS 2-13)
##############################################################

clusters.w213 <- read.csv(file = "Intermediate_files/original_weekly_clusters_w2_to_w13_(feb2016).csv")  

f.fullcourse <- cbind(cl.w2, cl.w3, cl.w4, cl.w5, cl.w6, cl.w7, cl.w8, 
                      cl.w9, cl.w10, cl.w11, cl.w12, cl.w13) ~ 1

eval.metrics <- data.frame()
require(poLCA)
set.seed(3003)
for(i in 3:7) {
  lc <- poLCA(f.fullcourse, data = clusters.w213, nrep = 50, nclass = i, verbose = F, na.rm = F)
  metrics <- c(i, lc$aic, lc$bic, lc$llik, lc$Chisq)
  eval.metrics <- as.data.frame( rbind(eval.metrics, metrics) )
}
colnames(eval.metrics) <- c('nclass', 'AIC', 'BIC', 'LogLike', 'ChiSquare')
require(knitr)
kable(x = eval.metrics, format = "rst")

require(poLCA)
set.seed(3003)
## examine the model with 5 classes
# lc5.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, nrep = 50, 
#                         nclass = 5, graphs = T, na.rm = F)
# # order the latent classes based on their size ($P gives the size each latent class)
# probs.start <- poLCA.reorder(lc5.fullcourse$probs.start, order(lc5.fullcourse$P,decreasing=T))
# lc5.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, graphs = T, 
#                         nclass = 5, probs.start = probs.start, na.rm = F)

## examine the model with 4 classes
# lc4.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, nrep = 50, 
#                         nclass = 4, graphs = T, na.rm = F)
# probs.start <- poLCA.reorder(lc4.fullcourse$probs.start, order(lc4.fullcourse$P,decreasing=T))
# lc4.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, graphs = T, 
#                         nclass = 4, probs.start = probs.start, na.rm = F)

## examine the model with 6 classes
lc6.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, nrep = 50, 
                        nclass = 6, na.rm = F)
probs.start <- poLCA.reorder(lc6.fullcourse$probs.start, order(lc6.fullcourse$P,decreasing=T))
lc6.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, graphs = T, 
                        nclass = 6, probs.start = probs.start, na.rm = F)


## add the predicted class to each observation
clusters.w213.pred <- clusters.w213
# clusters.w213.pred$lca5 <- as.factor(lc5.fullcourse$predclass)
# clusters.w213.pred$lca4 <- as.factor(lc4.fullcourse$predclass)
clusters.w213.pred$lca6 <- as.factor(lc6.fullcourse$predclass)
str(clusters.w213.pred)

## write the features + the LCA class values to a file
write.csv(x = clusters.w213.pred, file = "results/lca_w2_to_w13_6classes(April2016).csv", 
          row.names = F, quote = F)


#############################################################################
## MAP THE ORIGINAL CLUSTERS INTO THE DEVISED 'COMPACT' CLUSTER CODING SCHEME 
## 
## THE IDEA IS TO TRANSFORM THE 'ORIGINAL' SET OF WEEKLY CLUSTERS OBTAINED 
## (THROUGH HIERARCH. CLUSTERING) FOR ALL THE COURSE WEEKS (2-13) INTO 
## A SMALLER SET THAT WILL BE SUBSEQUENTLY USED FOR SEQUENCE ANALYSIS AND 
## CLUSTERING OF STUDENTS' LEARNING SEQUENCES (COMPRISED OF CLUSTERS
## THEY 'BELONGED TO' THROUGHOUT THE COURSE). 
## THE 'ORIGINAL' SET OF CLUSTERS CONSISTS OF 19 DIFFERENT CLUSTERS
## THE 'COMPACT' SET COMPRISES 11 DIFFERENT CLUSTERS
##
## SEQUENCE ANALYSIS IS DONE USING TraMineR PACKAGE 
#############################################################################

orig.cl.w213 <- read.csv(file = "Intermediate_files/original_weekly_clusters_w2_to_w13_(feb2016).csv")  
n.users <- nrow(orig.cl.w213)

## cluster codes in the 'compact' coding scheme
compact.codes <- c('HA_BAL', 'HA_BAL_MC', 'HA_NEG_DIFF_MC',
                   'A_POS_DIFF', 'A_NEG_DIFF',
                   'MA_ALL_ACT_BAL', 'MA_MANY_ACT_BAL_MC',
                   'SAS_ONLY_BAL', 'SAS_ONLY_NEG_DIFF',
                   'SAS_MOSTLY_NEG-DIFF',
                   'DISENGAGED')

## do the mapping for week 2
new.cl.w2 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w2[i]
  if ( is.na(orig.cl) ) { 
    new.cl.w2[i] <- NA
    next
  }
  if ( orig.cl == 1 ) new.cl.w2[i] <- 'DISENGAGED'
  else if ( orig.cl %in% c(2,3) ) new.cl.w2[i] <- 'A_POS_DIFF'
  else if (orig.cl == 4) new.cl.w2[i] <- 'SAS_ONLY_BAL'
  else new.cl.w2[i] <- 'MA_ALL_ACT_BAL'
}

## do the mapping for week 3
new.cl.w3 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w3[i]
  if ( is.na(orig.cl) ) { 
    new.cl.w3[i] <- NA
    next
  }
  if ( orig.cl == 1 ) new.cl.w3[i] <- 'SAS_MOSTLY_NEG_DIFF'
  else if ( orig.cl == 2 ) new.cl.w3[i] <- 'HA_NEG_DIFF_MC'
  else if ( orig.cl == 3 ) new.cl.w3[i] <- 'A_NEG_DIFF'
  else if ( orig.cl == 4 ) new.cl.w3[i] <- 'SAS_ONLY_NEG_DIFF'
  else new.cl.w3[i] <- 'DISENGAGED'
}

## do the mapping for week 4
new.cl.w4 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w4[i]
  if ( is.na(orig.cl) ) 
    new.cl.w4[i] <- NA
  else
    new.cl.w4[i] <- switch(orig.cl, "HA_BAL_MC", "A_NEG_DIFF", 
                         "SAS_ONLY_NEG_DIFF", "SAS_MOSTLY_NEG_DIFF", "DISENGAGED")
}  
table(new.cl.w4)

## do the mapping for week 5
new.cl.w5 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w5[i]
  if ( is.na(orig.cl) ) 
    new.cl.w5[i] <- NA
  else
    new.cl.w5[i] <- switch(orig.cl, "MA_ALL_ACT_BAL", "SAS_ONLY_NEG_DIFF", 
                         "HA_NEG_DIFF_MC", "SAS_ONLY_BAL" )
} 
table(new.cl.w5)

## do the mapping for week 6
new.cl.w6 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w6[i]
  if ( is.na(orig.cl) ) 
    new.cl.w6[i] <- NA
  else new.cl.w6[i] <- switch(orig.cl, "A_POS_DIFF", "MA_MANY_ACT_BAL_MC", "DISENGAGED",
                         "HA_BAL_MC", "HA_BAL")
} 
table(new.cl.w6)


## do the mapping for week 7
new.cl.w7 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w7[i]
  if ( is.na(orig.cl) ) 
    new.cl.w7[i] <- NA
  else 
    new.cl.w7[i] <- switch(orig.cl, "A_NEG_DIFF", "SAS_ONLY_NEG_DIFF", "SAS_ONLY_BAL",
                         "MA_MANY_ACT_BAL_MC")
} 
table(new.cl.w7)


## do the mapping for week 8
new.cl.w8 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w8[i]
  if ( is.na(orig.cl) ) 
    new.cl.w8[i] <- NA
  else 
    new.cl.w8[i] <- switch(orig.cl, "SAS_ONLY_NEG_DIFF", "MA_ALL_ACT_BAL", "SAS_MOSTLY_NEG_DIFF",
                         "SAS_ONLY_BAL", "SAS_ONLY_NEG_DIFF")
} 
table(new.cl.w8)


## do the mapping for week 9
new.cl.w9 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w9[i]
  if ( is.na(orig.cl) ) 
    new.cl.w9[i] <- NA
  else 
    new.cl.w9[i] <- switch(orig.cl, "SAS_ONLY_NEG_DIFF", "A_NEG_DIFF", "HA_NEG_DIFF_MC", 
                         "SAS_ONLY_NEG_DIFF")
} 
table(new.cl.w9)


## do the mapping for week 10
new.cl.w10 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w10[i]
  if ( is.na(orig.cl) ) 
    new.cl.w10[i] <- NA
  else 
    new.cl.w10[i] <- switch(orig.cl, "SAS_ONLY_NEG_DIFF", "DISENGAGED", "MA_ALL_ACT_BAL", 
                         "HA_NEG_DIFF_MC", "SAS_ONLY_NEG_DIFF")
} 
table(new.cl.w10)


## do the mapping for week 11
new.cl.w11 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w11[i]
  if ( is.na(orig.cl) ) 
    new.cl.w11[i] <- NA
  else 
    new.cl.w11[i] <- switch(orig.cl, "A_NEG_DIFF", "SAS_ONLY_NEG_DIFF", 
                          "SAS_ONLY_NEG_DIFF", "SAS_ONLY_BAL")
} 
table(new.cl.w11)


## do the mapping for week 12
new.cl.w12 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w12[i]
  if ( is.na(orig.cl) ) 
    new.cl.w12[i] <- NA
  else 
    new.cl.w12[i] <- switch(orig.cl, "SAS_ONLY_NEG_DIFF", "MA_ALL_ACT_BAL", "SAS_ONLY_BAL",
                          "SAS_ONLY_NEG_DIFF")
} 
table(new.cl.w12)


## do the mapping for week 13
new.cl.w13 <- factor(compact.codes)
for (i in 1:n.users) {
  orig.cl <- orig.cl.w213$cl.w13[i]
  if ( is.na(orig.cl) ) 
    new.cl.w13[i] <- NA
  else 
    new.cl.w13[i] <- switch(orig.cl, "SAS_ONLY_BAL", "A_POS_DIFF", "HA_BAL",
                          "A_POS_DIFF", "HA_BAL")
} 
table(new.cl.w13)


## create a new df with the compact cluster coding scheme
compact.clusters <- data.frame(user_id=orig.cl.w213$user_id,
                               new.cl.w2, new.cl.w3, new.cl.w4, new.cl.w5, new.cl.w6,
                               new.cl.w7, new.cl.w8, new.cl.w9, new.cl.w10, new.cl.w11,
                               new.cl.w12, new.cl.w13)
str(compact.clusters)

## store the compact clusters in an .rdata file to preserve all the levels of the factor vars
## (saving to .csv would not do that)
save(compact.clusters, file = "Intermediate_files/compact_clusters_w2_to_w13_(feb2016).RData")


