############################################################################
## DO SEQUENCE ANALYSIS AND CLUSTERING OF STUDENTS' LEARNING SEQUENCES 
## (COMPRISED OF CLUSTERS THEY 'BELONGED TO' THROUGHOUT THE COURSE). 
## TRY THIS WITH:
## - THE 'COMPACT' SET COMPRISING 11 DIFFERENT CLUSTERS
## - THE 'COARSE' SET COMPRISING 5 DIFFERENT CLUSTERS
##
## SEQUENCE ANALYSIS IS DONE USING TraMineR PACKAGE 
#############################################################################

library(TraMineR)

############################################################################
## CREATE 'COARSE' CLUSTER SCHEME BY MAKING THE 'COMPACT' CLUSTER SCHEME 
## EVEN COARSER GRAINED WITH THE FOLLOWING MAPPING:
## - 'HA_BAL', 'HA_BAL_MC', 'HA_NEG_DIFF_MC' => 'HIGHLY_ACTIVE' ('HA')
## - 'A_POS_DIFF', 'A_NEG_DIFF' => 'ACTIVE' ('A')
## - 'MA_ALL_ACT_BAL', 'MA_MANY_ACT_BAL_MC', 'SAS_MOSTLY_NEG-DIFF' => 'MODERATELY_ACTIVE' ('MA')
## - 'SAS_ONLY_BAL', 'SAS_ONLY_NEG_DIFF' => 'SUMMATIVE_ASSESS_ONLY' ('SAS')
## - 'DISENGAGED' ('DENG')
############################################################################

load("Intermediate_files/compact_clusters_w2_to_w13_(feb2016).RData")

n.users <- nrow(compact.clusters)
n.vars <- ncol(compact.clusters)
m <- matrix(data = rep(NA, n.users*(n.vars-1)), nrow = n.users, ncol = (n.vars-1), byrow = T)
for(i in 1:n.users) {
  for (j in 1:(n.vars-1)) {
    cl <- compact.clusters[i,j+1]
    if ( is.na(cl) ) next
    if ( grepl("HA_BAL|HA_BAL_MC|HA_NEG_DIFF_MC", cl) )
      m[i,j] <- 'HA'
    else if ( grepl("A_POS_DIFF|A_NEG_DIFF", cl) )
      m[i,j] <- 'A'
    else if ( grepl("MA_ALL_ACT_BAL|MA_MANY_ACT_BAL_MC|SAS_MOSTLY_NEG-DIFF", cl) )
      m[i,j] <- 'MA'
    else if ( grepl("SAS_ONLY_BAL|SAS_ONLY_NEG_DIFF", cl) )
      m[i,j] <- 'SAS'
    else m[i,j] <- 'DENG'
  }  
}

coarse.clusters <- as.data.frame(m)
coarse.clusters <- data.frame(cbind(user_id=compact.clusters$user_id, coarse.clusters))
colnames(coarse.clusters)[2:13] <- paste("cl.w",2:13, sep = "")

##########################################################
## CREATE SEQUENCES OF WEEKLY CLUSTERS AND CLUSTER THEM ##
##########################################################

coarse.codes <- c('HA', 'A', 'MA', 'SAS', 'DENG')
coarse.labels <- c('HIGHLY_ACTIVE', 'ACTIVE', 'MODERATELY_ACTIVE', 
                   'SUMMATIVE_ASSESS_ONLY', 'DISENGAGED')

## create a sequence object
trace.seq <- seqdef(data = coarse.clusters, var = 2:13, alphabet = coarse.codes,
                    states = coarse.codes, labels = coarse.labels)

## the index plot of the first 10 sequences
par(mfrow=c(1,1))
seqiplot(trace.seq, title = "Index plot (10 first sequences)", border = NA, withlegend=F)
seqlegend(trace.seq, fontsize = 0.75)

## the index plot of all the sequences
png(file = "graphics/coarse-clusters-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(trace.seq, title = "Sequences of coarse-grained weekly clusters (all sequences)", border = NA, 
         space=0, tlim=0, withlegend="right", cex.legend=0.3, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(trace.seq, title="Cluster distribution through time", withlegend=F)
seqlegend(trace.seq, fontsize = 0.75)

## the sequence frequency plot of the 10 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(trace.seq, border=NA, title="Sequence frequency plot")

## do hierarchical clustering of the trace sequences

## first, distances among the sequences have to be computed
## compute distances between sequences by the method of optimal matching;
## first, a substitution-cost matrix has to be created; it is created
## using the transition rates computed from the sequence data
submat <- seqsubm(seqdata = trace.seq, method = "TRATE", with.missing = T)
## use this matrix to compute distances
## the Optimal Matching method is used and the default value 1 for the
## the insertion/deletion cost is kept
dist.om3 <- seqdist(seqdata = trace.seq, method = "OM", sm = submat, with.missing = T, indel = 3)

## now, do the clustering
library(cluster)
## build a Ward hierarchical clustering of the sequences using 
## the computed (optimal matching) distances
cl.ward <- agnes(dist.om3, diss = T, method = "ward")
par(mfrow=c(1,1))
plot(cl.ward)
## check solution with 5 clusters
cl5 <- cutree(cl.ward, k = 5)
table(cl5)
## get the cluster assignment for each sequence
cl5.fac <- factor(cl5, labels = paste("class:",1:5))
## plot the state distribution at each time point for each cluster
seqdplot(seqdata = trace.seq, group = cl5.fac, title="State distribution across time")

## check solution with 4 clusters
cl4 <- cutree(cl.ward, k = 4)
table(cl4)
## get the cluster assignment for each sequence
cl4.fac <- factor(cl4, labels = paste("class:",1:4))
## plot the state distribution at each time point for each cluster
seqdplot(seqdata = trace.seq, group = cl4.fac, title="State distribution across time", cex.legend=0.5)

## add the computed sequence clusters to the instances
coarse.clusters$seq.cl <- cl4

## store the data about coarse clusters and the computed clusters of sequences
write.csv(coarse.clusters, file = "results/coarse_clusters_w2-13_plus_sequence_clusters(feb2016).csv",
          quote = F, row.names = F)


#####################################################################
## DO SEQUENCE ANALYSIS USING THE TRACE LOG DATA
## (ORIGINALLY CREATED FOR PROCESS MINING) 
## 
## 1) LOOK FOR SEQUENCE PATTERNS IN THE SEQUENCES OF THE BEST 
##    PERFORMING STUDENTS
## 2) DO THE SAME FOR THE WORST PERFORMING STUDENTS
## 3) EXAMINE IF THERE ARE DIFFERENCES IN THOSE PATTERNS
#####################################################################

###################################################
## EXAMINING SEQUENCES OF BEST PERFORMING STUDENTS
## TOP 10% ON BOTH MIDTERM AND FINAL EXAMS
###################################################

load(file="Intermediate_files/top10perc_sequences_SPS_format.RData")
print(head(top10.seq), format = "SPS")

## check the length of the sequences
s.length <- seqlength(top10.seq)
summary(s.length)
quantile(x = s.length, probs = c(0.9, 0.95, 0.99, 1))

## the index plot of all the sequences
png(file = "graphics/top10-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(top10.seq, title = "10% best performing students (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
par(mfrow=c(1,1))
seqdplot(top10.seq, title="States (activities) distribution through time", border=NA, 
         withlegend=F, axes=F)
#seqlegend(top10.seq, fontsize=0.75)

## the sequence frequency plot of the 20 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(top10.seq, border=NA, title="Sequence frequency plot", cex.legend=0.5, axes=F, tlim=1:20)

## the sequence frequency plot of the 10 most frequent sequences with above median length
m <- median(s.length)
seqfplot(top10.seq[s.length>m,], border=NA, title="10 most frequent sequences with above median length", 
         cex.legend=0.5, axes=F)

## plot the mean time spent in different states
seqmtplot(top10.seq, title="Mean time spent in different activities", cex.legend=0.5)

## compute entropy
top10.ent <- seqient(top10.seq)
summary(top10.ent)
## plot the entropy
seqHtplot(top10.seq, title="Entropy of sequences of top 10% students")

## check the tansition rates
seqtrate(top10.seq)
## see what it looks like if lag is increased
seqtrate(top10.seq, lag = 2)

## see what clustering will produce
## first, compute dissimilarities among clusters using the Optimal Matching method
## normalize the computed distances to account for differences in sequence lengths
submat <- seqsubm(seqdata = top10.seq, method = "TRATE")
dist.om1 <- seqdist(seqdata = top10.seq, method = "OM", sm = submat, norm = T)
require(cluster)
top10.ward <- agnes(dist.om1, diss = T, method = "ward")
plot(top10.ward)
## choose solution with 4 clusters
cl4 <- cutree(top10.ward, k = 4)
table(cl4)
## get the cluster assignment for each sequence
cl4.fac <- factor(cl4, labels = paste("cluster:",1:4))
## plot the state distribution at each time point for each cluster
seqdplot(seqdata = top10.seq, group = cl4.fac, title="State distribution across time",
         withlegend=F)


#######################################################
## EXAMINING SEQUENCES OF THE WORST PERFORMING STUDENTS
## BOTTOM 25% ON BOTH MIDTERM AND FINAL EXAMS
## (BOTTOM 10% WERE INITIALLY CONSIDERED BUT PRODUCED 
## TOO FEW SEQUENCES THAT WERE UNUSABLE)
#######################################################

load(file="Intermediate_files/worst25perc_sequences_SPS_format.RData")
print(head(worst25.seq), format = "SPS")

require(TraMineR)
## the index plot of all the sequences
png(file = "graphics/worst25-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(worst25.seq, title = "25% worst performing students (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(worst25.seq, border=NA, withlegend=F, axes=F, 
         title="States (activities) distribution through time")
## plot next to one another the distributions of both best and worst performing students
par(mfrow=c(1,2))
seqdplot(top10.seq, border=NA, withlegend=F, axes=F)
seqdplot(worst25.seq, border=NA, withlegend=F, axes=F, yaxis=F, ylab=NULL)
par(mfrow=c(1,1))
seqlegend(worst25.seq, position = "bottom", fontsize = 0.65)

## the sequence frequency plot of the 20 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(worst25.seq, border=NA, title="Sequence frequency plot", cex.legend=0.5, axes=F, tlim=1:20)

## plot the mean time spent in different states
seqmtplot(worst25.seq, title="Mean time spent in different activities", cex.legend=0.5)

## compute entropy
worst25.ent <- seqient(worst25.seq)
summary(worst25.ent)
## plot it
seqHtplot(worst25.seq, title="Entropy of sequences of bottom 25% students")
## exhibits similar pattern as that of top 10% students

## see what clustering will produce
## first, compute dissimilarities among clusters using the Optimal Matching method  
submat.w25 <- seqsubm(seqdata = worst25.seq, method = "TRATE")
dist.om1.w25 <- seqdist(seqdata = worst25.seq, method = "OM", sm = submat.w25, norm = T)
require(cluster)
worst25.ward <- agnes(dist.om1.w25, diss = T, method = "ward")
plot(worst25.ward)

## check te solution with 5 clusters
cl5 <- cutree(worst25.ward, k = 5)
table(cl5)
## get the cluster assignment for each sequence
cl5.fac <- factor(cl5, labels = paste("cluster:",1:5))
## plot the state distribution at each time point for each cluster
png(file = "graphics/worst25-clusters-plot.png", width = 1750, height = 1900, pointsize = 50)
seqdplot(seqdata = worst25.seq, group = cl5.fac, title="State distribution",
         withlegend=F, cex.plot=0.65)
dev.off()

## check te solution with 4 clusters
cl4 <- cutree(worst25.ward, k = 4)
table(cl4)
## get the cluster assignment for each sequence
cl4.fac <- factor(cl4, labels = paste("cluster:",1:4))
## plot the state distribution at each time point for each cluster
seqdplot(seqdata = worst25.seq, group = cl4.fac, title="State distribution",
         cex.plot=0.65, withlegend=F)


#######################################################
## EXAMINING SEQUENCES OF THE STUDENT GROUPS OBTAINED 
## BY APPLYING LCA TO THE WEEKLY CLUSTERS 
#######################################################

require(TraMineR)

load(file = "Intermediate_files/sequences_for_LCA_classes_SPS_format.RData")

##############################
## examine the 1st LCA class
##############################

lca1.seq <- lca.sequences$lca1

## the index plot of all the sequences
png(file = "graphics/lca1-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(lca1.seq, title = "LCA_1 cohort (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(lca1.seq, title="LCA 1: sequence distribution through time", border=NA, cex.legend=0.5, axes=F)

## the sequence frequency plot of the 20 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(lca1.seq, border=NA, title="LCA 1 sequence frequency plot", axes=F, tlim=1:20, withlegend=F)

## plot the mean time spent in different states
seqmtplot(lca1.seq, title="LCA 1: mean time spent in different activities", cex.legend=0.5)

## compute entropy
lca1.ent <- seqient(lca1.seq)
summary(lca1.ent)
## plot it
seqHtplot(lca1.seq, title="LCA 1: entropy of sequences")

##############################
## examine the 2nd LCA class
##############################

lca2.seq <- lca.sequences$lca2

## the index plot of all the sequences
png(file = "graphics/lca2-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(lca2.seq, title = "LCA 2 cohort (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(lca2.seq, title="LCA 2: sequence distribution through time", border=NA, 
         cex.legend=0.5, axes=F)

## the sequence frequency plot of the 20 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(lca2.seq, border=NA, title="LCA 2: sequence frequency plot", axes=F, tlim=1:20, withlegend=F)

## plot the mean time spent in different states
seqmtplot(lca2.seq, title="LCA 2: mean time spent in different activities", cex.legend=0.5)

## compute entropy
lca2.ent <- seqient(lca2.seq)
summary(lca2.ent)
## plot it
seqHtplot(lca2.seq, title="LCA 2: entropy of sequences")

##############################
## examine the 3rd LCA class
##############################

lca3.seq <- lca.sequences$lca3

## the index plot of all the sequences
png(file = "graphics/lca3-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(lca3.seq, title = "LCA 3 cohort (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(lca3.seq, title="LCA 3: sequence distribution through time", border=NA, cex.legend=0.5, axes=F)

## the sequence frequency plot of the 20 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(lca3.seq, border=NA, title="LCA 3: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)

## plot the mean time spent in different states
seqmtplot(lca3.seq, title="LCA 3: mean time spent in different activities", cex.legend=0.5)

## compute entropy
lca3.ent <- seqient(lca3.seq)
summary(lca3.ent)
## plot it
seqHtplot(lca3.seq, title="LCA 3: entropy of sequences")

##############################
## examine the 4th LCA class
##############################

lca4.seq <- lca.sequences$lca4

## the index plot of all the sequences
png(file = "graphics/lca4-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(lca4.seq, title = "LCA 4 cohort (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(lca4.seq, title="LCA 4: sequence distribution through time", 
         border=NA, cex.legend=0.5, axes=F)

## the sequence frequency plot of the 20 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(lca4.seq, border=NA, title="LCA 4: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)

## plot the mean time spent in different states
seqmtplot(lca4.seq, title="LCA 4: mean time spent in different activities", cex.legend=0.5)

## compute entropy
lca4.ent <- seqient(lca4.seq)
summary(lca4.ent)
## plot it
seqHtplot(lca4.seq, title="LCA 4: entropy of sequences")

##############################
## examine the 4th LCA class
##############################

lca5.seq <- lca.sequences$lca5

## index plot of all the sequences
png(file = "graphics/lca5-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(lca5.seq, title = "LCA 5 cohort (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(lca5.seq, title="LCA 5: sequence distribution through time", 
         border=NA, cex.legend=0.5, axes=F)

## the sequence frequency plot of the 20 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(lca5.seq, border=NA, title="LCA 5: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)

## plot the mean time spent in different states
seqmtplot(lca5.seq, title="LCA 5: mean time spent in different activities", cex.legend=0.5)

seqlegend(lca5.seq, fontsize = 0.65)

## compute entropy
lca5.ent <- seqient(lca5.seq)
summary(lca5.ent)
## plot it
seqHtplot(lca5.seq, title="LCA 5: entropy of sequences")

#################################################################
## compute some sequence-related statistics for the 5 LCA classes
## median, Q1, Q3 for sequence length, entropy
#################################################################

lca.cl <- read.csv(file = "results/lca_w2_to_w13_(feb2016).csv")
n.users <- table(lca.cl$lca5)
n.seq <- sapply(lca.sequences, nrow)

comparison <- data.frame(cbind(num.users=n.users, num.seq=n.seq))
rownames(comparison) <- paste("class_", rownames(comparison), sep = "")

comparison$avg.seq.per.user <- round(comparison$num.seq/comparison$num.users, digits = 2)

seq.length <- vector(length = 5)
for (i in 1:5) {
  q <- quantile(seqlength(lca.sequences[[i]]), probs = c(0.25,0.5,0.75))
  seq.length[i] <- paste(q[2], " (", q[1], ", ", q[3], ")", sep = "")
}
seq.length
comparison$seq.length <- seq.length

seq.ent <- vector(length = 5)
for (i in 1:5) {
  q <- quantile(seqient(lca.sequences[[i]]), probs = c(0.25,0.5,0.75))
  seq.ent[i] <- paste(round(q[2],digits = 2), " (", q[1], ", ", round(q[3], digits = 2), ")", sep = "")
}
seq.ent
comparison$seq.entropy <- seq.ent

require(knitr)
kable(comparison, format = "rst")


#########################################################################
## DO SEQUENCE ANALYSIS USING THE SESSION-BASED LEARNING TRACES 
## (ORIGINALLY CREATED FOR PROCESS MINING) FOR ALL THE STUDENTS (N=290)
##
## 1) cluster the sequences; 
## 2) cluster students based on the sequence clusters identified in step 1
##########################################################################

seq.df <- readRDS(file = "Intermediate_files/all_students_sequences_dataframe.RData")
seq.df$V1 <- as.integer(seq.df$V1)
seq.df$V2 <- as.integer(seq.df$V2)

sequences <- readRDS(file = "Intermediate_files/all_students_sequences_SPS_format.RData") 

library(TraMineR)

## first, examine the sequences using the different diagrams

## index plot of all the sequences
png(file = "graphics/entire_sample_seqiplot_all_sequences.png", width = 1800, height = 1500, pointsize = 50)
seqiplot(sequences, title = "Entire student population (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(sequences, title="Entire student population: state (activity) distribution through time", 
         cex.legend=0.5, axes=F)

## the sequence frequency plot of the 20 most frequent sequences with bar width proportional
## to the frequencies
seqfplot(sequences, border=NA, title="Entire student population: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)

## plot the mean time spent in different states
seqmtplot(lca5.seq, title="Entire student population: mean time spent in different activities", 
          cex.legend=0.5)

seqlegend(lca5.seq, fontsize = 0.65)

## do the clustering 

## first, compute dissimilarities among clusters using the Optimal Matching method
## normalize the computed distances to account for differences in sequence lengths
submat <- seqsubm(seqdata = sequences, method = "TRATE")
dist.om1 <- seqdist(seqdata = sequences, method = "OM", sm = submat, norm = T)
## examine the results with different values of the distance measure parameters
#dist.om2 <- seqdist(seqdata = sequences, method = "OM", sm = submat, norm = T, indel = 2)
#dist.om3 <- seqdist(seqdata = sequences, method = "OM", sm = submat, norm = T, indel = 3)
## the best value for the indel parameter is definitively the default one (1)

## now, do hierarchical clustering
require(cluster)
set.seed(123)
seq.ward <- agnes(dist.om1, diss = T, method = "ward")
save(seq.ward, file = "Intermediate_files/seq.ward.RData")
plot(seq.ward)

## check the solution with 4 clusters
cl4 <- cutree(seq.ward, k = 4)
table(cl4)
## get the cluster assignment for each sequence
cl4.fac <- factor(cl4, labels = paste("cluster:",1:4))
## plot the state distribution at each time point for each cluster
seqdplot(seqdata = sequences, group = cl4.fac, title="State distribution across time",
         withlegend=F)
seqlegend(sequences, fontsize = 0.65)

## examine also the solution with 5 clusters
cl5 <- cutree(seq.ward, k=5)
table(cl5)
cl5.fac <- factor(cl5, labels = paste("cluster:",1:5))
seqdplot(seqdata = sequences, group = cl5.fac, title="State distribution across time",
         withlegend=F)
## the one with 4 clusters looks better

## create a new data frame with one row for each session (sequence) and 3 columns: 
## 1) sesssion id
## 2) id of the student who 'performed' that session/sequence, 
## 3) the cluster the sequence was assigned to
seq.clusters <- seq.df[,c(1,2)]
seq.clusters$cluster <- as.factor(cl4)
str(seq.clusters)
colnames(seq.clusters)[1:2] <- c('stud.id', 'session.id')
## save the sequence cluster assignment 
write.csv(seq.clusters, file = "results/all_stud_sequences_4_seq_clusters.csv", 
          quote = F, row.names = F)


#######################################################################################
## 1) compute for each student, the number of sequences in each sequence-based cluster
## 2) use the cluster-based sequence counts as variables for clustering students
#######################################################################################

seq.clusters <- read.csv(file = "results/all_stud_sequences_4_seq_clusters.csv")

stud.ids <- unique(seq.clusters$stud.id)
n.stud <- length(stud.ids)
## create a matrix to store stud.id (1st col), and the number of the student's 
## sequences in each of the sequence clusters 
seq.dist.m <- matrix(nrow = n.stud, ncol=5, byrow = T)
for(i in 1:n.stud) {
  stud.data <- seq.clusters[seq.clusters$stud.id==stud.ids[i],]
  seq.dist.m[i,1] <- stud.ids[i]
  seq.dist.m[i,2] <- nrow(stud.data[stud.data$cluster==1,])
  seq.dist.m[i,3] <- nrow(stud.data[stud.data$cluster==2,])
  seq.dist.m[i,4] <- nrow(stud.data[stud.data$cluster==3,])
  seq.dist.m[i,5] <- nrow(stud.data[stud.data$cluster==4,])
}
head(seq.dist.m)
seq.dist.df <- as.data.frame(seq.dist.m)
colnames(seq.dist.df) <- c('stud.id', 'cl1', 'cl2', 'cl3', 'cl4')
head(seq.dist.df)

## cluster the students using the columns of the seq.dist.df as variables
summary(seq.dist.df)
## no real need for normalizing the variables, but better do it

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}

norm.vars <- as.data.frame( apply(X = seq.dist.df[,c(2:5)], 
                                      MARGIN = 2, 
                                      FUN = normalize.feature) )
## next, compute the distance between the observations
distance <- dist(norm.vars)
## use the computed distances to do the clustering
set.seed(1234)
hc <- hclust(distance, method = "ward.D2")
## plot the clustering tree
plot(hc, cex=0.69, main = "Student clustering")

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(3:8, function(ncl) table(cutree(hc, k = ncl)))
names(clusters) <- c(3:8)
clusters

## examine the solution with 5 clusters
rect.hclust(hc, k=5)
hcl5 <- cutree(hc, k = 5) 
hcl5.df <- data.frame(stud.id=seq.dist.df$stud.id, cluster=hcl5)

## and the one with 4 clusters
plot(hc, cex=0.69, main = "Student clustering")
rect.hclust(hc, k=4)
hcl4 <- cutree(hc, k = 4)
hcl4.df <- seq.dist.df
hcl4.df$cluster <- hcl4
str(hcl4.df)
## the solution with 4 clusters looks better

## store the computed clusters
write.csv(x = hcl4.df, file = "results/sequence_based_student_clusters.csv",
          quote = F, row.names = F)

## compute summary stats for the clustering features
source(file="util_functions.R")
hcl4.stats <- summary.stats(feature.set = hcl4.df[,c(2:5)], clusters = hcl4, cl.number = 4)
require(knitr)
kable(hcl4.stats, format = 'rst')

######################################################################
## compare clusters based on students' midterm and final exams scores
######################################################################

hcl4.df <- read.csv(file = "results/sequence_based_student_clusters.csv")

## add the total column as the sum of sequences in the 4 clusters
hcl4.df$total <- hcl4.df$cl1 + hcl4.df$cl2 + hcl4.df$cl3 + hcl4.df$cl4
hcl4.df <- hcl4.df[,c(1:5,7,6)]
str(hcl4.df)

## get the students' scores on the final exam
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the relevant score
scores <- all.scores[, c('user_id', 'SC_FE_TOT', 'SC_MT_TOT')]

setdiff(hcl4.df$stud.id, scores$user_id)
## no differences in the student ids
hcl4.plus.scores <- merge(x = hcl4.df, y = scores, 
                          by.x = "stud.id", by.y = 'user_id', all = T)
str(hcl4.plus.scores)
colnames(hcl4.plus.scores)[7] <- 'class'
hcl4.plus.scores <- hcl4.plus.scores[,c(1:6,9,8,7)]

## compute the summary statistics for a number of variables that describe clusters
hcl4.stats <- summary.stats(hcl4.plus.scores[,c(2:8)], hcl4.plus.scores$class, 4)
hcl4.stats$attributes <- c('cluster', 'n.students', 'seq.clust1', 'seq.clust2', 'seq.clust3',
                            'seq.clust4', 'seq.total', 'midterm.score', 'final.exam.score')
kable(x = hcl4.stats, format = "rst")

## compute summary statistics for the final exam score for each class
summary.data <- final.score.per.class.stats(hcl4.plus.scores)                  
require(knitr)
kable(x = summary.data, format = "rst")
## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(hcl4.plus.scores$SC_FE_TOT ~ hcl4.plus.scores$class)
## chi-squared = 36.284, df = 3, p-value = 6.521e-08
## there is a sign. difference bewtween the classes; check where the difference is

## apply Mann-Whitney U Test to do pair-wise comparisons
## compare class 1 with classes 2, 3, and 4
comparison <- matrix(nrow = 6, ncol = 5, byrow = T)
comparison[1,] <- c(1, 2, compare.fexam.scores.Mann.Whitney.test(hcl4.plus.scores, 1, 2))
comparison[2,] <- c(1, 3, compare.fexam.scores.Mann.Whitney.test(hcl4.plus.scores, 1, 3))
comparison[3,] <- c(1, 4, compare.fexam.scores.Mann.Whitney.test(hcl4.plus.scores, 1, 4))
## compare class 2 with classes 3 and 4
comparison[4,] <- c(2, 3, compare.fexam.scores.Mann.Whitney.test(hcl4.plus.scores, 2, 3))
comparison[5,] <- c(2, 4, compare.fexam.scores.Mann.Whitney.test(hcl4.plus.scores, 2, 4))
## compare classes 3 and 4
comparison[6,] <- c(3, 4, compare.fexam.scores.Mann.Whitney.test(hcl4.plus.scores, 3, 4))
comparison.df <- as.data.frame(comparison)
colnames(comparison.df) <- c('c1', 'c2', 'Z', 'p', 'r')
comparison.df

## apply the FDR correction to the comparisons
comparison.df <- apply.FDR.correction(comparison.df)
kable(x = comparison.df, format = 'rst')

## compute summary statistics for the midterm exam score for each class
summary.data <- midterm.score.per.class.stats(hcl4.plus.scores)                  
kable(x = summary.data, format = "rst")
## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(hcl4.plus.scores$SC_MT_TOT ~ hcl4.plus.scores$class)
## chi-squared = 36.27, df = 3, p-value = 6.567e-08
## there is a sign. difference bewtween the classes; check where the difference is

## apply Mann-Whitney U Test to do pair-wise comparisons
## compare class 1 with classes 2, 3, and 4
comparison <- matrix(nrow = 6, ncol = 5, byrow = T)
comparison[1,] <- c(1, 2, compare.midterm.scores.Mann.Whitney.test(hcl4.plus.scores, 1, 2))
comparison[2,] <- c(1, 3, compare.midterm.scores.Mann.Whitney.test(hcl4.plus.scores, 1, 3))
comparison[3,] <- c(1, 4, compare.midterm.scores.Mann.Whitney.test(hcl4.plus.scores, 1, 4))
## compare class 2 with classes 3 and 4
comparison[4,] <- c(2, 3, compare.midterm.scores.Mann.Whitney.test(hcl4.plus.scores, 2, 3))
comparison[5,] <- c(2, 4, compare.midterm.scores.Mann.Whitney.test(hcl4.plus.scores, 2, 4))
## compare classes 3 and 4
comparison[6,] <- c(3, 4, compare.midterm.scores.Mann.Whitney.test(hcl4.plus.scores, 3, 4))
comparison.df <- as.data.frame(comparison)
colnames(comparison.df) <- c('c1', 'c2', 'Z', 'p', 'r')
comparison.df

## apply the FDR correction to the comparisons
comparison.df <- apply.FDR.correction(comparison.df)
kable(x = comparison.df, format = 'rst')


#################################################
## COMPARE SEQUENCES OF BEST PERFORMING STUDENTS
## TOP 10% ON BOTH MIDTERM AND FINAL EXAMS
## IN WEEKS 1, 6 AND 13
#################################################

seq.df <- readRDS(file = "Intermediate_files/all_students_sequences_dataframe.RData")
seq.df$V1 <- as.integer(seq.df$V1)
seq.df$V2 <- as.integer(seq.df$V2)
colnames(seq.df)[1:2] <- c('stud.id', 'session.id')

## load sesssions data in order to match sessions with course weeks
all.sessions <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv")
## select sessions for weeks 2, 6 and 13 
w2.sessions <- unique(all.sessions$CASE_ID[all.sessions$WEEK==2])
w6.sessions <- unique(all.sessions$CASE_ID[all.sessions$WEEK==6])
w13.sessions <- unique(all.sessions$CASE_ID[all.sessions$WEEK==13])

## now, create sequence data frame for each of the 3 weeks separately
w2.seq.df <- subset(seq.df, session.id %in% w2.sessions)
w6.seq.df <- subset(seq.df, session.id %in% w6.sessions)
w13.seq.df <- subset(seq.df, session.id %in% w13.sessions)

## identify best and worst performing students
## load the f. for identifying top performing students
source("util_functions.R")
counts.data <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv") 
## extract exam scores
scores <- counts.data[, c('user_id', 'SC_MT_TOT', 'SC_FE_TOT')]
best.and.worst <- best.and.worst.performers(scores)
top25.ids <- best.and.worst$top25

## filter further sequences for weeks 2, 6, and 13 to include only top 25% students
w2.best.seq.df <- subset(w2.seq.df, stud.id %in% top25.ids)
w6.best.seq.df <- subset(w6.seq.df, stud.id %in% top25.ids)
w13.best.seq.df <- subset(w13.seq.df, stud.id %in% top25.ids)

## create (TraMineR) sequences out of the data frame
require(TraMineR)
w2.best.seq <- seqdef(data = w2.best.seq.df, var = 3:ncol(w2.best.seq.df), 
                      informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"))
print(w2.best.seq[1:10, 1:30], format = 'SPS')

## the state distribution by time points
seqdplot(w2.best.seq, title="Distribution of activities in learning sequences of 25% top students in week 2", withlegend=F)
seqlegend(trace.seq, fontsize = 0.75)
seqfplot(w2.best.seq, border=NA, title="25% best students in week 2: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)
seqiplot(w2.best.seq, title = "25% top students in week 2 (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)


w6.best.seq <- seqdef(data = w6.best.seq.df, var = 3:ncol(w6.best.seq.df), 
                      informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"))
seqdplot(w6.best.seq, title="Distribution of activities in learning sequences of 25% top students in week 6", withlegend=F)
seqfplot(w6.best.seq, border=NA, title="25% best students in week 6: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)
seqiplot(w6.best.seq, title = "25% top students in week 6 (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)


w13.best.seq <- seqdef(data = w13.best.seq.df, var = 3:ncol(w13.best.seq.df), 
                      informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"))
seqdplot(w13.best.seq, title="Distribution of activities in learning sequences of 25% top students in week 13", withlegend=F)
seqfplot(w13.best.seq, border=NA, title="25% best students in week 13: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)
seqiplot(w13.best.seq, title = "25% top students in week 13 (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)


## now, do the same but for the 25% weakest students
## filter further sequences for weeks 2, 6, and 13 to include only the weakest 25% students
wakest25.ids <- best.and.worst$worst25
w2.weakest.seq.df <- subset(w2.seq.df, stud.id %in% wakest25.ids)
w6.weakest.seq.df <- subset(w6.seq.df, stud.id %in% wakest25.ids)
w13.weakest.seq.df <- subset(w13.seq.df, stud.id %in% wakest25.ids)

w2.weakest.seq <- seqdef(data = w2.weakest.seq.df, var = 3:ncol(w2.weakest.seq.df), 
                      informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"))
seqdplot(w2.weakest.seq, title="Distribution of activities in learning sequences of the 25% weakest students in week 2", withlegend=F)
seqfplot(w2.weakest.seq, border=NA, title="25% weakest students in week 2: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)
seqiplot(w2.weakest.seq, title = "25% weakest students in week 2 (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)

w6.weakest.seq <- seqdef(data = w6.weakest.seq.df, var = 3:ncol(w6.weakest.seq.df), 
                         informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"))
seqdplot(w6.weakest.seq, title="Distribution of activities in learning sequences of the 25% weakest students in week 6", withlegend=F)
seqfplot(w6.weakest.seq, border=NA, title="25% weakest students in week 6: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)
seqiplot(w6.weakest.seq, title = "25% weakest students in week 6 (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)

w13.weakest.seq <- seqdef(data = w13.weakest.seq.df, var = 3:ncol(w13.weakest.seq.df), 
                         informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"))
seqdplot(w13.weakest.seq, title="Distribution of activities in learning sequences of the 25% weakest students in week 13", withlegend=F)
seqfplot(w13.weakest.seq, border=NA, title="25% weakest students in week 13: sequence frequency plot", axes=F, 
         tlim=1:20, withlegend="right", cex.legend=0.5)
seqiplot(w13.weakest.seq, title = "25% weakest students in week 13 (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)

## examine the entropy of sequences
ent.stats <- function(sequences) {
  q <- quantile(seqient(sequences), probs = c(0.25,0.5,0.75))
  paste(round(q[2],digits = 2), " (", round(q[1], digits = 2), ", ", round(q[3], digits = 2), ")", sep = "")
}

seq.ent <- vector(length = 6)
seq.ent[1] <- ent.stats(w2.best.seq)
seq.ent[2] <- ent.stats(w6.best.seq)
seq.ent[3] <- ent.stats(w13.best.seq)
seq.ent[4] <- ent.stats(w2.weakest.seq)
seq.ent[5] <- ent.stats(w6.weakest.seq)
seq.ent[6] <- ent.stats(w13.weakest.seq)
seq.ent
comparison.df <- data.frame(week=c(2,6,13,2,6,13),
                            group=c(rep('top25%', 3), rep('worst25%', 3)),
                            entropy=seq.ent)
comparison.df
