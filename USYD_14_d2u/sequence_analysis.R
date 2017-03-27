library(TraMineR)

#####################################################################
## DO SEQUENCE ANALYSIS USING THE TRACE LOG DATA
## (ORIGINALLY CREATED FOR PROCESS MINING) 
## 
## 1) LOOK FOR SEQUENCE PATTERNS IN THE SEQUENCES OF THE BEST 
##    PERFORMING STUDENTS
## 2) DO THE SAME FOR THE WORST PERFORMING STUDENTS
## 3) EXAMINE IF THERE ARE DIFFERENCES IN THOSE PATTERNS
#####################################################################

#############################################################
## EXAMINING SEQUENCES OF BEST PERFORMING STUDENTS, THAT IS,
## STUDENTS WITH EXAM SCORES (BOTH MIDTERM AND FINAL EXAMS) 
## ABOVE 90TH PERCENTILE 
#############################################################

load(file="Intermediate_files/top10perc_sequences_SPS_format.RData")
print(head(top10.seq), format = "SPS")

## check the length of the sequences
s.length <- seqlength(top10.seq)
summary(s.length)
quantile(x = s.length, probs = c(0.9, 0.95, 0.99, 1))

## define the color pallet to be used
## the pallet is colorblind safe, 
## obtained from http://colorbrewer2.org/
color.pallet <- 
  c('#ff7f00','#377eb8','#ffff33','#984ea3','#f781bf','#e41a1c','#a65628','#4daf4a','#999999')
#    rev(c("#4aac8d", "#c65c8a","#5fb048","#8d72c9","#aca534","#cb5f56","#8a8c4a","#c96232","#de9329"))
#   c('#01665e', '#35978f', '#80cdc1','#c7eae5','#f5f5f5','#f6e8c3','#dfc27d','#bf812d','#8c510a')
#   c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')
#  rev(c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4'))
#  rev(c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837'))
#   rev(c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))
  
## the index plot of all the sequences
png(file = "graphics/top10-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(top10.seq, title = "10% best performing students (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
par(mfrow=c(1,1))
seqdplot(top10.seq, title="States (activities) distribution through time", #cpal=color.pallet, 
         border=NA, withlegend=F, axes=F)
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


#################################################################
## EXAMINING SEQUENCES OF THE WORST PERFORMING STUDENTS, THAT IS,
## STUDENTS WITH EXAM SCORES (ON BOTH MIDTERM AND FINAL EXAMS) 
## BELLOW THE 25TH PERCENTILE 
## (THOSE WITH SCORES BELOW THE 10TH PERCENTILE WERE INITIALLY  
## CONSIDERED BUT THEY HAD TOO FEW SEQUENCES)
#################################################################

load(file="Intermediate_files/worst25perc_sequences_SPS_format.RData")
print(head(worst25.seq), format = "SPS")

require(TraMineR)
## the index plot of all the sequences
png(file = "graphics/worst25-seqiplot-all.png", width = 1600, height = 1200, pointsize = 50)
seqiplot(worst25.seq, title = "25% worst performing students (all sequences)", border = NA, 
         space=0, tlim=0, withlegend=F, ylab=NA, cex.plot=0.5)
dev.off()

## the state distribution by time points
seqdplot(worst25.seq, border=NA, withlegend=F, axes=F, #cpal=color.pallet,
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


#########################################################################
## DO SEQUENCE ANALYSIS USING THE SESSION-BASED LEARNING TRACES 
## (ORIGINALLY CREATED FOR PROCESS MINING) FOR ALL THE STUDENTS (N=290)
##
## 1) cluster the sequences; 
## 2) cluster students based on the sequence clusters identified in step 1
##########################################################################

seq.df <- readRDS(file = "Intermediate_files/all_students_sequences_dataframe.RData")
#seq.df <- readRDS(file = "Intermediate_files/SPQ_students_sequences_dataframe.RData")
seq.df$V1 <- as.integer(seq.df$V1)
seq.df$V2 <- as.integer(seq.df$V2)

sequences <- readRDS(file = "Intermediate_files/all_students_sequences_SPS_format.RData") 
#sequences <- readRDS(file = "Intermediate_files/SPQ_students_sequences_SPS_format.RData") 

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

set.seed(1804)

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
seq.ward <- agnes(dist.om1, diss = T, method = "ward")
save(seq.ward, file = "Intermediate_files/seq_ward_all_students_data.RData")
plot(seq.ward)

## check the solution with 4 clusters
cl4 <- cutree(seq.ward, k = 4)
table(cl4)
## get the cluster assignment for each sequence
cl4.fac <- factor(cl4, labels = paste("cluster:",1:4))
# seq.clusters <- read.csv(file = "results/all_stud_sequences_4_seq_clusters.csv")
# cl4.fac <- factor(seq.clusters[,3])
## plot the state distribution at each time point for each cluster
seqdplot(seqdata = sequences, group = cl4.fac, title="State distribution across time",
         withlegend=F) #cpal=color.pallet)
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
colnames(seq.clusters) <- c('stud.id', 'session.id')
seq.clusters$cluster <- as.factor(cl4)
str(seq.clusters)
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
seq.dist.m <- matrix(nrow = n.stud, ncol=6, byrow = T)
for(i in 1:n.stud) {
  stud.data <- seq.clusters[seq.clusters$stud.id==stud.ids[i],]
  seq.dist.m[i,1] <- stud.ids[i]
  seq.dist.m[i,2] <- nrow(stud.data[stud.data$cluster==1,])
  seq.dist.m[i,3] <- nrow(stud.data[stud.data$cluster==2,])
  seq.dist.m[i,4] <- nrow(stud.data[stud.data$cluster==3,])
  seq.dist.m[i,5] <- nrow(stud.data[stud.data$cluster==4,])
  seq.dist.m[i,6] <- nrow(stud.data)
}
head(seq.dist.m)
seq.dist.df <- as.data.frame(seq.dist.m)
colnames(seq.dist.df) <- c('stud.id', 'cl1', 'cl2', 'cl3', 'cl4', 'total')
head(seq.dist.df)

## cluster the students using the columns of the seq.dist.df as variables
summary(seq.dist.df)

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}

norm.vars <- as.data.frame( apply(X = seq.dist.df[,c(2:6)], 
                                      MARGIN = 2, 
                                      FUN = normalize.feature) )

set.seed(1804)
## next, compute the distance between the observations
distance <- dist(norm.vars)
## use the computed distances to do the clustering
hc <- hclust(distance, method = "ward.D2")
## plot the clustering tree
plot(hc, cex=0.69, main = "Student clustering")

# Let us see what happens when we vary the number of clusters  
clusters <- sapply(3:8, function(ncl) table(cutree(hc, k = ncl)))
names(clusters) <- c(3:8)
clusters

## examine the solution with 5 clusters
plot(hc, cex=0.69, main = "Student clustering")
rect.hclust(hc, k=5)
hcl5 <- cutree(hc, k = 5) 
hcl5.df <- seq.dist.df
hcl5.df$cluster <- hcl5

## and the one with 4 clusters
plot(hc, cex=0.69, main = "Student clustering")
rect.hclust(hc, k=4)
hcl4 <- cutree(hc, k = 4)
hcl4.df <- seq.dist.df
hcl4.df$cluster <- hcl4
str(hcl4.df)

## store the computed clusters
write.csv(x = hcl4.df, file = "results/sequence_based_student_clusters_4cl_(Nov2016).csv",
          quote = F, row.names = F)
write.csv(x = hcl5.df, file = "results/sequence_based_student_clusters_5cl_(Nov2016).csv",
          quote = F, row.names = F)

# store only the identified clusters
hcl5.info <- hcl5.df[,c(1,7)]
colnames(hcl5.info) <- c("STUDENT_ID", "STRAT_GROUP")
write.csv(x = hcl5.info, file = "results/sequence_based_5_student_groups_(Nov2016).csv",
          quote = F, row.names = F)


## compute summary stats for the clustering features
source(file="util_functions.R")
hcl4.stats <- summary.stats(feature.set = hcl4.df[,c(2:6)], clusters = hcl4, cl.number = 4)
require(knitr)
kable(hcl4.stats, format = 'rst')

hcl5.stats <- summary.stats(feature.set = hcl5.df[,c(2:6)], clusters = hcl5, cl.number = 5)
kable(hcl5.stats, format = 'rst')


######################################################################
## compare clusters based on students' midterm and final exams scores
######################################################################
source(file = "util_functions.R")

# hcl4.df <- read.csv(file = "results/sequence_based_student_clusters_4cl.csv")

## add the total column as the sum of sequences in the 4 clusters
# hcl4.df$total <- hcl4.df$cl1 + hcl4.df$cl2 + hcl4.df$cl3 + hcl4.df$cl4
# hcl4.df <- hcl4.df[,c(1:5,7,6)]
# str(hcl4.df)

## the same for 5 clusters
# hcl5.df$total <- hcl5.df$cl1 + hcl5.df$cl2 + hcl5.df$cl3 + hcl5.df$cl4
# hcl5.df <- hcl5.df[,c(1:5,7,6)]

hcl5.df <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv")
str(hcl5.df)

## get the students' scores on the final exam
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the relevant score
scores <- all.scores[, c('user_id', 'SC_FE_TOT', 'SC_MT_TOT')]

setdiff(hcl5.df$stud.id, scores$user_id)
## no differences in the student ids

hcl4.plus.scores <- merge(x = hcl4.df, y = scores, 
                           by.x = "stud.id", by.y = 'user_id', all.x = T, all.y = F)
str(hcl4.plus.scores)
colnames(hcl4.plus.scores)[7] <- 'class'
hcl4.plus.scores <- hcl4.plus.scores[,c(1:6,9,8,7)]

hcl5.plus.scores <- merge(x = hcl5.df, y = scores, 
                          by.x = "stud.id", by.y = 'user_id', all.x = T, all.y = F)
str(hcl5.plus.scores)
colnames(hcl5.plus.scores)[7] <- 'class'
hcl5.plus.scores <- hcl5.plus.scores[,c(1:6,9,8,7)]

## compute the summary statistics for a number of variables that describe clusters
hcl4.stats <- summary.stats(hcl4.plus.scores[,c(2:8)], hcl4.plus.scores$class, 4)
attribute.names <- c('cluster', 'n.students', 'seq.clust1', 'seq.clust2', 'seq.clust3',
                      'seq.clust4', 'seq.total', 'midterm.score', 'final.exam.score')
hcl4.stats$attributes <- attribute.names
kable(x = hcl4.stats, format = "rst")

## the same for 5 clusters
hcl5.stats <- summary.stats(hcl5.plus.scores[,c(2:8)], hcl5.plus.scores$class, 5)
hcl5.stats$attributes <- attribute.names
require(knitr)
kable(x = hcl5.stats, format = "rst")

## compute summary statistics for the final exam score for each class
fe.summary.4cl <- final.score.per.class.stats(hcl4.plus.scores)                  
kable(x = fe.summary.4cl, format = "rst")

## the same for 5 clusters
fe.summary.5cl <- final.score.per.class.stats(hcl5.plus.scores)                  
kable(x = fe.summary.5cl, format = "rst")

## the final exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(hcl4.plus.scores$SC_FE_TOT ~ hcl4.plus.scores$class)
## chi-squared = 36.232, df = 3, p-value = 6.689e-08
## there is a sign. difference bewtween the classes; check where the difference is

## the same for 5 clusters
kruskal.test(hcl5.plus.scores$SC_FE_TOT ~ hcl5.plus.scores$class)
## chi-squared = 36.425, df = 4, p-value = 2.366e-07


## f. for pair-wise comparisons using the Mann-Whitney U Test
## FDR correction is used to avoid family-wise error 
pairwise.fexam.compare <- function(nclust, ncomparison, hcl.plus.scores) {
  comparison <- matrix(nrow = ncomparison, ncol = 5, byrow = T)
  k <- 1
  for(i in 1:(nclust-1)) {
    for(j in (i+1):nclust) {
      comparison[k,] <- c(i, j, compare.fexam.scores.Mann.Whitney.test(hcl.plus.scores, i, j))
      k <- k+1
    }
  }
  comparison.df <- as.data.frame(comparison)
  colnames(comparison.df) <- c('c1', 'c2', 'Z', 'p', 'r')
  comparison.df <- apply.FDR.correction(comparison.df)
  comparison.df
}

## pairwise comparison for 4 clusters
comparison.4hcl <- pairwise.fexam.compare(4, 6, hcl4.plus.scores)
kable(x = comparison.4hcl, format = 'rst')

## pairwise comparison for 5 clusters
comparison.5hcl <- pairwise.fexam.compare(5, 10, hcl5.plus.scores)
kable(x = comparison.5hcl, format = 'rst')

## compute summary statistics for the midterm exam score for each class
mt.summary.4cl <- midterm.score.per.class.stats(hcl4.plus.scores)                  
kable(x = mt.summary.4cl, format = "rst")
mt.summary.5cl <- midterm.score.per.class.stats(hcl5.plus.scores)                  
kable(x = mt.summary.5cl, format = "rst")

## the midterm exam score is not normaly distributed (checked before)
## use non-parametric test to examine differences across the clusters
kruskal.test(hcl4.plus.scores$SC_MT_TOT ~ hcl4.plus.scores$class)
## chi-squared = 39.048, df = 3, p-value = 1.695e-08
## there is a sign. difference bewtween the classes; check where the difference is

kruskal.test(hcl5.plus.scores$SC_MT_TOT ~ hcl5.plus.scores$class)
## chi-squared = 39.102, df = 4, p-value = 6.636e-08

## apply Mann-Whitney U Test to do pair-wise comparisons
pairwise.mtxam.compare <- function(nclust, ncomparison, hcl.plus.scores) {
  comparison <- matrix(nrow = ncomparison, ncol = 5, byrow = T)
  k <- 1
  for(i in 1:(nclust-1)) {
    for(j in (i+1):nclust) {
      comparison[k,] <- c(i, j, compare.midterm.scores.Mann.Whitney.test(hcl.plus.scores, i, j))
      k <- k+1
    }
  }
  comparison.df <- as.data.frame(comparison)
  colnames(comparison.df) <- c('c1', 'c2', 'Z', 'p', 'r')
  comparison.df <- apply.FDR.correction(comparison.df)
  comparison.df
}

comparison.4hcl <- pairwise.mtxam.compare(4, 6, hcl4.plus.scores)
kable(x = comparison.4hcl, format = 'rst')

comparison.5hcl <- pairwise.mtxam.compare(5, 10, hcl5.plus.scores)
kable(x = comparison.5hcl, format = 'rst')


###########################################################################
## EXAMINE THE DISTRIBUTION OF THE IDENTIFIED LEARNING STRATEGIES OVER  
## THE 12 WEEKS OF THE COURSE; DO THAT FOR EACH STUDENT CLUSTER SEPARATELY
###########################################################################

## collect the data required for the analysis

## get sequence cluster assignment
sequence.clusters <- read.csv(file = "results/all_stud_sequences_4_seq_clusters.csv")
str(sequence.clusters)
colnames(sequence.clusters)[2:3] <- c('seq.id', 'seq.cluster')

## get course weeks for the sequences
traces <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv")
str(traces)
traces <- traces[,c(1,4,5)]
colnames(traces) <- c('seq.id', 'stud.id', 'week')
## keep only data for weeks 2-13
traces <- subset(traces, week > 1 & week < 14)
## associate each sequence with the week it belongs to
unique.seq.ids <- unique(traces$seq.id) 
seq.weeks <- vector(length = length(unique.seq.ids))
for(i in 1:length(unique.seq.ids)) {
  seq.id <- unique.seq.ids[i]
  seq.w <- unique(traces[traces$seq.id == seq.id,3])
  seq.weeks[i] <- seq.w[1]
}
seq.weeks.df <- data.frame(seq.id=unique.seq.ids, seq.week=seq.weeks)
str(seq.weeks.df)


## f. that creates a dataframe for one student cluster 
## with the following columns:
## - student id
## - seq.cluster (learning strategy)
## - week2 - week13 - 12 columns, each keeping the number of learning sequences of the 
##   given student in the given week that belong to the given sequence cluster  
create.weekly.strat.df.for.stud.clust <- function(stud.clust, nstrat) {
  
  stud.clust.data <- subset(strat.data, stud.cluster == stud.clust)
  stud.clust.data <- stud.clust.data[,-5]
  stud.clust.data <- stud.clust.data[ order(stud.clust.data$stud.id, stud.clust.data$seq.cluster), ]
  
  sclust.weekly <- data.frame()
  sclust.ids <- sort(unique(stud.clust.data$stud.id))
  row.cnt <- 1
  for(i in 1:length(sclust.ids)) {
    s.id <- sclust.ids[i]
    for(j in 1:nstrat) {
      sclust.weekly[row.cnt, 1] <- s.id
      sclust.weekly[row.cnt, 2] <- j
      weekly.strat.data <- subset(stud.clust.data, stud.id == s.id & seq.cluster == j)
      for(w in 2:13) {
        sclust.weekly[row.cnt, 1+w] <- nrow(weekly.strat.data[weekly.strat.data$seq.week == w,])
      }
      row.cnt <- row.cnt + 1
    }
  }
  colnames(sclust.weekly) <- c('stud.id', 'strat.id', paste("week", 2:13, sep = ""))
  sclust.weekly
  
}

## f. that creates a dataframe for one student cluster 
## with the following columns:
## - id of sequence cluster (learning strategy)
## - week2 - week13 - 12 columns, each keeping the median number of learning 
##   sequences in the given week that belong to the given sequence cluster 
compute.weekly.medians.for.stud.clust <- function(stud.clust.data, nstrat) {
  sclust.strat.medians <- data.frame()
  k <- 1
  for(i in 1:nstrat) {
    strat.weekly <- stud.clust.data[stud.clust.data$strat.id == i, ]
    for(j in 3:14) { # weekly counts are in the columns 3:14
      sclust.strat.medians[k,1] <- i
      sclust.strat.medians[k,2] <- (j-1)
      sclust.strat.medians[k,3] <- median(strat.weekly[,j])
      k <- k + 1
    }
  }
  colnames(sclust.strat.medians) <- c('strat.id', 'week', 'median')
  sclust.strat.medians
}

## f. that creates a plot of learning strategies for the given student cluster
## based on: http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/

###### ----------- TODO: UPDATE THE F. TO INCLUDE THE NAME OF THE CLUSTER -----------
### -------- ALSO ADD LEGEND DESCRIPTION

plot.strat.for.stud.clust <- function(sclust.strat.medians, cl.name, legend.labels) {
  gp <- ggplot(data=sclust.strat.medians, 
             aes(x=week, y=median, group=strat.id, color=strat.id, shape=strat.id)) +
        geom_line() +
        geom_point() +
        scale_colour_hue(name="Learning strategies\n",
                         labels=legend.labels) +
        scale_shape(name="Learning strategies\n",
                    labels=legend.labels) +
        scale_x_continuous(breaks=seq(2,13,1)) +
        ylim(0,5) +  
        xlab("\nCourse weeks") + ylab("Median num. of sequences\n") + 
        ggtitle(paste("\n", cl.name, "group")) +  
        theme_bw() +
        theme(legend.position = "right",
              axis.title.x = element_text(size=11),
              axis.title.y = element_text(size=11),
              legend.key = element_rect(colour = NA)) 
  gp
}

## taken from: 
## http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

##############################################################################
## FIRST DO THE ANALYSIS USING THE CLUSTERING SOLUTION WITH 4 STUDENT CLUSTERS
##############################################################################

## get student cluster assignment
student.clusters <- read.csv(file = "results/sequence_based_student_clusters_4cl.csv")
str(student.clusters)
student.clusters <- student.clusters[,c(1,7)]
colnames(student.clusters)[2] <- 'stud.cluster'

## merge the 3 datasets
## first connect (clustered) sequences with the course weeks they originate from 
strat.data <- merge(x = sequence.clusters, y = seq.weeks.df, by = 'seq.id',
                    all.x = T, all.y = F)
str(strat.data)
## now, connect students with the clusters they were assigned to
strat.data <- merge(x = strat.data, y = student.clusters, by = 'stud.id',
                    all.x = T, all.y = T)
str(strat.data)
## change the order of columns
strat.data <- strat.data[,c(2,3,4,1,5)]
## save the data
write.csv(x = strat.data, file = "Intermediate_files/student_and_sequence_clusters_4cl.csv",
          row.names = F, quote = F)


nstrat <- length(unique(strat.data$seq.cluster))
## df for the 1st student cluster
stud.clust1.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 1, nstrat)
## df for the 2nd student cluster
stud.clust2.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 2, nstrat)
## df for the 3rd student cluster
stud.clust3.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 3, nstrat)
## df for the 4th student cluster
stud.clust4.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 4, nstrat)


sclust1.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust1.data, nstrat)
sclust1.strat.medians$strat.id <- factor(sclust1.strat.medians$strat.id)
sclust2.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust2.data, nstrat)
sclust2.strat.medians$strat.id <- factor(sclust2.strat.medians$strat.id)
sclust3.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust3.data, nstrat)
sclust3.strat.medians$strat.id <- factor(sclust3.strat.medians$strat.id)
sclust4.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust4.data, nstrat)
sclust4.strat.medians$strat.id <- factor(sclust4.strat.medians$strat.id)


library(ggplot2)
strategy.labels <- c('focus on formative assessment',
                     'focus on summative assessment',
                     'focus on reading materials',
                     'focus on videos')
## plot for student cluster 1
gp.clust1 <- plot.strat.for.stud.clust(sclust1.strat.medians, 1, strategy.labels)
gp.legend <- get_legend(gp.clust1)
## remove the legend from the box plot (to plot it separately)
gp.clust1 <- gp.clust1 + theme(legend.position="none")
## plot for student cluster 2
gp.clust2 <- plot.strat.for.stud.clust(sclust2.strat.medians, 2, strategy.labels)
gp.clust2 <- gp.clust2 + theme(legend.position="none")
## plot for student cluster 3
gp.clust3 <- plot.strat.for.stud.clust(sclust3.strat.medians, 3, strategy.labels)
gp.clust3 <- gp.clust3 + theme(legend.position="none")
## plot for student cluster 4
gp.clust4 <- plot.strat.for.stud.clust(sclust4.strat.medians, 4, strategy.labels)
gp.clust4 <- gp.clust4 + theme(legend.position="none")

#library(gridExtra)
## arrange the plots
grid.arrange(gp.clust1, gp.clust2, gp.clust3, gp.clust4, gp.legend, 
             ncol=2, nrow=3, widths=c(2.4, 2.4), heights=c(2.4, 2.4, 0.3),
             layout_matrix = rbind(c(1,2), c(3,4), c(5,5)))


###################################################################################
## NOW, DO THE SAME ANALYSIS USING THE CLUSTERING SOLUTION WITH 5 STUDENT CLUSTERS
###################################################################################

## get student cluster assignment
student.clusters <- read.csv(file = "results/sequence_based_student_clusters_5cl.csv")
str(student.clusters)
student.clusters <- student.clusters[,c(1,7)]
colnames(student.clusters)[2] <- 'stud.cluster'

## merge the 3 datasets
## first connect (clustered) sequences with the course weeks they originate from 
strat.data <- merge(x = sequence.clusters, y = seq.weeks.df, by = 'seq.id',
                    all.x = T, all.y = F)
str(strat.data)
## now, connect students with the clusters they were assigned to
strat.data <- merge(x = strat.data, y = student.clusters, by = 'stud.id',
                    all.x = T, all.y = T)
str(strat.data)
## change the order of columns
strat.data <- strat.data[,c(2,3,4,1,5)]
## save the data
write.csv(x = strat.data, file = "Intermediate_files/student_and_sequence_clusters_5cl.csv",
          row.names = F, quote = F)

strat.data <- read.csv(file = "Intermediate_files/student_and_sequence_clusters_5cl.csv")

nstrat <- length(unique(strat.data$seq.cluster))
## df for the 1st student cluster
stud.clust1.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 1, nstrat)
## df for the 2nd student cluster
stud.clust2.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 2, nstrat)
## df for the 3rd student cluster
stud.clust3.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 3, nstrat)
## df for the 4th student cluster
stud.clust4.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 4, nstrat)
## df for the 5th student cluster
stud.clust5.data <- create.weekly.strat.df.for.stud.clust(stud.clust = 5, nstrat)

## for each student cluster, compute median number of learning sequences for 
## each learning strategy (i.e., sequence cluster) and each course week (2-13)
sclust1.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust1.data, nstrat)
sclust1.strat.medians$strat.id <- factor(sclust1.strat.medians$strat.id)
sclust2.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust2.data, nstrat)
sclust2.strat.medians$strat.id <- factor(sclust2.strat.medians$strat.id)
sclust3.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust3.data, nstrat)
sclust3.strat.medians$strat.id <- factor(sclust3.strat.medians$strat.id)
sclust4.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust4.data, nstrat)
sclust4.strat.medians$strat.id <- factor(sclust4.strat.medians$strat.id)
sclust5.strat.medians <- compute.weekly.medians.for.stud.clust(stud.clust5.data, nstrat)
sclust5.strat.medians$strat.id <- factor(sclust5.strat.medians$strat.id)


library(ggplot2)
strategy.labels <- c('focus on formative assessment',
                   'focus on summative assessment',
                   'focus on reading materials',
                   'focus on videos')
## plot for student cluster 1
gp.clust1 <- plot.strat.for.stud.clust(sclust1.strat.medians, 'Intensive', strategy.labels)
gp.legend <- get_legend(gp.clust1)
## remove the legend from the box plot (to plot it separately)
gp.clust1 <- gp.clust1 + theme(legend.position="none")
## plot for student cluster 2
gp.clust2 <- plot.strat.for.stud.clust(sclust2.strat.medians, 'Strategic', strategy.labels)
gp.clust2 <- gp.clust2 + theme(legend.position="none")
## plot for student cluster 3
gp.clust3 <- plot.strat.for.stud.clust(sclust3.strat.medians, 'Highly Stategic', strategy.labels)
gp.clust3 <- gp.clust3 + theme(legend.position="none")
## plot for student cluster 4
gp.clust4 <- plot.strat.for.stud.clust(sclust4.strat.medians, 'Selective', strategy.labels)
gp.clust4 <- gp.clust4 + theme(legend.position="none")
## plot for student cluster 5
gp.clust5 <- plot.strat.for.stud.clust(sclust5.strat.medians, 'Highly Selective', strategy.labels)
gp.clust5 <- gp.clust5 + theme(legend.position="none")

library(gridExtra)
## arrange the plots
grid.arrange(gp.clust1, gp.clust2, gp.clust3, gp.clust4, gp.clust5, gp.legend, 
             ncol=2, nrow=3, widths=c(2.4, 2.4), heights=c(2.4, 2.4, 2.4),
             layout_matrix = rbind(c(1,2), c(3,4), c(5,6)))

