################################################################################################
## CLUSTER STUDENTS BASED ON THE VARIABLES DERIVED THROUGH THE STUDY PROCESS QUESTIONNAIRE (SPQ)
## IN PARTICULAR, THE FOLLOWING VARIABLES ARE USED:
## - DA: Deep approach
## - SA: Surface approach
## - DM: Deep motive
## - SM: Surface motive
## - DS: Deep strategy
## - SS: Surface strategy
################################################################################################

## get the students' scores on the above given SPQ variables
## Load all the scores
all.scores <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv")
## keep only the SPQ-related scores 
spq.scores <- all.scores[, c('user_id', 'SPQ_DA', 'SPQ_SA', 'SPQ_DM', 'SPQ_SM', 'SPQ_DS', 'SPQ_SS')]
str(spq.scores)

## check the number of students who filled in the questionnaire
length(which(complete.cases(spq.scores)))
## 144

## keep only those observtions where SPQ variables have value
## (students who filled in the SPQ questionnaire)
spq.scores <- spq.scores[which(complete.cases(spq.scores)),]
summary(spq.scores)
## all the variables have the same range, so, no need for normalization

set.seed(0106)
## next, compute the distance between the observations
distance <- dist(spq.scores[,c(4:7)])
## use the computed distances to do the clustering
hc <- hclust(distance, method = "ward.D2")
## plot the clustering tree
plot(hc, cex=0.69, main = "Student clustering")
rect.hclust(hc, k=3)

# examine clusters for different values of k
clusters <- sapply(2:6, function(ncl) table(cutree(hc, k = ncl)))
names(clusters) <- c(2:6)
clusters


## load the data about the observed learning strategies
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
create.stud.strat.summary <- function(hcl.df) {
  stud.summary <- hcl.df[,c(1,8)]
  colnames(stud.summary) <- c('stud.id','hcl')
  n <- nrow(stud.summary)
  stud.summary$strat1 <- vector(mode = 'numeric', length = n)
  stud.summary$strat2 <- vector(mode = 'numeric', length = n)
  stud.summary$strat3 <- vector(mode = 'numeric', length = n)
  stud.summary$strat4 <- vector(mode = 'numeric', length = n)
  for (i in 1:n) {
    current.stud.id <- stud.summary[i,1] 
    stud.summary[i,c(3:6)] <- count.sessions.per.strategy(strategies, current.stud.id)
  }
  stud.summary
}

#######################################
## examine the solution with 3 clusters
#######################################
plot(hc, cex=0.69, main = "Student clustering")
rect.hclust(hc, k=3)
hcl3 <- cutree(hc, k = 3) 
hcl3.df <- spq.scores
hcl3.df$cluster <- hcl3

## compute descriptive statistics for the clusters
source(file = "util_functions.R")
## check if variables are normally distributed
apply(X = spq.scores[,c(4:7),], MARGIN = 2, FUN = shapiro.test)
## all except SPQ_SM are normally distr.
hcl3.stats <- summary.stats2(spq.scores[,c(4:7)], hcl3)
require(knitr)
kable(hcl3.stats, format = 'rst')

stud.summary <- create.stud.strat.summary(hcl3.df)
str(stud.summary)

## use variables strat1 - strat4 to compare the SPQ-based clusters 
## first check if variables are normally distributed
apply(stud.summary, 2, shapiro.test)
## far from normal distr.
hcl3.strat.stats <- summary.stats(stud.summary[,c(3:6)], stud.summary$hcl, 3)
kable(hcl3.strat.stats, format = 'rst')

## check if there are sign. difference among the clusters 
kruskal.test(strat1 ~ hcl, data = stud.summary)
## chi-squared = 1.2233, df = 2, p-value = 0.5424 NO DIFF
kruskal.test(strat2 ~ hcl, data = stud.summary)
## chi-squared = 0.041047, df = 2, p-value = 0.9797 NO DIFF
kruskal.test(strat3 ~ hcl, data = stud.summary)
## chi-squared = 3.1872, df = 2, p-value = 0.2032 NO DIFF
kruskal.test(strat4 ~ hcl, data = stud.summary)
## chi-squared = 2.8073, df = 2, p-value = 0.2457 NO DIFF


#######################################
## examine the solution with 2 clusters
#######################################
plot(hc, cex=0.69, main = "Student clustering")
rect.hclust(hc, k=2)
hcl2 <- cutree(hc, k = 2) 
hcl2.df <- spq.scores
hcl2.df$cluster <- hcl2

## compute descriptive statistics for the clusters
hcl2.stats <- summary.stats2(spq.scores[,c(4:7)], hcl2)
kable(hcl2.stats, format = 'rst')

stud.summary <- create.stud.strat.summary(hcl2.df)
str(stud.summary)

## use variables strat1 - strat4 to compare the SPQ-based clusters 
## first check if variables are normally distributed
apply(stud.summary, 2, shapiro.test)
## far from normal distr.
hcl2.strat.stats <- summary.stats(stud.summary[,c(3:6)], stud.summary$hcl, 2)
kable(hcl2.strat.stats, format = 'rst')

## check if there are sign. difference among the clusters 
kruskal.test(strat1 ~ hcl, data = stud.summary)
## chi-squared = 0.046075, df = 1, p-value = 0.83 NO DIFF
kruskal.test(strat2 ~ hcl, data = stud.summary)
## chi-squared = 0.039932, df = 1, p-value = 0.8416 NO DIFF
kruskal.test(strat3 ~ hcl, data = stud.summary)
## chi-squared = 2.0248, df = 1, p-value = 0.1547 NO DIFF
kruskal.test(strat4 ~ hcl, data = stud.summary)
## chi-squared = 2.4738, df = 1, p-value = 0.1158 NO DIFF


#######################################
## examine the solution with 4 clusters
#######################################
plot(hc, cex=0.69, main = "Student clustering")
rect.hclust(hc, k=4)
hcl4 <- cutree(hc, k = 4) 
hcl4.df <- spq.scores
hcl4.df$cluster <- hcl4

## compute descriptive statistics for the clusters
hcl4.stats <- summary.stats2(spq.scores[,c(4:7)], hcl4)
kable(hcl4.stats, format = 'rst')

stud.summary <- create.stud.strat.summary(hcl4.df)
str(stud.summary)

## use variables strat1 - strat4 to compare the SPQ-based clusters 
## first check if variables are normally distributed
apply(stud.summary, 2, shapiro.test)
## far from normal distr.
hcl4.strat.stats <- summary.stats(stud.summary[,c(3:6)], stud.summary$hcl, 4)
kable(hcl4.strat.stats, format = 'rst')

## check if there are sign. difference among the clusters 
kruskal.test(strat1 ~ hcl, data = stud.summary)
## chi-squared = 2.1693, df = 3, p-value = 0.538 NO DIFF
kruskal.test(strat2 ~ hcl, data = stud.summary)
## chi-squared = 0.86497, df = 3, p-value = 0.8339 NO DIFF
kruskal.test(strat3 ~ hcl, data = stud.summary)
## chi-squared = 3.5747, df = 3, p-value = 0.3112 NO DIFF
kruskal.test(strat4 ~ hcl, data = stud.summary)
## chi-squared = 4.2226, df = 3, p-value = 0.2384 NO DIFF