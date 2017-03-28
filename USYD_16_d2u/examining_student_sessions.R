traces <- readRDS(file = "Intermediate_results/trace_data_with_sessions_w2-13.RData")
str(traces)

## compute the number of sessions per student and per week
## create a matrix with students in the rows and weekly session counts in the columns
stud.ids <- unique(traces$USER_ID)
n.stud <- length(stud.ids)
sessions.m <- matrix(nrow = n.stud, ncol = 13, byrow = T,
                     data = rep(x = 0, times=(n.stud*13))) 
for(i in 1:n.stud) {
  stud.data <- subset(traces, USER_ID==stud.ids[i])
  weekly.counts <- tapply(X = stud.data$SESSION_ID, INDEX = stud.data$WEEK,
                                    FUN = function(x) {length(unique(x))})
  w.cnts.df <- data.frame(week=as.integer(unlist(dimnames(weekly.counts))), 
                            freq=as.vector(weekly.counts), stringsAsFactors = F)
  
  ## if the data is available for all the course weeks
  if (nrow(w.cnts.df)==12) { 
    sessions.m[i,] <- c(stud.ids[i], w.cnts.df$freq)
    next
  }
  
  ## if the data (sessions) is not available for all the course weeks
  ## find which weeks that lack the data (sessions)
  missing.weeks <- setdiff(c(2:13), w.cnts.df$week)
  ## add zeros for such weeks
  for(j in 1:length(missing.weeks)) {
    w.cnts.df <- as.data.frame(rbind(w.cnts.df, c(missing.weeks[j],0)))
  }
  ## now, sort the w.cnts.df based on the week column so that
  ## weekly counts (frequences) are given in the expected order
  w.cnts.df <- w.cnts.df[ order(w.cnts.df$week, decreasing = F),]
  sessions.m[i,] <- c(stud.ids[i], w.cnts.df$freq)
}

head(sessions.m)
tail(sessions.m)
sessions.df <- as.data.frame(x = sessions.m, row.names = NULL, stringsAsFactors = F)
colnames(sessions.df) <- c("USER_ID", paste0("W", c(2:13)))

## save the data
saveRDS(object = sessions.df, file = "Intermediate_results/weekly_per_student_session_counts.RData")

#######################################################################################
## transform weekly session counts into multinomial variables, each one 
## with 5 possible values: 
## - 0 - student had no sessions in the given week 
## - 1 - the student's number of sessions belongs to the 1st quartile for the given week 
## - 2 - the student's number of sessions belongs to the 2nd quartile for the given week 
## - 3 - the student's number of sessions belongs to the 3rd quartile for the given week 
## - 4 - the student's number of sessions belongs to the 4th quartile for the given week
## 
## then, use these data to group students (clustering, LCA)
########################################################################################

session.counts <- readRDS(file = "Intermediate_results/weekly_per_student_session_counts.RData")

## create a new data frame with multinomial variables as described above
multinom.sdata <- session.counts
for(j in 2:13) {
  ## q includes week, 25th perc, median, 75th perc 
  q <- quantile(x = session.counts[,j], probs = c(0.25, 0.5, 0.75), names = F)
  multinom.sdata[,j][multinom.sdata[,j] > 0 & multinom.sdata[,j] <= q[1]] <- 1
  multinom.sdata[,j][multinom.sdata[,j] > q[1] & multinom.sdata[,j] <= q[2]] <- 2
  multinom.sdata[,j][multinom.sdata[,j] > q[2] & multinom.sdata[,j] <= q[3]] <- 3
  multinom.sdata[,j][multinom.sdata[,j] > q[3]] <- 4
}

## turn into factor variables
multinom.sdata[,c(2:13)] <- as.data.frame(apply(multinom.sdata[,c(2:13)], 2, factor))
head(multinom.sdata)

####################################################################################
## use LCA to group students based on the session distribution throughout the course
## use the computed multinomial variables as the features
####################################################################################

library(poLCA)

## increment all the data points by 1, as LCA does not work with zero values
# for(j in 2:13) {
#   multinom.sdata[,j] <- multinom.sdata[,j] + 1
# }

weekly.sessions <- cbind(W2, W3, W4, W5, W6, W7, W8, W9, W10, W11, W12, W13) ~ 1

eval.metrics <- data.frame()
set.seed(20032017)
for(i in 2:7) {
  lc <- poLCA(weekly.sessions, data = multinom.sdata, nrep = 50, 
              nclass = i, verbose = F, na.rm = F)
  metrics <- c(nclass = i, 
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

## examine the model with 3 classes (BIC and cAIC drop and then rise again)
set.seed(20032017)
lc3 <- poLCA(weekly.sessions, data = multinom.sdata, nrep = 50, 
             nclass = 3, graphs = T, na.rm = F)
probs.start <- poLCA.reorder(lc3$probs.start, order(lc3$P,decreasing=T))
lc3 <- poLCA(weekly.sessions, data = multinom.sdata, graphs = T, 
             nclass = 3, probs.start = probs.start, na.rm = F)

## add the predicted class to each observation
lc3.results <- multinom.sdata
lc3.results$LCA_CL <- as.factor(lc3$predclass)
str(lc3.results)
table(lc3.results$LCA_CL)
prop.table(table(lc3.results$LCA_CL))

## write the features + the LCA class values to a file
write.csv(x = lc3.results, file = "results/LCA_based_on_weekly_session_counts.csv",
          quote = F, row.names = F)


## plot the classes in the manner suggested at:
## http://statistics.ohlsen-web.de/latent-class-analysis-polca/
require(reshape2)

lc3.model <- reshape2::melt(lc3$probs)
lc3.model$L1 <- factor(lc3.model$L1, 
                       levels = c('W2','W3','W4','W5','W6','W7',
                                  'W8','W9','W10','W11','W12','W13'))
lc3.model$Var2 <- factor(lc3.model$Var2, 
                         labels = c('NONE','Q1','Q2','Q3','Q4'))
lc3.model$Var1 <- factor(lc3.model$Var1, 
                         labels = c('group 1','group 2','group 3'))
str(lc3.model)

require(ggplot2)
custom.pallet <- c('#bebada','#ffffb3', '#80b1d3', '#8dd3c7', '#fb8072')
zp1 <- ggplot(lc3.model,aes(x = L1, y = value, fill = Var2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_grid(Var1 ~ .) 
#zp1 <- zp1 + scale_fill_manual(values=custom.pallet) 
zp1 <- zp1 + scale_fill_brewer() + theme_bw()
zp1 <- zp1 + labs(x = "Course weeks", y="Probability", fill ="Session count quartiles")
zp1 <- zp1 + theme( axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),                    
                    panel.grid.major.y=element_blank())
zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
print(zp1)


#############################################################
## examine exam scores of students from different LCA groups
#############################################################

scores <- read.csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
scores <- scores[,-2]

lca.and.scores <- merge(x = lc3.results[,c(1,14)], y = scores,
                        by = "USER_ID", all.x = T, all.y = F)

mt.stats <- quantile(x = lca.and.scores$SC_MT_TOT, probs = c(0.25,0.5,0.75), 
                     na.rm = T, names = F)
fe.stats <- quantile(x = lca.and.scores$SC_FE_TOT, probs = c(0.25,0.5,0.75), 
                     na.rm = T, names = F)

## for each student, determine the quartile they belong to w.r.t. their 
## midterm and final exam scores
lca.and.scores$MT_QUARTILE <- "Q1"
lca.and.scores$MT_QUARTILE[lca.and.scores$SC_MT_TOT > mt.stats[1] &
                               lca.and.scores$SC_MT_TOT <= mt.stats[2]] <- "Q2"
lca.and.scores$MT_QUARTILE[lca.and.scores$SC_MT_TOT > mt.stats[2] &
                               lca.and.scores$SC_MT_TOT <= mt.stats[3]] <- "Q3"
lca.and.scores$MT_QUARTILE[lca.and.scores$SC_MT_TOT > mt.stats[3]] <- "Q4"
lca.and.scores$MT_QUARTILE <- as.factor(lca.and.scores$MT_QUARTILE)

lca.and.scores$FE_QUARTILE <- "Q1"
lca.and.scores$FE_QUARTILE[lca.and.scores$SC_FE_TOT > fe.stats[1] &
                             lca.and.scores$SC_FE_TOT <= fe.stats[2]] <- "Q2"
lca.and.scores$FE_QUARTILE[lca.and.scores$SC_FE_TOT > fe.stats[2] &
                             lca.and.scores$SC_FE_TOT <= fe.stats[3]] <- "Q3"
lca.and.scores$FE_QUARTILE[lca.and.scores$SC_FE_TOT > fe.stats[3]] <- "Q4"
lca.and.scores$FE_QUARTILE <- as.factor(lca.and.scores$FE_QUARTILE)


## examine midterm scores of the students from the low engagement LCA group
table(lca.and.scores$MT_QUARTILE[lca.and.scores$LCA_CL==3])
## check now for all the students
with(lca.and.scores, table(LCA_CL, MT_QUARTILE))
with(lca.and.scores, round(prop.table(table(LCA_CL, MT_QUARTILE), 
                                      margin = 1), digits = 3))

## use statistical tests to check for difference
with(lca.and.scores, kruskal.test(SC_MT_TOT ~ LCA_CL))
# chi-squared = 82.511, df = 2, p-value < 2.2e-16

## apply Mann-Whitney U Test to do pair-wise comparisons
## of LCA groups based on student midterm scores
source(file = "util_functions.R")
colnames(lca.and.scores)[2] <- "class"
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
kable(pairwise.mtxam.compare(3, 3, lca.and.scores), format = "rst")


## do the same for final exam scores

## examine final exam scores of the students from the low engagement LCA group
table(lca.and.scores$FE_QUARTILE[lca.and.scores$class==3])
## check now for all the students
with(lca.and.scores, table(class, FE_QUARTILE))
with(lca.and.scores, round(prop.table(table(class, FE_QUARTILE), 
                                      margin = 1), digits = 3))

## use statistical tests to check for difference
with(lca.and.scores, kruskal.test(SC_FE_TOT ~ class))
# chi-squared = 117.66, df = 2, p-value < 2.2e-16

## apply Mann-Whitney U Test to do pair-wise comparisons
## of LCA groups based on student final exam scores
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
kable(pairwise.fexam.compare(3, 3, lca.and.scores), format = "rst")


## examine more closely the students from the 3rd LCA group with high scores

colnames(lca.and.scores)[2] <- "LCA_CL"
table(lca.and.scores$MT_QUARTILE[lca.and.scores$LCA_CL==3])
# Q1 Q2 Q3 Q4 
# 50 27 19  3 
## look into the session counts of students from Q3 and Q4
to.examine <- lca.and.scores$USER_ID[lca.and.scores$LCA_CL==3 &
                                       lca.and.scores$MT_QUARTILE %in% c("Q3", "Q4")]
## merge session counts and scores to examine the selected students
counts.and.scores <- merge(x = session.counts, y = lca.and.scores,
                           by = "USER_ID", all = T)
colnames(counts.and.scores)[17:18] <- c("MT_Q", "FE_Q")
lca3.stats <- counts.and.scores[counts.and.scores$USER_ID %in% to.examine, c(1:13,17,18)]
kable(lca3.stats, format = "pandoc")
