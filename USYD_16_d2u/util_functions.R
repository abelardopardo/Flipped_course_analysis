## f. for identifying the best and the worst performing students, based on exam scores
## the f. returns a list with 4 elements: 
## - top10 - students with midterm and final exam scores above the 90th percentile 
## - top25 - students with midterm and final exam scores above the 75th percentile 
## - worst10 - students with midterm and final exam scores below the 10th percentile 
## - worst25 - students with midterm and final exam scores below the 25th percentile
high.and.low.achievers <- function(scores) {
  
  ## first remove scores that are NAs
  no.scores <- which(complete.cases(scores)==F)
  scores <- scores[-no.scores,]
  
  ## first identify students with high and low performance on the midterm exam
  midterm.percentiles <- quantile(x = scores$SC_MT_TOT, probs = c(0.10, 0.15, 0.2, 0.25, 0.75, 0.8, 0.85, 0.90))
  top10.midterm <- scores$user_id[scores$SC_MT_TOT > midterm.percentiles[8]]
  top15.midterm <- scores$user_id[scores$SC_MT_TOT > midterm.percentiles[7]]
  top20.midterm <- scores$user_id[scores$SC_MT_TOT > midterm.percentiles[6]]
  top25.midterm <- scores$user_id[scores$SC_MT_TOT > midterm.percentiles[5]]
  bottom25.midterm <- scores$user_id[scores$SC_MT_TOT < midterm.percentiles[4]]
  bottom20.midterm <- scores$user_id[scores$SC_MT_TOT < midterm.percentiles[3]]
  bottom15.midterm <- scores$user_id[scores$SC_MT_TOT < midterm.percentiles[2]]
  bottom10.midterm <- scores$user_id[scores$SC_MT_TOT < midterm.percentiles[1]]
  
  ## then, identify students with high and low performance on the final exam
  fexam.percentiles <- quantile(x = scores$SC_FE_TOT, probs = c(0.10, 0.15, 0.2, 0.25, 0.75, 0.8, 0.85, 0.90))
  top10.fexam <- scores$user_id[scores$SC_FE_TOT > fexam.percentiles[8]]
  top15.fexam <- scores$user_id[scores$SC_FE_TOT > fexam.percentiles[7]]
  top20.fexam <- scores$user_id[scores$SC_FE_TOT > fexam.percentiles[6]]
  top25.fexam <- scores$user_id[scores$SC_FE_TOT > fexam.percentiles[5]]
  bottom25.fexam <- scores$user_id[scores$SC_FE_TOT < fexam.percentiles[4]]
  bottom20.fexam <- scores$user_id[scores$SC_FE_TOT < fexam.percentiles[3]]
  bottom15.fexam <- scores$user_id[scores$SC_FE_TOT < fexam.percentiles[2]]
  bottom10.fexam <- scores$user_id[scores$SC_FE_TOT < fexam.percentiles[1]]
  
  ## identify students with high and low performance on both midterm and final exams
  result <- list()
  result$top10 <- intersect(top10.midterm, top10.fexam)
  result$top15 <- intersect(top15.midterm, top15.fexam)
  result$top20 <- intersect(top20.midterm, top20.fexam)
  result$top25 <- intersect(top25.midterm, top25.fexam)
  result$worst10 <- intersect(bottom10.midterm, bottom10.fexam)
  result$worst15 <- intersect(bottom15.midterm, bottom15.fexam)
  result$worst20 <- intersect(bottom20.midterm, bottom20.fexam)
  result$worst25 <- intersect(bottom25.midterm, bottom25.fexam)
  
  result
} 

# f. generates summary statistics aimed at giving some insight into the clusters
# the first paramter is the original feature set (not normalized data)
# the 2nd parameter is a vector with cluster identifiers 
# (indicating the cluster each student belongs to)
# the 3rd parameter is the number of clusters
summary.stats <- function(feature.set, clusters, cl.number) {
  sum.stats <- aggregate(x = feature.set, 
                         by = list(clusters), 
                         FUN = function(x) { 
                           q <- quantile(x, probs = c(0.25, 0.5, 0.75), names = F, na.rm = T)
                           paste(round(q[2], digits = 2), "; (", 
                                 round(q[1], digits = 2), ", ",
                                 round(q[3], digits = 2), ")", sep = "")
                         })
  sum.stat.df <- data.frame(cluster = sum.stats[,1], 
                            freq = as.vector(table(clusters)),
                            sum.stats[,-1])
  
  sum.stats.transpose <- t( as.matrix(sum.stat.df) )
  sum.stats.transpose <- as.data.frame(sum.stats.transpose)
  attributes <- rownames(sum.stats.transpose)
  sum.stats.transpose <- as.data.frame( cbind(attributes, sum.stats.transpose) )
  colnames(sum.stats.transpose) <- c( "attributes", rep("Median; (Q1, Q3)", cl.number) )
  rownames(sum.stats.transpose) <- NULL
  
  sum.stats.transpose
  
}


################################################################################
## UTILITY FUNCTIONS REQUIRED FOR COMPARISONS OF STUDENT GROUPS/CLASSES/CLUSTERS
################################################################################

## f. for computing summary statistics for the final exam score for each class
final.score.per.class.stats <- function(fscore.data) {
  require(plyr)
  desc.stats <- ddply(fscore.data, 
                      c('class'), 
                      summarise,
                      N    = length(SC_FE_TOT),
                      median = round(median(SC_FE_TOT, na.rm = T), digits = 2),
                      Q1  = round(quantile(SC_FE_TOT, probs = 0.25, names = F, na.rm = T), digits = 2),
                      Q3  = round(quantile(SC_FE_TOT, probs = 0.75, names = F, na.rm = T), digits = 2))
  desc.stats
}

## f. for computing summary statistics for the midterm exam score for each class
midterm.score.per.class.stats <- function(mtscore.data) {
  require(plyr)
  desc.stats <- ddply(mtscore.data, 
                      c('class'), 
                      summarise,
                      N    = length(SC_MT_TOT),
                      median = round(median(SC_MT_TOT, na.rm = T), digits = 2),
                      Q1  = round(quantile(SC_MT_TOT, probs = 0.25, names = F, na.rm = T), digits = 2),
                      Q3  = round(quantile(SC_MT_TOT, probs = 0.75, names = F, na.rm = T), digits = 2))
  desc.stats
}


## f. for performing Mann-Whitney U Test on the dataset 
## with students' final exam scores
compare.fexam.scores.Mann.Whitney.test <- function(scores.data, class1, class2) {
  score.c1 <- scores.data$SC_FE_TOT[scores.data$class == class1]
  score.c2 <- scores.data$SC_FE_TOT[scores.data$class == class2]
  do.Mann.Whitney.test(score.c1, score.c2, class1, class2)
}

## f. for performing Mann-Whitney U Test on the dataset 
## with students' midterm exam scores
compare.midterm.scores.Mann.Whitney.test <- function(scores.data, class1, class2) {
  score.c1 <- scores.data$SC_MT_TOT[scores.data$class == class1]
  score.c2 <- scores.data$SC_MT_TOT[scores.data$class == class2]
  do.Mann.Whitney.test(score.c1, score.c2, class1, class2)
}

do.Mann.Whitney.test <- function(score.c1, score.c2, class1, class2) {
  require(coin)
  g <- factor(c(rep(class1, length(score.c1)), rep(class2, length(score.c2))))
  v <- c(score.c1, score.c2)
  w <- wilcox_test(v ~ g, distribution="exact")
  z.value <- round(statistic(w)[[1]], digits = 4)
  n <- length(score.c1) + length(score.c2)
  r <- round(abs(z.value)/sqrt(n), digits = 4)
  c(Z=z.value, p=round(pvalue(w), digits = 6), effect.size=r)
}


## f. applies FDR correction on the results in the input data frame
## and adds a new column indicating if the result is significant or not
## order the comparisons data based on the p value
## to prepare it for applying the FDR correction
apply.FDR.correction <- function(results.df) {
  results.df <- results.df[order(results.df$p),]
  ## deterine significance using the FDR correction
  alpha <- 0.05
  n.results <- nrow(results.df)
  results.df$significant <- vector(length = n.results)
  results.df$alpha <- vector(length = n.results)
  for (i in 1:n.results) {
    alpha.adjusted <- (i/n.results)*alpha 
    if ( results.df$p[i] <=  alpha.adjusted ) { 
      results.df$significant[i] <- 'YES'
      results.df$alpha[i] <- alpha.adjusted
    } else {
      for (j in i:n.results) {
        results.df$significant[j] <- 'NO'
        results.df$alpha[j] <- (j/n.results)*alpha
      }
      break
    }
  }
  results.df
}

## compute Cohen's D (effect size) for t-test
## http://stackoverflow.com/questions/15436702/estimate-cohens-d-for-effect-size
cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
}
