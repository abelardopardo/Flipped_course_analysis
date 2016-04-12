###########################################################################
## COMPARE STUDENTS WHO FILLED IN THE MSLQ AND SPQ QUESTIONNAIRES AND THOSE 
## WHO DIDN'T ALONG ALL THE VARIABLES USED FOR HIERARCHICAL CLUSTERING
## (THE LATEST FEATURE SET):
## -	EQT.TOT = EQT.CO + EQT.IN
## -	EQT.DIFF = EQT.CO - EQT.IN 
## -	EQT.SH 
## -	EXC.TOT = EXC.CO + EXC.IN
## -	EXC.DIFF = EXC.CO - EXC.IN
## -	VEQ.TOT = VEQ.CO + VEQ.IN
## -	VEQ.DIFF = VEQ.CO - VEQ.IN
## -	VEQ.SH
## -	VID.TOT = VID.PA + VID.PL
## -	ORG.VIEW
## -	DBOARD.VIEW
## -	HOF.VIEW 
## -  GDEN (TRANS. GRAPH DENSITY)
###########################################################################

## first, identify students who filled in the questionnaires and those who didn't
counts.data <- read.csv(file = "datasets/data2u_sem2_14_student_all_variables.csv") 
completed.q <- counts.data[is.na(counts.data$MSLQ_IVAL) == FALSE, ] 
completed.ids <- as.vector(completed.q$user_id)

not.completed.q <-counts.data[is.na(counts.data$MSLQ_IVAL),]
not.completed.ids <- as.vector(not.completed.q$user_id)

######################################################################
## COMPARE THE TWO GROUPS BASED ON THEIR MIDTERM AND FINAL EXAM SCORES
######################################################################

## extract exam scores
scores <- counts.data[, c('user_id', 'SC_MT_TOT', 'SC_FE_TOT')]
scores$COMPLETED_QUEST <- vector(mode = "character", length = nrow(scores))
for (i in 1:nrow(scores)) {
  if ( scores$user_id[i] %in% completed.ids ) scores$COMPLETED_QUEST[i] <- "Y"
  else scores$COMPLETED_QUEST[i] <- 'N'
}
scores$COMPLETED_QUEST <- as.factor(scores$COMPLETED_QUEST)

## check if the midterm exam scores are normally distributed
qqnorm(scores$SC_MT_TOT)
qqline(scores$SC_MT_TOT)
shapiro.test(scores$SC_MT_TOT)
## W = 0.95137, p-value = 3.169e-08 - the midterm scores are not normally distributed
## use non-parametric tests

kruskal.test(scores$SC_MT_TOT ~ scores$COMPLETED_QUEST)
# chi-squared = 3.0398, df = 1, p-value = 0.08125
# NO SIGNIFICANT DIFF. W.R.T MIDTERM EXAM SCORES

## check if the final exam scores are normally distributed
qqnorm(scores$SC_FE_TOT)
qqline(scores$SC_FE_TOT)
shapiro.test(scores$SC_FE_TOT)
# W = 0.95035, p-value = 2.418e-08 - the final exam scores are not normally distributed
# use non-parametric tests

kruskal.test(scores$SC_FE_TOT ~ scores$COMPLETED_QUEST)
# chi-squared = 8.3114, df = 1, p-value = 0.00394
# SIGNIFICANT DIFFERENCE W.R.T THE FINAL EXAM SCORE (p=.004)

###############################################################################
## COMPARE THE TWO GROUPS BASED ON THE VARIABLES (FEATURES) USED FOR CLUSTERING
###############################################################################

## import functions for computing the features
source("clustering_related_functions.R")

## for table layout
library(knitr)

## f. compares the two groups (completed and not-completed) along all the features
compare.two.groups <- function(weekly.features) {

  comparison <- data.frame(feature=colnames(weekly.features)[2:14],
                           p.value=vector(mode = "numeric", length = 13),
                           significant=vector(length = 13))
  
    ## EQT.TOT
    # qqnorm(weekly.features$EQT.TOT)
    # qqline(weekly.features$EQT.TOT)
    # shapiro.test(weekly.features$EQT.TOT)
    # far from normally distributed
    kt <- kruskal.test(weekly.features$EQT.TOT ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "EQT.TOT",2] <- kt$p.value 
    
    ## EQT.DIFF
    # qqnorm(weekly.features$EQT.DIFF)
    # qqline(weekly.features$EQT.DIFF)
    # shapiro.test(weekly.features$EQT.DIFF)
    # far from normally distributed
    kt <- kruskal.test(weekly.features$EQT.DIFF ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "EQT.DIFF",2] <- kt$p.value 
    
    ## EQT.SH
    # qqnorm(weekly.features$EQT.SH)
    # qqline(weekly.features$EQT.SH)
    # shapiro.test(weekly.features$EQT.SH)
    # far from normally distributed
    kt <- kruskal.test(weekly.features$EQT.SH ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "EQT.SH",2] <- kt$p.value 
    
    ## EXC.TOT
    # qqnorm(weekly.features$EXC.TOT)
    # qqline(weekly.features$EXC.TOT)
    # shapiro.test(weekly.features$EXC.TOT)
    # far from normally distributed
    kt <- kruskal.test(weekly.features$EXC.TOT ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "EXC.TOT",2] <- kt$p.value 
    
    ## EXC.DIFF
    # qqnorm(weekly.features$EXC.DIFF)
    # qqline(weekly.features$EXC.DIFF)
    # shapiro.test(weekly.features$EXC.DIFF)
    # far from normally distributed
    kt <- kruskal.test(weekly.features$EXC.DIFF ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "EXC.DIFF",2] <- kt$p.value 
    
    ## VEQ.TOT
    # qqnorm(weekly.features$VEQ.TOT)
    # qqline(weekly.features$VEQ.TOT)
    # shapiro.test(weekly.features$VEQ.TOT)
    # far from normally distributed
    kt <- kruskal.test(weekly.features$VEQ.TOT ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "VEQ.TOT",2] <- kt$p.value 
    
    ## since none of the previously examined count variables was normally distributed
    ## in fact, far from norm. distr., I'll assume that other variables are also non-normal
    
    ## VEQ.DIFF
    kt <- kruskal.test(weekly.features$VEQ.DIFF ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "VEQ.DIFF",2] <- kt$p.value 
    
    ## VEQ.SH
    kt <- kruskal.test(weekly.features$VEQ.SH ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "VEQ.SH",2] <- kt$p.value 
    
    ## VID.TOT
    kt <- kruskal.test(weekly.features$VID.TOT ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "VID.TOT",2] <- kt$p.value 
    
    ## ORG.VIEW
    kt <- kruskal.test(weekly.features$ORG.VIEW ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "ORG.VIEW",2] <- kt$p.value 
    
    ## DBOARD.VIEW
    kt <- kruskal.test(weekly.features$DBOARD.VIEW ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "DBOARD.VIEW",2] <- kt$p.value 
    
    ## HOF.VIEW
    kt <- kruskal.test(weekly.features$HOF.VIEW ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "HOF.VIEW",2] <- kt$p.value 
    
    ## GDEN
    kt <- kruskal.test(weekly.features$GDEN ~ weekly.features$COMPLETED_QUEST)
    comparison[comparison$feature == "GDEN",2] <- kt$p.value 
    
    ## use False Discovery Rate (FDR) to deal with multiple testing
    ## first, order the comparison df based on the computed p value (column 2)
    comparison <- comparison[order(comparison$p.value),]
    
    ## number of comparisons
    n.comparison <- length(which(is.nan(comparison$p.value) == F))
    print(n.comparison)
    
    alpha <- 0.05
    for(i in 1:nrow(comparison)) {
      if ( is.na(comparison$p.value[i]) == T | is.nan(comparison$p.value[i]) == T) {
        comparison$significant[i] <- NA
        next
      }
      if ( comparison$p.value[i] <= ((i/n.comparison)*alpha) ) comparison$significant[i] <- "YES"
      else {
        for(j in i:nrow(comparison))
          comparison$significant[j] <- "NO"
        break
      }
    }
    
    comparison
}

## f. for computing and printing weekly comparisons
weekly.group.comparison <- function(week, data.file, trace.data) {
  
  weekly.f <- create.new.feature.set(course_week = week,
                                 event_counts_file = data.file, 
                                 scores_file = "data2u_sem2_14_student_all_variables.csv",
                                 score_columns = c("SC_MT_TOT", "SC_FE_TOT"),
                                 trace.data)
 
  ## add the indicator of the questionnaire completion
  weekly.data <- merge(x = weekly.f, y = scores[,c(1,4)], by = 'user_id', all.x = T, all.y = F)
  
  weekly.comparison <- compare.two.groups(weekly.data)
 
  require(knitr)
  kable(x = weekly.comparison, format = "rst")
}

## load the trace data required for computing transition graphs, i.e., GDEN feature
trace.data <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv")
trace.data <- trace.data[trace.data$WEEK %in% c(2:13),]

## WEEK 2
weekly.group.comparison(2, "up_to_w02.csv", trace.data)

## WEEK 3
weekly.group.comparison(3, "up_to_w03.csv", trace.data)

## WEEK 4
weekly.group.comparison(4, "up_to_w04.csv", trace.data)

## WEEK 5
weekly.group.comparison(5, "up_to_w05.csv", trace.data)

## WEEK 6
weekly.group.comparison(6, "up_to_w06.csv", trace.data)

## WEEK 7
weekly.group.comparison(7, "up_to_w07.csv", trace.data)

## WEEK 8
weekly.group.comparison(8, "up_to_w08.csv", trace.data)

## WEEK 9
weekly.group.comparison(9, "up_to_w09.csv", trace.data)

## WEEK 10
weekly.group.comparison(10, "up_to_w10.csv", trace.data)

## WEEK 11
weekly.group.comparison(11, "up_to_w11.csv", trace.data)

## WEEK 12
weekly.group.comparison(12, "up_to_w12.csv", trace.data)

## WEEK 13
weekly.group.comparison(13, "up_to_w13.csv", trace.data)


## since only in week 2 significant difference among the 
## two groups can be observed, let's see the summary stats
## for the two groups for week 2
week2.f <- create.new.feature.set(course_week = 2,
                                   event_counts_file = "up_to_w02.csv", 
                                   scores_file = "data2u_sem2_14_student_all_variables.csv",
                                   score_columns = c("SC_MT_TOT", "SC_FE_TOT"))

## add the indicator of the questionnaire completion
week2.data <- merge(x = week2.f, 
                    y = scores[,c(1,4)], 
                    by = 'user_id', all.x = T, all.y = F)
week2.data$COMPLETED_QUEST <- as.integer(week2.data$COMPLETED_QUEST)
week2.stats <- summary.stats(week2.data[,c(2:13)], week2.data$COMPLETED_QUEST, 2)
kable(week2.stats, format = "rst")
