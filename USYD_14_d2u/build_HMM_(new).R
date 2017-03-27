#####################################################################
## The set of variables to use for model building:
## -- MCQ.TOT.FACT: compute MCQ.TOT as EQT.CO + EQT.IN + VEQ.CO + VEQ.IN, 
##    then discretize it as follows: all instances with MCQ.TOT == 0 put in one group; 
##    instances with MCQ.TOT >= 1 split into quantiles; then, create a factor variable 
##    MCQ.TOT.FACT with 5 levels: one for instances with MCQ.TOT == 0, and 4 for the 4 quantile groups 
## -- MCQ.PERC.CO.FACT: percentage of correctly solved MCQs; compute MCQ.PERC.CO as 
##    ((EQT.CO+VEQ.CO)/EQT.TOT)*100; then discretize it into 5 level factor variable: 
##    one level for the situation when no MCQ was solved (no MCQs related activities) 
##    and the other 4 corresponding to the 4 quartiles of MCQ.PERC.CO
## -- EXC.TOT.FACT: EXC.TOT = EXC.CO + EXC.IN; then discretize analog to MCQ.TOT.FACT
## -- EXC.PERC.CO.FACT: compute as EXC.CO*100/EXC.TOT, the discretize analog to MCQ.PERC.CO.FACT
## -- VID.TOT.FACT: VID.TOT = VID.PL + VID.PA, then analog to MCQ.TOT.FACT 
## -- MCQ.SH.TOT.FACT: MCQ.SH.TOT = EQT.SH + VEQ.SH, then analog to MCQ.TOT.FACT
## -- TG.DENS.FACT: transition graph density; should be discretized like the variables described above
## -- MC.EVAL.FACT: DBOARD.VIEW + HF.VIEW, then discretize analog to MCQ.TOT.FACT
## -- MC.ORIENT.FACT: MC.ORIENT discretize analog to MCQ.TOT.FACT - EXCLUDED!!!
## -- CONTENT.ACCESS.FACT: CONTENT_ACCESS discretize analog to MCQ.TOT.FACT

#####################################################################

#########################################
## FUNCTIONS FOR BUILDING THE FEATURE SET
#########################################

## event-related variables (columns) from the source data to be used for model building
relevant_columns <- c('VID.PL', # number of video play events
                      'VID.PA', # nmber of video pause events
                      'VEQ.CO', # number of correctly solved MC questions (MCQs) associated with a video
                      'VEQ.IN', # number of incorrectly solved MCQs associated with a video
                      'VEQ.SH', # number of MCQs associated with a video where solution was requested
                      'EQT.CO', # number of correctly solved MCQs embedded in the lecture text
                      'EQT.IN', # number of incorrectly solved MCQs embedded in the lecture text
                      'EQT.SH', # number of MCQs embedded in the lecture, where solution was requested
                      'EXC.CO', # number of correctly solved exercises
                      'EXC.IN') # number of incorrectly solved exercises


## f. for computing the X.TOT.FACT features 
## (MCQ.TOT.FACT, EXC.TOT.FACT, VID.TOT.FACT, MCQ.SH.TOT.FACT,...)
compute.tot.fact.feature <- function(tot.count) {
  tot.pos <- tot.count[ tot.count > 0 ]
  q <- as.vector(summary(tot.pos))
  if ( length(q) > length(unique(q)) )  ## some of the quantiles have the same value
    q <- q + seq_along(q) * 0.0001  # .Machine$double.eps
  tot.count.fact <- cut(tot.count, 
                        breaks=c(0, q[2], q[3], q[5], q[6]), 
                        include.lowest = F,
                        labels = c("Q1", "Q2", "Q3", "Q4"))
  tot.count.fact <- factor(tot.count.fact, levels = c("0", levels(tot.count.fact)))
  tot.count.fact[is.na(tot.count.fact)] <- "0"
  tot.count.fact
}

## f. for computing the X.PERC.CO.FACT features 
## (MCQ.PERC.CO.FACT, EXC.PERC.CO.FACT)
compute.perc.correct.fact.feature <- function(co.count, tot.count) {
  perc.correct <- vector(length = length(tot.count))
  for(i in 1:length(perc.correct)) {
    perc.correct[i] <- ifelse(test = tot.count[i] > 0, 
                              yes = (co.count[i]*100)/tot.count[i],
                              no = 0)  
  }
  perc.correct.pos <- perc.correct[which(perc.correct>0)] 
  q <- as.vector(summary(perc.correct.pos))
  if ( length(q) > length(unique(q)) )  ## some of the quantiles have the same value
    q <- q + seq_along(q) * 0.0001  # .Machine$double.eps
  perc.correct.fact <- cut(perc.correct, 
                       breaks=c(0, q[2], q[3], q[5], q[6]), 
                       include.lowest = F,
                       labels = c("Q1", "Q2", "Q3", "Q4"))
  perc.correct.fact <- factor(perc.correct.fact, levels = c("0", levels(perc.correct.fact)))
  perc.correct.fact[is.na(perc.correct.fact)] <- "0"
  perc.correct.fact
  
}

compute.transition.matrix.density <- function(weekly.trace.data, activities) {
  require(statnet)
  
  a.count <- length(activities)
  user.ids <- unique(weekly.trace.data$RESOURCE_ID)
  n.users <- length(user.ids)
  densities <- vector(mode = "numeric", length = n.users)
  for(i in 1:n.users) {
    ## create initial transition (adjacency) matrix
    trans.matrix <- matrix(data = rep.int(x = 0, times = a.count*a.count), 
                           nrow = a.count, ncol = a.count,
                           dimnames = list(activities, activities))
    ## take traces of one particular user
    user.traces <- weekly.trace.data[weekly.trace.data$RESOURCE_ID == user.ids[i],]
    ## consider that traces originate from study sessions (cases)
    user.cases <- unique(user.traces$CASE_ID)
    n.cases <- length(user.cases)
    for(j in 1:n.cases) {
      ## take activities from each session individualy
      case.activities <- user.traces$ACTIVITY[user.traces$CASE_ID == user.cases[j]]
      n.activities <- length(case.activities)
      ## skip sessions that have just one activity
      if (n.activities <= 1) next
      ## update the transition matrix
      for(k in 1:(length(case.activities)-1)) {
        trans.matrix[case.activities[k], case.activities[k+1]] <- trans.matrix[case.activities[k], case.activities[k+1]] + 1
      }
    }
    ## create a graph using the transition matrix as adjacency matrix
    g <- network(x = trans.matrix, matrix.type="adjacency", 
                 directed=T, loops = T, multiple = T,
                 ignore.eval=FALSE, names.eval='sample')
    ## compute graph density
    g.den <- gden(g, diag = T, ignore.eval = F)
    if ( is.na(g.den) == T ) g.den <- 0
    densities[i] <- g.den
  }
  
  ## return a df comprising user ids and the corresponding graph densities
  data.frame(user_id=user.ids, GDEN=densities)
  
}

create.weekly.features <- function(weekly_counts_file, week_number, trace_data) {
  
  ## Load the event counts data
  source_counts <- read.csv(paste("datasets/", weekly_counts_file, sep = ""))
  
  columns_to_select = c('user_id', relevant_columns)
  ## Pick up only the chosen set of columns
  source_counts <- source_counts[, names(source_counts) %in% columns_to_select]
  
  ## load the additional counts data
  more.counts <- read.csv(file = "Intermediate_files/mc-orient_mc-eval_content-access_weekly_counts.csv")
  
  ## extract student ids, will be needed when computing several features
  user.ids <- source_counts[,1]
  
  ## add user_id to the resulting feature set
  features <- data.frame(user_id = user.ids)
  ## add week indicator
  features$week <- c(rep(x = week_number, nrow(source_counts)))
  
  ## compute the MCQ.TOT.FACT feature 
  mcq.tot <- source_counts$EQT.CO + source_counts$EQT.IN + 
             source_counts$VEQ.CO + source_counts$VEQ.IN
  features$MCQ.TOT.FACT <- compute.tot.fact.feature(mcq.tot) 
  
  ## compute the MCQ.PERC.CO.FACT feature
  mcq.co <- source_counts$EQT.CO + source_counts$VEQ.CO
  features$MCQ.PERC.CO.FACT <- compute.perc.correct.fact.feature(co.count = mcq.co,
                                                                 tot.count = mcq.tot)
  
  ## compute the EXC.TOT.FACT feature 
  exc.tot <- source_counts$EXC.CO + source_counts$EXC.IN
  features$EXC.TOT.FACT <- compute.tot.fact.feature(exc.tot) 
  
  ## compute the EXC.PERC.CO.FACT feature 
  features$EXC.PERC.CO.FACT <- compute.perc.correct.fact.feature(co.count = source_counts$EXC.CO,
                                                                 tot.count = exc.tot)
  
  ## compute the VID.TOT.FACT feature 
  features$VID.TOT.FACT <- compute.tot.fact.feature(source_counts$VID.PA + source_counts$VID.PL) 
  
  ## compute the MCQ.SH.TOT.FACT feature 
  features$MCQ.SH.TOT.FACT <- compute.tot.fact.feature(source_counts$EQT.SH + source_counts$VEQ.SH) 
  
  ## compute TG.DENS.FACT
  weekly.traces <- trace_data[trace_data$WEEK == week_number,]
  tg.dens <- compute.transition.matrix.density(weekly.trace.data = weekly.traces, 
                                               activities = unique(trace_data$ACTIVITY))
  ## for those users where density could not be computed, set 0 for GDEN value
  gden <- vector(length = length(user.ids))
  for(i in 1:length(user.ids)) {
    if ( user.ids[i] %in% tg.dens$user_id ) 
      gden[i] <- tg.dens$GDEN[tg.dens$user_id==user.ids[i]]
    else gden[i] <- 0
  }
  features$TG.DENS.FACT <- compute.tot.fact.feature(gden)
  
  ## compute MC.EVAL.FACT
  mc.eval <- more.counts$mc.eval[more.counts$week == week_number &
                                   more.counts$user_id %in% user.ids]
  features$MC.EVAL.FACT <- compute.tot.fact.feature(mc.eval)
  
  ## compute MC.ORIENT.FACT
  mc.orient <- more.counts$mc.orient[more.counts$week == week_number &
                                     more.counts$user_id %in% user.ids]
  features$MC.ORIENT.FACT <- compute.tot.fact.feature(mc.orient)
  
  ## compute CONTENT.ACCESS.FACT
  cont.access <- more.counts$content.access[more.counts$week == week_number &
                                            more.counts$user_id %in% user.ids]
  features$CONTENT.ACCESS.FACT <- compute.tot.fact.feature(cont.access)
  
  
  features
}


######################################################
## COMPUTE FEATURES FOR EACH WEEK OF THE COURSE (2-13)
######################################################

## load the trace data required for computing transition graphs, i.e., GDEN feature
trace.data <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv")
trace.data <- trace.data[trace.data$WEEK %in% c(2:13),]
## substitute DBOARD_ACCESS and HOF_ACCESS with MC_EVAL, and ORIENT with MC_ORIENT
activity.rev <- as.vector(sapply(trace.data$ACTIVITY, function(x) {
  if (x == "DBOARD_ACCESS") x <- "MC_EVAL"
  else if (x == "ORIENT") x <- "MC_ORIENT"
  x
}))
trace.data$ACTIVITY <- activity.rev


## WEEK 2
f.w2 <- create.weekly.features('up_to_w02.csv', 2, trace.data)
str(f.w2)
## WEEK 3
f.w3 <- create.weekly.features('up_to_w03.csv', 3, trace.data)
str(f.w3)
## WEEK 4
f.w4 <- create.weekly.features('up_to_w04.csv', 4, trace.data)
str(f.w4)
## WEEK 5
f.w5 <- create.weekly.features('up_to_w05.csv', 5, trace.data)
str(f.w5)
## WEEK 6
f.w6 <- create.weekly.features('up_to_w06.csv', 6, trace.data)
str(f.w6)
## WEEK 7
f.w7 <- create.weekly.features('up_to_w07.csv', 7, trace.data)
## WEEK 8
f.w8 <- create.weekly.features('up_to_w08.csv', 8, trace.data)
## WEEK 9
f.w9 <- create.weekly.features('up_to_w09.csv', 9, trace.data)
## WEEK 10
f.w10 <- create.weekly.features('up_to_w10.csv', 10, trace.data)  
## WEEK 11
f.w11 <- create.weekly.features('up_to_w11.csv', 11, trace.data) 
## WEEK 12
f.w12 <- create.weekly.features('up_to_w12.csv', 12, trace.data)
## WEEK 13
f.w13 <- create.weekly.features('up_to_w13.csv', 13, trace.data)

## merge now data for all the weeks
feature.set <- as.data.frame( rbind(f.w2, f.w3, f.w4, f.w5, f.w6,f.w7, f.w8,
                                    f.w9, f.w10, f.w11, f.w12, f.w13))
str(feature.set)

## now order the data and compute ntimes attribute required for the depmix function
features.sorted <- feature.set[ order(feature.set$user_id, feature.set$week), ]
features.sorted[1:25, ]

ntimes <- vector(mode = "integer", length = length(unique(features.sorted$user_id)))
k <- 1
i <- 1
while(i <= nrow(features.sorted)) {
  current.student <- features.sorted$user_id[i]
  count <- 1
  if ( i == nrow(features.sorted)) {
    ntimes[k] <- count
    break
  }
  same.student <- T
  j <- i + 1
  while( same.student == T & j <= nrow(features.sorted)) {
    if ( features.sorted$user_id[j] == current.student ) { 
      count <- count + 1
      j <- j + 1 
    } else {
      same.student <- F
    }
  }
  i <- j
  ntimes[k] <- count
  k <- k + 1
}
ntimes[1:110]

## some of the students were not active in several weeks of the course
## let's check how many of them 
length(which(ntimes<10))
## and who they are
low.active.users <- unique(features.sorted$user_id)[which(ntimes<10)]
features.sorted[features.sorted$user_id %in% low.active.users,]
## not sure what to do about these users; let's keep them for now

seq.lengths <- as.data.frame( cbind(user_id = unique(features.sorted$user_id),
                                    length = ntimes))
str(seq.lengths)

## add the ntimes attribute to the feature set
features.final <- features.sorted
features.final$ntimes <- vector(mode = "integer", length = nrow(features.final))
for (i in 1:nrow(features.final)) {
   features.final$ntimes[i] <- seq.lengths[ seq.lengths$user_id == features.final$user_id[i], 2]  
}
str(features.final)

## write features to a file
write.csv(x = features.final, file = "Intermediate_files/HMM_10_features_(Feb2016).csv", 
          row.names = F, quote = F)


########################
## FIT THE MODEL
########################

install.packages("depmixS4")
library(depmixS4)

set.seed(123)
ns <- 5
mod <- depmix(response = list(MCQ.TOT.FACT ~ 1, MCQ.PERC.CO.FACT ~ 1, EXC.TOT.FACT ~ 1,
                              EXC.PERC.CO.FACT ~ 1, VID.TOT.FACT ~ 1, MCQ.SH.TOT.FACT ~ 1,
                              TG.DENS.FACT ~ 1, MC.EVAL.FACT ~ 1, CONTENT.ACCESS.FACT ~ 1), 
              data = features.final, nstates = ns,
              family = list( multinomial("identity"), multinomial("identity"), multinomial("identity"),
                             multinomial("identity"), multinomial("identity"), multinomial("identity"),
                             multinomial("identity"), multinomial("identity"), 
                             multinomial("identity")))


mod.fit <- fit(mod, verbose = FALSE)

print(mod.fit)
summary(mod.fit)

## get the estimated state for each observation 
esttrans <- posterior(mod.fit)
str(esttrans)
# add the estimated states to the features set and save it
features.and.results <- as.data.frame(cbind(features.final, esttrans))
str(features.and.results)
write.csv(x = features.and.results[,c(1:10,12:14)], file = "results/HMM_5_states_9_features_(March2016).csv", 
          quote = F, row.names = F)

#########################################
## VISUALIZE THE MODULES VS STATES MATRIX
#########################################

## following the examples given at:
## http://thecoatlessprofessor.com/programming/creating-stacked-barplot-and-grouped-barplot-in-r-using-base-graphics-no-ggplot2/

dat <- read.csv(file = "results/HMM_5_states_9_features_(March2016).csv")
str(dat)
dat <- dat[,c(2,13)]
m <- as.matrix(table(dat$week, dat$state))
row.names(m) <- c("W2", "W3", "W4", "W5", "W6", "W7",
                  "W8", "W9", "W10", "W11", "W12", "W13")
colnames(m) <- c("S1", "S2", "S3", "S4", "S5")

require(knitr)
kable(m, format = 'rst')

df <- as.data.frame(m)
colnames(df) <- c('Week', 'State', 'Freq')
df
library(ggplot2)
ggplot(data=df, aes(x=Week, y=Freq, fill=State)) +
  geom_bar(stat="identity") +
  ylab("Number of students") +
  scale_fill_manual(values = c("#0072B2","#56B4E9", "#009E73", "#F0E442", "#e41a1c"), # "#E69F00"
                    breaks=c("S1", "S2", "S3", "S4", "S5"),
                    labels=c("Low active (sum.assess)",
                              "Highly active (all activities)", 
                              "Very low activity / disengaged",
                              "Moderately active (all activities)",
                              "Moderately active (sum. assess + reading)")) +
  theme_bw() +
  theme(legend.title = element_text(size=11, face="bold"),
        legend.text = element_text(size = 10),
        legend.key = element_rect(colour = NA))
  

## Plot the percentages instead of the number of students
## Abelardo's code
df2 <- df
df2[df2$Week == 'W2',]$Freq <- 
  100 * df2[df2$Week == 'W2',]$Freq/sum(df2[df2$Week == 'W2',]$Freq)
df2[df2$Week == 'W3',]$Freq <- 
  100 * df2[df2$Week == 'W3',]$Freq/sum(df2[df2$Week == 'W3',]$Freq)
df2[df2$Week == 'W4',]$Freq <- 
  100 * df2[df2$Week == 'W4',]$Freq/sum(df2[df2$Week == 'W4',]$Freq)
df2[df2$Week == 'W5',]$Freq <- 
  100 * df2[df2$Week == 'W5',]$Freq/sum(df2[df2$Week == 'W5',]$Freq)
df2[df2$Week == 'W6',]$Freq <- 
  100 * df2[df2$Week == 'W6',]$Freq/sum(df2[df2$Week == 'W6',]$Freq)
df2[df2$Week == 'W7',]$Freq <- 
  100 * df2[df2$Week == 'W7',]$Freq/sum(df2[df2$Week == 'W7',]$Freq)
df2[df2$Week == 'W8',]$Freq <- 
  100 * df2[df2$Week == 'W8',]$Freq/sum(df2[df2$Week == 'W8',]$Freq)
df2[df2$Week == 'W9',]$Freq <- 
  100 * df2[df2$Week == 'W9',]$Freq/sum(df2[df2$Week == 'W9',]$Freq)
df2[df2$Week == 'W10',]$Freq <- 
  100 * df2[df2$Week == 'W10',]$Freq/sum(df2[df2$Week == 'W10',]$Freq)
df2[df2$Week == 'W11',]$Freq <- 
  100 * df2[df2$Week == 'W11',]$Freq/sum(df2[df2$Week == 'W11',]$Freq)
df2[df2$Week == 'W12',]$Freq <- 
  100 * df2[df2$Week == 'W12',]$Freq/sum(df2[df2$Week == 'W12',]$Freq)
df2[df2$Week == 'W13',]$Freq <- 
  100 * df2[df2$Week == 'W13',]$Freq/sum(df2[df2$Week == 'W13',]$Freq)
df2$State <- as.vector(df2$State)
# Reorder the states
df2[df2$State == 'S1',]$State <- 'T2'
df2[df2$State == 'S2',]$State <- 'T5'
df2[df2$State == 'S3',]$State <- 'T1'
df2[df2$State == 'S4',]$State <- 'T4'
df2[df2$State == 'S5',]$State <- 'T3'
df2 <- df2[with(df2, order(Week, State)),]


ggplot(data=df2, aes(x=Week, y=Freq, fill=State)) +
  geom_bar(stat="identity") +
  ## geom_bar(aes(y = (..count..)/sum(..count..))) +
  ylab("Percentage of students") +
  ## scale_fill_manual(values = c("#0072B2","#56B4E9", "#009E73", "#F0E442", "#e41a1c"), # "#E69F00"
  scale_fill_brewer(
    breaks=c("T5", "T4", "T3", "T2", "T1"),
    labels=c("High activity",
             "Moderate activity (all act.)",
             "Moderate activity (some act.)",
             "Low activity",
             "Very low activity")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size = 20),
        legend.key = element_rect(colour = NA))

ggsave('hmm_transitions.png', 
       path = "~/Papers/1_Work/Jelena_flipped_trajectories",
       width = 18.46)

