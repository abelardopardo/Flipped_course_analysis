#####################################################################
## Try creating a HMM with states corresponding to the clusters that 
## I observed occurring from week to week: highly active, active, 
## exercise-focused and inactive (disengaged); as observed variables, 
## the following might be used: 
## -- (EQT.CO - EQT.IN) 
## -- (VEQ.CO - VEQ.IN) 
## -- (EXC.CO-EXC.IN) 
## -- VID.PL 
## -- VID.PA 
##
## Another set of variables to consider:
## -- EQT.TOT = EQT.CO + EQT.IN
## -- EQT.DIFF = EQT.CO - EQT.IN
## -- VEQ.TOT = VEQ.CO + VEQ.IN
## -- VEQ.DIFF = VEQ.CO - VEQ.IN
## -- EXC.TOT = EXC.CO + EXC.IN
## -- EXC.DIFF = EXC.CO - EXC.IN
## -- VID.TOT = VID.PL + VID.PA 
##
## The 3rd set of variables to consider:
## -- EQT.TOT.FACT: compute EQT.TOT as EQT.CO + EQT.IN, then discretize it as follows:
##    all instances with EQT.TOT == 0 put in one group; instances with EQT.TOT >= 1 split
##    into 4 quantiles; then, create a factor variable EQT.TOT.FACT with 5 levels: one for 
##    instances with EQT.TOT == 0, and 4 for the 4 quantile groups 
## -- EQT.DIFF.SIGN: 4 level factor variable with values: "none" if EQT.CO == EQT.IN == 0;
##    "equal" if EQT.CO == EQT.IN (and EQT.TOT > 0); "neg" if EQT.CO < EQT.IN, 
##    and "pos" if EQT.CO > EQT.IN
## -- EQT.DIFF.FACT: compute EQT.DIFF as |EQT.CO - EQT.IN|, the discretize it as follows:
##    all instances with EQT.DIFF == 0 put in one group; instances with EQT.DIFF >= 1 split
##    into 4 quantiles; then, create a factor variable EQT.DIFF.FACT with 5 levels: one for 
##    instances with EQT.DIFF == 0, and 4 for the 4 quantile groups 
## -- VEQ.TOT.FACT: analog to EQT.TOT.FACT
## -- VEQ.DIFF.SIGN: analog to EQT.DIFF.SIGN
## -- VEQ.DIFF.FACT: analog to EQT.DIFF.FACT
## -- EXC.TOT.FACT: analog to EQT.TOT.FACT
## -- EXC.DIFF.SIGN: analog to EQT.DIFF.SIGN
## -- EXC.DIFF.FACT: analog to EQT.DIFF.FACT
## -- VID.TOT.FACT: VID.TOT = VID.PL + VID.PA, then analog to EQT.TOT.FACT 
## -- MCQ.SH.TOT.FACT: MCQ.SH.TOT = EQT.SH + VEQ.SH, then analog to EQT.TOT.FACT ***NEW***

#####################################################################

########################
## BUILD THE FEATURE SET
########################

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
compute.tot.fact.feature <- function(co.count.feature, in.count.feature) {
  tot.count <- co.count.feature + in.count.feature
  tot.pos <- tot.count[ tot.count > 0 ]
  q <- as.vector(summary(tot.pos))
  tot.count.fact <- cut(tot.count, 
                        breaks=c(0, q[2], q[3], q[5], q[6]), 
                        include.lowest = F,
                        labels = c("Q1", "Q2", "Q3", "Q4"))
  tot.count.fact <- factor(tot.count.fact, levels = c("0", levels(tot.count.fact)))
  tot.count.fact[is.na(tot.count.fact)] <- "0"
  tot.count.fact
}

## f. for computing the X.DIFF.SIGN features
compute.diff.sign.feature <- function(co.count.feature, in.count.feature) {
  tot.feature <- co.count.feature + in.count.feature
  diff.feature <- co.count.feature - in.count.feature
  diff.fact <- vector(length = length(diff.feature))
  for(i in 1:length(diff.feature)) {
    if (diff.feature[i] == 0 & tot.feature[i] == 0)
      diff.fact[i] <- "none"
    else if (diff.feature[i] == 0 & tot.feature[i] > 0)
      diff.fact[i] <- "equal"
    else if (diff.feature[i] > 0)
      diff.fact[i] <- "pos"
    else diff.fact[i] <- "neg"
  }
  diff.fact <- factor(diff.fact, levels = c("none", "equal", "pos", "neg"))
  diff.fact
}

## f. for computing the X.DIFF.FACT features 
compute.diff.fact.feature <- function(co.count.feature, in.count.feature) {
  diff.feature <- abs(co.count.feature - in.count.feature)
  diff.pos <- diff.feature[ diff.feature > 0 ]
  q <- as.vector(summary(diff.pos))
  if ( length(q) > length(unique(q)) )  ## some of the quantiles have the same value
    q <- q + seq_along(q) * .Machine$double.eps
  diff.feature.fact <- cut(diff.feature, 
                        breaks=c(0, q[2], q[3], q[5], q[6]), 
                        include.lowest = F,
                        labels = c("Q1", "Q2", "Q3", "Q4"))
  diff.feature.fact <- factor(diff.feature.fact, levels = c("0", levels(diff.feature.fact)))
  diff.feature.fact[is.na(diff.feature.fact)] <- "0"
  diff.feature.fact
}

create.weekly.features <- function(event_counts_file, week_number) {
  
  ## Load the events data
  source_events <- read.csv(paste("datasets/", event_counts_file, sep = ""))
  
  columns_to_select = c('user_id', relevant_columns)
  ## Pick up only the chosen set of columns
  source_events <- source_events[, names(source_events) %in% columns_to_select]
  
  ## add user_id
  features <- data.frame(user_id = source_events[,1])
  ## add week indicator
  features$week <- c(rep(x = week_number, nrow(source_events)))
  
  ## compute the EQT.TOT.FACT feature 
  features$EQT.TOT.FACT <- compute.tot.fact.feature(source_events$EQT.CO, source_events$EQT.IN) 
  ## compute the EQT.DIFF.SIGN feature 
  features$EQT.DIFF.SIGN <- compute.diff.sign.feature(source_events$EQT.CO, source_events$EQT.IN) 
  ## compute the EQT.DIFF.FACT feature 
  features$EQT.DIFF.FACT <- compute.diff.fact.feature(source_events$EQT.CO, source_events$EQT.IN) 
  
  ## compute the VEQ.TOT.FACT feature 
  features$VEQ.TOT.FACT <- compute.tot.fact.feature(source_events$VEQ.CO, source_events$VEQ.IN) 
  ## compute the VEQ.DIFF.SIGN feature 
  features$VEQ.DIFF.SIGN <- compute.diff.sign.feature(source_events$VEQ.CO, source_events$VEQ.IN) 
  ## compute the VEQ.DIFF.FACT feature 
  features$VEQ.DIFF.FACT <- compute.diff.fact.feature(source_events$VEQ.CO, source_events$VEQ.IN) 
  
  ## compute the EXC.TOT.FACT feature 
  features$EXC.TOT.FACT <- compute.tot.fact.feature(source_events$EXC.CO, source_events$EXC.IN) 
  ## compute the EXC.DIFF.SIGN feature 
  features$EXC.DIFF.SIGN <- compute.diff.sign.feature(source_events$EXC.CO, source_events$EXC.IN) 
  ## compute the EXC.DIFF.FACT feature 
  features$EXC.DIFF.FACT <- compute.diff.fact.feature(source_events$EXC.CO, source_events$EXC.IN) 
  
  ## compute the VID.TOT.FACT feature 
  features$VID.TOT.FACT <- compute.tot.fact.feature(source_events$VID.PA, source_events$VID.PL) 
  
  ## compute the MCQ.SH.TOT.FACT feature 
  features$MCQ.SH.TOT.FACT <- compute.tot.fact.feature(source_events$EQT.SH, source_events$VEQ.SH) 
  
  features
}

## WEEK 2
f.w2 <- create.weekly.features('up_to_w02.csv', 2)
str(f.w2)
## WEEK 3
f.w3 <- create.weekly.features('up_to_w03.csv', 3)
str(f.w3)
## WEEK 4
f.w4 <- create.weekly.features('up_to_w04.csv', 4)
str(f.w4)
## WEEK 5
f.w5 <- create.weekly.features('up_to_w05.csv', 5)
str(f.w5)
## WEEK 6
f.w6 <- create.weekly.features('up_to_w06.csv', 6)
str(f.w6)
## WEEK 7
f.w7 <- create.weekly.features('up_to_w07.csv', 7)
## WEEK 8
f.w8 <- create.weekly.features('up_to_w08.csv', 8)
## WEEK 9
f.w9 <- create.weekly.features('up_to_w09.csv', 9)
## WEEK 10
f.w10 <- create.weekly.features('up_to_w10.csv', 10)  
## WEEK 11
f.w11 <- create.weekly.features('up_to_w11.csv', 11) 
## WEEK 12
f.w12 <- create.weekly.features('up_to_w12.csv', 12)
## WEEK 13
f.w13 <- create.weekly.features('up_to_w13.csv', 13)

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
ntimes[1:10]

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
write.csv(x = features.final, file = "Intermediate_files/HMM_11_features.csv", 
          row.names = F, quote = F)


########################
## FIT THE MODEL
########################

install.packages("depmixS4")
library(depmixS4)

set.seed(123)
ns <- 5
mod <- depmix(response = list(EQT.TOT.FACT ~ 1, EQT.DIFF.SIGN ~ 1, EQT.DIFF.FACT ~ 1,
                              VEQ.TOT.FACT ~ 1, VEQ.DIFF.SIGN ~ 1, VEQ.DIFF.FACT ~ 1,
                              EXC.TOT.FACT ~ 1, EXC.DIFF.SIGN ~ 1, EXC.DIFF.FACT ~ 1,
                              VID.TOT.FACT ~ 1, MCQ.SH.TOT.FACT ~ 1), 
              data = features.final, nstates = ns,
              family = list( multinomial("identity"), multinomial("identity"), multinomial("identity"),
                             multinomial("identity"), multinomial("identity"), multinomial("identity"),
                             multinomial("identity"), multinomial("identity"), multinomial("identity"),
                             multinomial("identity"), multinomial("identity")))


mod.fit <- fit(mod, verbose = FALSE)

print(mod.fit)
summary(mod.fit)

## get the estimated state for each observation 
esttrans <- posterior(mod.fit)
str(esttrans)
# add the estimated states to the features set and save it
features.and.results <- as.data.frame(cbind(features.final, esttrans))
str(features.and.results)
write.csv(x = features.and.results, file = "results/HMM_5_states_11_features_plus_results.csv", 
          quote = F, row.names = F)

#########################################
## VISUALIZE THE MODULES VS STATES MATRIX
#########################################

## following the examples given at:
## http://thecoatlessprofessor.com/programming/creating-stacked-barplot-and-grouped-barplot-in-r-using-base-graphics-no-ggplot2/

dat <- read.csv(file = "results/HMM_5_states_11_features_plus_results.csv")
str(dat)
dat <- dat[,c(2,15)]
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
  ylab("Count") +
  scale_fill_manual(values = c("#0072B2","#56B4E9", "#009E73", "#F0E442", "#e41a1c"), # "#E69F00"
                    breaks=c("S1", "S2", "S3", "S4", "S5"),
                    labels=c("Medium activity\n(with low VID, and absence of VEQ)",
                              "Active", "Minimalists /\nDisengaged",
                              "Medium activity (with very low EQT)",
                              "Disengaged\nexcept for exercises")) +
  theme_bw() +
  theme(legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size = 12))
  
