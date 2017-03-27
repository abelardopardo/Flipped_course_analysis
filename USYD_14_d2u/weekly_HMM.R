################################################################################
## create HMMs for each course week, using only the type of the learning action 
## as the only predictor (observable variable); use the learning action types 
## as those used in sequence analysis and hyerarchical clustering: 
## CONTENT_ACCESS, EXE_CO, EXE_IN, MCQ_CO, MCQ_IN, MCQ_SR, 
## MC_EVAL, MC_ORIENT, VIDEO_PLAY  
## besides action types, for building the model, we
## - student id
## - timestamp
#################################################################################

traces <- read.csv(file = "Intermediate_files/trace_data_with_sessions_(no-1-event)_w0-16.csv",
                   stringsAsFactors = F)
str(traces)

## restrict sessions to those that took place during the course,
## that is, during the weeks 2-13  
traces <- subset(traces, traces$WEEK %in% c(2:13))

## transform the timestamp into format suitable for date-based comparison and sorting
traces$TIMESTAMP <- as.POSIXct(traces$TIMESTAMP)

table(traces$ACTIVITY)
## substitute DBOARD_ACCESS with MC_EVAL, and ORIENT with MC_ORIENT
traces$ACTIVITY[traces$ACTIVITY=="DBOARD_ACCESS"] <- "MC_EVAL"
traces$ACTIVITY[traces$ACTIVITY=="ORIENT"] <- "MC_ORIENT"

## sort the data based on the 1) week, 2) student, 3) timestamp
sorted.traces <- traces[ with(traces, order(WEEK, RESOURCE_ID, TIMESTAMP)), ]
head(sorted.traces)

## turn activity to factor variable
sorted.traces$ACTIVITY <- factor(sorted.traces$ACTIVITY)

## rename the variables
colnames(sorted.traces) <- c("SESSION.ID", "STUDENT.ID", "TIMESTAMP", "ACTION", "WEEK", "TOPIC")

########################
## FIT WEEKLY HMM MODELs
########################

#install.packages("depmixS4")
library(depmixS4)

#########
## WEEK 2
#########
w2.traces <- subset(sorted.traces, WEEK == 2)
w2.ntimes.vector <- compute.ntimes(w2.traces)
w2.traces <- add.ntimes.feature(w2.traces, w2.ntimes.vector)

compare.models(w2.traces, max.ns = 7)
## both AIC and BIC have the lowest values for ns=5, and then their values rise again
## so, build a model with 5 states

w2.mod.5s <- depmix(response = ACTION ~ 1, data = w2.traces, nstates = 5,
              family = multinomial("identity"))
w2.5s.fit <- fit(w2.mod.5s, verbose = FALSE)
summary(w2.5s.fit)

## get the estimated state for each observation
head(posterior(w2.5s.fit))
## add them to the trace data
w2.traces$state <- posterior(w2.5s.fit)$state
round(prop.table(table(w2.traces$state)), digits = 3)

#########
## WEEK 3
#########
w3.traces <- subset(sorted.traces, WEEK == 3)
w3.ntimes.vector <- compute.ntimes(w3.traces)
w3.traces <- add.ntimes.feature(w3.traces, w3.ntimes.vector)

compare.models(w3.traces, max.ns = 7)
## it is not clear from the metrics, but it seems that the model with ns=6 
## might be the best one; alternatively, the one with ns = 3

w3.mod.6s <- depmix(response = ACTION ~ 1, data = w3.traces, nstates = 6,
                    family = multinomial("identity"))
w3.6s.fit <- fit(w3.mod.6s, verbose = FALSE)
summary(w3.6s.fit)

## check also the model with 3 states
w3.mod.3s <- depmix(response = ACTION ~ 1, data = w3.traces, nstates = 3,
                    family = multinomial("identity"))
w3.3s.fit <- fit(w3.mod.3s, verbose = FALSE)
summary(w3.3s.fit)
# model with ns=6 is better

## curious if the states in the model with ns=5 will match those in the week 2 model
w3.mod.5s <- depmix(response = ACTION ~ 1, data = w3.traces, nstates = 5,
                    family = multinomial("identity"))
w3.5s.fit <- fit(w3.mod.5s, verbose = FALSE)
summary(w3.5s.fit)
## no, the two models do not match

## all in all, ns=6 is the best solution
## add the estimated state for each observation to the trace data
w3.traces$state <- posterior(w3.6s.fit)$state
round(prop.table(table(w3.traces$state)), digits = 3)

#########
## WEEK 4
#########
w4.traces <- subset(sorted.traces, WEEK == 4)
w4.ntimes.vector <- compute.ntimes(w4.traces)
w4.traces <- add.ntimes.feature(w4.traces, w4.ntimes.vector)

compare.models(w4.traces, max.ns = 7)
## both AIC and BIC have the lowest values for ns=6, and then their values rise again
## so, build a model with 6 states

w4.mod.6s <- depmix(response = ACTION ~ 1, data = w4.traces, nstates = 6,
                    family = multinomial("identity"))
w4.6s.fit <- fit(w4.mod.6s, verbose = FALSE)
summary(w4.6s.fit)

## check also the model with 3 or 4 states, the one with 6 states was not very useful (when interpreted) 
w4.mod.3s <- depmix(response = ACTION ~ 1, data = w4.traces, nstates = 3,
                    family = multinomial("identity"))
w4.3s.fit <- fit(w4.mod.3s, verbose = FALSE)
summary(w4.3s.fit)
## this one is better than ns=6

## now with ns=4
w4.mod.4s <- depmix(response = ACTION ~ 1, data = w4.traces, nstates = 4,
                    family = multinomial("identity"))
w4.4s.fit <- fit(w4.mod.4s, verbose = FALSE)
summary(w4.4s.fit)
## this one seems to be the best, that is, with the meaningful interpretation

## add the estimated state for each observation to the trace data
w4.traces$state <- posterior(w4.4s.fit)$state
round(prop.table(table(w4.traces$state)), digits = 3)

#########
## WEEK 5
#########
w5.traces <- subset(sorted.traces, WEEK == 5)
w5.ntimes.vector <- compute.ntimes(w5.traces)
w5.traces <- add.ntimes.feature(w5.traces, w5.ntimes.vector)

compare.models(w5.traces, max.ns = 7)
## both AIC and BIC have the lowest values for ns=4, and then their values rise again
## so, build a model with 4 states

w5.mod.4s <- depmix(response = ACTION ~ 1, data = w5.traces, nstates = 4,
                    family = multinomial("identity"))
w5.4s.fit <- fit(w5.mod.4s, verbose = FALSE)
summary(w5.4s.fit)
# this model looks fine, but check also the one with 5 states

w5.mod.5s <- depmix(response = ACTION ~ 1, data = w5.traces, nstates = 5,
                    family = multinomial("identity"))
w5.5s.fit <- fit(w5.mod.5s, verbose = FALSE)
summary(w5.5s.fit)
# no, model with 4 states is definitively better

## add the estimated state for each observation to the trace data
w5.traces$state <- posterior(w5.4s.fit)$state
round(prop.table(table(w5.traces$state)), digits = 3)

#########
## WEEK 6
#########
w6.traces <- subset(sorted.traces, WEEK == 6)
w6.ntimes.vector <- compute.ntimes(w6.traces)
w6.traces <- add.ntimes.feature(w6.traces, w6.ntimes.vector)

compare.models(w6.traces, max.ns = 7)
## according to the metrics, models with 3 and 6 states look promising; examine both 

## check the model with ns=6
w6.mod.6s <- depmix(response = ACTION ~ 1, data = w6.traces, nstates = 6,
                    family = multinomial("identity"))
w6.6s.fit <- fit(w6.mod.6s, verbose = FALSE)
summary(w6.6s.fit)

## check also the model with 3 states
w6.mod.3s <- depmix(response = ACTION ~ 1, data = w6.traces, nstates = 3,
                    family = multinomial("identity"))
w6.3s.fit <- fit(w6.mod.3s, verbose = FALSE)
summary(w6.3s.fit)

## model with 6 states is (far) better
w6.traces$state <- posterior(w6.6s.fit)$state
round(prop.table(table(w6.traces$state)), digits = 3)

#########
## WEEK 7
#########
w7.traces <- subset(sorted.traces, WEEK == 7)
w7.ntimes.vector <- compute.ntimes(w7.traces)
w7.traces <- add.ntimes.feature(w7.traces, w7.ntimes.vector)

compare.models(w7.traces, max.ns = 7)
## according to the metrics, models with 6 states seems to be the best 

## check the model with ns=6
w7.mod.6s <- depmix(response = ACTION ~ 1, data = w7.traces, nstates = 6,
                    family = multinomial("identity"))
w7.6s.fit <- fit(w7.mod.6s, verbose = FALSE)
summary(w7.6s.fit)

## since the model with 6 states have a couple of almost identical states,
## it might be worth exploring a model with lower number of states
w7.mod.4s <- depmix(response = ACTION ~ 1, data = w7.traces, nstates = 4,
                    family = multinomial("identity"))
w7.4s.fit <- fit(w7.mod.4s, verbose = FALSE)
summary(w7.4s.fit)

w7.traces$state <- posterior(w7.4s.fit)$state
round(prop.table(table(w7.traces$state)), digits = 3)

#########
## WEEK 8
#########
w8.traces <- subset(sorted.traces, WEEK == 8)
w8.ntimes.vector <- compute.ntimes(w8.traces)
w8.traces <- add.ntimes.feature(w8.traces, w8.ntimes.vector)

compare.models(w8.traces, max.ns = 7)
## according to the metrics, models with 3 and 5 states look promising; examine both 

## check the model with ns=5
w8.mod.5s <- depmix(response = ACTION ~ 1, data = w8.traces, nstates = 5,
                    family = multinomial("identity"))
w8.5s.fit <- fit(w8.mod.5s, verbose = FALSE)
summary(w8.5s.fit)

## check also the model with 3 states
w8.mod.3s <- depmix(response = ACTION ~ 1, data = w8.traces, nstates = 3,
                    family = multinomial("identity"))
w8.3s.fit <- fit(w8.mod.3s, verbose = FALSE)
summary(w8.3s.fit)

w8.traces$state <- posterior(w8.5s.fit)$state
round(prop.table(table(w8.traces$state)), digits = 3)

#########
## WEEK 9
#########
w9.traces <- subset(sorted.traces, WEEK == 9)
w9.ntimes.vector <- compute.ntimes(w9.traces)
w9.traces <- add.ntimes.feature(w9.traces, w9.ntimes.vector)

compare.models(w9.traces, max.ns = 7)
## both AIC and BIC have the lowest values for ns=4, and then their values rise again

w9.mod.4s <- depmix(response = ACTION ~ 1, data = w9.traces, nstates = 4,
                    family = multinomial("identity"))
w9.4s.fit <- fit(w9.mod.4s, verbose = FALSE)
summary(w9.4s.fit)

## check also the model with 5 states
m <- depmix(response = ACTION ~ 1, data = w9.traces, nstates = 5,
                    family = multinomial("identity"))
m.fit <- fit(m, verbose = FALSE)
summary(m.fit)


w9.traces$state <- posterior(m.fit)$state
round(prop.table(table(w9.traces$state)), digits = 3)

##########
## WEEK 10
##########
w10.traces <- subset(sorted.traces, WEEK == 10)
w10.ntimes.vector <- compute.ntimes(w10.traces)
w10.traces <- add.ntimes.feature(w10.traces, w10.ntimes.vector)

compare.models(w10.traces, max.ns = 7)
## according to the metrics, models with 6 states seems to be the best 

## check the model with ns=6
w10.mod.6s <- depmix(response = ACTION ~ 1, data = w10.traces, nstates = 6,
                    family = multinomial("identity"))
w10.6s.fit <- fit(w10.mod.6s, verbose = FALSE)
summary(w10.6s.fit)
# 4 out of 6 states are related exclusively to the summative assessment 
# (out of those, 2 seem to be almost identical); therefore, it might be worth 
# examining models with lower number of states

w10.mod.4s <- depmix(response = ACTION ~ 1, data = w10.traces, nstates = 4,
                     family = multinomial("identity"))
w10.4s.fit <- fit(w10.mod.4s, verbose = FALSE)
summary(w10.4s.fit)

w10.mod.5s <- depmix(response = ACTION ~ 1, data = w10.traces, nstates = 5,
                     family = multinomial("identity"))
w10.5s.fit <- fit(w10.mod.5s, verbose = FALSE)
summary(w10.5s.fit)
# this one (ns=5) looks reasonable, prone to explanation

w10.traces$state <- posterior(w10.5s.fit)$state
round(prop.table(table(w10.traces$state)), digits = 3)

##########
## WEEK 11
##########
w11.traces <- subset(sorted.traces, WEEK == 11)
w11.ntimes.vector <- compute.ntimes(w11.traces)
w11.traces <- add.ntimes.feature(w11.traces, w11.ntimes.vector)

compare.models(w11.traces, max.ns = 7)
# model with 5 states seems to be the best option (based on both AIC and BIC) 

w11.mod.5s <- depmix(response = ACTION ~ 1, data = w11.traces, nstates = 5,
                     family = multinomial("identity"))
w11.5s.fit <- fit(w11.mod.5s, verbose = FALSE)
summary(w11.5s.fit)

# m <- depmix(response = ACTION ~ 1, data = w11.traces, nstates = 6,
#        family = multinomial("identity"))
# m.fit <- fit(m, verbose = FALSE)
# summary(m.fit)

w11.traces$state <- posterior(w11.5s.fit)$state
round(prop.table(table(w11.traces$state)), digits = 3)

##########
## WEEK 12
##########
w12.traces <- subset(sorted.traces, WEEK == 12)
w12.ntimes.vector <- compute.ntimes(w12.traces)
w12.traces <- add.ntimes.feature(w12.traces, w12.ntimes.vector)

compare.models(w12.traces, max.ns = 7)
# the metrics are unclear about the best model; choosing ns=5 as that is where AIC and BIC
# have the last significant drop in value

w12.mod.5s <- depmix(response = ACTION ~ 1, data = w12.traces, nstates = 5,
                     family = multinomial("identity"))
w12.5s.fit <- fit(w12.mod.5s, verbose = FALSE)
summary(w12.5s.fit)

w12.traces$state <- posterior(w12.5s.fit)$state
round(prop.table(table(w12.traces$state)), digits = 3)

##########
## WEEK 13
##########
w13.traces <- subset(sorted.traces, WEEK == 13)
w13.ntimes.vector <- compute.ntimes(w13.traces)
w13.traces <- add.ntimes.feature(w13.traces, w13.ntimes.vector)

compare.models(w13.traces, max.ns = 7)
# model with 5 states seems to be the best option (based on both AIC and BIC) 

w13.mod.5s <- depmix(response = ACTION ~ 1, data = w13.traces, nstates = 5,
                     family = multinomial("identity"))
w13.5s.fit <- fit(w13.mod.5s, verbose = FALSE)
summary(w13.5s.fit)

w13.traces$state <- posterior(w13.5s.fit)$state
round(prop.table(table(w13.traces$state)), digits = 3)

###########################
# merge all the trace data 
###########################

merged.data <- as.data.frame(rbind(w2.traces, w3.traces, w4.traces, w5.traces, w6.traces, w7.traces,
                                   w8.traces, w9.traces, w10.traces, w11.traces, w12.traces, w13.traces))

str(merged.data)
saveRDS(merged.data, file = "Intermediate_files/weekly_HMMs_states.RData")


####################
## UTILITY FUNCTIONS
####################

## the f. computes 'ntimes' attribute required for the depmix function
## the f. assumes that the input df (trace.data) is sorted based on STUDENT.ID
compute.ntimes <- function(trace.data) {
  ntimes <- vector()
  k <- 1
  i <- 1
  while(i <= nrow(trace.data)) {
    current.student <- trace.data$STUDENT.ID[i]
    count <- 1
    if ( i == nrow(trace.data)) {
      ntimes[k] <- count
      break
    }
    same.student <- T
    j <- i + 1
    while( same.student == T & j <= nrow(trace.data)) {
      if ( trace.data$STUDENT.ID[j] == current.student ) { 
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
  ntimes
}

## the f. adds the ntimes vector to the trace data, by associating each 
## observation related to a particular student with the number of observations
## available for that student (ntimes)
add.ntimes.feature <- function(trace.data, ntimes.vector) {
 
  seq.lengths <- as.data.frame( cbind(stud.id = unique(trace.data$STUDENT.ID),
                                      length = ntimes.vector))
  trace.data$ntimes <- vector(mode = "integer", length = nrow(trace.data))
  for (i in 1:nrow(trace.data)) {
    trace.data$ntimes[i] <- 
      seq.lengths[ seq.lengths$stud.id == trace.data$STUDENT.ID[i], 2]  
  }
  trace.data
  
}

## the f. computes several HMM models, with ns=>min.ns & ns<=max.ns
## using the ACTIONS variable from the weekly.traces data
compare.models <- function(weekly.traces, min.ns=2, max.ns=6) {
  set.seed(2401)
  eval.metrics <- data.frame()
  for (ns in min.ns:max.ns) {
    mod <- depmix(response = ACTION ~ 1, data = weekly.traces, 
                  nstates = ns, family = multinomial("identity"))
    mod.fit <- fit(mod, verbose = FALSE)
    metrics <- c(ns, AIC(mod.fit), BIC(mod.fit), logLik(mod.fit))
    eval.metrics <- as.data.frame( rbind(eval.metrics, metrics) )
  }
  colnames(eval.metrics) <- c('n.states', 'AIC', 'BIC', 'logLik')
  require(knitr)
  kable(x = eval.metrics, format = "rst")
}