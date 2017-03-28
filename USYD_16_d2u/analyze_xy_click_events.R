events <- readRDS(file = "Intermediate_results/filtered_events_w2-13.RData")
str(events)

table(events$action_id)
## select only xy-click events
xy.clicks <- subset(events, action_id=="xy-click") 
str(xy.clicks)
# 10957 events of this type
## remove columns that are not needed
xy.clicks <- xy.clicks[,-c(3,7,8)]

## examine the payload data
xy.clicks$payload[500:520]

## load the f. for processing the payload field
source(file = "event_payload_extract_functions.R")

## Evaluation categories:
## - H_D: Helped/Difficult (I quadrant)
## - H_E: Helped/Easy   (II quadrant)
## - C_D: Confused/Difficult (III quadrant)
## - C_E: Confused/Easy (IV quadrant)
eval_categories <- c("H_D", "H_E","C_D", "C_E")

## Add also 2 variables expressing:
## - the level of easiness - the value on the x axis represents the level of easiness as the
##   percentage (0-100); the negative value stands for the level of difficulty expressed, again, 
##   as the percentage 
## - the level of helpfulness - the value on the y axis represents the level of helpfulness as 
##   the percentage (0-100); the negative value stands for the level of confusion, expressed, 
##   again, as the percentage

for(i in 1:nrow(xy.clicks)) {
  xy.data <- xyclick.data.extract(xy.clicks$payload[i])
  xy.clicks$resource_id[i] <- xy.data[1]
  x <- as.integer(xy.data[2])
  y <- as.integer(xy.data[3])
  
  ## introduced to deal with errors in the data
  if (x > 100) x <- 100
  if (x < -100) x <- -100
  if (y > 100) y <- 100
  if (y < -100) y <- -100
  
  xy.clicks$easiness[i] <- x
  xy.clicks$helpfulness[i] <- y
  if ( x < 0 & y > 0 )
    xy.clicks$eval_cat[i] <- eval_categories[1]
  else if ( x > 0 & y > 0 ) xy.clicks$eval_cat[i] <- eval_categories[2]
  else if ( x < 0 & y < 0 ) xy.clicks$eval_cat[i] <- eval_categories[3]
  else xy.clicks$eval_cat[i] <- eval_categories[4]
}
str(xy.clicks)

which(is.na(xy.clicks$easiness))
which(is.na(xy.clicks$helpfulness))
# none

## check the eval_cat column
unique(xy.clicks$eval_cat)
length(which(is.na(xy.clicks$eval_cat)==T))
xy.clicks$eval_cat <- factor(xy.clicks$eval_cat)
table(xy.clicks$eval_cat)
round(prop.table(table(xy.clicks$eval_cat)), digits = 3)
#  C_D      C_E     H_D    H_E 
# 0.155   0.119   0.328   0.398 

## check the resource_id column
unique(xy.clicks$resource_id)
length(which(is.na(xy.clicks$resource_id)==T))
xy.clicks$resource_id <- factor(xy.clicks$resource_id)

summary(xy.clicks$easiness)
summary(xy.clicks$helpfulness)

str(xy.clicks)
## remove payload as it is not needed any more
xy.clicks <- xy.clicks[,-3]

## for the same activity / resource, a student can make several consecutive
## clicks on the canvas; only the last one in the sequence should be considered
## so, for each student and each resource remove all click events except the last one 

## sort the data based on the user_id, resource_id, timestamp
xy.clicks <- xy.clicks[ order(xy.clicks$user_id, xy.clicks$resource_id, xy.clicks$timestamp), ]
xy.clicks[100:140,]
filtered.xy.clicks <- data.frame()
for(i in 1:(nrow(xy.clicks)-1)) {
  if (xy.clicks$resource_id[i] != xy.clicks$resource_id[i+1]) {
    filtered.xy.clicks <- rbind(filtered.xy.clicks, xy.clicks[i,])
  } 
}
filtered.xy.clicks <- rbind(filtered.xy.clicks, xy.clicks[nrow(xy.clicks),])
filtered.xy.clicks[1:40,]
nrow(filtered.xy.clicks)
# only 4325 (before filtering: 10957; ~60% of events removed)

## sort filtered clicks based on the student, week and timestamp
filtered.xy.clicks <- filtered.xy.clicks[order(filtered.xy.clicks$user_id,
                                               filtered.xy.clicks$week,
                                               filtered.xy.clicks$timestamp),]
filtered.xy.clicks[1:40,]


## store the results
write.csv(x = filtered.xy.clicks, file = "Intermediate_results/processed_XY-click_events.csv",
          quote = F, row.names = F)
saveRDS(object = filtered.xy.clicks, file = "Intermediate_results/processed_XY-click_events.RData")


## examine the distribution of activities across the 4 evaluation categories
table(filtered.xy.clicks$eval_cat)
# C_D  C_E  H_D  H_E 
# 542  355 1389 2039
round(prop.table(table(filtered.xy.clicks$eval_cat)), digits = 3)
#  C_D   C_E   H_D   H_E 
# 0.125 0.082 0.321 0.471

## examine how different subject topics were perceived by the students
topics <- c('CST', 'COD', 'DRM', 'CDL', 'SDL', 'ARC', 'ISA', 'ASP', 'ADM', 'HLP')
filtered.xy.clicks$topic <- as.character(filtered.xy.clicks$topic)
topic.evals <- subset(filtered.xy.clicks, topic %in% topics)
table(topic.evals$eval_cat, topic.evals$topic)
topic.evals.prop <- round(prop.table(table(topic.evals$eval_cat, topic.evals$topic), margin = 2), digits = 3)

require(knitr)
kable(topic.evals.prop, format = "rst")

## plot the data
topic.eval.df <- as.data.frame(topic.evals.prop)
str(topic.eval.df)
colnames(topic.eval.df) <- c("eval_cat", "topic", "percent")

library(ggplot2)
ggplot(data=topic.eval.df, aes(x=topic, y=percent, fill=eval_cat)) +
  geom_bar(stat="identity") +
  ylab("Percent of topic-related activities\n") + 
  xlab("\nCourse topics") +
  scale_fill_manual(values = c("#0072B2","#56B4E9", "#009E73", "#F0E442"),
                    name = "Evaluation \ncategories",
                    breaks = c("C_D", "C_E", "H_D", "H_E"),
                    labels = c("Confused/Difficult", "Confused/Easy",
                               "Helped/Difficult", "Helped/Easy")) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        legend.title = element_text(size=11, face="bold"),
        legend.text = element_text(size = 10),
        legend.key = element_rect(colour = NA))


################################################################################
## examine how many students used 2D canvas to provide the feedback on resource
## easiness / helpfulness, and how successful they were in the exams
################################################################################

xy.clicks <- readRDS(file = "Intermediate_results/processed_XY-click_events.RData")
xy.users <- unique(xy.clicks$user_id)
length(xy.users)
# 422

scores <- read.csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
str(scores)
scores <- scores[,-2]
scores <- subset(scores, is.na(USER_ID)==F)
# 488

## find students who didn't evaluated resources
no.evals <- setdiff(scores$USER_ID, xy.users)
# 73 students
setdiff(xy.users, scores$USER_ID)
# 7 students
## get their scores
no.evals.scores <- subset(scores, USER_ID %in% no.evals)

## how many of such students have exam scores below the average score

apply(scores[,c(2,3)], 2, shapiro.test)
# not normally distributed; use median as the average value
mt.median <- median(scores$SC_MT_TOT, na.rm = T)
fe.median <- median(scores$SC_FE_TOT, na.rm = T)
length(which(no.evals.scores$SC_MT_TOT < mt.median))
# 37
length(which(no.evals.scores$SC_FE_TOT < fe.median))
# 44
length(which(no.evals.scores$SC_MT_TOT < mt.median & no.evals.scores$SC_FE_TOT < fe.median))
# 32

## examine the number of resources ealuated by each student
## who evaluated at least one resource
evals.per.stud <- as.data.frame(table(xy.clicks$user_id))
colnames(evals.per.stud) <- c("user_id", "eval_count") 

## merge these data with scores data
evals.per.stud <- merge(x = evals.per.stud, y = scores, 
                        by.x = "user_id", by.y = "USER_ID",
                        all.x = F, all.y = F)

summary(evals.per.stud$eval_count)

## examine correlation between the number of evaluations and exam scores
with(evals.per.stud, cor.test(x = eval_count, y = SC_MT_TOT, method = "spearman"))
# S = 8722300, p-value = 3.02e-08, rho=0.2677857
with(evals.per.stud, cor.test(x = eval_count, y = SC_FE_TOT, method = "spearman"))
# S = 8455400, p-value = 1.703e-09, rho=0.290188

################################################################################
## associate resource id - as the one used in xy-click events - to the events
## of the type "resource-view" and "embedded-question"
##
## should be also done for exercises ("exco-answer" events), but it seems that 
## it's not possible due to the missmatch between the identifiers used in "xy-click" 
## and "exco-answer" events
################################################################################

clicks <- readRDS(file = "Intermediate_results/processed_XY-click_events.RData")
str(clicks)
sort(unique(clicks$resource_id))

events <- readRDS(file = "Intermediate_results/filtered_events_w2-13.RData")
str(events)

## examine payloads of different event types - those potentially assocaited with
## resources that students evaluated on the 2D canvas

events$payload[events$action_id=="resource-view" & events$topic=="ADM"][2100:2130]
# 'url' of resource-view payload can be matched against resource_id from xy-clicks
# example: "{\"url\":\"https://flip.ee.usyd.edu.au/elec1601/Material/ADM/Activities/ADM_addfields_program/ADM_addfields_program.html\"}"

events$payload[events$action_id=="embedded-question" & events$topic=="ADM"][510:540]
# 'question-id' of the 'embedded-question' payload can be used for matching, as well
# example: "{\"question_id\":\"ADM-regdirindir-videoeqt-eqt_1\",\"answer\":0}" 

events$payload[events$action_id=="exco-answer" & events$topic=="CDL"][1000:1010]
events$payload[events$action_id=="embedded-video" & events$topic=="CDL"][1000:1010]
# no field to be used for matching

## focus on the events that can be matched to students' evaluations (based on resource id)
## 2 type of events: "resource-view" and "embedded-question"
events <- subset(events, action_id %in% c("resource-view", "embedded-question"))
table(events$action_id)
events$action_id <- as.character(events$action_id)

## process the payloads and extract resource ids

## load the f. for processing the payload field
source(file = "event_payload_extract_functions.R")

## extract 'url' from resource-view events and assign it to 'res_id' variable
events$res_id[events$action_id=="resource-view"] <- 
                        resview.id.extract(events$payload[events$action_id=="resource-view"])
## extract 'question_id' from embedded-question events and assign it to 'res_id' variable
events$res_id[events$action_id=="embedded-question"] <- 
                        mcq.id.extract(events$payload[events$action_id=="embedded-question"])

str(events)
which(is.na(events$res_id))
# no NAs

# remove payload - no longer needed
events <- events[,-4]

## now, do the matching between xy-clicks' 'resource_id' and events' 'res_id'
## in all the cases, resource_id should be a a part (substring) of res_id  

## check if the topics in the two datasets match
setdiff(unique(events$topic), unique(clicks$topic))
# "CST" "ORG" "TEA" in events that are not in clicks
setdiff(unique(clicks$topic), unique(events$topic))
# 'EXAM' in clicks, but not in events
common.topics <- intersect(unique(events$topic), unique(clicks$topic))

events$topic <- as.character(events$topic)
clicks$topic <- as.character(clicks$topic)
clicks$resource_id <- as.character(clicks$resource_id)
events$xy_res_id <- NA
for(i in 1:length(common.topics)) {
  t <- common.topics[i]
  # get all resource_id values associated with the current topic t
  topic.res.ids <- unique(clicks$resource_id[clicks$topic==t])
  for(j in 1:length(topic.res.ids)){
      matching <- which(grepl(pattern = topic.res.ids[j], 
                              x = events$res_id[events$topic==t], 
                              fixed = T))
      events$xy_res_id[events$topic==t][matching] <- topic.res.ids[j]
      # replace dashes (-) with underscores (_) in resource_ids from xy-click events
      # and repeat the matching
      topic.res.id.alt <- gsub(pattern = "-", replacement = "_", x = topic.res.ids[j], fixed = T)
      matching.alt <- which(grepl(pattern = topic.res.id.alt, 
                                  x = events$res_id[events$topic==t], 
                                  fixed = T))
      events$xy_res_id[events$topic==t][matching.alt] <- topic.res.ids[j]
  }
}

length(unique(events$xy_res_id))
# 116 (out of 119)
## check which resource_ids from xy-click events are not found in the events data
setdiff(unique(clicks$resource_id), unique(events$xy_res_id))
# "SDL-preparedesign-problem" "Exam-simulation-prepare"   "Exam-simulation" 

length(which(is.na(events$xy_res_id)))
# 99437 events were not assigned xy_res_id
length(which(is.na(events$xy_res_id))) / nrow(events)
# 47.4% 

## keep only events that can be associated with student evaluation of 
## difficulty and helpfulness of the corresponding resource 
events.w.eval <- subset(events, is.na(xy_res_id)==F)
str(events.w.eval)
## remove the assess_type attribute as it is not applicable to the two types of events
events.w.eval <- events.w.eval[,-7]

## save the data as an intermediate result
saveRDS(object = events.w.eval, 
        file = "Intermediate_results/res-view_and_embed-question_events_w_xy-res-ids.RData")


#############################################################################
## compare/contrast student level of engagement with different resources 
## (identified with xy_res_id) with the student evaluation of those resources 
## (easiness, helpfulness)
##
## this can be done using only a subset of all the events where a matching
## was possible based on the resource identifiers 
## (47.4% of all the resource-view and embedded-question events) 
#########################################################################

## create a data frame with the number of resource-view events (actions) for each of the 
## xy-resource-ids (i.e. resources that were evaluated by the students)
res.view.dist <- as.data.frame(table(events.w.eval$xy_res_id[events$action_id=="resource-view"]))
colnames(res.view.dist) <- c("resource_id", "count")
res.view.dist$resource_id <- as.character(res.view.dist$resource_id)
head(res.view.dist)

## now, do the same for the "embedded-question" event, but first, differentiate between
## correct, incorrect, and answer-requested question-answering attempts
mcq.events <- subset(events.w.eval, action_id=="embedded-question")
str(mcq.events)
mcq.events$response <- as.integer(mcq.events$response)

mcq.counts <- as.matrix(table(mcq.events$xy_res_id, mcq.events$response))
res.ids <- row.names(mcq.counts)
mcq.counts.df <- data.frame(resource_id=row.names(mcq.counts),
                            count_c=mcq.counts[,3],
                            count_i=mcq.counts[,2],
                            count_sr=mcq.counts[,1], stringsAsFactors = F, row.names = NULL)
mcq.counts.df


## now, summarize the evaluation data
## using the xy-click data, for each resource_id calculate the number of times it was evaluated
## as difficult, easy, helpful, confusing
str(clicks)
clicks$easiness_bin <- as.numeric(clicks$easiness > 0)
clicks$helpfulness_bin <- as.numeric(clicks$helpfulness > 0)
table(clicks$easiness_bin)
table(clicks$helpfulness_bin)

res.ease.cnt <- tapply(X = clicks$easiness_bin, 
                                 INDEX = clicks$resource_id, 
                                 FUN = sum, na.rm = FALSE)

res.helpy.cnt <- tapply(X = clicks$helpfulness_bin, 
                                    INDEX = clicks$resource_id, 
                                    FUN = sum, na.rm = FALSE)

res.eval <- as.data.frame(cbind(resource_id=unlist(dimnames(res.ease.cnt)),
                                easy_count=as.vector(res.ease.cnt),
                                helpful_count=as.vector(res.helpy.cnt)), stringsAsFactors = F)
str(res.eval)
res.eval$easy_count <- as.integer(res.eval$easy_count)
res.eval$helpful_count <- as.integer(res.eval$helpful_count)
res.eval


#################################################################
## examine, first, the correspondance between resource visits 
## (resource-view events) and students' evaluation of resources 
#################################################################

## merge access frequency (from res.view.dist) with evaluation (from res.eval)
res.view.eval <- merge(x = res.view.dist, y = res.eval,
                       by = "resource_id", all.x = T, all.y = F)
str(res.view.eval)

## store these data
saveRDS(object = res.view.eval, file = "Intermediate_results/res-view_visits_evals_counts.RData")

## check if the data is normally distributed
apply(X = res.view.eval[,c(2:4)], MARGIN = 2, shapiro.test)
# none is normally distributed

## use Spearman correlation coefficient to examine the presence of association between 
## resource visits and 
## - number of times it was characterized as easy (easy_count)
## - number of times it was characterized as helpful (helpful_count)
with(res.view.eval, cor.test(x = count, y = easy_count, method = "spearman", alternative = "g"))
# S = 30658, p-value < 2.2e-16, rho=0.8821428 
# there is a significant strong correlation
with(res.view.eval, cor.test(x = count, y = helpful_count, method = "spearman", alternative = "g"))
# S = 17074, p-value < 2.2e-16, rho=0.9343618
# again, a strong significant correlation

## plot the counts data

## first, prepare the data for plotting
res.view.eval$resource_id <- as.factor(res.view.eval$resource_id)
require(tidyr)
res.view.eval.long <- gather(data = res.view.eval, key = count_type, value = count_vals,
                             ... = count:helpful_count, factor_key = T)
head(res.view.eval.long)
## add the topic column
res.view.eval.long$topic <- strtrim(res.view.eval.long$resource_id, width = 3)

## now, plot the data
require(ggplot2)
ggplot(data=res.view.eval.long[res.view.eval.long$topic=="ADM",], 
       aes(x=resource_id, y=count_vals)) +
  geom_bar(aes(fill = count_type), stat="identity", position = "dodge") +
  ylab("Counts\n") + 
  xlab("\nEvaluated course resources") +
  scale_fill_manual(values = c("#0072B2", "#F0E442", "#009E73"), #"#56B4E9"
                    name = "Counts",
                    breaks = c("count", "easy_count", "helpful_count"),
                    labels = c("Visits count", "Marked easy count",
                               "Marked helpful count")) +
  coord_flip()+
  theme_bw() +
  theme(panel.border = element_blank(), 
        legend.title = element_text(size=11, face="bold"),
        legend.text = element_text(size = 10),
        legend.key = element_rect(colour = NA))
        #axis.text.y = element_blank())

## SUMMARY: 
## strong significant correlation between the number of times a resource
## was visited and the number of times it was assessed as:
## i) easy, ii) helpful  

#################################################################
## now, examine the correspondance between question (formative
## assessment) attempts (embedded-question events) and students' 
## evaluation of those questions 
#################################################################

## merge MCQ attempts (from mcq.counts.df) with evaluation (from res.eval)
mcq.eval <- merge(x = mcq.counts.df, y = res.eval,
                  by = "resource_id", all.x = T, all.y = F)
str(mcq.eval)
mcq.eval

## store these data
saveRDS(object = mcq.eval, file = "Intermediate_results/mcq_attempts_evals_counts.RData")

## check if the data is normally distributed
apply(X = mcq.eval[,c(2:6)], MARGIN = 2, shapiro.test)
# only count_i is normally distributed

## use Spearman correlation coefficient to examine the presence of association between 
## - correct attempts and perceived easiness (easy_count)
## - incorrect attempts and perceived easiness (easy_count)
## - correct attempts and perceived helpfulness (helpful_count)
## - incorrect attempts and perceived helpfulness (helpful_count)
with(mcq.eval, cor.test(x = count_c, y = easy_count, method = "spearman", alternative = "g"))
# S = 785.43, p-value = 2.107e-10, rho=0.8560426 
# there is a significant strong two-sided correlation
with(mcq.eval, cor.test(x = count_i, y = easy_count, method = "spearman", alternative = "g"))
# S = 1935.1, p-value = 3.332e-05, rho=0.6453329
# a significant strong two-sided correlation
with(mcq.eval, cor.test(x = count_c, y = helpful_count, method = "spearman", alternative = "g"))
# S = 249.18, p-value < 2.2e-16, rho=0.9543288
# a significant strong two-sided correlation
with(mcq.eval, cor.test(x = count_i, y = helpful_count, method = "spearman", alternative = "g"))
# S = 987.72, p-value = 5.044e-09, rho=0.8189656
with(mcq.eval, cor.test(x = count_sr, y = helpful_count, method = "spearman", alternative = "g"))
# S = 656.48, p-value = 1.686e-11, rho=0.8796773

## plot the counts data

## first, prepare the data for plotting
mcq.eval$resource_id <- as.factor(mcq.eval$resource_id)
mcq.eval.long <- gather(data = mcq.eval, key = count_type, value = count_vals,
                        ... = count_c:helpful_count, factor_key = T)
head(mcq.eval.long)

## now, draw the plot
ggplot(data=mcq.eval.long, aes(x=resource_id, y=count_vals)) +
        geom_bar(aes(fill = count_type), stat="identity", position = "dodge") +
        ylab("Counts\n") + 
        xlab("\nEvaluated course resources") +
        scale_fill_manual(values = c('#377eb8','#e41a1c','#4daf4a','#ff7f00','#984ea3'),
                          name = "Counts",
                          breaks = c("count_c", "count_i","count_sr",
                                     "easy_count", "helpful_count"),
                          labels = c("Correct attempts", "Incorrect attempts", 
                                     "Answer requested", "Marked easy count",
                                     "Marked helpful count")) +
        coord_flip()+
        theme_bw() +
        theme(panel.border = element_blank(), 
              legend.title = element_text(size=11, face="bold"),
              legend.text = element_text(size = 10),
              legend.key = element_rect(colour = NA))
      #axis.text.y = element_blank())


#########################################################################
## do a similar kind of analysis as the one given above, but instead of  
## considering the total number of revisits / MCQ attempts / evaluations, 
## examine these numbers at the student and resource level
#########################################################################

## start with MCQ attempts
## for each student, compute the number of correct, incorrect and answer-requested
## attempts for each MCQ resource
stud.ids <- unique(mcq.events$user_id)
stud.mcq.counts <- list()
k <- 1
for(i in 1:length(stud.ids)) {
  stud.dat <- subset(mcq.events, user_id==stud.ids[i])
  mcqs <- unique(stud.dat$xy_res_id)
  for(j in 1:length(mcqs)) {
    count_c <- nrow(stud.dat[stud.dat$response==1 & stud.dat$xy_res_id==mcqs[j],])
    if (is.na(count_c)) count_c <- 0
    count_i <- nrow(stud.dat[stud.dat$response==0 & stud.dat$xy_res_id==mcqs[j],])
    if (is.na(count_i)) count_i <- 0
    count_sr <- nrow(stud.dat[stud.dat$response==-1 & stud.dat$xy_res_id==mcqs[j],])
    if (is.na(count_sr)) count_sr <- 0
    stud.mcq.counts[[k]] <- c(stud.ids[i], mcqs[j], count_c, count_i, count_sr)
    k <- k + 1
  }
}
str(stud.mcq.counts)
head(stud.mcq.counts)

stud.mcq.counts.df <- data.frame(matrix(unlist(stud.mcq.counts), 
                                        nrow=length(stud.mcq.counts), ncol = 5, byrow=T),
                                 stringsAsFactors=FALSE)
str(stud.mcq.counts.df)
colnames(stud.mcq.counts.df) <- c("user_id", "resource_id", "count_c", "count_i", "count_sr")
stud.mcq.counts.df$user_id <- as.integer(stud.mcq.counts.df$user_id)
stud.mcq.counts.df$count_c <- as.integer(stud.mcq.counts.df$count_c)
stud.mcq.counts.df$count_i <- as.integer(stud.mcq.counts.df$count_i)
stud.mcq.counts.df$count_sr <- as.integer(stud.mcq.counts.df$count_sr)

## merge the counts with the resource evaluation data
stud.mcq.dat <- merge(x = stud.mcq.counts.df, y = clicks[,c(1,5:10)], 
              by = c("user_id", "resource_id"), all.x = F, all.y = F)
str(stud.mcq.dat)

## store the merged data
saveRDS(object = stud.mcq.dat, file = "Intermediate_results/stud_mcq_counts_evals.RData")

## check if the data are normally distributed
apply(X = stud.mcq.dat[,c(3:7)], MARGIN = 2, shapiro.test)
# no variable is normally distributed

## use Spearman correlation coefficient to examine the presence of association between 
## - correct attempts (count_c) and perceived easiness (easiness)
## - incorrect attempts (count_i) and perceived easiness (easiness)
## - correct attempts (count_c) and perceived helpfulness (helpfulness)
## - incorrect attempts (count_i) and perceived helpfulness (helpfulness)
with(stud.mcq.dat, cor.test(x = count_c, y = easiness, method = "spearman", alternative = "g"))
# S = 2227100000, p-value = 0.05102, rho=0.0334
# positive correlation is not significant, neither is two-sided correlation
with(stud.mcq.dat, cor.test(x = count_i, y = easiness, method = "spearman"))
# S = 3026300000, p-value < 2.2e-16, rho=-0.3135
# significant negative correlation
with(stud.mcq.dat, cor.test(x = count_c, y = helpfulness, method = "spearman"))
# S = 2089700000, p-value = 4.993e-06, rho=0.0930
# significant positive correlation, though very small in magnitude
with(stud.mcq.dat, cor.test(x = count_i, y = helpfulness, method = "spearman"))
# S = 2590400000, p-value = 4.993e-10, rho=0.1243
# significant negative correlation, though small in magnitude
with(stud.mcq.dat, cor.test(x = count_sr, y = helpfulness, method = "spearman"))
# S = 2505700000, p-value = 1.755e-05, rho=-0.0875
# significant negative correlation, though very small in magnitude


## Now, do the same for resource visits ("resource-view" events); namely,
## for each student, compute the number of visits (views) of each evaluated resource
res.views <- subset(events, action_id=="resource-view")
stud.ids <- unique(res.views$user_id)
stud.res.counts <- list()
k <- 1
for(i in 1:length(stud.ids)) {
  stud.dat <- subset(res.views, user_id==stud.ids[i])
  stud.res <- unique(stud.dat$xy_res_id)
  for(j in 1:length(stud.res)) {
    if (is.na(stud.res[j]) || stud.res[j]=="NA") next
    count <- nrow(stud.dat[stud.dat$xy_res_id==stud.res[j],])
    if (is.na(count)) count <- 0
    stud.res.counts[[k]] <- c(stud.ids[i], stud.res[j], count)
    k <- k + 1
  }
}
head(stud.res.counts)

stud.res.count.df <- data.frame(matrix(unlist(stud.res.counts), 
                                       nrow=length(stud.res.counts), ncol = 3, byrow=T),
                                stringsAsFactors=FALSE)
str(stud.res.count.df)
colnames(stud.res.count.df) <- c("user_id", "resource_id", "count")
stud.res.count.df$user_id <- as.integer(stud.res.count.df$user_id)
stud.res.count.df$count <- as.integer(stud.res.count.df$count)

## merge access counts and resource evaluation
stud.res.dat <- merge(x = stud.res.count.df, y = clicks[,c(1,5:10)], 
                      by = c("user_id", "resource_id"), all.x = F, all.y = F)
str(stud.res.dat)

## store the merged data
saveRDS(object = stud.res.dat, file = "Intermediate_results/stud_res-view_counts_evals.RData")

## use Spearman correlation coefficient to examine the presence of association between 
## - number of resource visits (count) and perceived easiness of the resource (easiness)
## - number of resource visits (count) and perceived helpfulness of the resource (helpfulness)
with(stud.res.dat, cor.test(x = count, y = easiness, method = "spearman"))
# no correlation: S = 1.3064e+10, p-value = 0.5588, rho=-0.009
with(stud.res.dat, cor.test(x = count, y = helpfulness, method = "spearman"))
# significant positive correlation, though small in magnitude:
# S = 1.1772e+10, p-value = 2.773e-09, rho=0.0908