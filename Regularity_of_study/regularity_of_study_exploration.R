library(tidyverse)

######################################################################################
# create a box plot showing the distribution of study sessions for each day of a week
######################################################################################
wd <- read.csv("Intermediate_results/regularity_of_study/weekday_session_props.csv")
str(wd)

head(wd)
wd <- wd[,c(1:8)]
colnames(wd) <- c('user_id', 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')

wd_long <- gather(wd, weekday, weekday_cnt, Sun:Sat, factor_key=TRUE)
str(wd_long)
head(wd_long)

ggplot(wd_long, aes(x=weekday, y=weekday_cnt, fill=weekday)) + geom_boxplot()


####################################################################
## create a plot showing the daily distribution of study sessions 
## (ie. the number of sessions per each day of the course)
####################################################################
require(lubridate)

session.data <- readRDS("Intermediate_results/filtered_sessions_w2to13.RData")
session.data$start_time <- as.POSIXct(session.data$start_time, tz = 'Australia/Sydney')

str(session.data)
session.data$date <- date(session.data$start_time)
length(unique(session.data$date))
# 92 days

daily.counts <- as.data.frame(table(session.data$date))
str(daily.counts)
colnames(daily.counts)[1] <- "date"

ggplot(data = daily.counts, aes(x = date, y = Freq, group = 1)) +
  geom_line() +
  ylab("Number of sessions") +
  theme_minimal() +
  theme(axis.text.x = element_blank())


######################################################################################
# create a plot showing the number of active students per each day of the course
# (active students = students who had at least one session during a particular day)
######################################################################################
student.daily.counts <- as.data.frame(table(session.data$user_id, session.data$date), 
                                      stringsAsFactors = F)
str(student.daily.counts)
colnames(student.daily.counts) <- c("user_id", "date", "ses_cnt")
student.daily.counts$user_id <- as.integer(student.daily.counts$user_id)

## keep only rows (student - date combination) where ses_cnt is greater than zero
student.daily.counts <- student.daily.counts %>%
  filter(ses_cnt > 0)

## for each date, compute the number of students  
dates <- unique(student.daily.counts$date)
stud.per.day <- matrix(nrow = length(dates), ncol = 2)
for(d in 1:length(dates)) {
  stud.cnt <- student.daily.counts %>% filter(date==dates[d]) %>% nrow()
  stud.per.day[d,] <- c(dates[d], stud.cnt)  
}
stud.per.day.df <- data.frame(stud.per.day, stringsAsFactors = F)
colnames(stud.per.day.df) <- c('date', 'stud_cnt')  
stud.per.day.df$stud_cnt <- as.integer(stud.per.day.df$stud_cnt)
stud.per.day.df$stud_prop <- stud.per.day.df$stud_cnt / length(unique(student.daily.counts$user_id))

## plot it
ggplot(data = stud.per.day.df, aes(x = date, y = stud_cnt, group = 1)) +
  geom_line() +
  ylab("Proportion of active students") +
  theme_minimal() +
  theme(axis.text.x = element_blank())


##########################################################################
# create heat maps that show the number of active days per week for 
# each student and each week of the course
#
# since there are too many students to present them all on one heat map,
# create several maps, each for different cohort of students 
##########################################################################

weekly.counts <- read.csv("Intermediate_results/regularity_of_study/weekly_counts_of_daily_logins.csv")
weekly.counts <- weekly.counts %>%
  select(user_id, c(72:82))
str(weekly.counts)
colnames(weekly.counts)[2:11] <- paste0('W',c(2:5,7:12)) 

## order the data based on the total number of active days
wcount.sorted <- weekly.counts[order(weekly.counts$tot_cnt, decreasing = T),]
head(wcount.sorted)

## required for plotting
wcount.sorted$user_id <- as.factor(wcount.sorted$user_id)
wcount.sorted$user_id <- with(wcount.sorted, reorder(user_id, tot_cnt))

## since there are too many students (486), if all are presented, the heat map is
## barely legible; so, draw a map using top 50 active and least active 50

## transform the data from wide to long format
top50.long <- gather(data = wcount.sorted[1:50,], key = week, value = week_cnt, 
                          ... = W2:W12, factor_key=TRUE)

hm.top50 <- plot_active_days_heatmap(top50.long)
hm.top50 <- hm.top50 + ggtitle("Weekly counts of active days for 50 most active students")
hm.top50

last50.long <- gather(data = wcount.sorted[436:486,], key = week, value = week_cnt, 
                     ... = W2:W12, factor_key=TRUE)

hm.last50 <- plot_active_days_heatmap(last50.long)
hm.last50 <- hm.last50 + ggtitle("Weekly counts of active days for 50 least active students")
hm.last50

## create the same kind of plots but for best and worst performing students

## retrieve students' exam scores
exam.scores <- read.csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
str(exam.scores)

## merge weekly counts data with exam scores
counts_and_scores <- merge(x = weekly.counts, y = exam.scores[,-2], 
                          by.x = 'user_id', by.y = 'USER_ID',
                          all.x = T, all.y = F)
summary(counts_and_scores)
# scores are missing for 9 students; remove them
counts_and_scores <- counts_and_scores %>% filter( !is.na(SC_FE_TOT) )

## identify the best and worst performing students
source(file = "util_functions.R")
stud.perf.groups <- high.and.low.achievers(counts_and_scores)
## take 20 top and 20 bottom percentile
top20p.perf <- stud.perf.groups$top20
worst20p.perf <- stud.perf.groups$worst20

## plot data for students in the top 20 percentile of the exam scores  
top20p_long <- counts_and_scores %>% 
  select(-tot_cnt) %>% 
  filter(user_id %in% top20p.perf) %>%
  mutate(user_id = factor(user_id)) %>%
  mutate(user_id = reorder(user_id, SC_FE_TOT)) %>%
  gather(key = week, value = week_cnt, W2:W12, factor_key = TRUE)
  
hm.top20perc <- plot_active_days_heatmap(top20p_long)
hm.top20perc <- hm.top20perc + 
  ggtitle(paste0("Weekly counts of active days for students (", 
                 length(unique(top20p_long$user_id)),
                ") with exam scores in top 20 percentile"))
hm.top20perc

## plot data for students in the bottom 20 percentile of the exam scores
bottom_20p_long <- counts_and_scores %>% 
          select(-tot_cnt) %>% 
          filter(user_id %in% worst20p.perf) %>%
          mutate(user_id = factor(user_id)) %>%
          mutate(user_id = reorder(user_id, SC_FE_TOT)) %>%
          gather(key = week, value = week_cnt, W2:W12, factor_key = TRUE)

hm.bottom20perc <- plot_active_days_heatmap(bottom_20p_long)
hm.bottom20perc <- hm.bottom20perc + 
  ggtitle(paste0("Weekly counts of active days for students (", 
                 length(unique(bottom_20p_long$user_id)),
                 ") with exam scores in the bottom 20 percentile"))
hm.bottom20perc


##########################################################################
# examine the presence of association / correlation between indicators
# based on student daily engagement and exam scores
##########################################################################

weekly.counts <- read.csv("Intermediate_results/regularity_of_study/weekly_counts_of_daily_logins.csv")
weekly.counts <- weekly.counts %>%
  select(user_id, c(72:84))
str(weekly.counts)

## load also data about time gaps between consecutive active days
time.gaps <- read.csv("Intermediate_results/regularity_of_study/gaps_between_consecutive_logins.csv")

counts <- merge(x = weekly.counts, y = time.gaps, by = 'user_id', all = T)

exam.scores <- read.csv(file = "Intermediate_results/exam_scores_with_student_ids.csv")
str(exam.scores)

## merge weekly counts data with exam scores
counts_and_scores <- merge(x = counts, y = exam.scores[,-2], 
                           by.x = 'user_id', by.y = 'USER_ID',
                           all.x = T, all.y = F)
summary(counts_and_scores)
counts_and_scores <- counts_and_scores %>% filter( is.na(SC_FE_TOT)==FALSE )

## check for normality of variables
apply(counts_and_scores[,-1], 2, shapiro.test)
# except for tot_cnt, no other variable is normally distributed -> use Spearman corr.

## use the ggcorr f. for computing and plotting the correlations
## https://briatte.github.io/ggcorr/
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")

ggcorr(counts_and_scores[,-c(1,12,15,17)], method = c("complete","spearman"), 
       #      geom = "circle", min_size = 0, max_size = 15,
       label = TRUE, label_size = 3.5,
       hjust = 0.85, size = 4, layout.exp = 1)


#########################
## UTILITY FUNCTIONS
#########################

plot_active_days_heatmap <- function(weekly_counts_long) {
  require(ggplot2)
  
  hm <- ggplot(weekly_counts_long, aes(week, user_id)) + 
            geom_tile(aes(fill = week_cnt), colour = "white") + 
            scale_fill_gradient(name = "Active days\nper week",
                               low = "white", high = "steelblue") +
            ylab("Student IDs") + 
            xlab("\nCourse weeks (minus exam preparation weeks)") +
            theme_grey()
  
  hm
}