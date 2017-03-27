##################################################################################
## COMPUTE STUDENTS' LEVEL OF ENGAGEMENT WITH WEEKLY CLASS PREPARATION ACTIVITIES
## FOR EACH WEEK AND EACH STUDENT, COMPUTE THE FREQUENCY OF THE FOLLOWING ACTIONS:
## - FORMATIVE ASSESSMENT: MCQ_CO, MCQ_IN, MCQ_SR
## - SUMMATIVE ASSESSMENT: EXE_CO, EXE_IN
## - VIDEO WATCHING: VIDEO_PLAY
## - ACCESS TO COURSE MATERIALS: CONTENT_ACCESS
## - METACOGNITIVE: DBOARD, ORIENT
##################################################################################

## load the data
traces <- read.csv(file = "Intermediate_files/trace_data_with_sessions_w0-16.csv",
                   stringsAsFactors = F)
str(traces)
colnames(traces)[c(1,4)] <- c('SESSION.ID', 'STUD.ID')

## restrict sessions to those that took place during the weeks 2-13 
traces <- subset(traces, traces$WEEK %in% c(2:13))
table(traces$WEEK)

table(traces$ACTIVITY)
## substitute DBOARD_ACCESS with MC_EVAL, also ORIENT with MC_ORIENT
traces$ACTIVITY[traces$ACTIVITY=='DBOARD_ACCESS'] <- 'MC_EVAL'
traces$ACTIVITY[traces$ACTIVITY=='ORIENT'] <- 'MC_ORIENT'
table(traces$ACTIVITY)

## f. that, for the given week, computes the number of
## the different kinds of learning actions for each student
weekly.action.counts <- function(trace.data, week) {
  students <- unique(trace.data$STUD.ID)
  actions.list <- list()
  for(i in 1:length(students)) {
    stud.traces <- subset(trace.data, STUD.ID==students[i] & WEEK==week)
    mcq_co <- nrow(stud.traces[stud.traces$ACTIVITY=="MCQ_CO",])
    mcq_in <- nrow(stud.traces[stud.traces$ACTIVITY=="MCQ_IN",])
    mcq_sr <- nrow(stud.traces[stud.traces$ACTIVITY=="MCQ_SR",])
    exe_co <- nrow(stud.traces[stud.traces$ACTIVITY=="EXE_CO",])
    exe_in <- nrow(stud.traces[stud.traces$ACTIVITY=="EXE_IN",])
    video <- nrow(stud.traces[stud.traces$ACTIVITY=="VIDEO_PLAY",])
    dboard <- nrow(stud.traces[stud.traces$ACTIVITY=="MC_EVAL",])
    orient <- nrow(stud.traces[stud.traces$ACTIVITY=="MC_ORIENT",])
    actions.list[[i]] <- c(students[i], mcq_co, mcq_in, mcq_sr, exe_co, exe_in, video, dboard, orient)
  }
  actions.df <- data.frame(matrix(unlist(actions.list), nrow=length(students), byrow=T))
  colnames(actions.df) <- c('STUD_ID', 'MCQ_CO', 'MCQ_IN', 'MCQ_SR', 'EXE_CO', 'EXE_IN',
                            'VIDEO_PL', 'MC_EVAL', 'MC_ORIENT')
  actions.df
}

## for each week, compute action frequencies and store the results in a file
for (w in 2:13) {
  week.actions <- weekly.action.counts(traces, w)
  saveRDS(object = week.actions, 
          file = paste0("Intermediate_files/weekly_action_counts/week", w, ".RData"))
}
