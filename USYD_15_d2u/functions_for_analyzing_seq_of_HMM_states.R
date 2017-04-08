
## the f. computes the number of sessions per student
## it assumes that student id is represented with the variable STUDENT.ID
## and session id with variable SESSION.ID
get.session.count <- function(trace.data) {
  stud.ids <- unique(trace.data$STUDENT_ID)
  stud.sessions <- list()
  for(s in 1:length(stud.ids)) {
    stud.sessions[[s]] <- unique(trace.data$SESSION_ID[trace.data$STUDENT_ID==stud.ids[s]])
  }
  session.count <- sapply(X = stud.sessions, FUN = length)
  session.count
}

## the f. computes features for each study session made by each student;
## the feature set consists of 7 features: FA_PERC, FA_CO_PERC, SA_PERC, 
## SA_CO_PERC, VID_PERC, READ_PERC, METACOG_PERC  
## the input includes: 1) a vector of the session identifiers, and 
## 2) a data frame of trace data sorted based on the student and timestamp
## it returs a dataframe with the feature values, plus the session id (the 1st column)
compute.stud.session.features <- function(stud.session.data, sorted.traces) {
  feature.data <- list()
  for(s in 1:length(stud.session.data)) {
    stud.session <- stud.session.data[s]
    session.data <- subset(sorted.traces, SESSION_ID==stud.session)
    n.actions <- nrow(session.data)
    n.fa <- nrow(session.data[session.data$ACTION %in% c("FA_CO", "FA_IN", "FA_SR"),])
    n.sa <- nrow(session.data[session.data$ACTION %in% c("SA_CO", "SA_IN"),])
    FA_PERC <- n.fa/n.actions
    FA_CO_PERC <- 0
    FA_SR_PERC <- 0
    if (n.fa > 0) {
      FA_CO_PERC <- nrow(session.data[session.data$ACTION=="FA_CO",])/n.fa
    } 
    SA_PERC <- n.sa/n.actions
    SA_CO_PERC <- 0
    if (n.sa > 0) {
      SA_CO_PERC <- nrow(session.data[session.data$ACTION=="SA_CO",])/n.sa  
    }
    VID_PERC <- nrow(session.data[session.data$ACTION=="VIDEO_PL",])/n.actions
    READ_PERC <- nrow(session.data[session.data$ACTION=="CONTENT_ACCESS",])/n.actions
    METACOG_PERC <- nrow(session.data[session.data$ACTION %in% c("MC_ORIENT", "MC_EVAL"),])/n.actions
    
    feature.data[[s]] <- c(stud.session, FA_PERC, FA_CO_PERC, SA_PERC, SA_CO_PERC, 
                           VID_PERC, READ_PERC, METACOG_PERC)
  }
  
  feature.matrix <- matrix(data = unlist(feature.data), 
                           nrow = length(feature.data), ncol = 8, byrow = T) 
  feature.df <- as.data.frame(feature.matrix)
  colnames(feature.df) <- c('SESSION_ID', 'FA_PERC', 'FA_CO_PERC', 'SA_PERC', 
                            'SA_CO_PERC', 'VID_PERC', 'READ_PERC', 'METACOG_PERC')
  feature.df
}

## the f. computes 'ntimes' attribute required for the depmix function
## the f. assumes that the input df (feature.data) is sorted based on STUDENT_ID
compute.ntimes <- function(feature.data) {
  ntimes <- vector()
  k <- 1
  i <- 1
  while(i <= nrow(feature.data)) {
    current.student <- feature.data$STUDENT_ID[i]
    count <- 1
    if ( i == nrow(feature.data)) {
      ntimes[k] <- count
      break
    }
    same.student <- T
    j <- i + 1
    while( same.student == T & j <= nrow(feature.data)) {
      if ( feature.data$STUDENT_ID[j] == current.student ) { 
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

## the f. adds the ntimes vector to the feature data, by associating each 
## observation related to a particular student with the number of observations
## (ntimes) available for that student (students are identified via STUDENT_ID)
add.ntimes.feature <- function(feature.data, ntimes.vector) {
  
  seq.lengths <- as.data.frame( cbind(stud.id = unique(feature.data$STUDENT_ID),
                                      length = ntimes.vector))
  feature.data$ntimes <- vector(mode = "integer", length = nrow(feature.data))
  for (i in 1:nrow(feature.data)) {
    feature.data$ntimes[i] <- 
      seq.lengths[ seq.lengths$stud.id == feature.data$STUDENT_ID[i], 2]  
  }
  feature.data
  
}

## the f. discretizes the given feature set, as the feature values in their  
## 'original' form (continuous data not normally distributed) cannot be used with depmix
## The discretization is done using 10 equal intervals, thus generating 10 possible values
## for each feature; in addition, zero is added as the 11th value as those values that 
## were originally zero (indicating the total absence of certain type of learning action 
## within a session) should remain zero also in the discretized dataset
discretize.features <- function(features.data) {
  require(infotheo)
  
  discret.features <- as.data.frame(apply(X = features.data[,c(3:9)], MARGIN = 2, 
                                          FUN = discretize, disc = "equalwidth", nbins = 10))
  colnames(discret.features) <- c('FA_PERC', 'FA_CO_PERC', 'SA_PERC', 
                                  'SA_CO_PERC', 'VID_PERC', 'READ_PERC', 'METACOG_PERC')
  discret.features <- as.data.frame(cbind(features.data[,c(1,2,10)], discret.features))
  discret.features <- discret.features[,c(1,2,4:10,3)]
  
  ## those feature values that are originaly zero should be set to zero 
  ## also in the discretized dataset
  for(i in 1:nrow(discret.features)) {
    for(j in 3:9) { # features are in the columns 3:9
      if (features.data[i,j]==0) discret.features[i,j] <- 0
    }
  }
  discret.features
}

## the f. turns all the features into factor variables and 
## stores them in a file with the given path
factorize.and.store <- function(discrete.features, f.path) {
  
  final.features <- as.data.frame(apply(X = discrete.features[,c(3:9)], MARGIN = 2, FUN = factor))
  final.features <- as.data.frame(cbind(discrete.features[,c(1,2,10)], final.features))
  final.features <- final.features[,c(1,2,4:10,3)]
  
  ## save the feature set
  saveRDS(object = final.features, 
          file = paste0(f.path,".RData"))
  
  final.features
}

## the f. computes several HMM models, with ns=>min.ns & ns<=max.ns
## and produces a table with evaluation metrics
compare.models <- function(feature.data, min.ns=2, max.ns=6, seed) {
  eval.metrics <- data.frame()
  for (ns in min.ns:max.ns) {
    mod.fit <- fit.HMM(feature.data, ns, seed)
    metrics <- c(ns, AIC(mod.fit), BIC(mod.fit), logLik(mod.fit))
    eval.metrics <- as.data.frame( rbind(eval.metrics, metrics) )
  }
  colnames(eval.metrics) <- c('n.states', 'AIC', 'BIC', 'logLik')
  require(knitr)
  kable(x = eval.metrics)
}

## the f. fits an HMM with the given feature data and the given number of states
fit.HMM <- function(feature.data, ns, seed) {
  set.seed(seed)
  mod <- depmix(response = list(FA_PERC ~ 1, FA_CO_PERC ~ 1, 
                                SA_PERC ~ 1, SA_CO_PERC ~ 1, 
                                VID_PERC ~ 1, READ_PERC ~ 1, METACOG_PERC ~ 1),
                data = feature.data, 
                nstates = ns, 
                family = list(multinomial("identity"), multinomial("identity"), 
                              multinomial("identity"), multinomial("identity"), 
                              multinomial("identity"), multinomial("identity"), multinomial("identity")))
  
  mod.fit <- fit(mod, verbose = FALSE)
  mod.fit
}


## the f. plots for each student percentages of sessions per each state
plot.session.percents <- function(states.perc.long, n.states, c.pallet) {
  
  require(ggplot2)
  ggplot(data=states.perc.long, aes(x=STUDENT_ID, y=percent, fill=state)) +
    geom_bar(stat="identity") +
    ylab("Percent of student sessions") + 
    xlab("Students") +
    scale_fill_manual(values = c.pallet, 
                      breaks=paste0("ST", 1:n.states, "PERC"),
                      labels=paste0("S",1:n.states)) +
    scale_x_discrete(breaks=NULL) +
    theme_bw() +
    theme(panel.border = element_blank(),
          legend.title = element_text(size=11, face="bold"),
          legend.text = element_text(size = 10),
          legend.key = element_rect(colour = NA))
  
}


## the f. creates a data frame with the number of sessions (SESSION_CNT)
## for each student (STUDENT_ID)
compute.seqence.length <- function(stud.data) {
  stud.ids <- unique(stud.data$STUDENT_ID)
  n.stud <- length(stud.ids)
  seq.len <- list()
  for(s in 1:n.stud) {
    s.cnt <- nrow(stud.data[stud.data$STUDENT_ID==stud.ids[s],])
    seq.len[[s]] <- c(stud.ids[s],s.cnt)
  }
  seq.len.df <- as.data.frame(matrix(unlist(seq.len), 
                                     nrow = n.stud, ncol = 2, byrow = T))
  colnames(seq.len.df) <- c("STUDENT_ID", "SESSION_CNT")
  seq.len.df
} 


## the f. first creates a new sequence list where all sequences will be
## of the same length - max.length - by extending shorter sequences with NAs
## then, this list is transformed into a dataframe, as required for the 
## creation of sequences in TraMineR
create.equal.length.seq <- function(sequences, max.length) { 
  eq.len.sequences <- list()
  for(i in 1:length(sequences)) {
    if ( length(sequences[[i]]) < max.length ) {
      d <- max.length - length(sequences[[i]])
      eq.len.sequences[[i]] <- c(sequences[[i]], rep(NA, times=d))
    } else {
      eq.len.sequences[[i]] <- sequences[[i]]
    }
  }
  ## transform the eq.len.sequences list into a dataframe
  ## from: http://stackoverflow.com/questions/4227223/r-list-to-data-frame?lq=1
  seq.df <- data.frame(matrix(unlist(eq.len.sequences), 
                              nrow=length(eq.len.sequences), byrow=T),
                       stringsAsFactors=FALSE)
  seq.df
}


## the f. create sequences of states in the format required by the TraMineR toolkit;
## it stores the created sequences in 2 different formats: a TraMineR's native format
## and as a regular data frame; the directory to store the files and the base for creating
## file names are provided as the 2nd input argument
create.state.seq <- function(seq.data, f.path) {
  
  sequence.list <- list()
  n.stud <- length(unique(seq.data$STUDENT_ID))
  for(i in 1:n.stud) {
    stud.states <- subset(seq.data, 
                          STUDENT_ID==unique(seq.data$STUDENT_ID)[i], 
                          select = state)
    stud.states <- as.vector(stud.states$state)
    stud.states.new <- vector()
    current.state <- stud.states[1]
    st.cnt <- 1
    for(j in 2:length(stud.states)) {
      if (current.state==stud.states[j]) {
        st.cnt <- st.cnt + 1
      } else {
        stud.states.new <- c(stud.states.new, paste0(current.state,"/",st.cnt))
        current.state <- stud.states[j]
        st.cnt <- 1
      }
      if (j == length(stud.states)) 
        stud.states.new <- c(stud.states.new, paste0(current.state,"/",st.cnt))
    }
    sequence.list[[i]] <- stud.states.new
  }
  
  ## compute length of the sequences; required for finding the sequence with 
  ## the max length, where the length isconsidered in terms of the <STATE/OCCURRENCE_CNT> elements
  seq.length <- sapply(X = sequence.list, FUN = function(x) length(x))
  ## make a list where all the sequences are of the same length (which is the length
  ## of the longest sequence)
  eq.len.seq.df <- create.equal.length.seq(sequence.list, max(seq.length))
  ## add student ids
  eq.len.seq.df <- as.data.frame(cbind(STUDENT_ID=unique(seq.data$STUDENT_ID), 
                                       eq.len.seq.df))
  ## create sequence out of the data frame
  require(TraMineR)
  traminer.seq <- seqdef(data = eq.len.seq.df, var = 2:ncol(eq.len.seq.df), 
                         informat = "SPS", SPS.in = list(xfix = "", sdsep = "/"))
  ## store the sequence data
  saveRDS(object = traminer.seq, 
          file = paste0(f.path,"_SPS_format.RData"))
  ## store the created sequences
  saveRDS(object = eq.len.seq.df, 
          file = paste0(f.path, "_df.RData"))
  # return TraMineR seq object
  traminer.seq
}


compute.cost.matrix <- function(state.trans.matrix, n.state=4) {
  cost.matrix <- matrix(data = rep(2,(n.state*n.state)), 
                        nrow = n.state, ncol = n.state, byrow = T)
  for(i in 1:nrow(cost.matrix)) {
    for(j in 1:ncol(cost.matrix))
      if (i==j) 
        cost.matrix[i,j] <- 0  ## all elements on the diagonal must be zero (which makes sense)
      else
        cost.matrix[i,j] <- cost.matrix[i,j] - state.trans.matrix[i,j] - state.trans.matrix[j,i]
  }
  cost.matrix
}


## the f. receives: (hierarchical) clustering model, number of clusters, and a 
## colors pallet to use when plotting distribution plots for the clusters
examine.clusters <- function(cl.mod, n.clust, custom.pallet) {
  clusters <- cutree(cl.mod, k = n.clust)
  print(table(clusters))
  ## plot the state distribution for each cluster
  clusters.fac <- factor(clusters, labels = paste("cl:",1:n.clust))
  seqdplot(seqdata = traminer.seq, group = clusters.fac, #axes = "bottom", cex.plot = 0.5, 
           title="State dist.", withlegend=F, cpal = custom.pallet)
  seqlegend(traminer.seq, fontsize = 0.65, cpal=custom.pallet)
  return(clusters)
}


## the f. does pairwise cluster comparison w.r.t. the students' exam scores
## by applying Mann Whitney U test
## it also applies the FDR correction required when doing multiple comparison  
pairwise.exam.compare <- function(nclust, ncomparison, clust.and.scores, exam) {
  if ( !(exam %in% c("MT", "FE")) ) {
    print("The last argument must be either MT or FE")
    return(NA)
  }
  comparison <- matrix(nrow = ncomparison, ncol = 5, byrow = T)
  k <- 1
  for(i in 1:(nclust-1)) {
    for(j in (i+1):nclust) {
      if (exam == "MT") 
        comparison[k,] <- c(i, j, compare.midterm.scores.Mann.Whitney.test(clust.and.scores, i, j))
      else 
        comparison[k,] <- c(i, j, compare.fexam.scores.Mann.Whitney.test(clust.and.scores, i, j))
      k <- k+1
    }
  }
  comparison.df <- as.data.frame(comparison)
  colnames(comparison.df) <- c('c1', 'c2', 'Z', 'p', 'r')
  comparison.df <- apply.FDR.correction(comparison.df)
  comparison.df
}


## the f. creates a data frame that for each student contains student id, state counts, 
## and the total number of states (ie sessions)
compute.state.dist.per.student <- function(state.data, n.states) {
  
  n.stud <- length(unique(state.data$STUDENT_ID))
  stud.states <- matrix(data = rep(0, n.states*n.stud), 
                        nrow = n.stud, ncol = n.states,
                        dimnames = list(unique(state.data$STUDENT_ID), 
                                        paste0("ST",1:n.states,"_CNT")))
  for(i in 1:n.stud) {
    stud.data <- subset(state.data, STUDENT_ID==rownames(stud.states)[i])
    for(j in 1:n.states)
      stud.states[i,j] <- nrow(stud.data[stud.data$state==j,])
  }
  
  stud.states.df <- as.data.frame(stud.states, stringsAsFactors = F)
  stud.states.df$STUDENT_ID <- as.integer(rownames(stud.states))
  stud.states.df <- stud.states.df[,c(n.states+1,1:n.states)]
  stud.states.df$ST_TOTAL <- as.vector(apply(stud.states.df[,c(2:(n.states+1))], 1, sum))
  
  stud.states.df
}


merge.state.dist.and.clusters <- function(state.data, cluster.data, n.states) {
  
  stud.states.df <- compute.state.dist.per.student(state.data, n.states)
  
  stud.states.clust <- merge(x = stud.states.df, y = cluster.data, 
                             by = "STUDENT_ID", all.x = F, all.y = T)
  stud.states.clust
}

