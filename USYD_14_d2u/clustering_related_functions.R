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
#    'ACE') # number of expanded activities

## f. for creating a feature set to be used for clustering
create.feature.set <- function(course_week, event_counts_file, 
                               scores_file, score_columns) {
  
  ## Load the events data
  source_events <- read.csv(paste("datasets/", event_counts_file, sep = ""))
  
  columns_to_select = c('user_id', relevant_columns)
  ## Pick up only the chosen set of columns, i.e., the columns listed 
  ## in the above given relevant_columns variable
  source_events <- source_events[, names(source_events) %in% columns_to_select]
  
  ## Load all the scores
  scores <- read.csv(paste('datasets/', scores_file, sep = ''))
  ## keep only the relevant score
  scores <- scores[, names(scores) %in% c('user_id', score_columns)]
  
  ## Merge scores with event counts
  features <- merge(source_events, scores, by = 'user_id', all.x = T)
  
  ## additional events that include:
  ## ORG - Access to the schedule and the learning objective pages
  ## DBOARD - Access to the dashboard screen
  ## HOF - Access to the pages showing the Hall of Fame
  additional.events <- read.csv(file = "Intermediate_files/org_dboad_weekly_counts.csv")
  additional.events <- additional.events[ additional.events$week == course_week, ]
  colnames(additional.events)[2:3] <- c('ORG.VIEW', 'DBOARD.VIEW')
  
  ## Add additonal events to the features set
  features <- merge(x = features, y = additional.events, by='user_id', all.x = T, all.y = F)
  
  features
  
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


## A new feature set:
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
## -	HOF.VIEW <- EXCLUDED
## -  GDEN

## f. for creating this new feature set starting from the features created by 
## the create.feature.set f.
create.new.feature.set <- function(course_week, event_counts_file, 
                                   scores_file, score_columns,
                                   trace.data) {
  
  initial.features <- create.feature.set(course_week, event_counts_file, scores_file, score_columns)
  
  ## add also the density of the transition matrix as an additional feature
  trace.activities = unique(trace.data$ACTIVITY)
  weekly.traces <- trace.data[trace.data$WEEK == course_week,]
  graph.densities <- compute.transition.matrix.density(weekly.trace.data = weekly.traces, 
                                                       activities = trace.activities)
  features <- merge(x = initial.features, y = graph.densities, 
                    by='user_id', all.x = T, all.y = T)
  ## for those users where GDEN could not be computed, set 0 for GDEN value
  ## instead of NA, to prevent errors when computing clusters
  for(i in 1:nrow(features)) {
    if (is.na(features$GDEN[i])) features$GDEN[i] <- 0
  }
  
  new.features <- as.data.frame(cbind(user_id = features$user_id,
                                      EQT.TOT = (features$EQT.CO + features$EQT.IN),
                                      EQT.DIFF = (features$EQT.CO - features$EQT.IN),
                                      EQT.SH = features$EQT.SH,
                                      EXC.TOT = (features$EXC.CO + features$EXC.IN),
                                      EXC.DIFF = (features$EXC.CO - features$EXC.IN),
                                      VEQ.TOT = (features$VEQ.CO + features$VEQ.IN),
                                      VEQ.DIFF = (features$VEQ.CO - features$VEQ.IN),
                                      VEQ.SH = features$VEQ.SH,
                                      VID.TOT = (features$VID.PL + features$VID.PA),
                                      ORG.VIEW = features$ORG.VIEW,
                                      DBOARD.VIEW = features$DBOARD.VIEW,
 ##                                     HOF.VIEW = features$HOF.VIEW,
                                      GDEN = features$GDEN))  
  
  new.features <- as.data.frame(cbind(new.features, 
                                      features[, names(features) %in% score_columns]))
  
  new.features
}

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}


do.hclustering <- function(feature.set, hc.method) {
  
  # first, normalize the feature set
  norm.features <- as.data.frame( apply(X = feature.set, 
                                        MARGIN = 2, 
                                        FUN = normalize.feature) )
  
  # next, do the clustering 
  # compute the distance between the observations
  distance <- dist(norm.features)
  # use the computed distances to do the clustering
  hc <- hclust(distance, method = hc.method)
  # plot the clustering tree
  plot(hc, cex=0.69, main = "Student clustering")
  
  hc
}

do.kmeans.clustering <- function(feature.set, k.min, k.max) {
  
  # first, normalize the feature set
  norm.features <- as.data.frame( apply(X = feature.set, 
                                        MARGIN = 2, 
                                        FUN = normalize.feature) )
  
  eval.metrics <- data.frame()
  
  # run kmeans for all K values in the range k.min:k.max
  set.seed(123)
  for(i in k.min:k.max){
    km.res <- kmeans(x=norm.features, centers=i, iter.max=15, nstart = 100)
    # combine cluster number and the error measure, write to df
    eval.metrics <- rbind(eval.metrics, c(i, km.res$tot.withinss, km.res$betweenss/km.res$totss)) 
  }
  names(eval.metrics) <- c("cluster", "tot.within.ss", "ratio")
  print(eval.metrics)
  
  # draw the Elbow plot
  require("ggplot2")
  ggplot(data=eval.metrics, aes(x=cluster, y=tot.within.ss, group=1)) + 
    theme_bw(base_family="Garamond") + 
    geom_line(colour = "darkgreen") +
    theme(text = element_text(size=20)) +
    ggtitle("Reduction In Error For Values of 'k'\n") +
    xlab("\nClusters") + 
    ylab("Within Cluster Sum of Squares\n") +
    scale_x_continuous(breaks=seq(from=0, to=10, by=1))
  
}

do.kmedoids.clustering <- function(feature.set, k.min, k.max) {
  
  # first, normalize the feature set
  norm.features <- as.data.frame( apply(X = feature.set, 
                                        MARGIN = 2, 
                                        FUN = normalize.feature) )
  require('fpc')
  
  # partitioning around medoids with estimation of the optimal number of clusters
  pam.res <- pamk(norm.features, krange = c(k.min:k.max), metric="euclidean", diss = F)
  # number of clusters identified as the best
  print(pam.res$nc)
  
  # useful results are, in fact, stored in the pamobject element
  pam.res <- pam.res$pamobject
  
  # print the distribution of instances across the clusters
  print(table( pam.res$clustering ))
  
  pam.res
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
