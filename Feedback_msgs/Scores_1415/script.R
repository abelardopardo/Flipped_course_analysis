---
title: "Comparison of student midterm and final exam scores 2014/15"
author: "Abelardo Pardo"
date: "7 December 2015"
output: html_document
---
  
```{r setup, echo = FALSE, results = 'hide', messages = FALSE, warning = FALSE}
library(ggplot2)
library(plyr)

cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
  return(cd)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```

Compare Midterm Scores for 2014 and 2015
=========================================

```{r}
data <- read.csv('scores.csv', header = TRUE, sep = ",")
summary(data)

midterm_14 <- data[!is.na(data$X2014_MD),c('X2014_MD')]
final_mcq_14 <- data[!is.na(data$X2014.FMCQ),c('X2014.FMCQ')]
final_q1_14 <- data[!is.na(data$X2014.FQ1),c('X2014.FQ1')]
final_q2_14 <- data[!is.na(data$X2014.FQ21),c('X2014.FQ21')] +
  data[!is.na(data$X2014.FQ21),c('X2014.FQ21')]
final_q3_14 <- data[!is.na(data$X2014.FQ3),c('X2014.FQ3')]
final_q4_14 <- data[!is.na(data$X2014.FQ4),c('X2014.FQ4')]

midterm_15 <- data[!is.na(data$X2015_MD),c('X2015_MD')]
final_mcq_15 <- data[!is.na(data$X2015.FMCQ),c('X2015.FMCQ')]
final_q1_15 <- data[!is.na(data$X2015.FQ1),c('X2015.FQ1')]
final_q2_15 <- data[!is.na(data$X2015.FQ2),c('X2015.FQ2')]
final_q4_15 <- data[!is.na(data$X2015.FQ4),c('X2015.FQ4')]

#
# T.test for midterm score
# 
t.test(midterm_14, midterm_15, alternative = 'less')
wilcox.test(midterm_14, midterm_15, alternative = 'less')
cd <- cohens_d(midterm_14, midterm_15)

#
# Plot them: http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
#

# Create single data frame with column distinguishing years
df <- data.frame(Midterm = midterm_14, Year = '14')
df <- rbind(df, data.frame(Midterm = midterm_15, Year = '15'))

cdat <- ddply(df, "Year", summarise, Midterm.mean=mean(Midterm))
p1 <- ggplot(df, aes(x = Midterm, fill = Year)) + geom_density(alpha = .3) +
      geom_vline(data = cdat, aes(xintercept = Midterm.mean, colour = Year),
                 linetype="dashed", size=1)
  
df2 <- data.frame(Final = final_mcq_14 + final_q1_14 + final_q2_14 + final_q4_14,
                 Year = '14')
df2 <- rbind(df2,
            data.frame(Final = final_mcq_15 + final_q1_15 + final_q2_15 + 
                         final_q4_15, Year = '15'))

cdat2 <- ddply(df2, "Year", summarise, Final.mean=mean(Final))
p2 <- ggplot(df2, aes(x = Final, fill = Year)) + geom_density(alpha = .3) +
      geom_vline(data = cdat2, aes(xintercept = Final.mean, colour = Year),
             linetype="dashed", size=1)
multiplot(p1, p2, cols=2)
